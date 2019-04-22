use cranelift::prelude::{
    AbiParam,
    FunctionBuilder,
    FunctionBuilderContext,
    InstBuilder,
    Value,
};
use cranelift_module::{
    DataContext,
    Linkage,
    Module,
};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};

use crate::ast::{
    Declaration,
    Declaration::FunctionDeclaration,
    Declarations,
    Expr,
    Expr::{FunctionCall, Str},
};

pub fn generate(declarations: Declarations) -> Result<*const u8, String> {
    let builder = SimpleJITBuilder::new();
    let mut module = Module::new(builder);
    let mut builder_context = FunctionBuilderContext::new();
    let mut context = module.make_context();
    let mut data_context = DataContext::new();
    let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let entry_block = builder.create_ebb();
    builder.append_ebb_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);
    let mut generator = Generator {
        builder,
        data_context: &mut data_context,
        module: &mut module,
    };
    generator.gen_declarations(declarations)?;
    generator.builder.ins().return_(&[]);
    generator.builder.finalize();

    let ident = module.declare_function("main", Linkage::Export, &context.func.signature) // TODO: use function name.
        .map_err(|error| error.to_string())?;
    module.define_function(ident, &mut context)
        .map_err(|error| error.to_string())?;
    module.clear_context(&mut context);
    module.finalize_definitions();

    Ok(module.get_finalized_function(ident))
}

struct Generator<'a> {
    builder: FunctionBuilder<'a>,
    data_context: &'a mut DataContext,
    module: &'a mut Module<SimpleJITBackend>,
}

impl<'a> Generator<'a> {
    fn gen_declaration(&mut self, declaration: Declaration) -> Result<(), String> {
        match declaration {
            FunctionDeclaration { body, .. } => {
                let _ = self.gen_expr(body)?;
            },
        }
        Ok(())
    }

    fn gen_declarations(&mut self, declarations: Declarations) -> Result<(), String> {
        for declaration in declarations {
            self.gen_declaration(declaration)?;
        }
        Ok(())
    }

    fn gen_expr(&mut self, expr: Expr) -> Result<Value, String> {
        let int = self.module.target_config().pointer_type();
        match expr {
            FunctionCall { arguments, .. } => {
                let mut signature = self.module.make_signature();
                for _argument in &arguments { // TODO: get type from argument.
                    signature.params.push(AbiParam::new(int));
                }
                signature.returns.push(AbiParam::new(int)); // TODO: set proper return type.
                let callee = self.module.declare_function("puts", Linkage::Import, &signature) // TODO: use real function name.
                    .expect("declare function");
                let local_callee = self.module.declare_func_in_func(callee, &mut self.builder.func);

                let mut argument_exprs = vec![];
                for argument in arguments {
                    argument_exprs.push(self.gen_expr(argument)?);
                }
                let call = self.builder.ins().call(local_callee, &argument_exprs);
                Ok(self.builder.inst_results(call)[0])
            },
            Str(string) => {
                let name = "__string"; // TODO: generate name.
                self.data_context.define(string.into_bytes().into_boxed_slice());
                let ident = self.module.declare_data(name, Linkage::Export, true)
                    .map_err(|error| error.to_string())?; // TODO: do not convert error to string.
                self.module.define_data(ident, &self.data_context)
                    .map_err(|error| error.to_string())?; // TODO: do not convert error to string.
                self.data_context.clear();
                self.module.finalize_definitions();

                let local_ident = self.module.declare_data_in_func(ident, &mut self.builder.func);
                Ok(self.builder.ins().symbol_value(int, local_ident))
            },
        }
    }
}
