use std::fs::File;
use std::str::FromStr;

use cranelift::prelude::{
    AbiParam,
    Configurable,
    FunctionBuilder,
    FunctionBuilderContext,
    InstBuilder,
    Value,
    isa,
    settings,
};
use cranelift_module::{
    DataContext,
    Linkage,
    Module,
};
use cranelift_faerie::{
    FaerieBackend,
    FaerieBuilder,
    FaerieTrapCollection,
};
use target_lexicon::triple;

use crate::ast::{
    Declaration,
    Declaration::FunctionDeclaration,
    Declarations,
    Expr,
    Expr::{FunctionCall, Str},
};

pub fn generate(declarations: Declarations) -> Result<String, String> {
    let object_file = "main.o".to_string();
    let mut flag_builder = settings::builder();
    flag_builder.enable("is_pic").expect("flag");
    let isa_builder = isa::lookup(triple!("x86_64-unknown-unknown-elf")).expect("isa");
    let isa = isa_builder.finish(settings::Flags::new(flag_builder));
    let builder = FaerieBuilder::new(isa, object_file.clone(), FaerieTrapCollection::Disabled,
        FaerieBuilder::default_libcall_names())
        .expect("builder");
    let mut module = Module::new(builder);
    let mut builder_context = FunctionBuilderContext::new();
    let mut context = module.make_context();
    let mut data_context = DataContext::new();
    for declaration in declarations {
        let mut generator = FunctionGenerator {
            builder: FunctionBuilder::new(&mut context.func, &mut builder_context),
            data_context: &mut data_context,
            module: &mut module,
        };
        generator.gen_declaration(declaration)?;

        let ident = module.declare_function("main", Linkage::Export, &context.func.signature) // TODO: use function name.
            .map_err(|error| error.to_string())?;
        module.define_function(ident, &mut context)
            .map_err(|error| error.to_string())?;
    }
    module.clear_context(&mut context);
    module.finalize_definitions();

    let object = module.finish();
    let file = File::create(object.name()).expect("file create");
    object.write(file).expect("error writing to file");

    Ok(object_file)
}

struct FunctionGenerator<'a> {
    builder: FunctionBuilder<'a>,
    data_context: &'a mut DataContext,
    module: &'a mut Module<FaerieBackend>,
}

impl<'a> FunctionGenerator<'a> {
    fn gen_declaration(&mut self, declaration: Declaration) -> Result<(), String> {
        match declaration {
            FunctionDeclaration { body, .. } => {
                let entry_block = self.builder.create_ebb();
                self.builder.append_ebb_params_for_function_params(entry_block);
                self.builder.switch_to_block(entry_block);
                self.builder.seal_block(entry_block);

                let _ = self.gen_expr(body)?;

                //if name == "main" { // TODO
                    let int = self.module.target_config().pointer_type();
                    let mut signature = self.module.make_signature();
                    signature.params.push(AbiParam::new(int));
                    let callee = self.module.declare_function("exit", Linkage::Import, &signature)
                        .expect("declare exit function");
                    let local_callee = self.module.declare_func_in_func(callee, &mut self.builder.func);

                    let argument_exprs = vec![self.builder.ins().iconst(int, 0)];
                    self.builder.ins().call(local_callee, &argument_exprs);
                //}

                self.builder.ins().return_(&[]);
                self.builder.finalize();
            },
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
