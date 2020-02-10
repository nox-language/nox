use std::rc::Rc;

use ast::{
    Declaration,
    Expr,
    ExprWithPos,
};
use env::{Entry, Env};
use position::WithPos;
use symbol::{Strings, SymbolWithPos};
use types::Type;

pub fn call_with_array_help(args: &[ExprWithPos], env: &Env, function: &SymbolWithPos, strings: &Rc<Strings>) -> Option<String> {
    if let Some(&Entry::Var { ref typ, .. }) = env.look_var(function.node) {
        if args.len() == 1 {
            if let Type::Array(_, _) = typ {
                if let Expr::Sequence(ref sequence) = args[0].node {
                    if let Expr::Decl(ref declaration) = sequence[0].node {
                        if let Declaration::Variable { ref init, .. } = declaration.node {
                            if let Expr::Array { ref init, size } = init.node {
                                if size == 1 {
                                    let name = strings.get(function.node).expect("function");
                                    let value = value_to_string(init);
                                    return Some(format!("try the array indexing syntax {}.[{}]", name, value));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

fn value_to_string(value: &WithPos<Expr>) -> String {
    match value.node {
        Expr::Int { value } => value.to_string(),
        _ => unimplemented!("{:?}", value.node),
    }
}
