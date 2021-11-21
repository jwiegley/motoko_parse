use crate::parse::*;
use std::collections::HashMap;

pub enum Type {
    Nat,
    Int,
    Float,
    Char,
    Text,
    List(Box<Type>),
    Func {
        args: Vec<Box<Type>>,
        ret: Box<Type>,
    },
}

pub enum Value {
    Nat(nat),
    Int(int),
    Float(float),
    Char(char),
    Text(text),
    List(Vec<Box<Value>>),
    Func(Box<dyn Fn(Vec<Box<Value>>) -> Box<Value>>),
}

pub struct Scope {
    pub vars: HashMap<id, Value>,
    pub parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn lookup(&self, name: &id) -> Option<&Value> {
        self.vars
            .get(name)
            .or_else(|| self.parent.as_ref().map(|s| s.lookup(name)).flatten())
    }
}

pub struct Env {
    current_scope: Scope,
}

pub enum EvalError {
    IdentifierNotDefined(id),
}

pub fn eval_id<'a>(env: &'a Env, id: &id) -> Result<&'a Value, EvalError> {
    env.current_scope
        .lookup(id)
        .ok_or(EvalError::IdentifierNotDefined(id.clone()))
}
