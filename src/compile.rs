use super::parse;
use std::collections::HashMap;
use crate::parse::{Definition, Function};
use crate::parse::ParseError::Expected;

type FunctionId = usize;


// TODO this class is redundant
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct File {
    pub content: Scope
}


// TODO this class is redundant
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Environment<'a> {
    pub scopes: Scopes<'a>
}

pub struct Scopes<'c> {
    pub current: &'c Scope,
    pub parent: Option<&'c Scopes<'c>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Scope {
    pub definitions: HashMap<String, Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Expression {
    Kind(Kind),
    Value(Value)
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Kind {
    Tuple (Vec<Kind>),
    Product (HashMap<String, Kind>),
    Sum (HashMap<String, Option<Kind>>),
    Function (Box<FunctionKind>),

    F64,
    String // TODO should not need a native type
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionKind {
    parameter: Kind,
    result: Kind,
}


#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Value {
    Function(FunctionValueDefinition),
    FunctionCall(FunctionValueCall),
    Tuple(Vec<Value>),
    Variant((String, Option<Box<Value>>)),
    Product(HashMap<String, Value>),

    String(String),
    F64(f64),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionValueCall {
    function: FunctionValueDefinition,
    argument: Value,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionValueDefinition {
    pub parameter_kind: Kind,
    pub result: Value
}


pub type CompileResult<'a, T> = std::result::Result<T, CompileError<'a>>;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum CompileError<'a> {
    InvalidNumber(parse::Text<'a>),
    InvalidBindingSyntax(parse::Expression<'a>),
    NestedBindingSyntaxForbidden(parse::Reference<'a>),
    InvalidTypeAnnotation(parse::Expression<'a>),
    TypeMismatch { annotated: Kind, found: Kind },
    ExpectedKindButFoundValue(&'a Expression),
    ExpectedValueButFoundKind(&'a Expression),
}




impl Value {
    pub fn kind(&self) -> Kind {
        match self {
            Value::F64(_) => Kind::F64,
            Value::String(_) => Kind::String,

            Value::Tuple(members) =>
                Kind::Tuple(members.iter().map(Value::kind).collect()),

            Value::Variant((variant, value)) =>
                Kind::Sum([(variant.clone(), value.map(Value::kind))].iter().collect()),

            Value::Product(members) =>
                Kind::Product(members.iter().map(|(name, value)|
                    (name, value.kind())
                ).collect()),

            Value::Function(function) => {
                Kind::Function(Box::new(FunctionKind {
                    parameter: function.parameter_kind.clone(),
                    result: function.result.kind()
                }))
            }

            Value::FunctionCall(call) => {
                call.function.result.kind()
            }
        }
    }
}


pub fn compile_file<'a>(
    file: parse::File,
    environment: &Environment
) -> CompileResult<File>
{
    let mut scope = Scope { definitions: HashMap::new() };

    for definition in file.definitions {
        let Definition { binding, kind, expression } = definition;
        use parse::Expression::*;

        match binding {
            Identifier(identifier) => {
                if identifier.len() == 1 {
                    let identifier = String::from(identifier[0]);

                    let compiled_value = compile_value(expression, environment)?;
                    let expression_kind = compiled_value.kind();

                    let desired_kind = kind
                        .map(|k| compile_kind(k, environment))
                        .transpose()?;

                    if let Some(desired_kind) = desired_kind {
                        if expression_kind != desired_kind {
                            return Err(CompileError::TypeMismatch {
                                annotated: desired_kind, found: expression_kind
                            })
                        }
                    }

                    scope.definitions.insert(identifier, Expression::Value(compiled_value));
                }
                else {
                    return Err(CompileError::NestedBindingSyntaxForbidden(identifier))
                }
            }

            FunctionApplication(_) => panic!("Type parameters not supported yet"),
            Tuple(_) | Product(_) | Sum(_) => panic!("destructuring not supported yet"),
            invalid => return Err(CompileError::InvalidBindingSyntax(invalid))
        };

    }

    Ok(File { content: scope })
}




pub fn compile_kind(expression: parse::Expression, environment: &Environment) -> CompileResult<Kind> {
    use parse::Expression::*;

    match expression {
        Tuple(members) => compile_tuple_kind(members, environment),
        Product(members) => compile_product_kind(members, environment),
        Sum(members) => compile_sum_kind(members, environment),
        Identifier(reference) => environment.resolve(reference.as_slice()),
        Function(function) => compile_function_kind(function, environment),
        invalid => Err(CompileError::InvalidTypeAnnotation(invalid))
    }
}


pub fn compile_tuple_kind(
    members: Vec<parse::Expression>, environment: &Environment
) -> CompileResult<Kind>
{
    let members: Result<Vec<Kind>, CompileError> = members.into_iter()
        .map(|member| compile_kind(member, environment))
        .collect();

    Ok(Kind::Tuple(members?))
}

pub fn compile_product_kind(
    members: Vec<(parse::Text, parse::Expression)>, environment: &Environment
) -> CompileResult<Kind>
{
    let members: Result<HashMap<String, Kind>, CompileError> = members.into_iter()
        .map(|(name, member)| (name, compile_kind(member, environment)))
        .collect();

    Ok(Kind::Product(members?))
}

pub fn compile_sum_kind(
    members: Vec<(parse::Text, Option<parse::Expression>)>, environment: &Environment
) -> CompileResult<Kind>
{
    let members: Result<HashMap<String, Option<Kind>>, CompileError> = members.into_iter()
        .map(|(name, member)| (
            name, member.map(|member| compile_kind(member, environment))
        ))
        .collect();

    Ok(Kind::Sum(members?))
}


pub fn compile_value(
    expression: parse::Expression, environment: &Environment
) -> CompileResult<Value>
{
    use parse::Expression::*;

    Ok(match expression {
        String(value) => Value::String(value.to_owned()),
        Number(value) => Value::F64(parse_f64(value)?),

        Identifier(reference) => panic!("needs a way to reference values?ß??"),
        Tuple(members) => compile_tuple_value(members, environment),

        Product(members) => Value::Product(
            members.into_iter().map(|(name, member)|
                (name, compile_value(member, environment))
            ).collect()
        ),

        Sum(members) => {
            if members.len() != 1 { Err(CompileError::ExpectedValueButFoundKind(Sum(members))) }
            else {

            }
        }

        FunctionApplication(call) => compile_function_call(call, environment),

        _ => panic!()
    })
}


pub fn compile_tuple_value(members: Vec<parse::Expression>, environment: &Environment) -> CompileResult<Value> {
    let members: Result<Vec<Value>, CompileError> = members
        .into_iter().map(|member| compile_value(member, environment))
        .collect();

    Ok(Value::Tuple(members?))
}

pub fn compile_product_value(members: Vec<(parse::Text, parse::Expression)>, environment: &Environment) -> CompileResult<Value> {
    let members: Result<HashMap<String, Value>, CompileError> = members
        .into_iter().map(|(name, member)| (name, compile_value(member, environment)))
        .collect();

    Ok(Value::Product(members?))
}

//pub fn compile_function_kind()


pub fn compile_function_call(
    expression: parse::FunctionApplication, environment: &Environment
) -> CompileResult<Value>
{

    // Ok(Value::FunctionCall(FunctionValueCall { function, argument }))
}

pub fn parse_f64(text: &str) -> CompileResult<f64> {
    text.parse::<f64>().map_err(|_| CompileError::InvalidNumber(text))
}



impl Environment {
    pub fn resolve(&self, reference: &[&str]) -> Option<&Expression> {
        self.scopes.resolve(reference)
    }

    pub fn enter_scope(&self, scope: &Scope) -> Environment {
        Environment { scopes: self.scopes.enter_scope(scope) }
    }
}

impl Scopes {
    pub fn resolve(&self, reference: &[&str]) -> Option<&Expression> {
        self.current.resolve(reference)
            .or(self.parent.and_then(|parent| parent.resolve(reference)))
    }

    pub fn enter_scope(&self, scope: &Scope) -> Scopes {
        Scopes {
            current: scope,
            parent: Some(self)
        }
    }
}


impl Scope {
    pub fn resolve(&self, reference: &[&str]) -> Option<&Expression> {
        reference.split_first().and_then(|(identifier, rest)| {
            if rest.is_empty() {
                self.definitions.get(identifier)
            }
            else {
                self.child_modules.get(identifier)
                    .and_then(|m| m.resolve(rest))
            }
        })
    }
}


impl Expression {
    pub fn as_kind(&self) -> CompileResult<&Kind> {
        match self {
            Expression::Kind(kind) => Ok(kind),
            _ => Err(CompileError::ExpectedKindButFoundValue(self))
        }
    }

    pub fn as_value(&self) -> CompileResult<&Value> {
        match self {
            Expression::Value(value) => Ok(value),
            _ => Err(CompileError::ExpectedValueButFoundKind(self))
        }
    }
}

/*
/// otherwise, is kind
pub fn is_value(expression: &parse::Expression) -> bool {
    use parse::Expression::*;
    match expression {
        String(_) => true,
        Number(_) => true,
        Identifier(names) => reference_is_value(names),
        Tuple(members) => is_value(&members[0]),
        Sum(members) => is_value(&members[0].1.as_ref().unwrap()),
        Product(members) => is_value(&members[0].1),
        _ => panic!()
    }
}

fn reference_is_value(reference: &parse::Reference) -> bool {
    match reference.last() {
        Some(name) => {
            match name.chars().next() {
                Some(first_letter) => !first_letter.is_uppercase(),
                None => panic!()
            }
        },

        None => panic!()
    }
}*/



#[cfg(test)]
mod test {
    #[test]
    fn test(){

    }
}























