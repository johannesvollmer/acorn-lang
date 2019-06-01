use super::parse;
use std::collections::HashMap;
use crate::parse::{Definition};

type FunctionId = usize;



#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Module {
    pub file: File,
    pub children: HashMap<String, Module>,
}

// TODO this class is redundant
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct File {
    pub content: Scope
}

// TODO this class is redundant
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Environment<'s, 'm> {
    pub scopes: Scopes<'s>,
    pub main_module: &'m Module,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Scopes<'c> {
    pub current: &'c Scope,
    pub parent: Option<&'c Scopes<'c>>,
}


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Scope {
    pub definitions: HashMap<String, Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Kind(Kind),
    Value(Value)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Kind {
    Tuple (Vec<Kind>),
    Product (HashMap<String, Kind>),
    Sum (HashMap<String, Option<Kind>>),
    Function (Box<FunctionKind>),

    Reference(Vec<String>),

    F64,
    String // TODO should not need a native type
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionKind {
    parameter: Kind,
    result: Kind,
}


#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
    Function(Box<FunctionValueDefinition>),
    FunctionCall(Box<FunctionValueCall>),
    Tuple(Vec<Value>),
    Variant((String, Option<Box<Value>>)),
    Product(HashMap<String, Value>),

    Reference(Vec<String>),

    String(String),
    F64(NonNanF64),
}


#[derive(Clone, Debug, PartialOrd, Copy, Default)]
pub struct NonNanF64(f64);
impl Ord for NonNanF64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.0.is_nan() || other.0.is_nan() {
            panic!("NaN encountered!");
        }

        self.0.partial_cmp(&other.0)
            .expect("Cannot compare these floats")
    }
}
impl Eq for NonNanF64 {}
impl PartialEq for NonNanF64 {
    fn eq(&self, other: &Self) -> bool {
        if self.0.is_nan() || other.0.is_nan() {
            panic!("NaN encountered!");
        }

        self.0.eq(&other.0)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionValueCall {
    function: FunctionValueDefinition,
    argument: Value,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionValueDefinition {
    pub parameter_kind: Kind,
    pub result: Value
}


pub type CompileResult<T> = std::result::Result<T, CompileError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompileError {
    NotFound(Vec<String>),
    InvalidNumber(String),
    InvalidBindingSyntax(parse::Expression),
    NestedBindingSyntaxForbidden(parse::Reference),
    InvalidTypeAnnotation(parse::Expression),
    TypeMismatch { annotated: Kind, found: Kind },
    ExpectedKindButFoundValue(Value),
    ExpectedValueButFoundKind(Kind),
    ExpectedVariantButFoundKind(parse::Expression)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ExpressionKind {
    Value, Kind //, Unspecified
}



impl Value {
    /// return err on reference not found
    pub fn kind<'v, 'e>(&'v self, environment: Environment<'e, 'e>) -> CompileResult<Kind> {
        match self {
            Value::F64(_) => Ok(Kind::F64),
            Value::String(_) => Ok(Kind::String),

            Value::Reference(reference) => {
                environment.resolve(reference.as_slice())
                    .map(|expression| expression.as_value()).transpose()?
                    .map(|value| value.kind(environment)).transpose()?
                    .ok_or(CompileError::NotFound(reference.clone()))
            },

            Value::Tuple(members) => {
                let members : CompileResult<Vec<Kind>> = members.iter()
                    .map(|member| member.kind(environment))
                    .collect();

                Ok(Kind::Tuple(members?))
            },

            Value::Variant((variant, value)) =>
                Ok(Kind::Sum(vec![(
                    variant.clone(),
                    value.map(|member| member.kind(environment))
                        .transpose()?

                )].into_iter().collect())),

            Value::Product(members) => {
                let members : CompileResult<HashMap<String, Kind>> = collapse_named_results(
                    members.clone().into_iter()
                        .map(|(name, value)| (name, value.kind(environment)))
                );

                Ok(Kind::Product(members?))
            },

            Value::Function(function) => {
                Ok(Kind::Function(Box::new(FunctionKind {
                    parameter: function.parameter_kind.clone(),
                    result: function.result.kind(environment)?
                })))
            }

            Value::FunctionCall(call) => {
                call.function.result.kind(environment)
            }
        }
    }
}


pub fn compile_file(file: parse::File, main_module: &Module) -> CompileResult<File>
{
    let mut scope = Scope { definitions: HashMap::new() };

    for definition in file.definitions {
        let environment = Environment {
            scopes: Scopes { current: &scope, parent: None },
            main_module
        };

        let (identifier, expression) = compile_definition(definition, environment)?;
        scope.definitions.insert(identifier, expression);
    }

    Ok(File { content: scope })
}


pub fn compile_definition(definition: parse::Definition, environment: Environment)
    -> CompileResult<(String, Expression)>
{
    let Definition { binding, kind, expression } = definition;
    use parse::Expression::*;

    match binding {
        Identifier(identifier) => {
            if identifier.len() == 1 {
                let identifier = identifier[0].to_owned();

                match compile_expression(expression, environment)? {
                    Expression::Value(compiled_value) => {
                        if identifier.starts_with(|c:char| c.is_uppercase()) {
                            return Err(CompileError::ExpectedKindButFoundValue(compiled_value))
                        }

                        let expression_kind = compiled_value.kind(environment)?;
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

                        Ok((identifier, Expression::Value(compiled_value)))
                    },

                    Expression::Kind(compiled_kind) => {
                        if identifier.starts_with(|c:char| c.is_lowercase()) {
                            return Err(CompileError::ExpectedValueButFoundKind(compiled_kind))
                        }

                        Ok((identifier, Expression::Kind(compiled_kind)))
                    }
                }

            }
            else {
                return Err(CompileError::NestedBindingSyntaxForbidden(identifier))
            }
        }

        FunctionApplication(_) => panic!("Type parameters not supported yet"),
        Tuple(_) | Product(_) | Sum(_) => panic!("destructuring not supported yet"),
        invalid => return Err(CompileError::InvalidBindingSyntax(invalid))
    }
}


pub fn compile_expression(expression: parse::Expression, environment: Environment) -> CompileResult<Expression> {
    if expression_kind(&expression) == ExpressionKind::Value {
        compile_value(expression, environment)
            .map(|value| Expression::Value(value))
    }
    else {
        compile_kind(expression, environment)
            .map(|kind| Expression::Kind(kind))
    }
}


pub fn compile_kind(expression: parse::Expression, environment: Environment) -> CompileResult<Kind> {
    use parse::Expression::*;

    match expression {
        Tuple(members) => compile_tuple_kind(members, environment),
        Product(members) => compile_product_kind(members, environment),
        Sum(members) => compile_sum_kind(members, environment),
        // Function(function) => compile_function_kind(function, environment),

        Identifier(reference) =>
            Ok(Kind::Reference(reference)),

        invalid => Err(CompileError::InvalidTypeAnnotation(invalid))
    }
}


pub fn compile_tuple_kind(
    members: Vec<parse::Expression>, environment: Environment
) -> CompileResult<Kind>
{
    let members: Result<Vec<Kind>, CompileError> = members.into_iter()
        .map(|member| compile_kind(member, environment))
        .collect();

    Ok(Kind::Tuple(members?))
}

pub fn compile_product_kind(
    members: Vec<(String, parse::Expression)>, environment: Environment
) -> CompileResult<Kind>
{
    let members: Result<HashMap<String, Kind>, CompileError> = collapse_named_results(
    members.into_iter()
        .map(|(name, member)| (name, compile_kind(member, environment)))
    );

    Ok(Kind::Product(members?))
}

pub fn compile_sum_kind(
    members: Vec<(String, Option<parse::Expression>)>, environment: Environment
) -> CompileResult<Kind>
{
    let members: Result<HashMap<String, Option<Kind>>, CompileError> = collapse_named_results(
        members.into_iter()
        .map(|(name, member)| (
            name, member.map(|member| compile_kind(member, environment)).transpose()
        ))
    );

    Ok(Kind::Sum(members?))
}


pub fn compile_value(
    expression: parse::Expression, environment: Environment
) -> CompileResult<Value>
{
    use parse::Expression::*;

    Ok(match expression {
        String(value) => Value::String(value.to_owned()),
        Number(value) => Value::F64(NonNanF64(parse_f64(value)?)),

        Identifier(reference) => {
            let identifier = reference.iter().map(ToOwned::to_owned).collect();
            if reference.last().expect("Empty Reference").starts_with(char::is_uppercase) {
                return Err(CompileError::ExpectedValueButFoundKind(Kind::Reference(identifier)))
            }
            else {
                Value::Reference(identifier)
            }
        },

        Tuple(members) => compile_tuple_value(members, environment)?,
        Product(members) => compile_product_value(members, environment)?,

        Sum(mut members) => {
            if members.len() != 1 {
                return Err(CompileError::ExpectedVariantButFoundKind(Sum(members)))
            }

            let (name, value) = members.remove(0);
            Value::Variant((
               name.to_owned(),
               value.map(|value| compile_value(value, environment))
                   .transpose()?.map(Box::new)
            ))
        }

        // FunctionApplication(call) => compile_function_call(call, environment),

        _ => panic!()
    })
}


pub fn compile_tuple_value(
    members: Vec<parse::Expression>, environment: Environment
) -> CompileResult<Value>
{
    let members: Result<Vec<Value>, CompileError> = members
        .into_iter().map(|member| compile_value(member, environment))
        .collect();

    Ok(Value::Tuple(members?))
}

pub fn compile_product_value(
    members: Vec<(String, parse::Expression)>, environment: Environment
) -> CompileResult<Value>
{
    let members: Result<HashMap<String, Value>, CompileError> = collapse_named_results(
        members
        .into_iter().map(|(name, member)| (name, compile_value(member, environment)))
    );

    Ok(Value::Product(members?))
}

//pub fn compile_function_kind()


pub fn compile_function_call(
    expression: parse::FunctionApplication, environment: &Environment
) -> CompileResult<Value>
{
    panic!()
    // Ok(Value::FunctionCall(FunctionValueCall { function, argument }))
}

pub fn parse_f64(text: String) -> CompileResult<f64> {
    text.parse::<f64>().map_err(|_| CompileError::InvalidNumber(text))
}



impl Environment<'_, '_> {
    pub fn resolve(&self, reference: &[String]) -> Option<&Expression> {
        let local = {
            if reference.len() == 1 {
                self.scopes.resolve_local(&reference[0])
            }
            else { None }
        };

        local.or_else(|| self.main_module.resolve(reference))
    }

    pub fn enter_scope(&self, scope: &Scope) -> Environment {
        Environment {
            scopes: self.scopes.enter_scope(scope),
            main_module: self.main_module
        }
    }
}

impl<'s> Scopes<'s> {
    pub fn resolve_local(&self, identifier: &String) -> Option<&Expression> {
        self.current.definitions.get(identifier)
            .or_else(|| self.parent.and_then(|parent| parent.resolve_local(identifier)))
    }

    pub fn enter_scope(&self, scope: &'s Scope) -> Scopes<'s> where 'd: 's {
        Scopes {
            current: scope,
            parent: Some(self)
        }
    }
}


impl Module {
    pub fn resolve(&self, reference: &[String]) -> Option<&Expression> {
        reference.split_first().and_then(|(identifier, rest)| {
            if rest.is_empty() {
                self.file.content.definitions.get(&identifier)
            }
            else {
                self.children.get(identifier)
                    .and_then(|module| module.resolve(rest))
            }
        })
    }
}


impl Expression {
    pub fn as_kind(&self) -> CompileResult<&Kind> {
        match self {
            Expression::Kind(kind) => Ok(kind),
            Expression::Value(value) => Err(CompileError::ExpectedKindButFoundValue(value.clone()))
        }
    }

    pub fn as_value(&self) -> CompileResult<&Value> {
        match self {
            Expression::Value(value) => Ok(value),
            Expression::Kind(kind) => Err(CompileError::ExpectedValueButFoundKind(kind.clone()))
        }
    }
}




/// otherwise, is kind
pub fn expression_kind(expression: &parse::Expression) -> ExpressionKind {
    use parse::Expression::*;
    match expression {
        String(_) | Number(_) | FunctionApplication(_) => ExpressionKind::Value,
        Identifier(names) => reference_expression_kind(names),
        Tuple(members) => expression_kind(&members[0]), // TODO err on any member mismatch
        Sum(members) => expression_kind(&members[0].1.as_ref().unwrap()),
        Product(members) => expression_kind(&members[0].1),
        Function(function) => expression_kind(function.parameter.as_ref()),
        Scope(scope) => expression_kind(scope.result.as_ref()),
    }
}

fn reference_expression_kind(reference: &parse::Reference) -> ExpressionKind {
    let last_identifier = reference.last().expect("Empty Reference Chain");
    if last_identifier.starts_with(char::is_uppercase) { ExpressionKind::Kind }
    else { ExpressionKind::Value }
}


pub fn collapse_results<T, I>(results: I) -> CompileResult<Vec<T>>
    where I: Iterator<Item=CompileResult<T>>
{
    results.fold(
        Ok(Vec::new()),
        |mut vec, element| {
            vec.and_then(|mut vec|{
                match element {
                    Ok(element) => {
                        vec.push(element);
                        Ok(vec)
                    },

                    Err(error) => Err(error),
                }
            })
        }
    )
}

pub fn collapse_named_results<T, I>(results: I) -> CompileResult<HashMap<String, T>>
    where I: Iterator<Item=(String, CompileResult<T>)>
{
    results.fold(
        Ok(HashMap::new()),
        |mut vec, element| {
            vec.and_then(|mut vec|{
                match element {
                    (name, Ok(value)) => {
                        vec.insert(name, value);
                        Ok(vec)
                    },

                    (name, Err(error)) => Err(error),
                }
            })
        }
    )
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_expression_kind(){
        use parse::Expression::*;
        let expr = |expression|
            parse::parse_expression(expression).unwrap().0;

        assert_eq!(expression_kind(&String("hello".to_owned())), ExpressionKind::Value);
        assert_eq!(expression_kind(&Identifier(vec!["std".to_owned(), "string".to_owned()])), ExpressionKind::Value);
        assert_eq!(expression_kind(&Identifier(vec!["std".to_owned(), "string".to_owned(), "String".to_owned()])), ExpressionKind::Kind);
        assert_eq!(expression_kind(&expr("&a:A &b:B")), ExpressionKind::Kind);
        assert_eq!(expression_kind(&expr("|a:A |b:B")), ExpressionKind::Kind);
        assert_eq!(expression_kind(&expr("|d=4")), ExpressionKind::Kind);
        assert_eq!(expression_kind(&expr("|d:4")), ExpressionKind::Value);
        assert_eq!(expression_kind(&expr("&n:4 &t:T")), ExpressionKind::Value);
    }
}























