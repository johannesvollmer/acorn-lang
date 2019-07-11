use super::tokenize;
use std::collections::HashMap;


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Module {
    pub definitions: Definitions,
    pub children: HashMap<String, Module>,
}

pub type Definitions = HashMap<String, Expression>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Scope<'d, 'p> {
    pub current: &'d Definitions,
    pub parent: Option<&'p Scope<'d, 'p>>,
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

    WithLocalDefinitions(Definitions, Box<Value>),

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
    pub function: Value
}


pub type CompileResult<T> = std::result::Result<T, CompileError>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompileError {
    NotFound(Vec<String>),
    InvalidNumber(String),
    InvalidBindingSyntax(tokenize::Token),
    NestedBindingSyntaxForbidden(tokenize::Reference),
    InvalidTypeAnnotation(tokenize::Token),
    TypeMismatch { annotated: Kind, found: Kind },
    ExpectedKindButFoundValue(Value),
    ExpectedValueButFoundKind(Kind),
    ExpectedVariantButFoundKind(tokenize::Token)
}


#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ExpressionKind {
    Value, Kind, Unknown
}





/*

impl Value {
    /// return err on reference not found
    pub fn kind(&self, scope: Scope, main: &Module) -> CompileResult<Kind> {
        match self {
            Value::F64(_) => Ok(Kind::F64),
            Value::String(_) => Ok(Kind::String),

            Value::Reference(reference) => {
                let expression = match reference.as_slice() {
                    [plain] => scope.resolve_local(plain),
                    other => main.resolve(other),
                };

                expression
                    .map(|expression| expression.as_value()).transpose()?
                    .map(|value| value.kind(scope, main)).transpose()?
                    .ok_or(CompileError::NotFound(reference.clone()))
            },

            Value::Tuple(members) => {
                let members : CompileResult<Vec<Kind>> = members.iter()
                    .map(|member| member.kind(scope, main))
                    .collect();

                Ok(Kind::Tuple(members?))
            },

            Value::Variant((variant, value)) =>
                Ok(Kind::Sum(vec![(
                    variant.clone(),
                    value.as_ref()
                        .map(|member| member.kind(scope, main))
                        .transpose()?

                )].into_iter().collect())),

            Value::Product(members) => {
                let members : CompileResult<HashMap<String, Kind>> = collapse_named_results(
                    members.clone().into_iter()
                        .map(|(name, value)| (name, value.kind(scope, main)))
                );

                Ok(Kind::Product(members?))
            },

            Value::WithLocalDefinitions(local_scope, expression) => {
                expression.kind(scope.enter_sub_scope(local_scope), main)
            },

            Value::Function(function) => {
                Ok(Kind::Function(Box::new(FunctionKind {
                    parameter: function.parameter_kind.clone(),
                    result: function.function.kind(scope, main)?
                })))
            },

            Value::FunctionCall(call) => {
                call.function.function.kind(scope, main)
            },
        }
    }
}


pub fn compile_file(file: tokenize::File, main: &Module) -> CompileResult<Definitions>
{
    let mut definitions = Definitions::new();

    for definition in file.definitions {
        let scope = Scope { current: &definitions, parent: None };
        let (identifier, expression) = compile_definition(definition, scope, main)?;
        definitions.insert(identifier, expression);
    }

    Ok(definitions)
}


pub fn compile_definition(definition: tokenize::Definition, scope: Scope, main: &Module)
                          -> CompileResult<(String, Expression)>
{
    let Definition { binding, kind, expression } = definition;
    use tokenize::Token::*;

    match binding {
        Identifier(mut identifier) => {
            if identifier.len() == 1 {
                let identifier = identifier.remove(0);

                match compile_expression(expression, scope, main)? {
                    Expression::Value(compiled_value) => {
                        if identifier.starts_with(|c:char| c.is_uppercase()) {
                            return Err(CompileError::ExpectedKindButFoundValue(compiled_value))
                        }

                        let expression_kind = compiled_value.kind(scope, main)?;
                        let desired_kind = kind
                            .map(|k| compile_kind(k, scope, main))
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

        Application(_) => panic!("Type parameters not supported yet"),
        Tuple(_) | Product(_) | Sum(_) => panic!("destructuring not supported yet"),
        invalid => return Err(CompileError::InvalidBindingSyntax(invalid))
    }
}


pub fn compile_expression(expression: tokenize::Token, scope: Scope, main: &Module) -> CompileResult<Expression> {
    if expression_kind(&expression) == ExpressionKind::Value {
        compile_value(expression, scope, main)
            .map(|value| Expression::Value(value))
    }
    else {
        compile_kind(expression, scope, main)
            .map(|kind| Expression::Kind(kind))
    }
}


pub fn compile_kind(expression: tokenize::Token, scope: Scope, main: &Module) -> CompileResult<Kind> {
    use tokenize::Token::*;

    match expression {
        Tuple(members) => compile_tuple_kind(members, scope, main),
        Product(members) => compile_product_kind(members, scope, main),
        Sum(members) => compile_sum_kind(members, scope, main),
        // Function(function) => compile_function_kind(function, scope, main),

        Identifier(reference) =>
            Ok(Kind::Reference(reference)),

        invalid => Err(CompileError::InvalidTypeAnnotation(invalid))
    }
}


pub fn compile_tuple_kind(
    members: Vec<tokenize::Token>, scope: Scope, main: &Module
) -> CompileResult<Kind>
{
    let members: Result<Vec<Kind>, CompileError> = members.into_iter()
        .map(|member| compile_kind(member, scope, main))
        .collect();

    Ok(Kind::Tuple(members?))
}

pub fn compile_product_kind(
    members: Vec<ProductMember>, scope: Scope, main: &Module
) -> CompileResult<Kind>
{
    let members: Result<HashMap<String, Kind>, CompileError> = collapse_named_results(
    members.into_iter()
        .map(|member| (member.name, compile_kind(member.expression, scope, main)))
    );

    Ok(Kind::Product(members?))
}

pub fn compile_sum_kind(
    members: Vec<Variant>, scope: Scope, main: &Module
) -> CompileResult<Kind>
{
    let members: Result<HashMap<String, Option<Kind>>, CompileError> = collapse_named_results(
        members.into_iter()
        .map(|member| (
            member.name, member.content.map(|content| compile_kind(content.expression, scope, main)).transpose()
        ))
    );

    Ok(Kind::Sum(members?))
}


pub fn compile_value(
    expression: tokenize::Token, scope: Scope, main: &Module
) -> CompileResult<Value>
{
    use tokenize::Token::*;

    Ok(match expression {
        String(value) => Value::String(value),
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

        Tuple(members) => compile_tuple_value(members, scope, main)?,
        Product(members) => compile_product_value(members, scope, main)?,

        Sum(mut members) => {
            if members.len() != 1 {
                return Err(CompileError::ExpectedVariantButFoundKind(Sum(members)))
            }

            let member = members.remove(0);

            Value::Variant((
               member.name,
               member.content.map(|content| compile_value(content.expression, scope, main))
                   .transpose()?.map(Box::new)
            ))
        }

        // FunctionApplication(call) => compile_function_call(call, scope, main),

        _ => panic!()
    })
}


pub fn compile_tuple_value(
    members: Vec<tokenize::Token>, scope: Scope, main: &Module
) -> CompileResult<Value>
{
    let members: Result<Vec<Value>, CompileError> = members
        .into_iter().map(|member| compile_value(member, scope, main))
        .collect();

    Ok(Value::Tuple(members?))
}

pub fn compile_product_value(
    members: Vec<ProductMember>, scope: Scope, main: &Module
) -> CompileResult<Value>
{
    let members: Result<HashMap<String, Value>, CompileError> = collapse_named_results(
        members
        .into_iter().map(|member| (member.name, compile_value(member.expression, scope, main)))
    );

    Ok(Value::Product(members?))
}

//pub fn compile_function_kind()


pub fn compile_function_call(
    expression: tokenize::FunctionApplication, scope: Scope, main: &Module
) -> CompileResult<Value>
{
    panic!()
    // Ok(Value::FunctionCall(FunctionValueCall { function, argument }))
}

pub fn parse_f64(text: String) -> CompileResult<f64> {
    text.parse::<f64>().map_err(|_| CompileError::InvalidNumber(text))
}



impl<'d, 'p> Scope<'d, 'p> {
    pub fn enter_sub_scope(&'p self, definitions: &'d Definitions) -> Self {
        Scope {
            current: definitions,
            parent: Some(&self)
        }
    }

    pub fn resolve_local(&self, identifier: &String) -> Option<&Expression> {
        self.current.get(identifier)
            .or_else(|| self.parent.and_then(|parent| parent.resolve_local(identifier)))
    }
}


impl Module {
    pub fn resolve(&self, reference: &[String]) -> Option<&Expression> {
        reference.split_first().and_then(|(identifier, rest)| {
            if rest.is_empty() {
                self.definitions.get(identifier)
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
pub fn expression_kind(expression: &tokenize::Token) -> ExpressionKind {
    use tokenize::Token::*;
    match expression {
        String(_) | Number(_) | Application(_) => ExpressionKind::Value,
        Identifier(names) => reference_expression_kind(names),
        Tuple(members) => expression_kind(&members[0]), // TODO err on any member mismatch
        Sum(variants) => sum_expression_kind(&variants),
        Product(members) => expression_kind(&members[0].expression),
        Function(function) => expression_kind(&function.parameter),
        Scope(scope) => expression_kind(scope.result.as_ref()),
    }
}

fn sum_expression_kind(variants: &[tokenize::Variant]) -> ExpressionKind {

    if let Some(content) = variants[0].content.as_ref() {
        if content.operator == '=' {
            if variants.len() > 1 || expression_kind(&content.expression) == ExpressionKind::Kind {
                panic!("sum type content expression kind mismatch")
            }

            ExpressionKind::Value
        }
        else {
            ExpressionKind::Kind
        }
    }

    else {
        ExpressionKind::Unknown
    }
}

fn reference_expression_kind(reference: &tokenize::Reference) -> ExpressionKind {
    let last_identifier = reference.last().expect("Empty Reference Chain");
    if last_identifier.starts_with(char::is_uppercase) { ExpressionKind::Kind }
    else { ExpressionKind::Value }
}


pub fn collapse_results<T, I>(results: I) -> CompileResult<Vec<T>>
    where I: Iterator<Item=CompileResult<T>>
{
    results.fold(
        Ok(Vec::new()),
        |vec, element| {
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
        |vec, element| {
            vec.and_then(|mut vec|{
                match element {
                    (name, Ok(value)) => {
                        vec.insert(name, value);
                        Ok(vec)
                    },

                    (_name, Err(error)) => Err(error),
                }
            })
        }
    )
}


#[cfg(test)]
mod test {
    use super::*;

    fn tokenize_expression(text: &str) -> tokenize::Token {
        tokenize::tokenize(text).unwrap()
    }


    macro_rules! reference_value {
        ($($identifier:ident).*) => {
            Value::Reference(vec![
                $(stringify!($identifier) .to_owned() ,)*
            ])
        };
    }

    fn string(value: &str) -> Value {
        Value::String(value.to_owned())
    }

    fn scope(definitions: &Definitions) -> Scope {
        Scope {
            current: definitions,
            parent: None
        }
    }

    #[test]
    fn test_compile(){
        let scope_content = Definitions::new();
        let scope = scope(&scope_content);

        let main = Module {
            definitions: Definitions::new(),
            children: HashMap::new()
        };

        assert_eq!(
            compile_value(
                tokenize_expression("debug.crash \"Fatal Error\""),
                scope, &main
            ),

            Ok(Value::FunctionCall(Box::new(FunctionValueCall {
                function: FunctionValueDefinition {
                    parameter_kind: Kind::String,
                    function: reference_value!(debug.crash)
                },
                argument: string("Fatal Error")
            })))
        )
    }

    #[test]
    fn test_expression_kind(){
        use tokenize::Token::*;

        assert_eq!(expression_kind(&String("hello".to_owned())), ExpressionKind::Value);
        assert_eq!(expression_kind(&Identifier(vec!["std".to_owned(), "string".to_owned()])), ExpressionKind::Value);
        assert_eq!(expression_kind(&Identifier(vec!["std".to_owned(), "string".to_owned(), "String".to_owned()])), ExpressionKind::Kind);
        assert_eq!(expression_kind(&tokenize_expression("&a:A &b:B")), ExpressionKind::Kind);
        assert_eq!(expression_kind(&tokenize_expression("|a:A |b:B")), ExpressionKind::Kind);
        assert_eq!(expression_kind(&tokenize_expression("|d=4")), ExpressionKind::Value);
        assert_eq!(expression_kind(&tokenize_expression("|d:4")), ExpressionKind::Kind);
        assert_eq!(expression_kind(&tokenize_expression("&n:4 &t:T")), ExpressionKind::Value);
    }
}




*/



















