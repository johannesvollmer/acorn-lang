
pub type Source<'a> = &'a str;

// List, new, +, ...
pub type Identifier = String;

// std.List, List.new, pet.name, ...
pub type Reference = Vec<String>;

// any values, including case-expressions, not supported yet


#[derive(Clone, Eq, PartialEq, Debug)]
pub struct File {
    pub definitions: Vec<Definition>
}

// name: String, age = 5, name: String = "Peter"
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Definition {
    pub binding: Expression, // :
    pub kind: Option<Expression>, // =
    pub expression: Expression
}


#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expression {
    Function (Function),

    /// `a,b,c`, `A,B,C`, also used for destructuring assignments
    Tuple (Vec<Expression>),

    /// `| a | b: B | c`,  `|value = 5`, `|empty`, `|value: A |empty`
    Sum (Vec<Variant>),

    /// `& a = 5 & b = 6`, `a: A & b: C`, also used for destructuring assignments, ...
    Product (Vec<ProductMember>),

    /// std.string.String, Int, window.width, ...
    Identifier(Reference),

    /// `get_name person`, `List Int`, `get_name $` ...
    FunctionApplication (FunctionApplication),

    /// multi-line function bodies, modules, ...
    Scope (Scope),

    String (String),
    Number (String),
}


#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Variant {
    pub name: Identifier,
    pub content: Option<VariantContent>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct VariantContent {
    pub expression: Expression,
    pub operator: char, // may be `=` or `:`
}


#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ProductMember {
    pub name: Identifier,
    pub expression: Expression,
    pub operator: char, // may be `=` or `:`
}

/// `get_name person`, `Option.some 5`, `List Int`, ...
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FunctionApplication {
    pub subject: Box<Expression>,
    pub argument: Box<Expression>
}

/// includes function pub type declarations but also lambda expressions
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Function {
    pub parameter: Box<Expression>, // for a lambda, this is a destructuring or reference
    pub result: Box<Expression>
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Scope {
    pub definitions: Vec<Definition>,
    pub result: Box<Expression>
}






pub type ParseResult<T> = std::result::Result<T, ParseError>;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ParseError {
    ExpectedIdentifier { found: String },
    ExpectedSymbol { symbol: String, found: String }
}


fn parse_module(text: Source) -> ParseResult<File> {
    let mut definitions = Vec::new();

    let mut text = skip_white(text);
    while !text.is_empty() {
        let (definition, remaining) = parse_definition(text)?;

        definitions.push(definition);
        text = skip_white(remaining);
    }

    Ok(File { definitions })
}


/// identifier : pub type = value
fn parse_definition(text: Source) -> ParseResult<(Definition, Source)> {
    let (binding, text) = parse_expression(text)?;
    let text = skip_white(text);

    let (type_annotation, text) = {
        if let Some(text) = skip_symbol(text, ":"){
            let (kind, text) = parse_expression(text)?;
            (Some(kind), text)
        }
        else {
            (None, text)
        }
    };

    let text = skip_white(text);
    let text = expect_symbol(text, "=")?;

    let (expression, text) = parse_expression(text)?;

    Ok((
        Definition { binding, kind: type_annotation, expression },
        text
    ))
}


pub fn parse_expression(text: Source) -> ParseResult<(Expression, Source)> {
    parse_scope(text)
}


fn parse_scope(text: Source) -> ParseResult<(Expression, Source)> {
    let mut remaining = skip_white(text);
    let mut scope_definitions = Vec::new();

    let result_expression = loop {
        let (binding_or_result, text) = parse_function_or_other(remaining)?;
        let text = skip_white(text);

        let (type_annotation, text) = {
            if let Some(text) = skip_symbol(text, ":"){
                let (kind, text) = parse_function_or_other(text)?;
                (Some(kind), text)
            }
            else {
                (None, text)
            }
        };

        let text = skip_white(text);

        if text.is_empty() {
            remaining = skip_white(text);
            break binding_or_result;
        }

        // if a type annotation is present, force-parse an assignment
        else if let Some(annotation) = type_annotation {
            let text = expect_symbol(text, "=")?;
            let (value, text) = parse_function_or_other(text)?;

            remaining = skip_white(text);
            scope_definitions.push(Definition { binding: binding_or_result, kind: Some(annotation), expression: value });
        }

        // if another definition is found, parse its value and add it without ending the loop
        else if let Some(text) = skip_symbol(text, "=") {
            let (value, text) = parse_function_or_other(text)?;

            remaining = skip_white(text);
            scope_definitions.push(Definition { binding: binding_or_result, kind: None, expression: value });
        }

        else { // just a plain expression without sub definitions
            remaining = skip_white(text);
            break binding_or_result;
        }
    };


    if scope_definitions.is_empty() {
        Ok((result_expression, remaining))
    }
    else {
        Ok((Expression::Scope(Scope {
            definitions: scope_definitions,
            result: Box::new(result_expression)
        }), remaining))
    }
}

fn parse_function_or_other(text: Source) -> ParseResult<(Expression, Source)> {
    let (first_kind, text) = parse_tuple_or_other(text)?;
    let text = skip_white(text);

    if let Some(text) = skip_symbol(text, "->") {
        let (output, text) = parse_tuple_or_other(text)?;

        let function = Function {
            parameter: Box::new(first_kind),
            result: Box::new(output)
        };

        Ok((Expression::Function(function), text))
    }
    else {
        Ok((first_kind, text))
    }
}

fn parse_tuple_or_other(text: Source) -> ParseResult<(Expression, Source)> {
    let text = skip_white(text);
    let text = skip_symbol(text, ",").unwrap_or(text); // TODO respect this info?

    let (first, text) = parse_sum_or_other(text)?;

    let mut text = skip_white(text);
    if text.starts_with(","){
        let mut members = vec![ first ];

        while let Some(remaining) = skip_symbol(text, ",") {
            let (kind, remaining_text) = parse_sum_or_other(remaining)?;
            text = skip_white(remaining_text);
            members.push(kind);
        }

        Ok((Expression::Tuple(members), text))
    }
    else {
        Ok((first, text))
    }
}

fn parse_sum_or_other(text: Source) -> ParseResult<(Expression, Source)> {

    fn parse_variant(remaining: Source) -> ParseResult<(Variant, Source)> {
        let remaining = skip_white(remaining);

        let (member_name, remaining) = parse_identifier(remaining)?;
        let remaining = skip_white(remaining);

        let (operator, remaining) = {
            if let Some(remaining) = skip_symbol(remaining, ":") {
                (Some(':'), remaining)
            }
            else if let Some(remaining) = skip_symbol(remaining, "=") {
                (Some('='), remaining)
            }
            else {
                (None, remaining)
            }
        };

        let remaining = skip_white(remaining);
        let (member, remaining) = parse_product_or_other(remaining)?;

        Ok((
           Variant {
                name: member_name,
                content: operator.map(move |operator| VariantContent {
                    expression: member,
                    operator
                })
           },
           remaining
        ))
    }

    let text = skip_white(text);
    if text.starts_with("|") {
        let mut variants = Vec::new();

        let mut text = skip_white(text);
        while let Some(remaining) = skip_symbol(text, "|") {
            let (variant, remaining) = parse_variant(remaining)?;

            variants.push(variant);
            text = skip_white(remaining);
        }

        Ok((Expression::Sum(variants), text))
    }
    else {
        parse_product_or_other(text)
    }
}

fn parse_product_or_other(text: Source) -> ParseResult<(Expression, Source)> {
    let text = skip_white(text);

    if text.starts_with("&") {
        let mut members = Vec::new();

        let mut text = skip_white(&text);
        while let Some(remaining) = skip_symbol(text, "&") {
            let (member_name, remaining) = parse_identifier(remaining)?;
            let remaining = skip_white(remaining);

            let (operator, remaining) =
                skip_symbol(remaining, ":").map(|txt| (':', txt))
                    .or(skip_symbol(remaining, "=").map(|txt| ('=', txt)))

                    .ok_or_else(|| ParseError::ExpectedSymbol {
                        symbol: "`:` or `=`".to_owned(),
                        found: take_n(remaining, 80)
                    })?;

            let (member, remaining) = parse_function_application(remaining)?;

            text = skip_white(remaining);
            members.push(ProductMember {
                name: member_name,
                expression: member,
                operator
            });
        }

        Ok((Expression::Product(members), text))
    }
    else {
        parse_function_application(text)
    }
}

// TODO: indention??
fn parse_function_application(text: Source) -> ParseResult<(Expression, Source)> {
    let (first, remaining) = parse_atom(text)?;
    let remaining = skip_white(remaining);

    if remaining.starts_with(|c:char| c == '(' || !is_special_symbol(c)) {
        let (argument, remaining) = parse_atom(remaining)?;
        Ok((
            Expression::FunctionApplication(FunctionApplication {
                subject: Box::new(first), argument: Box::new(argument)
            }),
            remaining
        ))
    }
    else {
        Ok((first, remaining))
    }
}

fn parse_atom(text: Source) -> ParseResult<(Expression, Source)> {
    let text = skip_white(text);

    if let Some((number, remaining)) = parse_number(text) {
        Ok((Expression::Number(number.to_owned()), remaining))
    }
    else if let Some(remaining) = skip_symbol(text, "(") {
        let (contents, remaining) = parse_expression(remaining)?;

        let remaining = skip_white(remaining);
        let remaining = skip_symbol(remaining, ")")
            .ok_or(ParseError::ExpectedSymbol { symbol: ")".to_owned(), found: take_n(remaining, 80) })?;

        Ok((contents, remaining))
    }
    else if let Some(remaining) = skip_symbol(text, "\"") {
        if let Some(end_index) = remaining.find(|c| c == '"') {
            let (string, remaining) = remaining.split_at(end_index);
            let remaining = expect_symbol(remaining, "\"")?;
            Ok((Expression::String(string.to_owned()), remaining))
        }
        else {
            Err(ParseError::ExpectedSymbol { symbol: "Closing `\"".to_owned(), found: "".to_owned() })
        }
    }
    else {
        parse_identifier_chain(text)
    }
}


fn is_special_symbol(symbol: char) -> bool {
    symbol.is_whitespace() || "$&|@,():=.\"'<->".contains(symbol)
}

fn parse_number(text: Source) -> Option<(Source, Source)> {
    let end_index = skip_white(text)
        .find(|c:char| !c.is_digit(10) && c != '.');

    if let Some(index) = end_index {
        if index != 0 {
            let (content, remaining) = text.split_at(index);
            let successor = remaining.chars().next().unwrap();

            // do not parse number if is immediately continued with an identifier
            if is_special_symbol(successor) {
                Some((content, remaining))
            }
            else { None }
        }
        else { None }
    }
    else {
        Some((text, ""))
    }
}

fn parse_identifier_chain(text: Source) -> ParseResult<(Expression, Source)> {
    let (first, text) = parse_identifier(skip_white(text))?;
    let mut parts = vec![ first ];

    let mut text = skip_white(text);
    while let Some(remaining) = skip_symbol(text, ".") {
        let (identifier, remaining) = parse_identifier(remaining)?;
        text = skip_white(remaining);

        parts.push(identifier);
    }

    if parts.is_empty() {
        Err(ParseError::ExpectedIdentifier { found: take_n(text, 80) })
    }
    else {
        Ok((Expression::Identifier(parts), text))
    }
}


// name, prop1 & prop2,
fn parse_identifier(text: Source) -> ParseResult<(Identifier, Source)> {
    let text = skip_white(text);

    let is_not_identifier = |c: char|
        c.is_whitespace() || ("&|@,():=.\"'").contains(c);

    if let Some(identifier_end) = text.find(is_not_identifier){
        if identifier_end == 0 {
            (Err(ParseError::ExpectedIdentifier { found: take_n(text, 80) }))
        }
        else {
            let (result, remaining) = text.split_at(identifier_end);
            Ok((result.to_owned(), remaining))
        }
    }
    else { // no non-identifier found
        if text.is_empty() {
            Err(ParseError::ExpectedIdentifier { found: "".to_owned() })
        }
        else {
            Ok((text.to_owned(), ""))
        }
    }
}

fn expect_symbol<'t>(text: Source<'t>, symbol: &'t str) -> ParseResult<Source<'t>> {
    skip_symbol(text, symbol)
        .ok_or_else(|| ParseError::ExpectedSymbol { symbol: symbol.to_owned(), found: take_n(text, 80) })
        .unwrap();

    Ok(&text[symbol.len()..])
}

fn skip_symbol<'a>(text: Source<'a>, symbol: &str) -> Option<Source<'a>> {
    if text.starts_with(symbol) {
        Some(&text[symbol.len()..])
    }
    else {
        None
    }
}

fn skip_white(text: Source) -> Source {
    text.trim_start()
}


fn take_n(text: &str, count: usize) -> String {
    text[0..text.len().min(count)].to_owned() + "..."
}




// TODO negative tests

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! name {
        ($($identifier:ident).*) => {
            Expression::Identifier(vec![
                $(stringify!($identifier) .to_owned() ,)*
            ])
        };
    }


    #[test]
    fn test_parse_definition(){
        /// FIXME definition cannot be parsed because parsing the binding already introduces scoped local definitions

        assert_eq!(
            parse_definition("Name = | name: std.string.String | anonymous"),
            Ok((
                Definition {
                    binding: name!{ Name },
                    kind: None,
                    expression: Expression::Sum(vec![
                        Variant {
                            name: "name".to_string(),
                            content: Some(VariantContent {
                                expression: name!{ std.string.String },
                                operator: ':'
                            })
                        },
                        Variant {
                            name: "anonymous".to_string(),
                            content: None
                        },
                    ])
                }
                , ""
            ))
        );
    }

    #[test]
    fn test_parse_application(){
        assert_eq!(
            parse_function_application("string.trim user_name"),
            Ok((
                Expression::FunctionApplication(FunctionApplication {
                    subject: Box::new(name!{ string.trim }),
                    argument: Box::new(name! { user_name })
                })
                , ""
            ))
        );
    }

    #[test]
    fn test_parse_atom(){
        assert_eq!(parse_atom(" Name"), Ok((name!{ Name }, "")));
        assert_eq!(parse_atom(" a.Name n"), Ok((name!{ a.Name }, "n")));
        assert_eq!(parse_atom(" a3.1Name 0n"), Ok((Expression::Identifier(vec!["a3".to_owned(), "1Name".to_owned()]), "0n")));
        assert_eq!(parse_atom(" 3.1 0n"), Ok((Expression::Number("3.1".to_owned()), " 0n")));
        assert_eq!(parse_atom(" \"legendary\"xo"), Ok((Expression::String("legendary".to_owned()), "xo")));
    }


    #[test]
    fn test_parse_function_or_other_kind(){
        assert_eq!(
            parse_function_or_other("| name: std.String | anonymous"),
            Ok((Expression::Sum(vec![
                Variant {
                    name: "name".to_string(),
                    content: Some(VariantContent {
                        expression: name!{ std.String },
                        operator: ':'
                    })
                },
                Variant {
                    name: "anonymous".to_string(),
                    content: None
                }
            ] ), ""))
        );

        assert_eq!(
            parse_function_or_other("String -> Int"),
            Ok((Expression::Function(Function {
                parameter: Box::new(name!{ String }),
                result: Box::new(name!{ Int }),
            }), ""))
        );

        assert_eq!(
            parse_function_or_other("String -> ,Int,String"),
            Ok((Expression::Function(Function {
                parameter: Box::new(name!{ String }),
                result: Box::new(Expression::Tuple(vec![
                    name!{ Int },
                    name!{ String }
                ])),
            }), ""))
        );

        assert_eq!(
            parse_function_or_other("a,b -> &name = \"ab\" & age = 10 "),
            Ok((Expression::Function(Function {
                parameter: Box::new(Expression::Tuple(vec![name!{ a }, name! { b } ])),
                result: Box::new(Expression::Product(vec![
                    ProductMember {
                        name: "name".to_owned(),
                        expression: Expression::String("ab".to_owned()),
                        operator: '='
                    },
                    ProductMember {
                        name: "age".to_owned(),
                        expression: Expression::Number("10".to_owned()),
                        operator: '='
                    },
                ])),
            }), ""))
        );
    }

    #[test]
    fn test_parse_tuple_or_other_kind(){
        assert_eq!(
            parse_tuple_or_other(",Float ,str.String ,Int"),
            Ok((Expression::Tuple(vec![
                name!{ Float },
                name!{ str.String },
                name!{ Int },
            ]), ""))
        );

        assert_eq!(
            parse_tuple_or_other(r#" , 5 , "hello" , Int"#),
            Ok((Expression::Tuple(vec![
                Expression::Number("5".to_owned()),
                Expression::String("hello".to_owned()),
                name!{ Int },
            ]), ""))
        );

        assert_eq!(
            parse_tuple_or_other(",&name:String&age:Int ,str.String ,Int"),
            Ok((Expression::Tuple(vec![
                Expression::Product(vec![
                    ProductMember {
                        name: "name".to_owned(),
                        expression: name!{ String },
                        operator: ':'
                    },
                    ProductMember {
                        name: "age".to_owned(),
                        expression: name!{ Int },
                        operator: ':'
                    },
                ]),
                name!{ str.String },
                name!{ Int },
            ]), ""))
        );
    }


    #[test]
    fn test_parse_sum_or_other_kind(){
        assert_eq!(parse_sum_or_other(" std .str .String ->"), Ok((name!{ std.str.String }, "->")));


        assert_eq!(
            parse_sum_or_other("| name: std.String | anonymous"),
            Ok((Expression::Sum(vec![
                Variant {
                    name: "name".to_string(),
                    content: Some(VariantContent {
                        expression: name!{ std.String },
                        operator:':'
                    })
                },
                Variant {
                    name: "anonymous".to_string(),
                    content: None
                }
            ] ), ""))
        );

        assert_eq!(
            parse_sum_or_other("| name: &value: std.String &length: Int | anonymous"),
            Ok((Expression::Sum(vec![
                Variant {
                    name: "name".to_string(),
                    content: Some(VariantContent {
                        expression: Expression::Product(vec![
                            ProductMember {
                                name: "value".to_owned(),
                                expression: name!{ std.String },
                                operator: ':'
                            },

                            ProductMember {
                                name: "length".to_owned(),
                                expression: name!{ Int },
                                operator: ':'
                            },
                        ]),
                        operator:':'
                    })
                },
                Variant {
                    name: "anonymous".to_string(),
                    content: None,
                },
            ] ), ""))
        );
    }

    #[test]
    fn test_parse_product_or_other_kind(){
        assert_eq!(parse_product_or_other(" String"), Ok((name!{ String }, "")));
        assert_eq!(parse_product_or_other(" std.String"), Ok((name!{ std.String }, "")));
        assert_eq!(parse_product_or_other(" std .str .String"), Ok((name!{ std.str.String }, "")));

        assert_eq!(
            parse_sum_or_other("& name: String &age : num.Int"),
            Ok((Expression::Product(vec![
                ProductMember {
                    name: "name".to_owned(),
                    expression: name!{ String },
                    operator: ':'
                },
                ProductMember {
                    name: "age".to_owned(),
                    expression: name!{ num.Int },
                    operator: ':'
                },
            ]), ""))
        );

        assert_eq!(
            parse_sum_or_other(r#"& name = "world" &age = 12"#),
            Ok((Expression::Product(vec![

                ProductMember {
                    name: "name".to_owned(),
                    expression: Expression::String("world".to_owned()),
                    operator: '='
                },

                ProductMember {
                    name: "age".to_owned(),
                    expression: Expression::Number("12".to_owned()),
                    operator: '='
                },
            ]), ""))
        );
    }

    #[test]
    fn test_parse_chained_identifier(){
        assert_eq!(parse_identifier_chain("hello world"), Ok((name!{ hello }, "world")));
        assert_eq!(parse_identifier_chain("hello.world"), Ok((name!{ hello.world }, "")));
        assert_eq!(parse_identifier_chain(" the .world "), Ok((name!{ the.world }, "")));
        assert_eq!(parse_identifier_chain("hello .world but not this"), Ok((name!{ hello.world }, "but not this")));
        assert_eq!(parse_identifier_chain("hello .world@"), Ok((name!{ hello.world }, "@")));
    }

    #[test]
    fn test_parse_identifier(){
        assert_eq!(parse_identifier("hello world"), Ok(("hello".to_owned(), " world")));
        assert_eq!(parse_identifier("hello.world"), Ok(("hello".to_owned(), ".world")));
        assert_eq!(parse_identifier(" the .world "), Ok(("the".to_owned(), " .world ")));
        assert_eq!(parse_identifier(" world+"), Ok(("world+".to_owned(), "")));
        assert_eq!(parse_identifier(" world@"), Ok(("world".to_owned(), "@")));
        // assert_panics!(parse_identifier("("), (("", "(")));
        // assert_panics!(parse_identifier(""), (("", "")));
    }

    #[test]
    fn test_skip_symbol(){
        assert_eq!(skip_symbol("x", ""), Some("x"));
        assert_eq!(skip_symbol(".-01w", ".-"), Some("01w"));
        assert_eq!(skip_symbol("t", "t"), Some(""));
        assert_eq!(skip_symbol("__", "x"), None);
        assert_eq!(skip_symbol(" __", "_"), None);
        assert_eq!(skip_symbol("__", "_"), Some("_"));
    }

    #[test]
    fn test_skip_white(){
        assert_eq!(skip_white(" x"), "x");
        assert_eq!(skip_white(" .-01w"), ".-01w");
        assert_eq!(skip_white(" "), "");
        assert_eq!(skip_white("\n\t"), "");
        assert_eq!(skip_white("\t  ??"), "??");
        assert_eq!(skip_white(" __ \t "), "__ \t ");
    }

}