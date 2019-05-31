pub type Text<'a> = &'a str;

// List, new, +, ...
pub type Identifier<'a> = Text<'a>;

// std.List, List.new, pet.name, ...
pub type Reference<'a> = Vec<Identifier<'a>>;

// any values, including case-expressions, not supported yet


// name: String, age = 5, name: String = "Peter"
#[derive(Eq, PartialEq, Debug)]
pub struct Definition<'a> {
    binding: Expression<'a>, // :
    kind: Option<Expression<'a>>, // =
    expression: Expression<'a>
}


#[derive(Eq, PartialEq, Debug)]
pub enum Expression<'a> {
    Function (Function<'a>),

    /// `a,b,c`, `A,B,C`, also used for depub structuring assignments
    Tuple (Vec<Expression<'a>>),

    /// `| a | b: B | c`,  `|value = 5`, `|empty`, `|value: A |empty`
    Sum (Vec<(Identifier<'a>, Option<Expression<'a>>)>),

    /// `& a = 5 & b = 6`, `a: A & b: C`, also used for depub structuring assignments, ...
    Product (Vec<(Identifier<'a>, Expression<'a>)>),

    /// std.string.String, Int, window.width, ...
    Named (Reference<'a>),

    /// `get_name person`, `List Int`, `get_name $` ...
    FunctionApplication (FunctionApplication<'a>),

    /// multi-line function bodies, modules, ...
    Scope (Scope<'a>),

    String (Text<'a>),
    Number (Text<'a>),
}


/// `get_name person`, `Option.some 5`, `List Int`, ...
#[derive(Eq, PartialEq, Debug)]
pub struct FunctionApplication<'a> {
    subject: Box<Expression<'a>>,
    argument: Box<Expression<'a>>
}

/// includes function pub type declarations but also lambda expressions
#[derive(Eq, PartialEq, Debug)]
pub struct Function<'a> {
    parameter: Box<Expression<'a>>, // for a lambda, this is a depub structuring or reference
    result: Box<Expression<'a>>
}

#[derive(Eq, PartialEq, Debug)]
pub struct Scope<'a> {
    definitions: Vec<Definition<'a>>,
    result: Box<Expression<'a>>
}


pub enum ValueOrKind {
    Value,
    Kind
}

impl Expression<'_> {
    /// otherwise, is kind
    pub fn is_value(&self) -> bool {
        match self {
            Expression::String(_) => true,
            Expression::Number(_) => true,
            Expression::Named(names) => reference_is_value(names),
            Expression::Tuple(members) => members[0].is_value(),
            Expression::Sum(members) => members[0].1.as_ref().unwrap().is_value(),
            Expression::Product(members) => members[0].1.is_value(),
            _ => panic!()
        }
    }
}

fn reference_is_value(reference: &Reference) -> bool {
    match reference.last() {
        Some(name) => {
            match name.chars().next() {
                Some(first_letter) => !first_letter.is_uppercase(),
                None => panic!()
            }
        },

        None => panic!()
    }
}



/// identifier : pub type = value
fn parse_definition(text: Text) -> (Definition, Text) {
    let (binding, text) = parse_expression(text);
    let text = skip_white(text);

    let (type_annotation, text) = {
        if let Some(text) = skip_symbol(text, ":"){
            let (kind, text) = parse_expression(text);
            (Some(kind), text)
        }
        else {
            (None, text)
        }
    };

    let text = skip_white(text);
    let text = skip_symbol(text, "=").unwrap();

    let (expression, text) = parse_expression(text);

    (
        Definition { binding, kind: type_annotation, expression },
        text
    )
}


fn parse_expression(text: Text) -> (Expression, Text) {
    parse_function_or_other(text)
}

fn parse_function_or_other(text: Text) -> (Expression, Text) {
    let (first_kind, text) = parse_tuple_or_other(text);
    let text = skip_white(text);

    if let Some(text) = skip_symbol(text, "->") {
        let (output, text) = parse_expression(text);
        (Expression::Function(Function {
            parameter: Box::new(first_kind),
            result: Box::new(output)
        }), text)
    }
    else {
        (first_kind, text)
    }
}

fn parse_tuple_or_other(text: Text) -> (Expression, Text) {
    let text = skip_white(text);
    let text = skip_symbol(text, ",").unwrap_or(text); // TODO respect this info?

    let (first, mut text) = parse_sum_or_other(text);

    if text.starts_with(","){
        let mut members = vec![ first ];

        while let Some(remaining) = skip_symbol(text, ",") {
            let (kind, remaining_text) = parse_sum_or_other(remaining);
            text = skip_white(remaining_text);
            members.push(kind);
        }

        (Expression::Tuple(members), text)
    }
    else {
        (first, text)
    }
}

fn parse_sum_or_other(text: Text) -> (Expression, Text) {
    let text = skip_white(text);

    if text.starts_with("|") {
        let mut text = skip_white(&text);

        let mut members = Vec::new();
        while let Some(remaining) = skip_symbol(text, "|") {
            let (member_name, remaining) = parse_identifier(remaining);
            let mut remaining = skip_white(remaining);

            let (member_kind, remaining) = {

                if let Some(remaining) = skip_symbol(remaining, ":")
                    .or(skip_symbol(remaining, "=")) // TODO remember pub type vs value
                {
                    let (member, remaining) = parse_product_or_other(remaining);
                    (Some(member), remaining)
                }
                else {
                    (None, remaining)
                }
            };

            text = skip_white(remaining);
            members.push((member_name, member_kind));
        }

        (Expression::Sum(members), text)
    }
    else {
        parse_product_or_other(text)
    }
}

fn parse_product_or_other(text: Text) -> (Expression, Text) {
    let text = skip_white(text);

    if text.starts_with("&") {
        let mut text = skip_white(&text);

        let mut members = Vec::new();
        while let Some(remaining) = skip_symbol(text, "&") {
            //if let Some(letter) = remaining.chars().next() {
            //    if letter.is_lowercase() { // parse named member
            let (member_name, remaining) = parse_identifier(remaining);
            let mut remaining = skip_white(remaining);

            let remaining = skip_symbol(remaining, ":")
                .or(skip_symbol(remaining, "=")) // TODO remember pub type vs value
                .unwrap();

            let (member_kind, remaining) = parse_atom(remaining);

            text = skip_white(remaining);
            members.push((member_name, member_kind));
            //    }
            //    else { // parse base pub type

            //    }
//            }
//            else {
//                panic!("Expected composition member or base");
//            }
        }

        (Expression::Product(members), text)
    }
    else {
        parse_atom(text)
    }
}

fn parse_atom(text: Text) -> (Expression, Text) {
    let text = skip_white(text);

    if let Some((number, remaining)) = parse_number(text) {
        (Expression::Number(number), remaining)
    }
    else if let Some(remaining) = skip_symbol(text, "(") {
        let (contents, remaining) = parse_expression(remaining);

        let mut remaining = skip_white(remaining);
        let remaining = skip_symbol(remaining, ")").unwrap();
        (contents, remaining)
    }
    else if let Some(remaining) = skip_symbol(text, "\"") {
        if let Some(end_index) = remaining.find(|c| c == '"') {
            let (string, remaining) = remaining.split_at(end_index);
            let remaining = skip_symbol(remaining, "\"").unwrap();
            (Expression::String(string), remaining)
        }
        else { panic!("Expected closing `\"`") }
    }
    else {
        let (reference, remaining) = parse_reference(text);
        if reference.is_empty() { panic!("expected pub type declaration") }
        (Expression::Named(reference), remaining)
    }
}

fn parse_number(text: Text) -> Option<(Text, Text)> {
    let end_index = skip_white(text)
        .find(|c:char| !(c.is_digit(10) || c == '.'));

    if let Some(index) = end_index {
        if index == 0 { None }
        else { Some(text.split_at(index)) }
    }
    else {
        Some((text, ""))
    }
}

fn parse_reference(text: Text) -> (Reference, Text) {
    let (first, text) = parse_identifier(skip_white(text))/*?*/;
    let mut parts = vec![ first ];

    let mut text = skip_white(text);
    while let Some(remaining) = skip_symbol(text, ".") {
        let (identifier, remaining) = parse_identifier(remaining);
        text = skip_white(remaining);

        parts.push(identifier);
    }

    (parts, text)
}


// name, prop1 & prop2,
fn parse_identifier(text: Text) -> (Text, Text) {
    let text = skip_white(text);

    let is_not_identifier = |c: char|
        c.is_whitespace() || ("&|@,():=.\"'").contains(c);

    if let Some(identifier_end) = text.find(is_not_identifier){
        if identifier_end == 0
            { panic!("Expected identifier, found `{}`", text) }

        text.split_at(identifier_end)
    }
    else { // no non-identifier found
        if text.is_empty()
            { panic!("Expected identifier") }

        (text, "")
    }
    // TODO fail on empty
}

fn skip_symbol<'a>(text: Text<'a>, symbol: &str) -> Option<Text<'a>> {
    if text.starts_with(symbol) {
        Some(&text[symbol.len()..])
    }
    else {
        None
    }
}

fn skip_white(text: Text) -> Text {
    match text.find(|c: char| !c.is_whitespace()){
        Some(index) => &text[index..],
        None => "" // no non-white space could be found
    }
}


#[cfg(test)]
mod test {
    use super::*;

    macro_rules! reference {
        ($($identifier:ident).*) => {
            Expression::Named(vec![
                $(stringify!($identifier),)*
            ])
        };
    }


    #[test]
    fn test_parse_definition(){
        assert_eq!(
            parse_definition("Name = | name: std.string.String | anonymous"),
            (
                Definition {
                    binding: reference!{ Name },
                    kind: None,
                    expression: Expression::Sum(vec![
                        ("name", Some(reference!{ std.string.String })),
                        ("anonymous", None),
                    ])
                }
                , ""
            )
        );
    }

    #[test]
    fn test_parse_atom(){
        assert_eq!(parse_expression(" Name"), (reference!{ Name }, ""));
        assert_eq!(parse_expression(" a.Name n"), (reference!{ a.Name }, "n"));
        assert_eq!(parse_expression(" a3.1Name 0n"), (Expression::Named(vec!["a3", "1Name"]), "0n"));
        assert_eq!(parse_expression(" 3.1 0n"), (Expression::Number("3.1"), "0n"));
        assert_eq!(parse_expression(" \"legendary\"xo"), (Expression::String("legendary"), "xo"));
    }


    #[test]
    fn test_parse_function_or_other_kind(){
        assert_eq!(
            parse_function_or_other("| name: std.String | anonymous"),
            (Expression::Sum(vec![
                ("name", Some(reference!{ std.String })),
                ("anonymous", None),
            ] ), "")
        );

        assert_eq!(
            parse_function_or_other("String -> Int"),
            (Expression::Function(Function {
                parameter: Box::new(reference!{ String }),
                result: Box::new(reference!{ Int }),
            }), "")
        );

        assert_eq!(
            parse_function_or_other("String -> ,Int,String"),
            (Expression::Function(Function {
                parameter: Box::new(reference!{ String }),
                result: Box::new(Expression::Tuple(vec![
                    reference!{ Int },
                    reference!{ String }
                ])),
            }), "")
        );
    }

    #[test]
    fn test_parse_tuple_or_other_kind(){
        assert_eq!(
            parse_tuple_or_other("| name: std.String | anonymous"),
            (Expression::Sum(vec![
                ("name", Some(reference!{ std.String })),
                ("anonymous", None),
            ] ), "")
        );

        assert_eq!(
            parse_tuple_or_other(",Float ,str.String ,Int"),
            (Expression::Tuple(vec![
                reference!{ Float },
                reference!{ str.String },
                reference!{ Int },
            ]), "")
        );

        /*assert_eq!(
            parse_tuple_or_other(r#" , 5 , "hello" , Int"#),
            (Expression::Tuple(vec![
                Expression::Number("5"),
                Expression::String("hello"),
                reference!{ Int },
            ]), "")
        );*/ // FIXME

        assert_eq!(
            parse_tuple_or_other(",&name:String&age:Int ,str.String ,Int"),
            (Expression::Tuple(vec![
                Expression::Product(vec![
                    ("name", reference!{ String }),
                    ("age", reference!{ Int }),
                ]),
                reference!{ str.String },
                reference!{ Int },
            ]), "")
        );
    }


    #[test]
    fn test_parse_sum_or_other_kind(){
        assert_eq!(parse_sum_or_other(" std .str .String ->"), (reference!{ std.str.String }, "->"));

        assert_eq!(
            parse_sum_or_other("& name: String &age : num.Int"),
            (Expression::Product(vec![
                ("name", reference!{ String }),
                ("age", reference!{ num.Int }),
            ]), "")
        );

        assert_eq!(
            parse_sum_or_other("| name: std.String | anonymous"),
            (Expression::Sum(vec![
                ("name", Some(reference!{ std.String })),
                ("anonymous", None),
            ] ), "")
        );

        assert_eq!(
            parse_sum_or_other("| name: &value: std.String &length: Int | anonymous"),
            (Expression::Sum(vec![
                ("name", Some(Expression::Product(vec![
                    ("value", reference!{ std.String }),
                    ("length", reference!{ Int }),
                ]))),
                ("anonymous", None),
            ] ), "")
        );
    }

    #[test]
    fn test_parse_product_or_other_kind(){
        assert_eq!(parse_product_or_other(" String #"), (reference!{ String }, "#"));
        assert_eq!(parse_product_or_other(" std.String #"), (reference!{ std.String }, "#"));
        assert_eq!(parse_product_or_other(" std .str .String #"), (reference!{ std.str.String }, "#"));

        assert_eq!(
            parse_sum_or_other("& name: String &age : num.Int"),
            (Expression::Product(vec![
                ("name", Expression::Named(vec!["String"])),
                ("age", Expression::Named(vec!["num", "Int"])),
            ]), "")
        );

        assert_eq!(
            parse_sum_or_other(r#"& name = "world" &age = 12"#),
            (Expression::Product(vec![
                ("name", Expression::String("world")),
                ("age", Expression::Number("12")),
            ]), "")
        );
    }

    #[test]
    fn test_parse_reference(){
        assert_eq!(parse_reference("hello world"), (vec!["hello"], "world"));
        assert_eq!(parse_reference("hello.world"), (vec!["hello", "world"], ""));
        assert_eq!(parse_reference(" the .world "), (vec!["the", "world"], ""));
        assert_eq!(parse_reference("hello .world but not this"), (vec!["hello", "world"], "but not this"));
        assert_eq!(parse_reference(" hello .world+"), (vec!["hello", "world+"], ""));
        assert_eq!(parse_reference("hello .world@"), (vec!["hello", "world"], "@"));
    }

    #[test]
    fn test_parse_identifier(){
        assert_eq!(parse_identifier("hello world"), (("hello", " world")));
        assert_eq!(parse_identifier("hello.world"), (("hello", ".world")));
        assert_eq!(parse_identifier(" the .world "), (("the", " .world ")));
        assert_eq!(parse_identifier(" world+"), (("world+", "")));
        assert_eq!(parse_identifier(" world@"), (("world", "@")));
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