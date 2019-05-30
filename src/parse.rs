
type Text<'a> = &'a str;

// List, new, +, ...
type Identifier<'a> = Text<'a>;

// std.List, List.new, pet.name, ...
type Reference<'a> = Vec<Identifier<'a>>;

// TODO arguments and assignments should support destructuring
// case-expressions not supported yet


// name: String, age = 5, name: String = "Peter"
#[derive(Eq, PartialEq, Debug)]
struct Definition<'a> {
    binding: Binding<'a>, // :
    kind: Option<Kind<'a>>, // =
    expression: Expression<'a>
}

// variable name or destructuring assignment
#[derive(Eq, PartialEq, Debug)]
enum Binding<'a> {
    Tuple(Vec<Binding<'a>>),
    Product(Vec<(Text<'a>, Binding<'a>)>),
    Plain(Text<'a>),
}

// "Peter", List, std::List::new, a = b & b = b
#[derive(Eq, PartialEq, Debug)]
enum Expression<'a> {
    Kind (Kind<'a>),
    Value (Value<'a>)
}


// String, List, & name: String & age: Int32
#[derive(Eq, PartialEq, Debug)]
enum Kind<'a> {
    Function { from: Box<Kind<'a>>, to: Box<Kind<'a>> },
    Tuple (Vec<Kind<'a>>),
    Sum (Vec<(Identifier<'a>, Option<Kind<'a>>)>),
    Product (Vec<(Identifier<'a>, Kind<'a>)>),
    Alias (Reference<'a>),
}


// name, "Pete", 0, 1, a = 0 & b = "c", a -> a
#[derive(Eq, PartialEq, Debug)]
enum Value<'a> {
    Function (FunctionValue<'a>),
    Tuple (Vec<Value<'a>>),
    Variant (Reference<'a>, Box<Value<'a>>),
    Product(Vec<(Identifier<'a>, Value<'a>)>),
    Call (FunctionCall<'a>),
    Alias (Reference<'a>), // variable references
    String (Text<'a>),
    Number (Text<'a>),
}

// add 5, a + b,
#[derive(Eq, PartialEq, Debug)]
struct FunctionCall<'a> {
    function: Box<Value<'a>>,
    argument: Box<Value<'a>>
}

// a,b -> a + b
#[derive(Eq, PartialEq, Debug)]
struct FunctionValue<'a> {
    parameter: Binding<'a>,
    definitions: Vec<Definition<'a>>,
    result: Box<Value<'a>>
}



// identifier : type = value
fn parse_definition(text: Text) -> (Definition, Text) {
    let (binding, text) = parse_binding(text);
    let text = skip_white(text);

    let (type_annotation, text) = {
        if let Some(text) = skip_symbol(text, ":"){
            let (kind, text) = parse_kind(text);
            (Some(kind), text)
        }
        else {
            (None, text)
        }
    };

    let text = skip_white(text);
    let text = skip_symbol(text, "=").unwrap();

    let (expression, text) = {
        if binding.chars().next().unwrap().is_uppercase() {
            let (kind, text) = parse_kind(text);
            (Expression::Kind(kind), text)
        }
        else {
            // let (kind, text) = parse_value(text);
            // (Expression::Value(kind), text)
            panic!()
        }
    };

    (
        Definition { binding, kind: type_annotation, expression },
        text
    )
}



fn parse_kind(text: Text) -> (Kind, Text) {
    parse_function_or_other_kind(text)
}

fn parse_function_or_other_kind(text: Text) -> (Kind, Text) {
    let (first_kind, text) = parse_tuple_or_other_kind(text);
    let text = skip_white(text);

    if let Some(text) = skip_symbol(text, "->") {
        let (output, text) = parse_kind(text);
        (Kind::Function { from:  Box::new(first_kind), to: Box::new(output) }, text)
    }
    else {
        (first_kind, text)
    }
}

fn parse_tuple_or_other_kind(text: Text) -> (Kind, Text) {
    let (first, mut text) = parse_sum_or_other_kind(text);

    if text.starts_with(","){
        let mut members = vec![ first ];

        while let Some(remaining) = skip_symbol(text, ",") {
            let (kind, remaining_text) = parse_sum_or_other_kind(remaining);
            text = skip_white(remaining_text);
            members.push(kind);
        }

        (Kind::Tuple(members), text)
    }
    else {
        (first, text)
    }
}

fn parse_sum_or_other_kind(text: Text) -> (Kind, Text) {
    let text = skip_white(text);

    if text.starts_with("|") {
        let mut text = skip_white(&text);

        let mut members = Vec::new();
        while let Some(remaining) = skip_symbol(text, "|") {
            let (member_name, remaining) = parse_identifier(remaining);
            let mut remaining = skip_white(remaining);

            let (member_kind, remaining) = if remaining.starts_with(":") {
                remaining = skip_white(&remaining[":".len() ..]);
                let (member, remaining) = parse_product_or_other_kind(remaining);
                (Some(member), remaining)
            }
            else {
                (None, remaining)
            };

            text = skip_white(remaining);
            members.push((member_name, member_kind));
        }

        (Kind::Sum(members), text)
    }
    else {
        parse_product_or_other_kind(text)
    }
}

fn parse_product_or_other_kind(text: Text) -> (Kind, Text) {
    let text = skip_white(text);

    if text.starts_with("&") {
        let mut text = skip_white(&text);

        let mut members = Vec::new();
        while let Some(remaining) = skip_symbol(text, "&") {
            //if let Some(letter) = remaining.chars().next() {
            //    if letter.is_lowercase() { // parse named member
                    let (member_name, remaining) = parse_identifier(remaining);
                    let mut remaining = skip_white(remaining);

                    if remaining.starts_with(":") { remaining = skip_white(&remaining[":".len() ..]); }
                    else { panic!("invalid composition member declaration: missing `:`") }

                    let (member_kind, remaining) = parse_reference_or_parentheses_kind(remaining);

                    text = skip_white(remaining);
                    members.push((member_name, member_kind));
            //    }
            //    else { // parse base type

            //    }
//            }
//            else {
//                panic!("Expected composition member or base");
//            }
        }

        (Kind::Product(members), text)
    }
    else {
        parse_reference_or_parentheses_kind(text)
    }
}

fn parse_reference_or_parentheses_kind(text: Text) -> (Kind, Text) {
    let mut text = skip_white(text);

    if let Some(remaining) = skip_symbol(text, "(") {
        let (contents, remaining) = parse_kind(remaining);

        let mut remaining = skip_white(remaining);
        if !remaining.starts_with(")") { panic!("Expected `)`") }
        else { remaining = &remaining[")".len() ..] }

        (contents, remaining)
    }
    else {
        let (reference, remaining) = parse_reference(text);
        if reference.is_empty() { panic!("expected type declaration") }
        (Kind::Alias(reference), remaining)
    }
}


fn parse_reference(mut text: Text) -> (Reference, Text) {
    let (first, text) = parse_identifier(text)/*?*/;
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
            { panic!("Expected identifier") }

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
            Kind::Alias(vec![
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
                    binding: Binding::Plain("Name"),
                    kind: None,
                    expression: Expression::Kind(Kind::Sum(vec![
                        ("name", Some(reference!{ std.string.String })),
                        ("anonymous", None),
                    ]))
                }
                , ""
            )
        );
    }

    #[test]
    fn test_parse_binding(){
        assert_eq!(parse_binding("Name"), (Binding::Plain("Name"), ""));
        assert_eq!(parse_binding("(Name, "), (Binding::Plain("Name"), ""));
    }


    #[test]
    fn test_parse_function_or_other_kind(){
        assert_eq!(
            parse_function_or_other_kind("| name: std.String | anonymous"),
            (Kind::Sum(vec![
                ("name", Some(reference!{ std.String })),
                ("anonymous", None),
            ] ), "")
        );

        assert_eq!(
            parse_function_or_other_kind("String -> Int"),
            (Kind::Function {
                from: Box::new(reference!{ String }),
                to: Box::new(reference!{ Int }),
            }, "")
        );

        assert_eq!(
            parse_function_or_other_kind("String -> ,Int,String"),
            (Kind::Function {
                from: Box::new(reference!{ String }),
                to: Box::new(Kind::Tuple(vec![
                    reference!{ Int },
                    reference!{ String }
                ])),
            }, "")
        );
    }

    #[test]
    fn test_parse_tuple_or_other_kind(){
        assert_eq!(
            parse_tuple_or_other_kind("| name: std.String | anonymous"),
            (Kind::Sum(vec![
                ("name", Some(reference!{ std.String })),
                ("anonymous", None),
            ] ), "")
        );

        assert_eq!(
            parse_tuple_or_other_kind(",Float ,str.String ,Int"),
            (Kind::Tuple(vec![
                reference!{ Float },
                reference!{ str.String },
                reference!{ Int },
            ]), "")
        );

        assert_eq!(
            parse_tuple_or_other_kind(",&name:String&age:Int ,str.String ,Int"),
            (Kind::Tuple(vec![
                Kind::Product(vec![
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
        assert_eq!(parse_sum_or_other_kind(" std .str .String ->"), (reference!{ std.str.String }, "->"));

        assert_eq!(
            parse_sum_or_other_kind("& name: String &age : num.Int"),
            (Kind::Product(vec![
                ("name", reference!{ String }),
                ("age", reference!{ num.Int }),
            ]), "")
        );

        assert_eq!(
            parse_sum_or_other_kind("| name: std.String | anonymous"),
            (Kind::Sum(vec![
                ("name", Some(reference!{ std.String })),
                ("anonymous", None),
            ] ), "")
        );

        assert_eq!(
            parse_sum_or_other_kind("| name: &value: std.String &length: Int | anonymous"),
            (Kind::Sum(vec![
                ("name", Some(Kind::Product(vec![
                    ("value", reference!{ std.String }),
                    ("length", reference!{ Int }),
                ]))),
                ("anonymous", None),
            ] ), "")
        );
    }

    #[test]
    fn test_parse_product_or_other_kind(){
        assert_eq!(parse_product_or_other_kind(" String ->"), (reference!{ String }, "->"));
        assert_eq!(parse_product_or_other_kind(" std.String ->"), (reference!{ std.String }, "->"));
        assert_eq!(parse_product_or_other_kind(" std .str .String ->"), (reference!{ std.str.String }, "->"));

        assert_eq!(
            parse_sum_or_other_kind("& name: String &age : num.Int"),
            (Kind::Product(vec![
                ("name", Kind::Alias(vec!["String"])),
                ("age", Kind::Alias(vec!["num", "Int"])),
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