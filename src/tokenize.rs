
// List, new, +, ...
pub type Identifier = String;

// std.List, List.new, pet.name, ...
pub type Reference = Vec<String>;


#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct Source<'s> {
    pub line_indentation: usize,
    pub line_number: usize,
    pub line_byte_index: usize,

    pub source: &'s str,
    pub remaining: &'s str,
}



// TODO any values, including case-expressions, not supported yet


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
    Function (Box<Function>),

    /// `a,b,c`, `A,B,C`, also used for destructuring assignments
    Tuple (Vec<Expression>),

    /// `| a | b: B | c`,  `|value = 5`, `|empty`, `|value: A |empty`
    Sum (Vec<Variant>),

    /// `& a = 5 & b = 6`, `a: A & b: C`, also used for destructuring assignments, ...
    Product (Vec<ProductMember>),

    /// std.string.String, Int, window.width, ...
    Identifier(Reference),

    /// `get_name person`, `List Int`, `get_name $` ...
    FunctionApplication (Box<FunctionApplication>),

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
    pub subject: Expression,
    pub argument: Expression
}

/// includes function pub type declarations but also lambda expressions
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Function {
    pub parameter: Expression, // this is a destructuring declaration
    pub result: Expression
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Scope {
    pub definitions: Vec<Definition>,
    pub result: Box<Expression>
}




pub type ParseResult<T> = std::result::Result<T, ParseError>;


#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ParseError {
    ExpectedAtom { source: SourceLocation },
    ExpectedIdentifier { source: SourceLocation },
    ExpectedSymbol { symbol: String, source: SourceLocation },
    TabNotAllowed
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SourceLocation {
    byte_location: usize,
    line_number: usize,
    line_byte_loction: usize
}


pub fn parse(file: &str) -> ParseResult<File> {
    Source::new(file).parse_file()
}


pub fn parse_expression(expression: &str) -> ParseResult<Expression> {
    Source::new(expression).parse_expression()
}



impl<'s> Source<'s> {
    fn new(text: &str) -> Source {
        Source {
            line_indentation: 0,
            line_number: 0,
            line_byte_index: 0,
            source: text,
            remaining: text
        }
    }

    fn location(&self) -> SourceLocation {
        SourceLocation {
            line_number: self.line_number,
            line_byte_loction: self.line_byte_index,
            byte_location: self.source.len() - self.remaining.len(),
        }
    }

    fn is_special_symbol(symbol: char) -> bool {
        symbol.is_whitespace() || "$&|@,():=.\"'<->+*/".contains(symbol)
    }


    fn parse_file(&mut self) -> ParseResult<File> {
        let mut definitions = Vec::new();

        while !self.remaining.trim_start().is_empty() {
            definitions.push(self.parse_definition()?);
        }

        Ok(File { definitions })
    }

    fn parse_definition(&mut self) -> ParseResult<Definition> {
        let binding = self.parse_expression()?;

        if self.skip_symbol(":") {
            let annotation = self.parse_expression()?;
            self.skip_white_and_required_symbol("=")?;
            let expression = self.parse_expression()?;

            Ok(Definition {
                binding,
                kind: Some(annotation),
                expression
            })
        }
        else if self.skip_white_and_symbol("=")? {
            let expression = self.parse_expression()?;

            Ok(Definition {
                binding,
                kind: None,
                expression
            })
        }
        else {
            Err(ParseError::ExpectedSymbol { symbol: "=".to_owned(), source: self.location() })
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        self.parse_scope()
    }

    fn parse_scope(&mut self) -> ParseResult<Expression> {
        let old_indentation = self.line_indentation;
        let old_line = self.line_number;

        self.skip_white()?;
        if self.line_number != old_line && self.line_indentation > old_indentation {

            let mut scope_definitions = Vec::new();

            let result_expression = loop {
                self.skip_white()?;

                if self.line_number != old_line && self.line_indentation < old_indentation {
                    let result = self.parse_function()?;
//                    if self.skip_symbol(":") || self.skip_symbol("=") {
//                        return Err(ParseError::LastLineMustBeResult);
//                    }
                    break result
                }


                let binding_or_result = self.parse_function()?;

                if self.skip_symbol(":") {
                    let annotation = self.parse_function()?;
                    self.skip_white_and_required_symbol("=")?;
                    let expression = self.parse_function()?;

                    scope_definitions.push(Definition {
                        binding: binding_or_result,
                        kind: Some(annotation),
                        expression
                    })
                }
                else if self.skip_white_and_symbol("=")? {
                    let expression = self.parse_function()?;

                    scope_definitions.push(Definition {
                        binding: binding_or_result,
                        kind: None,
                        expression
                    })
                }
                else {
                    break binding_or_result
                }
            };

            if !scope_definitions.is_empty() {
                Ok(Expression::Scope(Scope {
                    definitions: scope_definitions,
                    result: Box::new(result_expression)
                }))
            }
            else {
                Ok(result_expression)
            }
        }

        else {
            self.parse_function()
        }

    }


    fn parse_function(&mut self) -> ParseResult<Expression> {
        let first = self.parse_function_application()?;

        if self.skip_white_and_symbol("->")? {
            let result = self.parse_function_application()?;

            Ok(Expression::Function(Box::new(Function {
                parameter: first,
                result
            })))
        }
        else {
            Ok(first)
        }
    }


    fn parse_function_application(&mut self) -> ParseResult<Expression> {
        let first = self.parse_tuple()?;

        fn is_arg_symbol(c: char) -> bool {
            c == '$' || c == '(' || ! Source::is_special_symbol(c)
        }

        if self.remaining.starts_with(is_arg_symbol) {
            let argument = self.parse_tuple()?;
            Ok(Expression::FunctionApplication(Box::new(FunctionApplication {
                subject: first, argument
            })))
        }
        else {
            Ok(first)
        }
    }

    fn parse_tuple(&mut self) -> ParseResult<Expression> {
        self.skip_white_and_symbol(",")?;

        let mut members = vec![ self.parse_sum()? ];
        while self.skip_white_and_symbol(",")? {
            members.push(self.parse_sum()?);
        }

        if members.len() < 2 {
            Ok(members.remove(0))
        }
        else {
            Ok(Expression::Tuple(members))
        }
    }

    fn parse_sum(&mut self) -> ParseResult<Expression> {
        let mut variants = Vec::new();
        while self.skip_white_and_symbol("|")? {
            variants.push(self.extract_variant()?)
        }

        if !variants.is_empty() {
            Ok(Expression::Sum(variants))
        }
        else {
            self.parse_product()
        }
    }

    fn extract_variant(&mut self) -> ParseResult<Variant> {
        let name = self.parse_identifier()?;

        let content = {
            if self.skip_white_and_symbol("=")? {
                Some(VariantContent {
                    expression: self.parse_product()?,
                    operator: '='
                })
            }
            else if self.skip_white_and_symbol(":")? {
                Some(VariantContent {
                    expression: self.parse_product()?,
                    operator: ':'
                })
            }
            else { None }
        };

        Ok(Variant {
            name, content
        })
    }

    fn parse_product(&mut self) -> ParseResult<Expression> {

        let mut members = Vec::new();
        while self.skip_white_and_symbol("&")? {
            members.push(self.extract_member()?);
        }

        if !members.is_empty() {
            Ok(Expression::Product(members))
        }
        else {
            self.parse_atom()
        }
    }

    fn extract_member(&mut self) -> ParseResult<ProductMember> {
        let name = self.parse_identifier()?;

        let operator = {
            if self.skip_white_and_symbol("=")? { '=' }
            else if self.skip_white_and_symbol(":")? { ':' }

            else {
                return Err(ParseError::ExpectedSymbol {
                    symbol: "= or :".to_owned(),
                    source: self.location()
                })
            }
        };

        let expression = self.parse_atom()?;

        Ok(ProductMember {
            name, expression, operator
        })
    }

    fn parse_atom(&mut self) -> ParseResult<Expression> {
        self.skip_white()?;

        if let Some(number) = self.attempt_parse_number() {
            Ok(Expression::Number(number.to_owned()))
        }

        else if self.skip_symbol("(") {
            let group = self.parse_expression()?;
            self.skip_white_and_required_symbol(")")?;
            Ok(group)
        }

        else if self.skip_symbol("\"") {
            let string = self.extract_chars_until(|c:char| c == '"').to_owned();
            self.skip_required_symbol("\"")?;
            Ok(Expression::String(string))
        }

        else {
            self.parse_reference()

                .map_err(|error|{ match error {
                    ParseError::ExpectedIdentifier { source } => ParseError::ExpectedAtom { source },
                    other => other
                }})
        }
    }

    fn attempt_parse_number(&mut self) -> Option<String> {
        fn is_not_number(c: char) -> bool {
            !c.is_digit(10) && c != '.'
        }

        let mut number_source = *self;
        let number = number_source.extract_chars_until(is_not_number).to_owned();

        if !number.is_empty() {
            // only parse number if it is finished with the next symbol
            let successor_finishes_number = {
                if let Some(successor) = number_source.remaining.chars().next() {
                    Source::is_special_symbol(successor)
                }
                else {
                    true
                }
            };

            if successor_finishes_number {
                // apply parsing changes and then return
                *self = number_source;
                Some(number)
            }
            else {
                None
            }
        }
        else {
            // do not apply parsing changes
            None
        }
    }



    fn parse_reference(&mut self) -> ParseResult<Expression> {
        let first = self.parse_identifier()?;
        let mut parts = vec![ first ];

        while self.skip_white_and_symbol(".")? {
            parts.push(self.parse_identifier()?);
        }

        if !parts.is_empty() {
            Ok(Expression::Identifier(parts))
        }
        else {
            panic!();
            Err(ParseError::ExpectedIdentifier { source: self.location() })
        }
    }



    /// name, _the_parameter, ...
    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        self.skip_white()?;

        fn is_not_identifier (c: char) -> bool {
            c.is_whitespace() || ("&|@,():=.\"'").contains(c)
        }

        let name = self.extract_chars_until(is_not_identifier);

        if !name.is_empty() {
            Ok(name.to_owned())
        }

        else {
            panic!();
            Err(ParseError::ExpectedIdentifier { source: self.location() })
        }
    }

    /// returns empty string on immediately found
    fn extract_chars_until<F>(&mut self, end_where: F) -> &str
        where F: Fn(char) -> bool
    {
        if let Some(end) = self.remaining.find(end_where){
            let (result, remaining) = self.remaining.split_at(end);

            self.line_byte_index += result.len();
            self.remaining = remaining;

            result
        }
        else { // if no end is found, parse all the remaining
            let result = self.remaining;
            self.line_byte_index += self.remaining.len();
            self.remaining = "";

            result
        }
    }

    fn skip_white_and_required_symbol(&mut self, symbol: &str) -> ParseResult<()> {
        self.skip_white()?;
        self.skip_required_symbol(symbol)
    }

    fn skip_required_symbol(&mut self, symbol: &str) -> ParseResult<()> {
        if self.skip_symbol(symbol) { Ok(()) }

        else {
            Err(ParseError::ExpectedSymbol { symbol: symbol.to_owned(), source: self.location() })
        }
    }

    fn skip_white_and_symbol(&mut self, symbol: &str) -> ParseResult<bool> {
        self.skip_white()?;
        Ok(self.skip_symbol(symbol))
    }

    /// does not update line index stats
    fn skip_symbol(&mut self, symbol: &str) -> bool {
        if self.remaining.starts_with(symbol) {
            self.remaining = &self.remaining[ symbol.len() .. ];
            self.line_byte_index += symbol.len();

            true
        }
        else {
            false
        }
    }

    fn skip_white(&mut self) -> ParseResult<()> {
        loop {
            if self.skip_symbol("\n") {
                self.line_number += 1;
                self.line_indentation = 0;
                self.line_byte_index = 0;
            }

            else if self.skip_symbol(" ") {

                // line_indentation counts the spaces, and the space char is 1 byte,
                // so we can simply compare line_byte_index and indentation
                if self.line_indentation + 1 == self.line_byte_index {

                    // as long as the remaining line starts with a whitespace,
                    // pull the line_indentation along with the byte index
                    self.line_indentation += 1;
                }
            }

            else if self.remaining.starts_with("\t") {
                return Err(ParseError::TabNotAllowed)
            }

            else {
                break
            }
        }

        Ok(())
    }
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


    fn source(source: &str) -> Source {
        Source {
            line_indentation: 0,
            line_number: 0,
            line_byte_index: 0,
            remaining: source,
            source,
        }
    }

    #[test]
    fn test_parse_definition(){
        assert_eq!(
            source("Name = | name: std.string.String | anonymous").parse_definition(),
            Ok(Definition {
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
            })
        );
    }

    #[test]
    fn test_parse_application(){
        assert_eq!(
            source("string.trim user_name").parse_function_application(),
            Ok(Expression::FunctionApplication(Box::new(FunctionApplication {
                subject: name!{ string.trim },
                argument: name! { user_name }
            })))
        );
    }

    #[test]
    fn test_parse_atom(){
        assert_eq!(source(" Name").parse_atom(), Ok(name!{ Name }));
        assert_eq!(source(" a.Name n").parse_atom(), Ok(name!{ a.Name }));
        assert_eq!(source(" a3.1Name 0n").parse_atom(), Ok(Expression::Identifier(vec!["a3".to_owned(), "1Name".to_owned()])));
        assert_eq!(source(" 3.1 0n").parse_atom(), Ok(Expression::Number("3.1".to_owned())));
        assert_eq!(source(" \"legendary\"xo").parse_atom(), Ok(Expression::String("legendary".to_owned())));
    }


    #[test]
    fn test_parse_function_or_other_kind(){
        assert_eq!(
            source("| name: std.String | anonymous").parse_function(),
            Ok(Expression::Sum(vec![
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
            ] ))
        );

        assert_eq!(
            source("String -> Int").parse_function(),
            Ok(Expression::Function(Box::new(Function {
                parameter: name!{ String },
                result: name!{ Int },
            })))
        );

        assert_eq!(
            source("String -> ,Int,String").parse_function(),
            Ok(Expression::Function(Box::new(Function {
                parameter: name!{ String },
                result: Expression::Tuple(vec![
                    name!{ Int },
                    name!{ String }
                ]),
            })))
        );

        assert_eq!(
            source("a,b -> &name = \"ab\" & age = 10 ").parse_function(),
            Ok(Expression::Function(Box::new(Function {
                parameter: Expression::Tuple(vec![name!{ a }, name! { b } ]),
                result: Expression::Product(vec![
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
                ]),
            })))
        );
    }

    #[test]
    fn test_parse_tuple_or_other_kind(){
        assert_eq!(
            source(",Float ,str.String ,Int").parse_tuple(),
            Ok(Expression::Tuple(vec![
                name!{ Float },
                name!{ str.String },
                name!{ Int },
            ]))
        );

        assert_eq!(
            source(r#" , 5 , "hello" , Int"#).parse_tuple(),
            Ok(Expression::Tuple(vec![
                Expression::Number("5".to_owned()),
                Expression::String("hello".to_owned()),
                name!{ Int },
            ]))
        );

        assert_eq!(
            source(",&name:String&age:Int ,str.String ,Int").parse_tuple(),
            Ok(Expression::Tuple(vec![
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
            ]))
        );
    }


    #[test]
    fn test_parse_sum_or_other_kind(){
        assert_eq!(source(" std .str .String ->").parse_sum(), Ok(name!{ std.str.String }));

        assert_eq!(
            source("| name: std.String | anonymous").parse_sum(),
            Ok(Expression::Sum(vec![
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
            ] ))
        );

        assert_eq!(
            source("| name: &value: std.String &length: Int | anonymous").parse_sum(),
            Ok(Expression::Sum(vec![
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
            ] ))
        );
    }

    #[test]
    fn test_parse_product_or_other_kind(){
        assert_eq!(source(" String").parse_product(), Ok(name!{ String }));
        assert_eq!(source(" std.String").parse_product(), Ok(name!{ std.String }));
        assert_eq!(source(" std .str .String").parse_product(), Ok(name!{ std.str.String }));

        assert_eq!(
            source("& name: String &age : num.Int").parse_product(),
            Ok(Expression::Product(vec![
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
            ]))
        );

        assert_eq!(
            source(r#"& name = "world" &age = 12"#).parse_product(),
            Ok(Expression::Product(vec![

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
            ]))
        );
    }

    #[test]
    fn test_parse_chained_identifier(){
        assert_eq!(source("hello world").parse_reference(), Ok(name!{ hello }));
        assert_eq!(source("hello.world").parse_reference(), Ok(name!{ hello.world }));
        assert_eq!(source(" the .world ").parse_reference(), Ok(name!{ the.world }));
        assert_eq!(source("hello .world but not this").parse_reference(), Ok(name!{ hello.world }));
        assert_eq!(source("hello .world@").parse_reference(), Ok(name!{ hello.world }));
    }

    #[test]
    fn test_parse_identifier(){
        assert_eq!(source("hello world").parse_identifier(), Ok("hello".to_owned()));
        assert_eq!(source("hello.world").parse_identifier(), Ok("hello".to_owned()));
        assert_eq!(source(" the .world ").parse_identifier(), Ok("the".to_owned()));
        assert_eq!(source(" world+").parse_identifier(), Ok("world+".to_owned()));
        assert_eq!(source(" world@").parse_identifier(), Ok("world".to_owned()));
        // assert_panics!(parse_identifier("("), (("", "(")));
        // assert_panics!(parse_identifier(""), (("", "")));
    }

    #[test]
    fn test_skip_symbol(){
        {
            let mut s = source("abc");
            assert_eq!(s.skip_symbol("a"), true);
            assert_eq!(s.remaining, "bc");

            assert_eq!(s.skip_symbol("a"), false);
            assert_eq!(s.remaining, "bc");

            assert_eq!(s.skip_symbol("b"), true);
            assert_eq!(s.remaining, "c");
        }

        assert_eq!(source("x").skip_symbol("x"), true);
        assert_eq!(source(" t").skip_symbol("t"), false);
    }

    #[test]
    fn test_skip_white(){
        let mut src : Source = source("a\nb \n  c\n d ");

        src.skip_white().unwrap();
        assert_eq!(src.line_number, 0);

        src.skip_required_symbol("a").unwrap();
        println!("skipped a {:?}", src);
        src.skip_white().unwrap();
        assert_eq!(src.line_number, 1);
        assert_eq!(src.line_indentation, 0);


        src.skip_required_symbol("b").unwrap();
        println!("skipped b {:?}", src);
        assert_eq!(src.line_number, 1);
        assert_eq!(src.line_indentation, 0);
        src.skip_white().unwrap();
        assert_eq!(src.line_number, 2);
        assert_eq!(src.line_indentation, 2);

        src.skip_required_symbol("c").unwrap();
        println!("skipped c {:?}", src);
        src.skip_white().unwrap();
        assert_eq!(src.line_number, 3);
        assert_eq!(src.line_indentation, 1);

        src.skip_required_symbol("d").unwrap();
        assert_eq!(src.line_number, 3);
        assert_eq!(src.line_indentation, 1);
        src.skip_white().unwrap();
        assert_eq!(src.line_number, 3);
        assert_eq!(src.line_indentation, 1);


    }

}