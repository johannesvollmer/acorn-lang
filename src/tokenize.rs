
// List, new, +, ...
pub type Identifier = String;

// std.List, List.new, pet.name, ...
pub type Reference = Vec<String>;


#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct Tokenizer<'s> {
    pub line_indentation: usize,
    pub line_number: usize,
    pub line_byte_index: usize,

    pub source: &'s str,
    pub remaining: &'s str,
}



// TODO any values, including case-expressions, not supported yet


/// token tree.
/// numbers are just constants named '4' or '8' that already have a value defined.
/// floating point numbers also have fields called '0' and '141592' and such.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Token {
    /// multi-line function bodies, modules, ...
    Scope (Vec<Token>),

    /// a = 5,  a & b = 5 & "hello"
    Assignment(Box<Token>, Box<Token>),

    /// b: Int, `(a, b): (String, Int)`
    Annotation(Box<Token>, Box<Token>),

    /// A -> B
    ValueFunction(Box<Token>, Box<Token>),

    /// A => B
    TypeFunction (Box<Token>, Box<Token>),

    /// `a,b,c`, `A,B,C`, also used for destructuring assignments
    Tuple (Vec<Token>),

    /// `| a | b: B | c`,  `|value = 5`, `|empty`, `|value: A |empty`
    OneOfSet (Vec<Token>),

    /// `& a = 5 & b = 6`, `& a: A & b: C`, also used for destructuring assignments, ...
    AllOfSet (Vec<Token>),

    /// `get_name person`, `List Int`, `get_name $` ...
    Application(Box<Token>, Box<Token>),

    /// `std.string.String`
    Access(Vec<Token>),

    /// std, Int, width, 3, 141592, ...
    Identifier(String),

    /// "hello", "world"
    String (String),
}



pub type ParseResult<T> = std::result::Result<T, ParseError>;


#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ParseError {
    ExpectedAtom { location: SourceLocation },
    ExpectedIdentifier { location: SourceLocation },
    ExpectedSymbol { symbol: String, location: SourceLocation },
    TabNotAllowed { location: SourceLocation }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SourceLocation {
    absolute_byte_location: usize,
    line_number: usize,
    line_byte_index: usize
}


pub fn tokenize(file: &str) -> ParseResult<Token> {
    Tokenizer::new(file).parse_file()
}




impl<'s> Tokenizer<'s> {
    fn new(text: &str) -> Tokenizer {
        Tokenizer {
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
            line_byte_index: self.line_byte_index,
            absolute_byte_location: self.source.len() - self.remaining.len(),
        }
    }

    fn is_special_symbol(symbol: char) -> bool {
        symbol.is_whitespace() || "$&|@,():=.\"'<->+*/".contains(symbol)
    }


    fn parse_file(&mut self) -> ParseResult<Token> {
        let mut contents = Vec::new();

        loop {
            self.skip_white();
            if !self.remaining.trim_start().is_empty() {
                contents.push(self.parse_element()?)
            }
            else {
                break Ok(Token::Scope(contents))
            }
        }
    }

    fn parse_element(&mut self) -> ParseResult<Token> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> ParseResult<Token> {
        self.parse_binary("=", Token::Assignment, Self::parse_annotation)
    }

    fn parse_annotation(&mut self) -> ParseResult<Token> {
        self.parse_binary(":", Token::Annotation, Self::parse_type_function)
    }

    fn parse_type_function(&mut self) -> ParseResult<Token> {
        self.parse_binary("=>", Token::TypeFunction, Self::parse_value_function)
    }

    fn parse_value_function(&mut self) -> ParseResult<Token> {
        self.parse_binary("->", Token::ValueFunction, Self::parse_tuple)
    }

    fn parse_scope(&mut self) -> ParseResult<Token> {
        self.parse_scoped(Self::parse_element, Self::parse_tuple)
    }

    fn parse_tuple(&mut self) -> ParseResult<Token> {
        self.parse_repeated(",", Token::Tuple, Self::parse_all_of_set)
    }

    fn parse_all_of_set(&mut self) -> ParseResult<Token> {
        self.parse_repeated(
            "&", Token::AllOfSet,
            Self::parse_element // |this| this.parse_identified(Self::parse_one_of_set)
        )
    }

    fn parse_one_of_set(&mut self) -> ParseResult<Token> {
        self.parse_repeated(
            "|", Token::OneOfSet,
            Self::parse_element // |this| this.parse_identified(Self::parse_application)
        )
    }

    fn parse_application(&mut self) -> ParseResult<Token> {
        let function = self.parse_access()?;

        if self.remaining.starts_with(" ") {
            self.skip_white();

            if !self.remaining.starts_with(Self::is_special_symbol)
                || self.remaining.starts_with("(") || self.remaining.starts_with("$")
            {
                let argument = self.parse_access()?;
                Ok(Token::Application(Box::new(function), Box::new(argument)))
            }
            else {
                Ok(function)
            }
        }
        else {
            Ok(function)
        }
    }

    fn parse_access(&mut self) -> ParseResult<Token> {
        self.parse_repeated(".", Token::Access, Self::parse_atom)
    }


    /// parse either brackets, string literals, or identifiers (including number literals)
    fn parse_atom(&mut self) -> ParseResult<Token> {
        self.skip_white()?;

        if self.skip_symbol("(") {
            let group = self.parse_element()?;
            self.skip_white_and_required_symbol(")")?;
            Ok(group)
        }

        else if self.skip_symbol("\"") {
            let string = self.extract_chars_until(|c:char| c == '"');
            self.skip_required_symbol("\"")?;
            Ok(Token::String(string.to_owned()))
        }

        else {
            // name, _the_parameter, 3, 141592, ... (includes number literals)
            let identifier = self.extract_chars_until(Self::is_special_symbol);
            if !identifier.is_empty() { Ok(Token::Identifier(identifier.to_owned())) }
            else { Err(ParseError::ExpectedAtom { location: self.location() }) }
        }
    }





    fn parse_scoped<FI, FE>(&mut self, indented_parser: FI, else_parser: FE) -> ParseResult<Token>
        where FI: Fn(&mut Self) -> ParseResult<Token>,  FE: Fn(&mut Self) -> ParseResult<Token>
    {
        let old_indentation = self.line_indentation;
        let old_line = self.line_number;

        self.skip_white()?;

        if self.line_number != old_line && self.line_indentation > old_indentation {
            let mut lines = Vec::new();

            loop {
                self.skip_white()?;

                if self.line_indentation <= old_indentation {
                    break Ok(Token::Scope(lines));
                }

                else {
                    lines.push(indented_parser(self)?);
                }
            }
        }

        else {
            else_parser(self)
        }
    }

    fn parse_identified<F>(&mut self, sub_parser: F) -> ParseResult<Token>
        where F: Fn(&mut Self) -> ParseResult<Token>,
    {
        let identifier = self.parse_atom()?;

        if self.skip_white_and_symbol("=")? {
            Ok(Token::Assignment(Box::new(identifier), Box::new(sub_parser(self)?)))
        }
        else if self.skip_white_and_symbol(":")? {
            Ok(Token::Annotation(Box::new(identifier), Box::new(sub_parser(self)?)))
        }
        else {
            Ok(identifier)
        }
    }

    fn parse_binary<F, V>(&mut self, operator: &'static str, variant: V, sub_parser: F) -> ParseResult<Token>
        where F: Fn(&mut Self) -> ParseResult<Token>, V: Fn(Box<Token>, Box<Token>) -> Token
    {
        let left = sub_parser(self)?;

        if self.skip_white_and_symbol(operator)? {
            let right = sub_parser(self)?;
            Ok(variant(Box::new(left), Box::new(right)))
        }
        else {
            Ok(left)
        }
    }

    fn parse_repeated<F, V>(&mut self, operator: &'static str, variant: V, sub_parser: F) -> ParseResult<Token>
        where F: Fn(&mut Self) -> ParseResult<Token>, V: Fn(Vec<Token>) -> Token
    {
        let first = sub_parser(self)?;

        self.skip_white();
        if self.remaining.starts_with(operator){
            let mut parts = vec![ first ];

            while self.skip_white_and_symbol(operator)? {
                parts.push(sub_parser(self)?);
            }

            Ok(variant(parts))
        }
        else {
            Ok(first)
        }
    }





    /// returns empty string on immediately found
    fn extract_chars_until<F>(&mut self, end_where: F) -> &'s str
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
            Err(ParseError::ExpectedSymbol { symbol: symbol.to_owned(), location: self.location() })
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
                return Err(ParseError::TabNotAllowed { location: self.location() })
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


    fn source(source: &str) -> Tokenizer {
        Tokenizer {
            line_indentation: 0,
            line_number: 0,
            line_byte_index: 0,
            remaining: source,
            source,
        }
    }

    #[test]
    fn test_parse_definition(){
        /*assert_eq!(
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
        );*/
    }

    #[test]
    fn test_parse_application(){
        /*assert_eq!(
            source("string.trim user_name").parse_function_application(),
            Ok(Expression::FunctionApplication(Box::new(FunctionApplication {
                subject: name!{ string.trim },
                argument: name! { user_name }
            })))
        );*/
    }

    #[test]
    fn test_parse_atom(){
        /*assert_eq!(source(" Name").parse_atom(), Ok(name!{ Name }));
        assert_eq!(source(" a.Name n").parse_atom(), Ok(name!{ a.Name }));
        assert_eq!(source(" a3.1Name 0n").parse_atom(), Ok(Expression::Identifier(vec!["a3".to_owned(), "1Name".to_owned()])));
        assert_eq!(source(" 3.1 0n").parse_atom(), Ok(Expression::Number("3.1".to_owned())));
        assert_eq!(source(" \"legendary\"xo").parse_atom(), Ok(Expression::String("legendary".to_owned())));*/
    }


    #[test]
    fn test_parse_function_or_other_kind(){
        /*assert_eq!(
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
        );*/
    }

    #[test]
    fn test_parse_tuple_or_other_kind(){
        /*assert_eq!(
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
        );*/
    }


    #[test]
    fn test_parse_sum_or_other_kind(){
        /*assert_eq!(source(" std .str .String ->").parse_sum(), Ok(name!{ std.str.String }));

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
        );*/
    }

    #[test]
    fn test_parse_product_or_other_kind(){
        /*assert_eq!(source(" String").parse_product(), Ok(name!{ String }));
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
        );*/
    }

    #[test]
    fn test_parse_chained_identifier(){
//        assert_eq!(source("hello world").parse_reference(), Ok(name!{ hello }));
//        assert_eq!(source("hello.world").parse_reference(), Ok(name!{ hello.world }));
//        assert_eq!(source(" the .world ").parse_reference(), Ok(name!{ the.world }));
//        assert_eq!(source("hello .world but not this").parse_reference(), Ok(name!{ hello.world }));
//        assert_eq!(source("hello .world@").parse_reference(), Ok(name!{ hello.world }));
    }

    #[test]
    fn test_parse_identifier(){
        assert_eq!(source("hello world").parse_atom(), Ok(Token::Identifier("hello".to_owned())));
        assert_eq!(source("hello.world").parse_atom(), Ok(Token::Identifier("hello".to_owned())));
        assert_eq!(source(" the .world ").parse_atom(), Ok(Token::Identifier("the".to_owned())));
        assert_eq!(source(" world+").parse_atom(), Ok(Token::Identifier("world+".to_owned())));
        assert_eq!(source(" world@").parse_atom(), Ok(Token::Identifier("world".to_owned())));
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
        let mut src : Tokenizer = source("a\nb \n  c\n d ");

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