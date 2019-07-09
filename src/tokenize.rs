
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

    /// `| a | b: B | c`,  `|value = 5`, `|empty`, `|value: A |empty`, also used for destructuring assignments
    Variants (Vec<Token>),

    /// `& a = 5 & b = 6`, `& a: A & b: C`, also used for destructuring assignments, ...
    Structure (Vec<Token>),

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


    // OPERATOR PRECEDENCE:
    // =
    // :
    // =>
    // ->
    // \t
    // ,
    // &
    // = (inside structures, assignments have highest level again)
    // : (inside structures, annotations have second highest level again)
    // |
    // = (inside variants, assignments have highest level again)
    // : (inside variants, annotations have second highest level again)
    // function argument
    // .
    // ()
    // ""
    // identifier




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
        self.parse_repeated(",", Token::Tuple, Self::parse_structure)
    }

    fn parse_structure(&mut self) -> ParseResult<Token> {
        self.parse_repeated(
            "&", Token::Structure,
//            Self::parse_variants //
            //|this| this.parse_identified(Self::parse_variants)
            Self::parse_structure_assignment
        )
    }

    fn parse_structure_assignment(&mut self) -> ParseResult<Token> {
        self.parse_binary("=", Token::Assignment, Self::parse_structure_annotation)
    }

    fn parse_structure_annotation(&mut self) -> ParseResult<Token> {
        self.parse_binary(":", Token::Annotation, Self::parse_variants)
    }

    fn parse_variants(&mut self) -> ParseResult<Token> {
        self.parse_repeated(
            "|", Token::Variants,
//            Self::parse_application
//                |this| this.parse_identified(Self::parse_application)
    Self::parse_variants_assignment
        )
    }

    fn parse_variants_assignment(&mut self) -> ParseResult<Token> {
        self.parse_binary("=", Token::Assignment, Self::parse_variants_annotation)
    }

    fn parse_variants_annotation(&mut self) -> ParseResult<Token> {
        self.parse_binary(":", Token::Annotation, Self::parse_application)
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

    /*fn parse_identified<F>(&mut self, sub_parser: F) -> ParseResult<Token>
        where F: Fn(&mut Self) -> ParseResult<Token>,
    {
        let identifier = sub_parser(self)?;

        if self.skip_white_and_symbol("=")? {
            Ok(Token::Assignment(Box::new(identifier), Box::new(sub_parser(self)?)))
        }
        else if self.skip_white_and_symbol(":")? {
            Ok(Token::Annotation(Box::new(identifier), Box::new(sub_parser(self)?)))
        }
        else {
            Ok(identifier)
        }
    }*/

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

    macro_rules! tokens {
        (identifiers $($identifier:ident).*) => {
            Token::Access(vec![
                $( tokens! { identifier $identifier } ,)*
            ])
        };
        (identifier $name:ident) => {
                Token::Identifier(stringify!($name) .to_owned())
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
    fn test_parse_element(){
        assert_eq!(source("a | b: T").parse_element(), Ok(
            Token::Variants(vec![
                Token::Identifier("a".to_owned()),
                Token::Annotation(
                    Box::new(Token::Identifier("b".to_owned())),
                    Box::new(Token::Identifier("T".to_owned())),
                ),
            ])
        ));

        assert_eq!(source("a & b: T & c: F").parse_element(), Ok(
            Token::Structure(vec![
                Token::Identifier("a".to_owned()),
                Token::Annotation(
                    Box::new(Token::Identifier("b".to_owned())),
                    Box::new(Token::Identifier("T".to_owned())),
                ),
                Token::Annotation(
                    Box::new(Token::Identifier("c".to_owned())),
                    Box::new(Token::Identifier("F".to_owned())),
                ),
            ])
        ));

        assert_eq!(source("a & b: (c: A & d: B)").parse_element(), Ok(
            Token::Structure(vec![
                Token::Identifier("a".to_owned()),
                Token::Annotation(
                    Box::new(Token::Identifier("b".to_owned())),
                    Box::new(Token::Structure(vec![
                        Token::Annotation(
                            Box::new(Token::Identifier("c".to_owned())),
                            Box::new(Token::Identifier("A".to_owned())),
                        ),
                        Token::Annotation(
                            Box::new(Token::Identifier("d".to_owned())),
                            Box::new(Token::Identifier("B".to_owned())),
                        ),
                    ])),
                ),

            ])
        ));

        assert_eq!(source("a | b: (c: A & d: B)").parse_element(), Ok(
            Token::Variants(vec![
                Token::Identifier("a".to_owned()),
                Token::Annotation(
                    Box::new(Token::Identifier("b".to_owned())),
                    Box::new(Token::Structure(vec![
                        Token::Annotation(
                            Box::new(Token::Identifier("c".to_owned())),
                            Box::new(Token::Identifier("A".to_owned())),
                        ),
                        Token::Annotation(
                            Box::new(Token::Identifier("d".to_owned())),
                            Box::new(Token::Identifier("B".to_owned())),
                        ),
                    ])),
                ),

            ])
        ));


        // TODO test tuples, assignment, file, scope, function, application

    }


    #[test]
    fn test_parse_chained_identifier(){
        assert_eq!(source("hello world").parse_access(), Ok(tokens!{ identifier hello }));
        assert_eq!(source("hello_world").parse_access(), Ok(tokens!{ identifier hello_world }));
        assert_eq!(source("hello . world.there").parse_access(), Ok(tokens!{ identifiers hello.world.there }));
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