
// List, new, +, ...
pub type Identifier = String;

// std.List, List.new, pet.name, ...
pub type Reference = Vec<String>;


#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct Tokenizer<'s> {
    // immediate location where we are currently parsing
    pub symbol_indentation: usize,
    pub symbol_line_number: usize,
    pub line_byte_index: usize,

    // location where the last (current) element was placed
    pub element_line_number: usize,
    pub element_indentation: usize,

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

    /// value @5: five -> b @10: ten -> d
    /// (first token in vector is the subject to be switched on)
    Match (Box<Token>, Vec<Token>),

    /// @a == b: "yes" @: "no"
    Conditions (Vec<Token>),

    /// @`|result: value`, @`a == b: value`, @`: default_value`
    Branch (Option<Box<Token>>, Box<Token>),

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


//trait TokenizerFn<'s>: Fn(&mut Tokenizer<'s>) -> ParseResult<Token> {}
//trait BinaryTokenFn: Fn(Box<Token>, Box<Token>) -> Token {}
//trait VecTokenFn: Fn(Vec<Token>) -> Token {}



impl<'s> Tokenizer<'s> {
    fn new(text: &str) -> Tokenizer {
        Tokenizer {
            symbol_indentation: 0,
            symbol_line_number: 0,
            line_byte_index: 0,
            element_line_number: 0,
            element_indentation: 0,
            source: text,
            remaining: text
        }
    }

    fn location(&self) -> SourceLocation {
        SourceLocation {
            line_number: self.symbol_line_number,
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
            self.skip_white()?;
            if !self.remaining.trim_start().is_empty() { // TODO must skip comments
                contents.push(self.parse_element()?)
            }
            else {
                break Ok(Token::Scope(contents))
            }
        }
    }

    fn parse_element(&mut self) -> ParseResult<Token> {
        println!("\n\nparse_element: `{}`", self.remaining);
        self.parse_assignment()
    }


    // OPERATOR PRECEDENCE:
    // =
    // :
    // =>
    // ->
    // @
    // ,
    // &
    //      = (inside structures, assignments have highest level again)
    //      : (inside structures, annotations have second highest level again)

    // |
    //      = (inside variants, assignments have highest level again)
    //      : (inside variants, annotations have second highest level again)

    // function argument
    // [indented scope]
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
        self.parse_binary("->", Token::ValueFunction, Self::parse_match)
    }

    fn parse_match(&mut self) -> ParseResult<Token> {
        self.skip_white()?;
        if self.symbol_is_next("@") {
            self.parse_prefix_repeated(
                "@", Token::Conditions,
                Self::parse_single_branch, Self::parse_tuple
            )
        }
        else {
            let first = self.parse_tuple()?;
            self.skip_white()?;
            if self.symbol_is_next("@") {
                self.parse_prefix_repeated(
                    "@", move |branches| Token::Match(Box::new(first), branches),
                    Self::parse_single_branch, Self::parse_tuple
                )
            }
            else {
                Ok(first)
            }
        }
    }

    fn parse_single_branch(&mut self) -> ParseResult<Token> {
        self.parse_branch(":", Self::parse_scope)
    }

    fn parse_tuple(&mut self) -> ParseResult<Token> {
        self.parse_infix_repeated(",", Token::Tuple, Self::parse_structure)
    }

    fn parse_structure(&mut self) -> ParseResult<Token> {
        self.parse_prefix_repeated("&", Token::Structure, Self::parse_assignment_in_structure, Self::parse_variants)
    }

    fn parse_assignment_in_structure(&mut self) -> ParseResult<Token> {
        self.parse_binary("=", Token::Assignment, Self::parse_annotation_in_structure)
    }

    fn parse_annotation_in_structure(&mut self) -> ParseResult<Token> {
        self.parse_binary(":", Token::Annotation, Self::parse_variants)
    }

    fn parse_variants(&mut self) -> ParseResult<Token> {
        self.parse_prefix_repeated("|", Token::Variants, Self::parse_assignment_in_variants, Self::parse_application)
    }

    fn parse_assignment_in_variants(&mut self) -> ParseResult<Token> {
        self.parse_binary("=", Token::Assignment, Self::parse_annotation_in_variants)
    }

    fn parse_annotation_in_variants(&mut self) -> ParseResult<Token> {
        self.parse_binary(":", Token::Annotation, Self::parse_application)
    }

    fn parse_application(&mut self) -> ParseResult<Token> {
        self.skip_white()?;

//        let line = self.symbol_line_number;
//        let indentation = self.symbol_line_number;
        let function = self.parse_scope()?;

        self.skip_white()?;

        let indented_or_inline =
            self.symbol_line_number == self.element_line_number || self.symbol_indentation > self.element_indentation;

        // TODO make this more flexible!
        if !self.remaining.is_empty() && indented_or_inline && (
            !self.char_is_next(Self::is_special_symbol) ||
            self.char_is_next(|c: char| c == '(' || c == '$' || c == '"')
        ) {
            let argument = self.parse_scope()?;
            Ok(Token::Application(Box::new(function), Box::new(argument)))
        }
        else {
            Ok(function)
        }
    }

    fn parse_scope(&mut self) -> ParseResult<Token> {
        self.parse_scoped(Self::parse_element, Self::parse_access)
    }

    fn parse_access(&mut self) -> ParseResult<Token> {
        self.parse_infix_repeated(".", Token::Access, Self::parse_atom)
    }


    /// parse either brackets, string literals, or identifiers (including number literals)
    fn parse_atom(&mut self) -> ParseResult<Token> {
        println!("parse_atom, remaining: `{}`", self.remaining);
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






    fn parse_scoped(
        &mut self,
        indented_parser: impl Fn(&mut Self) -> ParseResult<Token>,
        else_parser: impl Fn(&mut Self) -> ParseResult<Token>
    ) -> ParseResult<Token>
    {
        let old_indentation = self.element_indentation;
        let old_line = self.element_line_number;
        println!("testing for indentation. currently: {}:{}, remaining: `{}`", old_line, old_indentation, self.remaining);

        self.skip_white()?;

        if self.symbol_line_number != old_line && self.symbol_indentation > old_indentation {
            println!("detected indentation. now: {}:{}, remaining: `{}`", self.symbol_line_number, self.symbol_indentation, self.remaining);

//            let mut scope_parser = *self;
//            scope_parser.last_element_line_number = self.line_number;
//            scope_parser.last_element_indentation = self.line_indentation;
            self.element_indentation = self.symbol_indentation;
            self.element_line_number = self.symbol_line_number;

            let mut lines = Vec::new();

            loop {
                self.skip_white()?;

                if self.symbol_indentation <= old_indentation || self.remaining.is_empty() {
//                    self.remaining = scope_parser.remaining;
//                    self.line_number = scope_parser.line_number;
//                    self.line_indentation = scope_parser.line_indentation;
//                    self.line_byte_index = scope_parser.line_byte_index;

                    self.element_indentation = old_indentation;
                    self.element_line_number = old_line;
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

    fn parse_branch(
        &mut self, operator: &str,
        sub_parser: impl Fn(&mut Self) -> ParseResult<Token>
    ) -> ParseResult<Token>
    {
        if self.skip_white_and_symbol(operator)? {
            Ok(Token::Branch(None, Box::new(sub_parser(self)?)))
        }
        else {
            let left = sub_parser(self)?;
            if self.skip_white_and_symbol(operator)? {
                let right = sub_parser(self)?;
                Ok(Token::Branch(Some(Box::new(left)), Box::new(right)))
            }
            else {
                Ok(left)
            }
        }
    }

    fn parse_binary(
        &mut self, operator: &str,
        variant: impl FnOnce(Box<Token>, Box<Token>) -> Token,
        sub_parser: impl Fn(&mut Self) -> ParseResult<Token>
    ) -> ParseResult<Token>
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

    fn parse_prefix_repeated(
        &mut self, operator: &str,
        variant: impl FnOnce(Vec<Token>) -> Token,
        sub_parser: impl Fn(&mut Self) -> ParseResult<Token>,
        else_parser: impl Fn(&mut Self) -> ParseResult<Token>
    ) -> ParseResult<Token>
    {
        self.skip_white()?;

        if self.symbol_is_next(operator) {
            let mut parts = Vec::new();

            while self.skip_white_and_symbol(operator)? {
                parts.push(sub_parser(self)?);
            }

            Ok(variant(parts))
        }
        else {
            else_parser(self)
        }
    }

    fn parse_infix_repeated(
        &mut self, operator: &str,
        variant: impl FnOnce(Vec<Token>) -> Token,
        sub_parser: impl Fn(&mut Self) -> ParseResult<Token>
    ) -> ParseResult<Token>
    {
        let operator_prepended = self.skip_white_and_symbol(operator)?;
        let first = sub_parser(self)?;

        self.skip_white()?;
        if operator_prepended || self.symbol_is_next(operator){
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
    fn extract_chars_until(&mut self, end_where: impl Fn(char) -> bool) -> &'s str {
        // TODO should skip comments
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
        // TODO should skip comments
        if self.symbol_is_next(symbol) {
            self.remaining = &self.remaining[ symbol.len() .. ];
            self.line_byte_index += symbol.len();

            true
        }
        else {
            false
        }
    }

    /// will not skip comments
    pub fn char_is_next<P: Fn(char) -> bool>(&self, pattern: P) -> bool {
        self.remaining.starts_with(pattern)
    }

    pub fn symbol_is_next(&self, symbol: &str) -> bool {
        self.remaining.starts_with(symbol)
    }

    // TODO should skip comments too
    fn skip_white(&mut self) -> ParseResult<()> {
        loop {
            if self.skip_symbol("\n") {
                self.symbol_line_number += 1;
                self.symbol_indentation = 0;
                self.line_byte_index = 0;
            }

            else if self.skip_symbol(" ") {

                // line_indentation counts the spaces, and the space char is 1 byte,
                // so we can simply compare line_byte_index and indentation
                if self.symbol_indentation + 1 == self.line_byte_index {

                    // as long as the remaining line starts with a whitespace,
                    // pull the line_indentation along with the byte index
                    self.symbol_indentation += 1;
                }
            }

//            else if self.remaining.starts_with("/*") {
//
//            }
//
//            else if self.remaining.starts_with("//") {
//                while !self.remaining.starts_with('\n') {
//                    self.remaining = self.remaining[]
//                }
//            }

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

    fn id(name: &str) -> Token {
        Token::Identifier(name.to_owned())
    }

    fn access(name: &[&str]) -> Token {
        Token::Access(name.iter().map(|s|id(s)).collect())
    }

    fn variants(members: &[Token]) -> Token {
        Token::Variants(members.to_owned())
    }

    fn structure(members: &[Token]) -> Token {
        Token::Structure(members.to_owned())
    }

    fn tuple(members: &[Token]) -> Token {
        Token::Tuple(members.to_owned())
    }

    fn scope(members: &[Token]) -> Token {
        Token::Scope(members.to_owned())
    }

    fn matching(value: Token, members: &[Token]) -> Token {
        Token::Match(Box::new(value), members.to_owned())
    }

    fn conditions(members: &[Token]) -> Token {
        Token::Conditions(members.to_owned())
    }

    fn annotate(subject: Token, annotation: Token) -> Token {
        Token::Annotation(Box::new(subject), Box::new(annotation))
    }

    fn branch(condition: Option<Token>, result: Token) -> Token {
        Token::Branch(condition.map(Box::new), Box::new(result))
    }

    fn assign(binding: Token, value: Token) -> Token {
        Token::Assignment(Box::new(binding), Box::new(value))
    }

    fn function(argument: Token, result: Token) -> Token {
        Token::ValueFunction(Box::new(argument), Box::new(result))
    }

    fn type_function(argument: Token, result: Token) -> Token {
        Token::TypeFunction(Box::new(argument), Box::new(result))
    }

    fn apply(function: Token, argument: Token) -> Token {
        Token::Application(Box::new(function), Box::new(argument))
    }



    fn source(source: &str) -> Tokenizer {
        Tokenizer {
            symbol_indentation: 0,
            symbol_line_number: 0,
            line_byte_index: 0,
            element_line_number: 0,
            element_indentation: 0,
            remaining: source,
            source,
        }
    }


    #[test]
    fn test_parse_element(){
        assert_eq!(source("| a | b: std.T").parse_element(), Ok(
            variants(&[
                id("a"),
                annotate(
                    id("b"),
                    access(&["std", "T"])
                )
            ])
        ));

        assert_eq!(source("& a & b: T & c: F").parse_element(), Ok(
            structure(&[
                id("a"),
                annotate(id("b"), id("T")),
                annotate(id("c"), id("F")),
            ])
        ));

        assert_eq!(source("& a & b: (&c: A & d: B)").parse_element(), Ok(
            structure(&[
                id("a"),
                annotate(
                    id("b"),
                    structure(&[
                        annotate(
                            id("c"),
                            id("A"),
                        ),
                        annotate(
                            id("d"),
                            id("B"),
                        ),
                    ])
                )
            ]))
        );

        assert_eq!(source("|a |b: (|c: A |d: B)").parse_element(), Ok(
            variants(&[
                id("a"),
                annotate(
                    id("b"),
                    variants(&[
                        annotate(
                            id("c"),
                            id("A"),
                        ),
                        annotate(
                            id("d"),
                            id("B"),
                        ),
                    ]),
                ),
            ])
        ));

        assert_eq!(source("all,bll,cll = &duu,(euu,fuu)").parse_element(), Ok(
            assign(
                tuple(&[id("all"), id("bll"), id("cll")]),
                tuple(&[
                    structure(&[id("duu")]),
                    tuple(&[id("euu"), id("fuu")])
                ])
            )
        ));

        assert_eq!(source("f1, f2: T1, T2 = (x -> |vx = x), (t,x -> 4, t)").parse_element(), Ok(
            assign(
                annotate(
                    tuple(&[id("f1"), id("f2")]),
                    tuple(&[id("T1"), id("T2")]),
                ),
                tuple(&[
                    function(
                        id("x"),
                        variants(&[assign(id("vx"), id("x"))])
                    ),
                    function(
                        tuple(&[id("t"), id("x")]),
                        tuple(&[id("4"), id("t")]),
                    ),
                ])
            )
        ));

        assert_eq!(source("apply: Fn => &fn:Fn &arg:Fn.Arg -> Fn.Result = &fn &arg -> fn arg").parse_element(), Ok(
            assign(
                annotate(
                    id("apply"),
                    type_function(
                        id("Fn"),
                        function(
                            structure(&[
                                annotate(id("fn"), id("Fn")),
                                annotate(id("arg"), access(&["Fn", "Arg"]))
                            ]),
                            access(&["Fn", "Result"]),
                        )
                    )
                ),
                function(
                    structure(&[id("fn"), id("arg")]),
                    apply(id("fn"), id("arg")),
                )
            )
        ));


        let indented_source =
r#"
fn = a -> b
"#;
        assert_eq!(source(indented_source).parse_element(), Ok(
            assign(id("fn"), function(id("a"), id("b")))
        ));



        let indented_source =
            r#"
fn = a ->
    d = &a
    d.a
"#;
        assert_eq!(source(indented_source).parse_element(), Ok(
            assign(id("fn"), function(
                id("a"),
                scope(&[
                    assign(id("d"), structure(&[id("a")])),
                    access(&["d", "a"])
                ])
            ))
        ));



        let indented_source =
r#"
c = t ->
  x = compute 0.3

  xt = x
    @5: always 6
    @6: always 5
    @: x -> x

  compute_again xt
"#;
        assert_eq!(source(indented_source).parse_element(), Ok(
            assign(id("c"), function(
                id("t"),
                scope(&[
                    assign(
                        id("x"),
                        apply(id("compute"),access(&["0", "3"]))
                    ),
                    assign(
                        id("xt"),
                        matching(id("x"), &[
                            branch(Some(id("5")), apply(id("always"), id("6"))),
                            branch(Some(id("6")), apply(id("always"), id("5"))),
                            branch(None, function(id("x"), id("x")))
                        ])
                    ),
                ])
            ))
        ));


        // TODO test tuples, assignment, file, scope, function, application

    }


    #[test]
    fn test_parse_chained_identifier(){
        assert_eq!(source("hello world").parse_access(), Ok(id("hello")));
        assert_eq!(source("hello_world").parse_access(), Ok(id("hello_world")));
        assert_eq!(source("hello . world.there").parse_access(), Ok(access(&["hello", "world", "there"])));
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
        assert_eq!(src.symbol_line_number, 0);

        src.skip_required_symbol("a").unwrap();
        println!("skipped a {:?}", src);
        src.skip_white().unwrap();
        assert_eq!(src.symbol_line_number, 1);
        assert_eq!(src.symbol_indentation, 0);


        src.skip_required_symbol("b").unwrap();
        println!("skipped b {:?}", src);
        assert_eq!(src.symbol_line_number, 1);
        assert_eq!(src.symbol_indentation, 0);
        src.skip_white().unwrap();
        assert_eq!(src.symbol_line_number, 2);
        assert_eq!(src.symbol_indentation, 2);

        src.skip_required_symbol("c").unwrap();
        println!("skipped c {:?}", src);
        src.skip_white().unwrap();
        assert_eq!(src.symbol_line_number, 3);
        assert_eq!(src.symbol_indentation, 1);

        src.skip_required_symbol("d").unwrap();
        assert_eq!(src.symbol_line_number, 3);
        assert_eq!(src.symbol_indentation, 1);
        src.skip_white().unwrap();
        assert_eq!(src.symbol_line_number, 3);
        assert_eq!(src.symbol_indentation, 1);


    }

}