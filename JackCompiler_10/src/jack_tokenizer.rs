use regex::Regex;
use std::collections::HashMap;
use std::fs;
use std::io::Error as IoError;
use std::io::ErrorKind;

/* Tokenizing
- handles the compiler's input
- allows advancing the input
- supplies the current token's value and type (complete API, later)
*/

lazy_static! {
    static ref TOKEN_KEYWORD_SYMBOL_MAP: HashMap<&'static str, TokenTypes> = {
        let mut m = HashMap::new();

        for jack_keyword in [
            "class",
            "constructor",
            "function",
            "method",
            "field",
            "static",
            "var",
            "int",
            "char",
            "boolean",
            "void",
            "true",
            "false",
            "null",
            "this",
            "let",
            "do",
            "if",
            "else",
            "while",
            "return",
        ]
        .iter()
        {
            m.insert(*jack_keyword, TokenTypes::Keyword);
        }

        for jack_symbol in [
            "{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "|", "<", ">",
            "=", "~",
        ]
        .iter()
        {
            m.insert(*jack_symbol, TokenTypes::Symbol);
        }

        m
    };
}

#[derive(Debug)] // {:?}
pub enum TokenError {
    JackIOError(IoError),
    _RegExError(IoError),
    InvalidTokenError(String),
}

impl From<IoError> for TokenError {
    fn from(err: IoError) -> Self {
        TokenError::JackIOError(err)
    }
}
impl From<String> for TokenError {
    fn from(err: String) -> Self {
        TokenError::InvalidTokenError(err)
    }
}

#[derive(Debug, PartialEq, Copy, Clone)] // {:?}
pub enum TokenTypes {
    Keyword,
    Symbol,
    IntegerConstant,
    StringConstant,
    Identifier,
    InferType, // InferType used in Compilation Engine as an indicator to branch to the next command
}
impl TokenTypes {
    fn token_to_type(k: &str) -> Result<TokenTypes, TokenError> {
        if let Some(val) = TOKEN_KEYWORD_SYMBOL_MAP.get(k) {
            return Ok(*val);
        }
        // Test for integer
        if let Ok(_) = k.parse::<u16>() {
            return Ok(TokenTypes::IntegerConstant);
        }
        // Test for string
        if k.starts_with('\"') && k.ends_with('\"') {
            return Ok(TokenTypes::StringConstant);
        }
        // Test for indentifer -> checks first character != digit and no symbols
        if !k.starts_with(char::is_numeric)
            && k.chars().fold(true, |acc, ch| acc & ch.is_alphanumeric())
        {
            return Ok(TokenTypes::Identifier);
        }
        Err(TokenError::InvalidTokenError(format!("Not sure what this is! {}", k)))
    }
    pub fn string_token_type(tokentype: &TokenTypes) -> &'static str {
        match tokentype {
            TokenTypes::Keyword => "keyword",
            TokenTypes::Symbol => "symbol",
            TokenTypes::IntegerConstant => "integerConstant",
            TokenTypes::StringConstant => "stringConstant",
            TokenTypes::Identifier => "identifier",
            TokenTypes::InferType => panic!("Shouldn't use string_token_type function on InferType"),
        }
    }
}

pub struct Tokens {
    pub token_stream: Vec<String>,
    pub token_type: Vec<TokenTypes>,
}

impl Tokens {
    pub fn new() -> Tokens {
        Tokens {
            token_stream: vec![],
            token_type: vec![],
        }
    }
    pub fn append(&mut self, token: String, token_type: TokenTypes) {
        self.token_stream.push(token);
        self.token_type.push(token_type);
    }

    pub fn advance(&mut self) -> Option<(String, TokenTypes)> {
        if self.token_stream.len() != 0 && self.token_type.len() != 0 {
            let next_token = self.token_stream.remove(0);
            let next_type = self.token_type.remove(0);
            return Some((next_token, next_type));
        }
        None
    }
    pub fn peak_ahead(&self, num:usize) -> Option<(&String, &TokenTypes)> {
        if num <= self.token_stream.len() { 
            return Some((&self.token_stream[num], &self.token_type[num]));
        }
        None
    }

    pub fn _print(self) {
        for i in 0..self.token_stream.len() {
            println!("{:?} <-> {:?}\n", self.token_stream[i], self.token_type[i]);
        }
    }
}

pub fn to_token_stream(file: &String) -> Result<Tokens, TokenError> {
    // Splits lines
    let file_string = fs::read_to_string(file).unwrap();
    let file_no_comments = strip_comments(&file_string);

    let mut file_token_info = Tokens::new();

    // Tokenize with RegEx
    let re = Regex::new(
        r"(?x)
        \b(?P<keyword>class|constructor|function|method|field|static|var|int|char|boolean|void|
            true|false|null|this|let|do|if|else|while|return)\b| 
        (?P<symbols>[\{}\()\[\]\.,;\+\-\*/&_\|<>=~])",
    )
    .unwrap();

    for token in tokenize(&re, &file_no_comments) {
        let token_type = TokenTypes::token_to_type(&token)?;
        if token.contains('\"') {
            let token_string = token.split('\"').nth(1).unwrap().to_owned();
            Tokens::append(&mut file_token_info, token_string, token_type);
        } else {
            Tokens::append(&mut file_token_info, token, token_type);
        }
    }
    Ok(file_token_info)
}

fn tokenize(re: &Regex, text: &str) -> Vec<String> {
    // https://stackoverflow.com/questions/56921637/how-do-i-split-a-string-using-a-rust-regex-and-keep-the-delimiters
    let mut tokens = Vec::new();
    let mut last = 0;
    for (index, matched) in text.match_indices(re) {
        if last != index {
            let pre_push = text[last..index].trim().to_owned();
            if pre_push != "" {
                match pre_push.contains(" ") {
                    false => tokens.push(pre_push),
                    true => match pre_push.contains('\"') {
                        true => tokens.push(pre_push), // .split('\"').nth(1).unwrap().trim().to_owned()),
                        false => tokens.extend(pre_push.split_whitespace().map(|t| t.to_owned())),
                    },
                }
            }
        }
        tokens.push(matched.trim().to_owned());
        last = index + matched.len();
    }
    if last < text.len() {
        let last_push = text[last..].trim().to_owned();
        if !last_push.is_empty() {
            tokens.push(last_push);
        }
    }
    tokens
}

fn strip_comments(file_string: &str) -> String {
    // Regex confuses me: https://stackoverflow.com/questions/2319019/using-regex-to-remove-comments-from-source-files/18381470#18381470
    //               (matches new lines) (multi-line comments) | (single-line comments)
    let re: Regex = Regex::new(r"(?xs)(/\*.*?\*/)|(//[^\n]*)").unwrap();
    re.replace_all(file_string, "").to_string()
}

pub fn parse_file_or_directory(file_or_directory_name: &String) -> Result<Vec<String>, TokenError> {
    let meta = fs::metadata(file_or_directory_name)?;
    match meta.is_file() {
        true => file(file_or_directory_name),
        false => files_in_directory(file_or_directory_name),
    }
}

fn file(file_name: &str) -> Result<Vec<String>, TokenError> {
    // helper function for parse_file_or_directory
    match file_name.contains(".jack") {
        true => Ok(vec![file_name.to_owned()]),
        false => Err(TokenError::JackIOError(IoError::new(
            ErrorKind::Other,
            "Not a .jack file",
        ))),
    }
}

fn files_in_directory(dir_name: &str) -> Result<Vec<String>, TokenError> {
    // helper function for parse_file_or_directory
    let mut file_names = vec![];
    let paths = fs::read_dir(dir_name)?;

    for path in paths {
        let file_path = path?.path().into_os_string().into_string().unwrap();
        if file_path.contains(".jack") {
            file_names.push(file_path);
        }
    }
    return Ok(file_names);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_type_test() {
        assert_eq!(
            TokenTypes::Keyword,
            TokenTypes::token_to_type("class").unwrap()
        );
        assert_eq!(TokenTypes::Symbol, TokenTypes::token_to_type(";").unwrap());
        assert_eq!(
            TokenTypes::IntegerConstant,
            TokenTypes::token_to_type("92").unwrap()
        );
        assert_eq!(
            TokenTypes::StringConstant,
            TokenTypes::token_to_type("\"this is a jack test\"").unwrap()
        );
        assert_eq!(
            TokenTypes::Identifier,
            TokenTypes::token_to_type("why_this").unwrap()
        );
    }
    #[test]
    fn tokens_test() {
        let text = fs::read_to_string("/home/brs/Documents/JackCompiler/tests/ArrayTest/Main.jack")
            .unwrap();
        let text = strip_comments(&text);
        let re = Regex::new(
            r"(?x)
            \b(?P<keyword>class|constructor|function|method|field|static|var|int|char|boolean|void|
                true|false|null|this|let|do|if|else|while|return)\b| 
            (?P<symbols>[\{}\()\[\]\.,;\+\-\*/&\|<>=~])",
        )
        .unwrap();
        println!("{:?}", tokenize(&re, &text));
    }

    #[test]
    fn new_append_advance_tokens() {
        let mut test = Tokens::new();
        Tokens::append(&mut test, "class".to_owned(), TokenTypes::Keyword);
        Tokens::append(&mut test, "(".to_owned(), TokenTypes::Symbol);

        assert_eq!(
            Tokens::advance(&mut test).unwrap(),
            ("class".to_owned(), TokenTypes::Keyword)
        );
        assert_eq!(
            Tokens::advance(&mut test).unwrap(),
            ("(".to_owned(), TokenTypes::Symbol)
        );

        assert_eq!(Tokens::advance(&mut test), None);
    }

    #[test]
    fn strip_comments_test() {
        let comment_string = "g/* hi_there\nehy */ \"gibber\" g//dhued\nthree";
        assert_eq!(
            strip_comments(comment_string),
            "g \"gibber\" g\nthree".to_string()
        );
    }
    #[test]
    fn paths() {
        let dir_path = "/home/brs/Documents/JackCompiler/tests/ArrayTest".to_owned();
        let file_path = format!("{}/Main.jack", dir_path);

        assert_eq!(
            parse_file_or_directory(&dir_path).unwrap(),
            vec![file_path.clone()]
        );
        assert_eq!(
            parse_file_or_directory(&file_path).unwrap(),
            vec![file_path.clone()]
        );
    }
}
