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
    pub static ref TOKEN_KEYWORD_SYMBOL_MAP: HashMap<&'static str, TokenTypes> = {
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
            && k.chars().fold(true, |acc, ch| acc & ch.is_alphanumeric() )
        {
            return Ok(TokenTypes::Identifier);
        }
        
        Err(TokenError::InvalidTokenError(format!("Not sure what this is! {}", k)))
    }
}


#[derive(Debug)]
pub struct Tokens {
    pub token_stream: Vec<(String, TokenTypes)>,
}

impl Tokens {
    pub fn new() -> Tokens {
        Tokens {
            token_stream: Vec::new(),
        }
    }
    pub fn append(&mut self, token_set: (String, TokenTypes)) {
        self.token_stream.push(token_set);
    }

    pub fn peek(&self) -> Option<(String, TokenTypes)> {
        if self.token_stream.len() != 0 {
            return Some(self.token_stream[0].clone());
        } 
        None
    }
}

impl Iterator for Tokens {
    type Item = (String, TokenTypes);
    fn next(&mut self) -> Option<Self::Item> {
        if self.token_stream.len() != 0 {
            return Some(self.token_stream.remove(0));
        }
        None
    }
}

pub fn to_token_stream(file: &str) -> Result<Tokens, TokenError> {
    // Splits lines
    let file_string = fs::read_to_string(file).unwrap();
    let file_no_comments = strip_comments(&file_string);

    let mut token_info = Tokens::new();

    // Tokenize with RegEx
    let re = Regex::new(
        "(?x)
        \\b(?P<keyword>class|constructor|function|method|field|static|var|int|char|boolean|void|
            true|false|null|this|let|do|if|else|while|return)\\b| 
        (?P<symbols>[\\{}\\()\\[\\]\\.,;\\+\\-\\*/&\\|<>=~])|
        \"(?P<string>[^\"]*)\"
        ")
        .unwrap();


    for token in tokenize(&re, &file_no_comments) {
        let token_type = TokenTypes::token_to_type(&token)?;
        if token.contains('\"') {
            let token = token.split('\"').nth(1).unwrap().to_owned();
            Tokens::append(&mut token_info, (token, token_type));
        } else {
            Tokens::append(&mut token_info, (token.to_owned(), token_type));
        }
    }
    
    Ok(token_info)
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
                    true => match pre_push.contains('\"') {
                        true => tokens.push(pre_push), // .split('\"').nth(1).unwrap().trim().to_owned()),
                        false => tokens.extend(pre_push.split_whitespace().map(|s| s.to_owned())),
                    },
                    false => tokens.push(pre_push),
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
            TokenTypes::token_to_type("why4this").unwrap()
        );
    }
    #[test]
    fn tokens_test() {
        // let text = fs::read_to_string("/home/brs/Documents/JackCompiler/tests/ArrayTest/Main.jack")
        //     .unwrap();
        let text = "\"Test 1: expected result: 5; actual result: \"".to_owned(); //strip_comments(&text);
        let re = Regex::new(
            "(?x)
            \\b(?P<keyword>class|constructor|function|method|field|static|var|int|char|boolean|void|
                true|false|null|this|let|do|if|else|while|return)\\b| 
            (?P<symbols>[\\{}\\()\\[\\]\\.,;\\+\\-\\*/&\\|<>=~])|
            \"(?P<string>[^\"]*)\"
            ")
            .unwrap();
            
        println!("{:?}", tokenize(&re, &text));
    }
    
    #[test]
    fn iter_test(){
        let mut hi = Tokens::new();
        Tokens::append(&mut hi, (String::from("hi"), TokenTypes::Keyword));
        Tokens::append(&mut hi, (String::from("bye"), TokenTypes::Keyword));
        assert_eq!(hi.next(), Some((String::from("hi"), TokenTypes::Keyword)));
        assert_eq!(hi.next(), Some((String::from("bye"), TokenTypes::Keyword)));
        assert_eq!(hi.next(), None);
    }
    
    #[test]
    fn strip_comments_test() {
        let comment_string = "g/* hi_there\nehy */ \"gibber\" g//dhued\nthree";
        assert_eq!(
            strip_comments(comment_string),
            "g \"gibber\" g\nthree".to_string()
        );
    }
}