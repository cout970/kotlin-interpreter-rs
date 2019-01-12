use crate::Number;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Id(String),
    Number(Number),
    Char(char),
    StringStart,
    StringEnd,
    StringTemplateStart,
    StringTemplateEnd,
    StringContent(String),
    StringVariable(String),
    // Signs
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftAngleBracket,
    RightAngleBracket,
    LessEquals,
    GreaterEquals,
    At,
    Colon,
    DoubleColon,
    Dollar,
    Dot,
    DoubleDot,
    Comma,
    QuestionMark,
    SafeDot,
    ExclamationMark,
    DoubleExclamationMark,
    NotEquals,
    NotDoubleEquals,
    LeftArrow,
    Elvis,
    Plus,
    DoublePlus,
    Minus,
    DoubleMinus,
    Asterisk,
    Slash,
    Percent,
    Equals,
    DoubleEquals,
    TripleEquals,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DivEquals,
    ModEquals,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
    Underscore,
    // Keywords
    As,
    AsQuestionMark,
    Break,
    Class,
    Continue,
    Do,
    Else,
    False,
    For,
    Fun,
    If,
    In,
    NotIn,
    Is,
    NotIs,
    Interface,
    Null,
    Object,
    Package,
    Return,
    Super,
    This,
    Throw,
    True,
    Try,
    TypeAlias,
    Val,
    Var,
    When,
    While,
    // End of file
    EOF,
}