use crate::source_code::SourceCode;

pub struct CodeCursor {
    pub source: SourceCode,
    pub pos: u32,
    pub mode: Vec<(CodeCursorMode, i32)>,
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum CodeCursorMode {
    Normal,
    String,
    MultilineString,
    StringTemplate,
}

impl CodeCursor {
    #[inline]
    pub fn read_char(&mut self, index: u32) -> char {
        self.source[(self.pos + index) as usize] as char
    }

    #[inline]
    pub fn read_u8(&mut self, index: u32) -> u8 {
        self.source[(self.pos + index) as usize]
    }

    #[inline]
    pub fn moved(&mut self, index: u32) -> &mut Self {
        self.pos += index;
        self
    }

    #[inline]
    pub fn next(&mut self) {
        self.pos += 1;
    }

    #[inline]
    pub fn mode(&mut self) -> CodeCursorMode {
        self.mode.last().unwrap().0
    }

    #[inline]
    pub fn brace_count(&mut self) -> i32 {
        self.mode.last().unwrap().1
    }

    #[inline]
    pub fn inc_brace(&mut self) {
        self.mode.last_mut().unwrap().1 += 1;
    }

    #[inline]
    pub fn dec_brace(&mut self) {
        self.mode.last_mut().unwrap().1 -= 1;
    }

    pub fn code_ref(&mut self) -> SourceCode {
        self.source.clone()
    }
}