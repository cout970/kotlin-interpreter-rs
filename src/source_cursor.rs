use std::intrinsics::transmute;

use crate::source::{BytePos, ByteSpan, Source};

pub struct SourceCursor {
    source: Source,
    pos: u32,
}

impl SourceCursor {
    pub fn new(source: Source) -> Self {
        Self {
            source,
            pos: 0,
        }
    }

    pub fn consume_u8(&mut self) -> u8 {
        let byte = self.u8();
        self.next_byte();
        byte
    }

    pub fn consume_char(&mut self) -> char {
        let char = self.char();
        self.next_char();
        char
    }

    #[inline]
    pub fn u8(&self) -> u8 {
        self.source.content[self.pos as usize]
    }

    #[inline]
    pub fn next_u8(&self, offset: u32) -> u8 {
        self.source.content[(self.pos + offset) as usize]
    }

    pub fn char(&self) -> char {
        if let Ok(string) = std::str::from_utf8(&self.source.content[(self.pos as usize)..1]) {
            if !string.is_empty() {
                return string.chars().next().unwrap();
            }
        }
        if let Ok(string) = std::str::from_utf8(&self.source.content[(self.pos as usize)..2]) {
            if !string.is_empty() {
                return string.chars().next().unwrap();
            }
        }
        if let Ok(string) = std::str::from_utf8(&self.source.content[(self.pos as usize)..3]) {
            if !string.is_empty() {
                return string.chars().next().unwrap();
            }
        }
        if let Ok(string) = std::str::from_utf8(&self.source.content[(self.pos as usize)..4]) {
            if !string.is_empty() {
                return string.chars().next().unwrap();
            }
        }

        // This shouldn't happen as source is always valid ut8
        unreachable!("Invalid uft8 codepoint");
    }

    pub fn next_byte(&mut self) {
        self.pos = self.pos + 1;
    }

    pub fn next_char(&mut self) {
        self.pos = self.pos + self.char().len_utf8() as u32;
    }

    pub fn next(&mut self, offset: u32) {
        self.pos = self.pos + offset;
    }

    pub fn eof(&self) -> bool {
        self.u8() == 0u8
    }

    pub fn pos(&self) -> BytePos {
        BytePos(self.pos)
    }

    pub fn span(&self, start: BytePos) -> ByteSpan {
        ByteSpan::new(start.0, self.pos)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print_bytes() {
        let mut cursor = SourceCursor::new(Source::from_str("tést"));

        assert_eq!(cursor.u8(), b't');
        cursor.next_byte();
        assert_eq!(cursor.char(), 'é');
        cursor.next_char();
        assert_eq!(cursor.u8(), b's');
        cursor.next_byte();
        assert_eq!(cursor.u8(), b't');
        cursor.next_byte();
        assert_eq!(cursor.u8(), 0u8);
    }
}