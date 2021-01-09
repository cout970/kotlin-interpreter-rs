use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// To avoid extra checks the end of the program source is marked with several \0 bytes
const SOURCE_CODE_PADDING: usize = 4;

/// Storage for the program source code,
/// Arc<> allows to clone this struct cheaply with minimal memory usage
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Source {
    pub content: Arc<Vec<u8>>,
    pub origin: Arc<SourceOrigin>,
}

/// Metadata about the origin of the source code
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum SourceOrigin {
    /// The source comes from a file
    File {
        /// Path to the file where the source code is stored
        path: PathBuf,
    },
    /// The source comes from an string defined in code
    String,
    /// The source comes from a line in the REPL
    Repl {
        line: u32
    },
    /// Custom origin for plugins and users of this library
    Custom(String),
}

/// The offset from the beginning of the program source to the position of a byte
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct BytePos(pub u32);

/// A reference to a single byte in the program source
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CodeRef {
    pub source: Source,
    pub byte_pos: BytePos,
}

/// Line and column of a character in the program
/// Lines and columns start a 1
/// Columns count chars, not bytes, unicode chars may map to multiple bytes
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct CharPos {
    pub line: u32,
    pub column: u32,
}

/// Start and end of a section of the source
/// Defined by the byte positions
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ByteSpan { start: BytePos, end: BytePos }

/// Start and end of a section of the source
/// Defined by the char positions
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SourceSpan {
    pub source: Source,
    pub byte_span: ByteSpan,
    pub start: CharPos,
    pub end: CharPos,
}

impl Source {
    pub fn from_string(str: String) -> Source {
        let mut bytes = str.into_bytes();
        for _ in 0..SOURCE_CODE_PADDING {
            bytes.push(0u8);
        }

        Source {
            content: Arc::new(bytes),
            origin: Arc::from(SourceOrigin::String),
        }
    }

    pub fn from_str(str: &str) -> Source {
        let mut bytes = str.as_bytes().to_owned();
        for _ in 0..SOURCE_CODE_PADDING {
            bytes.push(0u8);
        }

        Source {
            content: Arc::new(bytes),
            origin: Arc::from(SourceOrigin::String),
        }
    }

    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Source, std::io::Error> {
        let path = path.as_ref();
        let mut file = File::open(path)?;

        // Make sure the file is valid utf8
        let mut bytes = String::new();
        file.read_to_string(&mut bytes)?;

        let mut bytes = bytes.into_bytes();

        for _ in 0..SOURCE_CODE_PADDING {
            bytes.push(0u8);
        }

        Ok(Source {
            content: Arc::new(bytes),
            origin: Arc::from(SourceOrigin::File { path: path.to_path_buf() }),
        })
    }

    pub fn len(&self) -> usize {
        self.content.len()
    }

    pub fn new_code_ref(&self, pos: BytePos) -> CodeRef {
        CodeRef {
            source: self.clone(),
            byte_pos: pos,
        }
    }
}

impl CodeRef {
    pub fn to_char_pos(&self) -> CharPos {
        let end = self.byte_pos.0 as usize;
        let slice = &self.source.content[0..end];
        let mut line = &self.source.content[0..end];
        let mut line_num = 1;

        for i in 0..slice.len() {
            if slice[i] == b'\n' {
                line = &self.source.content[(i + 1)..end];
                line_num += 1;
            }
        }

        let column_num = String::from_utf8_lossy(line).chars().count() as u32 + 1;

        CharPos {
            line: line_num,
            column: column_num,
        }
    }
}

impl ByteSpan {
    pub fn new(start: u32, end: u32) -> ByteSpan {
        ByteSpan {
            start: BytePos(start),
            end: BytePos(end),
        }
    }

    pub fn to_code_span(&self, source: Source) -> SourceSpan {
        SourceSpan {
            byte_span: *self,
            start: source.new_code_ref(self.start).to_char_pos(),
            end: source.new_code_ref(self.end).to_char_pos(),
            source,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_from_string() {
        let src = Source::from_string("1234".to_string());
        let debug = format!("{:?}", src);

        assert_eq!("Source { content: [49, 50, 51, 52, 0, 0, 0, 0], origin: String }", debug);
    }

    #[test]
    fn create_from_str() {
        let src = Source::from_str("1234");
        let debug = format!("{:?}", src);

        assert_eq!("Source { content: [49, 50, 51, 52, 0, 0, 0, 0], origin: String }", debug);
    }

    #[test]
    fn create_from_file() {
        let src = Source::from_file("./examples/tests/test_file_1.kt").expect("Unable to read file");
        let debug = format!("{:?}", src);

        assert_eq!("Source { content: [112, 114, 105, 110, 116, 108, 110, 40, 34, 104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 34, 41, 0, 0, 0, 0], origin: File { path: \"./examples/tests/test_file_1.kt\" } }", debug);
    }

    #[test]
    fn create_from_unicode() {
        let src = Source::from_str("áéíóúñ");
        let debug = format!("{:?}", src);

        assert_eq!("Source { content: [195, 161, 195, 169, 195, 173, 195, 179, 195, 186, 195, 177, 0, 0, 0, 0], origin: String }", debug);
    }

    #[test]
    fn to_char_pos() {
        let src = Source::from_str("1\n2\n3\n4");
        assert_eq!(
            src.new_code_ref(BytePos(0)).to_char_pos(),
            CharPos { line: 1, column: 1 }
        );
        assert_eq!(
            src.new_code_ref(BytePos(1)).to_char_pos(),
            CharPos { line: 1, column: 2 }
        );
        assert_eq!(
            src.new_code_ref(BytePos(2)).to_char_pos(),
            CharPos { line: 2, column: 1 }
        );
        assert_eq!(
            src.new_code_ref(BytePos(3)).to_char_pos(),
            CharPos { line: 2, column: 2 }
        );
        assert_eq!(
            src.new_code_ref(BytePos(4)).to_char_pos(),
            CharPos { line: 3, column: 1 }
        );
        assert_eq!(
            src.new_code_ref(BytePos(5)).to_char_pos(),
            CharPos { line: 3, column: 2 }
        );
    }
}