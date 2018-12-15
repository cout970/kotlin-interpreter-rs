use std::fmt::Write;
use std::sync::Arc;
use std::borrow::Cow;

pub type SourceCode = Arc<Vec<u8>>;

pub type Span = (u32, u32);

// Padding at the end of the source code to avoid check for the end of content
pub const SOURCE_CODE_PADDING: usize = 4;

pub fn from_str(code: &str) -> SourceCode {
    let mut vec: Vec<u8> = code.into();
    for _ in 0..SOURCE_CODE_PADDING {
        vec.push(0);
    }
    Arc::new(vec)
}

pub fn to_str(code: &SourceCode) -> Cow<str> {
    String::from_utf8_lossy(&code)
}


pub fn print_code_location(input: &str, span: Span) -> String {
    if input.is_empty() {
        return String::from("Empty");
    }

    let byte_input: &[u8] = input.as_bytes();
    let marker_start = span.0 as usize;
    let marker_end = span.1 as usize;

    let mut line_start = marker_start.min(byte_input.len() - 1).max(0);
    let mut line_end = marker_end.min(byte_input.len() - 1).max(0);

    while line_start > 0 {
        if byte_input[line_start] == b'\n' {
            line_start += 1;
            break;
        }
        line_start -= 1;
    }

    while line_end < byte_input.len() {
        if byte_input[line_end] == b'\n' {
            break;
        }
        line_end += 1;
    }

    let mut line = String::new();
    let mut pointer = String::new();
    let mut trail = String::new();

    for index in line_start..line_end {
        if index == marker_start {
            trail.push('┘');
            pointer.push('\u{028C}');
        } else if index < marker_start {
            trail.push('─');
            pointer.push(' ');
        } else if index < marker_end {
            pointer.push('\u{028C}');
        }
        line.push(byte_input[index] as char);
    }

    let line_num = (&byte_input[0..marker_start]).iter().filter(|&i| *i == b'\n').count();
    let line_num_str = format!("{}", line_num + 1);
    let mut spaces = String::new();

    for _ in 0..line_num_str.len() {
        spaces.push(' ');
    }

    let mut output = String::new();
    write!(&mut output, "\n{} │ {}\n{} │ {}\n{} │ {}", line_num_str, line, spaces, pointer, spaces, trail).unwrap();

    output
}
