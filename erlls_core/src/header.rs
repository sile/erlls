use orfail::{Failure, OrFail};
use std::io::{BufRead, Write};

#[derive(Debug, Clone)]
pub struct Header {
    pub content_length: usize,
    pub content_type: String,
}

impl Header {
    pub const DEFAULT_CONTENT_TYPE: &'static str = "application/vscode-jsonrpc; charset=utf-8";

    pub fn new(content_length: usize) -> Self {
        Self {
            content_length,
            content_type: Self::DEFAULT_CONTENT_TYPE.to_owned(),
        }
    }

    pub fn from_reader<R: BufRead>(reader: &mut R) -> orfail::Result<Self> {
        let mut this = Self::new(usize::MAX);
        for line in reader.lines() {
            let line = line.or_fail()?;
            let line = line.trim_end();
            if line.is_empty() {
                break;
            }

            let mut tokens = line.splitn(2, ':');
            match (tokens.next().or_fail()?, tokens.next().or_fail()?) {
                ("Content-Length", value) => {
                    this.content_length = value.trim().parse::<usize>().or_fail()?;
                }
                ("Content-Type", value) => {
                    this.content_type = value.trim().to_owned();
                }
                _ => {}
            }
        }
        if this.content_length == usize::MAX {
            return Err(Failure::new("Missing Content-Length header"));
        }
        Ok(this)
    }

    pub fn to_writer<W: Write>(&self, writer: &mut W) -> orfail::Result<()> {
        write!(writer, "Content-Length: {}\r\n", self.content_length).or_fail()?;
        write!(writer, "Content-Type: {}\r\n", self.content_type).or_fail()?;
        write!(writer, "\r\n").or_fail()?;
        Ok(())
    }
}
