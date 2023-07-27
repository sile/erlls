use erlls_core::{
    error::ResponseError, header::Header, message::ResponseMessage, server::LanguageServer,
};
use orfail::OrFail;
use serde::Serialize;
use std::io::{Read, Write};

fn main() -> orfail::Result<()> {
    env_logger::init();

    let mut server = LanguageServer::new();
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut stdin = stdin.lock();
    let mut stdout = stdout.lock();
    let mut content_buf = Vec::new();
    loop {
        let header = Header::from_reader(&mut stdin).or_fail()?;
        log::debug!("Received header: {:?}", header);

        content_buf.resize(header.content_length as usize, 0);
        stdin.read_exact(&mut content_buf).or_fail()?;
        log::debug!(
            "Received JSON: {}",
            std::str::from_utf8(&content_buf).or_fail()?
        );

        let response = match serde_json::from_slice(&content_buf) {
            Ok(msg) => server.handle_message(msg),
            Err(e) => {
                log::warn!("Invalid message: {e:?}");
                Some(ResponseMessage::error(ResponseError::from(e)))
            }
        };
        if let Some(response) = response {
            write_message(&mut stdout, &response, &mut content_buf).or_fail()?;
        }

        while let Some(notification) = server.take_notification() {
            write_message(&mut stdout, &notification, &mut content_buf).or_fail()?;
        }
    }
}

fn write_message<W: Write, T: Serialize>(
    writer: &mut W,
    msg: &T,
    mut buf: &mut Vec<u8>,
) -> orfail::Result<()> {
    buf.clear();
    serde_json::to_writer(&mut buf, msg).or_fail()?;
    Header::new(buf.len()).to_writer(writer).or_fail()?;
    writer.write_all(&buf).or_fail()?;
    writer.flush().or_fail()?;
    log::debug!("Sent JSON: {}", std::str::from_utf8(&buf).or_fail()?);
    Ok(())
}

#[derive(Debug)]
struct FileSystem;

impl erlls_core::fs::FileSystem for FileSystem {
    fn exists(path: &str) -> bool {
        std::path::Path::new(path).exists()
    }

    fn read_file(path: &str) -> orfail::Result<String> {
        std::fs::read_to_string(path).or_fail()
    }

    fn read_sub_dirs(path: &str) -> orfail::Result<Vec<String>> {
        let mut dirs = Vec::new();
        for entry in std::fs::read_dir(path).or_fail()? {
            let entry = entry.or_fail()?;
            if entry.file_type().or_fail()?.is_dir() {
                dirs.push(entry.path().to_str().or_fail()?.to_owned());
            }
        }
        Ok(dirs)
    }
}
