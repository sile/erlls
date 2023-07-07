use erlls::{
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
            "Received content JSON: {:?}",
            std::str::from_utf8(&content_buf).or_fail()?
        );

        let response = match serde_json::from_slice(&content_buf) {
            Ok(msg) => {
                log::debug!("Received message: {msg:?}");
                server.handle_message(&msg).or_fail()?
            }
            Err(e) => {
                log::warn!("Invalid message: {e:?}");
                Some(ResponseMessage::error(None, ResponseError::from(e)))
            }
        };
        if let Some(response) = response {
            write_message(&mut stdout, &response, &mut content_buf).or_fail()?;
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
    Ok(())
}
