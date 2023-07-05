use erlls::{header::Header, message::Message, server::LanguageServer};
use orfail::OrFail;
use std::io::Read;

fn main() -> orfail::Result<()> {
    env_logger::init();

    let mut server = LanguageServer::new();
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    let mut content_buf = Vec::new();
    loop {
        let header = Header::from_reader(&mut stdin).or_fail()?;
        log::debug!("Received header: {:?}", header);

        content_buf.resize(header.content_length as usize, 0);
        stdin.read_exact(&mut content_buf).or_fail()?;
        match Message::from_bytes(&content_buf) {
            Ok(msg) => {
                log::debug!("Received message: {:?}", msg);
                server.handle_message(&msg).or_fail()?;
            }
            Err(e) => {
                log::error!("Failed to parse message: {:?}", e);
                todo!()
            }
        }
    }
}
