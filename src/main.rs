use erlls::{header::Header, message::Message, server::LanguageServer};
use orfail::{Failure, OrFail};
use std::io::Read;

fn main() -> orfail::Result<()> {
    env_logger::init();

    let mut server = LanguageServer::new();
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    loop {
        let header = Header::from_reader(&mut stdin).or_fail()?;
        let mut content_reader = (&mut stdin).take(header.content_length as u64);
        log::debug!("Received header: {:?}", header);
        let msg = Message::from_reader(&mut content_reader).or_fail()?;
        log::debug!("Received message: {:?}", msg);
        if content_reader.read(&mut [0; 1]).or_fail()? != 0 {
            return Err(Failure::new().message("Content JSON is shorter than specified length"));
        }
        server.handle_message(&msg).or_fail()?;
    }
}
