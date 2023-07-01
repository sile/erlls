use orfail::OrFail;

fn main() -> orfail::Result<()> {
    env_logger::init();

    let mut server = erlls::server::LanguageServer::new();
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    loop {
        let msg = erlls::message::Message::from_reader(&mut stdin).or_fail()?;
        server.handle_message(&msg).or_fail()?;
    }
    // Ok(())
}
