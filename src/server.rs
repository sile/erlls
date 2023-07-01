use crate::message::Message;

#[derive(Debug)]
pub struct LanguageServer {}

impl LanguageServer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn handle_message(&mut self, _message: &Message) -> orfail::Result<()> {
        Ok(())
    }
}
