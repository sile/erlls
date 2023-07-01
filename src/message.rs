use std::io::Read;

#[derive(Debug)]
pub struct Message {}

impl Message {
    pub fn from_reader<R: Read>(_reader: &mut R) -> orfail::Result<Self> {
        todo!()
    }
}
