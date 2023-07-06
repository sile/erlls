use crate::error::ResponseError;

#[derive(Debug)]
pub struct Message {
    jsonrpc: &'static str,
}

impl Message {
    pub const JSONRPC_VERSION: &'static str = "2.0";

    pub fn from_bytes(buf: &[u8]) -> Result<Self, ResponseError> {
        serde_json::from_slice(buf);
        todo!()
    }
}
