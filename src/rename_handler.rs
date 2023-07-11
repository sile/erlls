use crate::{
    error::ResponseError,
    message::{RenameParams, ResponseMessage},
};
use std::path::PathBuf;

#[derive(Debug)]
pub struct RenameHandler {
    root_dir: PathBuf,
}

impl RenameHandler {
    pub fn new(root_dir: PathBuf) -> Self {
        Self { root_dir }
    }

    pub fn handle(&mut self, params: RenameParams) -> Result<ResponseMessage, ResponseError> {
        todo!()
    }
}
