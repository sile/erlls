use crate::message::DocumentUri;
use std::future::Future;

pub trait FileSystem {
    fn exists(&mut self, uri: &DocumentUri) -> Box<dyn Unpin + Future<Output = bool>>;

    fn read_file(
        &mut self,
        uri: &DocumentUri,
    ) -> Box<dyn Unpin + Future<Output = orfail::Result<String>>>;

    fn read_sub_dirs(
        &mut self,
        uri: &DocumentUri,
    ) -> Box<dyn Unpin + Future<Output = Vec<DocumentUri>>>;
}
