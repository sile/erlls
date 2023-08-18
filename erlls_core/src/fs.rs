use std::{
    future::Future,
    path::{Path, PathBuf},
};

pub trait FileSystem {
    fn exists<P: AsRef<Path>>(path: P) -> Box<dyn Unpin + Future<Output = bool>>;
    fn read_file<P: AsRef<Path>>(
        path: P,
    ) -> Box<dyn Unpin + Future<Output = orfail::Result<String>>>;
    fn read_sub_dirs<P: AsRef<Path>>(
        path: P,
    ) -> Box<dyn Unpin + Future<Output = orfail::Result<Vec<PathBuf>>>>;
}
