use std::path::{Path, PathBuf};

pub trait FileSystem {
    fn exists<P: AsRef<Path>>(path: P) -> bool;
    fn read_file<P: AsRef<Path>>(path: P) -> orfail::Result<String>;
    fn read_sub_dirs<P: AsRef<Path>>(path: P) -> orfail::Result<Vec<PathBuf>>;
}
