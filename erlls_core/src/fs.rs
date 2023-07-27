pub trait FileSystem {
    fn exists(path: &str) -> bool;
    fn read_file(path: &str) -> orfail::Result<String>;
    fn read_sub_dirs(path: &str) -> orfail::Result<Vec<String>>;
}
