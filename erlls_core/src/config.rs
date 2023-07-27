use std::path::PathBuf;

#[derive(Debug, Default, Clone)]
pub struct Config {
    pub root_dir: PathBuf,
    pub erl_libs: Vec<PathBuf>,
}
