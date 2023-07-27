use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    pub root_dir: PathBuf,
    #[serde(default)]
    pub erl_libs: Vec<PathBuf>,
}
