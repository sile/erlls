use crate::message::DocumentUri;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Config {
    pub root_uri: DocumentUri,
    #[serde(default)]
    pub erl_libs: Vec<PathBuf>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            root_uri: DocumentUri::default_dummyt_uri(),
            erl_libs: Vec::new(),
        }
    }
}
