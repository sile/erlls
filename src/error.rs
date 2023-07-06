use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResponseError {
    code: ErrorCode,

    #[serde(skip_serializing_if = "Option::is_none", default)]
    message: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none", default)]
    data: Option<serde_json::Value>,
}

impl ResponseError {
    pub fn parse_error() -> Self {
        Self {
            code: ErrorCode::PARSE_ERROR,
            message: None,
            data: None,
        }
    }

    pub fn message(mut self, s: &str) -> Self {
        self.message = Some(s.to_owned());
        self
    }

    pub fn data(mut self, v: serde_json::Value) -> Self {
        self.data = Some(v);
        self
    }
}

impl From<serde_json::Error> for ResponseError {
    fn from(value: serde_json::Error) -> Self {
        let line = value.line();
        let column = value.column();
        let category = value.classify();
        let error = Self::try_from(value).expect("bug");
        error.data(serde_json::json!({
            "line": line,
            "column": column,
            "category": format!("{category:?}"),
        }))
    }
}

impl std::fmt::Display for ResponseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ErrorCode(i32);

impl ErrorCode {
    pub const PARSE_ERROR: Self = Self(-32700);
}
