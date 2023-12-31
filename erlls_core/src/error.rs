use orfail::Failure;
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

    pub fn invalid_request() -> Self {
        Self {
            code: ErrorCode::INVALID_REQUEST,
            message: None,
            data: None,
        }
    }

    pub fn invalid_params() -> Self {
        Self {
            code: ErrorCode::INVALID_PARAMS,
            message: None,
            data: None,
        }
    }

    pub fn server_not_initialized() -> Self {
        Self {
            code: ErrorCode::SERVER_NOT_INITIALIZED,
            message: Some("Server not initialized".to_owned()),
            data: None,
        }
    }

    pub fn request_failed() -> Self {
        Self {
            code: ErrorCode::REQUEST_FAILED,
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
        let error = Self::invalid_params().message(&value.to_string());
        error.data(serde_json::json!({
            "line": line,
            "column": column,
            "category": format!("{category:?}"),
        }))
    }
}

impl From<Failure> for ResponseError {
    fn from(value: Failure) -> Self {
        ResponseError {
            code: ErrorCode::INVALID_PARAMS,
            message: Some(value.message),
            data: serde_json::to_value(&value.backtrace).ok(),
        }
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
    pub const REQUEST_FAILED: Self = Self(-32803);
    pub const PARSE_ERROR: Self = Self(-32700);
    pub const INVALID_REQUEST: Self = Self(-32600);
    pub const METHOD_NOT_FOUND: Self = Self(-32601);
    pub const INVALID_PARAMS: Self = Self(-32602);
    pub const INTERNAL_ERROR: Self = Self(-32603);
    pub const SERVER_NOT_INITIALIZED: Self = Self(-32002);
}

impl ErrorCode {
    pub const fn as_u32(self) -> u32 {
        self.0 as u32
    }
}
