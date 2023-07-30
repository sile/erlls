#![allow(improper_ctypes, non_snake_case, clippy::not_unsafe_ptr_arg_deref)]

use erlls_core::{config::Config, server::LanguageServer};
use orfail::OrFail;
use std::path::{Path, PathBuf};

extern "C" {
    fn consoleLog(msg: *const u8, msg_len: u32);
}

#[derive(Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct ConfigDelta {
    #[serde(default)]
    pub erl_libs: Option<Vec<PathBuf>>,
}

#[derive(Debug)]
pub struct FileSystem;

impl erlls_core::fs::FileSystem for FileSystem {
    fn exists<P: AsRef<Path>>(path: P) -> bool {
        extern "C" {
            fn fsExists(path: *const u8, path_len: u32) -> bool;
        }

        if let Some(path) = path.as_ref().to_str() {
            unsafe { fsExists(path.as_ptr(), path.len() as u32) }
        } else {
            false
        }
    }

    fn read_file<P: AsRef<Path>>(path: P) -> orfail::Result<String> {
        extern "C" {
            fn fsReadFile(path: *const u8, path_len: u32) -> *mut Vec<u8>;
        }

        let path = path.as_ref().to_str().or_fail()?;
        let vec = unsafe {
            let vec_ptr = fsReadFile(path.as_ptr(), path.len() as u32);
            if vec_ptr.is_null() {
                return Err(orfail::Failure::new().message("Failed to read file"));
            }
            *Box::from_raw(vec_ptr)
        };
        Ok(String::from_utf8(vec).or_fail()?)
    }

    fn read_sub_dirs<P: AsRef<Path>>(path: P) -> orfail::Result<Vec<PathBuf>> {
        extern "C" {
            fn fsReadSubDirs(path: *const u8, path_len: u32) -> *mut Vec<u8>;
        }

        let path = path.as_ref().to_str().or_fail()?;
        let vec = unsafe {
            let vec_ptr = fsReadSubDirs(path.as_ptr(), path.len() as u32);
            if vec_ptr.is_null() {
                return Err(orfail::Failure::new().message("Failed to read directory"));
            }
            *Box::from_raw(vec_ptr)
        };
        Ok(serde_json::from_slice(&vec).or_fail()?)
    }
}

#[no_mangle]
pub fn vecOffset(v: *mut Vec<u8>) -> *mut u8 {
    unsafe { &mut *v }.as_mut_ptr()
}

#[no_mangle]
pub fn vecLen(v: *mut Vec<u8>) -> i32 {
    unsafe { &*v }.len() as i32
}

#[no_mangle]
pub fn allocateVec(len: i32) -> *mut Vec<u8> {
    Box::into_raw(Box::new(vec![0; len as usize]))
}

#[no_mangle]
pub fn freeVec(v: *mut Vec<u8>) {
    let _ = unsafe { Box::from_raw(v) };
}

#[no_mangle]
pub fn newServer() -> *mut LanguageServer<FileSystem> {
    std::panic::set_hook(Box::new(|info| {
        let msg = info.to_string();
        unsafe {
            consoleLog(msg.as_ptr(), msg.len() as u32);
        }
    }));

    let config = Config::default();
    Box::into_raw(Box::new(LanguageServer::new(config)))
}

#[no_mangle]
pub fn updateConfig(server: *mut LanguageServer<FileSystem>, config_json_ptr: *mut Vec<u8>) {
    unsafe {
        let config_delta_json = *Box::from_raw(config_json_ptr);
        let Ok(config_delta) = serde_json::from_slice::<ConfigDelta>(&config_delta_json) else {
            return;
        };
        let server = &mut *server;
        let mut config = server.config().clone();
        if let Some(v) = config_delta.erl_libs {
            config.erl_libs = v;
        }
        server.update_config(config);
    }
}

#[no_mangle]
pub fn handleIncomingMessage(server: *mut LanguageServer<FileSystem>, message_ptr: *mut Vec<u8>) {
    unsafe {
        let message = *Box::from_raw(message_ptr);
        let server = &mut *server;
        server.handle_incoming_message(&message);
    }
}

#[no_mangle]
pub fn takeOutgoingMessage(server: *mut LanguageServer<FileSystem>) -> *mut Vec<u8> {
    unsafe {
        let server = &mut *server;
        if let Some(message) = server.take_outgoing_message() {
            Box::into_raw(Box::new(message))
        } else {
            std::ptr::null_mut()
        }
    }
}
