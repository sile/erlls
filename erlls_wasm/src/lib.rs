#![allow(improper_ctypes, clippy::not_unsafe_ptr_arg_deref)]

use erlls_core::{config::Config, server::LanguageServer};
use orfail::OrFail;
use std::path::{Path, PathBuf};

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
pub fn vec_offset(v: *mut Vec<u8>) -> *mut u8 {
    unsafe { &mut *v }.as_mut_ptr()
}

#[no_mangle]
pub fn vec_len(v: *mut Vec<u8>) -> i32 {
    unsafe { &*v }.len() as i32
}

#[no_mangle]
pub fn allocate_vec(len: i32) -> *mut Vec<u8> {
    Box::into_raw(Box::new(vec![0; len as usize]))
}

#[no_mangle]
pub fn free_vec(v: *mut Vec<u8>) {
    let _ = unsafe { Box::from_raw(v) };
}

#[no_mangle]
pub fn new_server() -> *mut LanguageServer<FileSystem> {
    let config = Config::default();
    Box::into_raw(Box::new(LanguageServer::new(config)))
}

#[no_mangle]
pub fn update_config(server: *mut LanguageServer<FileSystem>, config_json_ptr: *mut Vec<u8>) {
    unsafe {
        let config_json = *Box::from_raw(config_json_ptr);
        let Ok(config) = serde_json::from_slice::<ConfigDelta>(&config_json) else {
            return;
        };
        let server = &mut *server;
        if let Some(v) = config.erl_libs {
            server.config_mut().erl_libs = v;
        }
    }
}

#[no_mangle]
pub fn handle_incoming_message(server: *mut LanguageServer<FileSystem>, message_ptr: *mut Vec<u8>) {
    unsafe {
        let message = *Box::from_raw(message_ptr);
        let server = &mut *server;
        server.handle_incoming_message(&message);
    }
}

#[no_mangle]
pub fn take_outgoing_message(server: *mut LanguageServer<FileSystem>) -> *mut Vec<u8> {
    unsafe {
        let server = &mut *server;
        if let Some(message) = server.take_outgoing_message() {
            Box::into_raw(Box::new(message))
        } else {
            std::ptr::null_mut()
        }
    }
}
