#![allow(clippy::not_unsafe_ptr_arg_deref)]

use orfail::OrFail;
use std::{
    ffi::CStr,
    path::{Path, PathBuf},
};

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
            fn fsReadFile(path: *const u8, path_len: u32) -> *const u8;
        }

        let path = path.as_ref().to_str().or_fail()?;
        unsafe {
            let text_c_str = fsReadFile(path.as_ptr(), path.len() as u32);
            if text_c_str.is_null() {
                return Err(orfail::Failure::new().message("Failed to read file"));
            }

            let _text = CStr::from_ptr(text_c_str).to_str().or_fail()?;
            todo!()
            // .as_ref()
            //     .map(|ptr| {
            //         String::from_utf8_lossy(std::slice::from_raw_parts(ptr, path.len())).to_string()
            //     })
            //     .or_fail();
        }
    }

    fn read_sub_dirs<P: AsRef<Path>>(path: P) -> orfail::Result<Vec<PathBuf>> {
        todo!()
    }
}

#[derive(Debug)]
pub struct Bytes {}
