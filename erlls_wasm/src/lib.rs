#![allow(improper_ctypes, clippy::not_unsafe_ptr_arg_deref)]

use orfail::OrFail;
use std::path::{Path, PathBuf};

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
pub fn allocate_vec(len: i32) -> *mut Vec<u8> {
    Box::into_raw(Box::new(vec![0; len as usize]))
}
