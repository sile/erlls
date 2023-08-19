#![allow(improper_ctypes, non_snake_case, clippy::not_unsafe_ptr_arg_deref)]

use erlls_core::{config::Config, server::LanguageServer};
use futures::executor::LocalPool;
use futures::task::LocalSpawnExt;
use orfail::OrFail;
use std::{
    collections::HashMap,
    future::{self, Future},
    path::{Path, PathBuf},
    sync::mpsc::{self, TryRecvError},
    task::{Poll, Waker},
};

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
pub struct FileSystem {
    promise_id: u32,
    exists_promises: HashMap<u32, mpsc::Sender<bool>>,
    read_file_promises: HashMap<u32, mpsc::Sender<Option<Vec<u8>>>>,
    read_sub_dirs_promises: HashMap<u32, mpsc::Sender<Option<Vec<u8>>>>,
    waker_tx: mpsc::Sender<Waker>,
    waker_rx: mpsc::Receiver<Waker>,
}

impl FileSystem {
    fn next_promise_id(&mut self) -> u32 {
        self.promise_id += 1;
        self.promise_id
    }
}

impl Default for FileSystem {
    fn default() -> Self {
        let (waker_tx, waker_rx) = mpsc::channel();
        Self {
            waker_tx,
            waker_rx,
            promise_id: 0,
            exists_promises: HashMap::new(),
            read_file_promises: HashMap::new(),
            read_sub_dirs_promises: HashMap::new(),
        }
    }
}

#[no_mangle]
pub fn notifyFsExistsAsyncResult(
    server: *mut LanguageServer<FileSystem>,
    promise_id: u32,
    exists: bool,
) {
    let server = unsafe { &mut *server };
    let _ = server
        .fs_mut()
        .exists_promises
        .remove(&promise_id)
        .expect("unreachable")
        .send(exists);
}

#[no_mangle]
pub fn notifyFsReadFileAsyncResult(
    server: *mut LanguageServer<FileSystem>,
    promise_id: u32,
    vec_ptr: *mut Vec<u8>,
) {
    println("@@@ notifyFsReadFileAsyncResult");
    let vec = unsafe { (!vec_ptr.is_null()).then(|| *Box::from_raw(vec_ptr)) };
    let server = unsafe { &mut *server };
    let _ = server
        .fs_mut()
        .read_file_promises
        .remove(&promise_id)
        .expect("unreachable")
        .send(vec);
    let _ = server
        .fs_mut()
        .waker_rx
        .try_recv()
        .map(|waker| waker.wake());
}

#[no_mangle]
pub fn notifyFsReadSubDirsAsyncResult(
    server: *mut LanguageServer<FileSystem>,
    promise_id: u32,
    vec_ptr: *mut Vec<u8>,
) {
    let vec = unsafe { (!vec_ptr.is_null()).then(|| *Box::from_raw(vec_ptr)) };
    let server = unsafe { &mut *server };
    let _ = server
        .fs_mut()
        .read_sub_dirs_promises
        .remove(&promise_id)
        .expect("unreachable")
        .send(vec);
}

impl erlls_core::fs::FileSystem for FileSystem {
    fn exists<P: AsRef<Path>>(&mut self, path: P) -> Box<dyn Unpin + Future<Output = bool>> {
        extern "C" {
            fn fsExistsAsync(promise_id: u32, path: *const u8, path_len: u32);
        }

        if let Some(path) = path.as_ref().to_str() {
            let promise_id = self.next_promise_id();
            let (tx, rx) = mpsc::channel();
            self.exists_promises.insert(promise_id, tx);
            unsafe { fsExistsAsync(promise_id, path.as_ptr(), path.len() as u32) };
            Box::new(future::poll_fn(move |_ctx| {
                if let Ok(exists) = rx.try_recv() {
                    Poll::Ready(exists)
                } else {
                    Poll::Pending
                }
            }))
        } else {
            Box::new(future::ready(false))
        }
    }

    fn read_file<P: AsRef<Path>>(
        &mut self,
        path: P,
    ) -> Box<dyn Unpin + Future<Output = orfail::Result<String>>> {
        extern "C" {
            fn fsReadFileAsync(promise_id: u32, path: *const u8, path_len: u32);
        }

        match path.as_ref().to_str().or_fail() {
            Err(e) => Box::new(future::ready(Err(e))),
            Ok(path) => {
                let promise_id = self.next_promise_id();
                let (tx, rx) = mpsc::channel();
                let waker_tx = self.waker_tx.clone();
                self.read_file_promises.insert(promise_id, tx);
                unsafe { fsReadFileAsync(promise_id, path.as_ptr(), path.len() as u32) };
                Box::new(future::poll_fn(move |ctx| {
                    println("@@@ polled");
                    match rx.try_recv() {
                        Err(TryRecvError::Empty) => {
                            let _ = waker_tx.send(ctx.waker().clone());
                            Poll::Pending
                        }
                        Err(TryRecvError::Disconnected) | Ok(None) => {
                            Poll::Ready(Err(orfail::Failure::new("Failed to read file")))
                        }
                        Ok(Some(vec)) => Poll::Ready(String::from_utf8(vec).or_fail()),
                    }
                }))
            }
        }
    }

    fn read_sub_dirs<P: AsRef<Path>>(
        &mut self,
        path: P,
    ) -> Box<dyn Unpin + Future<Output = orfail::Result<Vec<PathBuf>>>> {
        extern "C" {
            fn fsReadSubDirsAsync(promise_id: u32, path: *const u8, path_len: u32);
        }

        match path.as_ref().to_str().or_fail() {
            Err(e) => Box::new(future::ready(Err(e))),
            Ok(path) => {
                let promise_id = self.next_promise_id();
                let (tx, rx) = mpsc::channel();
                self.read_sub_dirs_promises.insert(promise_id, tx);
                unsafe { fsReadSubDirsAsync(promise_id, path.as_ptr(), path.len() as u32) };
                Box::new(future::poll_fn(move |_ctx| match rx.try_recv() {
                    Err(TryRecvError::Empty) => Poll::Pending,
                    Err(TryRecvError::Disconnected) | Ok(None) => {
                        Poll::Ready(Err(orfail::Failure::new("Failed to read directory")))
                    }
                    Ok(Some(vec)) => Poll::Ready(serde_json::from_slice(&vec).or_fail()),
                }))
            }
        }
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
pub fn newLocalPool() -> *mut LocalPool {
    Box::into_raw(Box::new(LocalPool::new()))
}

#[no_mangle]
pub fn tryRunOne(pool: *mut LocalPool) -> bool {
    unsafe {
        let pool = &mut *pool;
        pool.try_run_one()
    }
}

#[no_mangle]
pub fn newServer() -> *mut LanguageServer<FileSystem> {
    std::panic::set_hook(Box::new(|info| {
        println(&info.to_string());
    }));

    let config = Config::default();
    Box::into_raw(Box::new(LanguageServer::new(config, FileSystem::default())))
}

fn println(msg: &str) {
    unsafe { consoleLog(msg.as_ptr(), msg.len() as u32) };
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
pub fn handleIncomingMessage(
    pool: *mut LocalPool,
    server: *mut LanguageServer<FileSystem>,
    message_ptr: *mut Vec<u8>,
) {
    unsafe {
        let pool = &mut *pool;
        let server = &mut *server;
        let message = *Box::from_raw(message_ptr);
        pool.spawner()
            .spawn_local(server.handle_incoming_message(message))
            .expect("unreachable");
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
