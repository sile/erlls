use erlls_core::{config::Config, header::Header, message::DocumentUri, server::LanguageServer};
use orfail::OrFail;
use std::{
    future::Future,
    io::{BufRead, Write},
    path::PathBuf,
};

fn main() -> orfail::Result<()> {
    env_logger::init();

    let config = create_config();
    let mut server = LanguageServer::<FileSystem>::new(config, FileSystem);
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut stdin = stdin.lock();
    let mut stdout = stdout.lock();
    loop {
        let buf = read_message(&mut stdin).or_fail()?;
        log::debug!("Received JSON: {}", std::str::from_utf8(&buf).or_fail()?);

        let future = server.handle_incoming_message(buf);
        futures::executor::block_on(future);

        while let Some(msg) = server.take_outgoing_message() {
            write_message(&mut stdout, &msg).or_fail()?;
        }
    }
}

fn read_message<R: BufRead>(reader: &mut R) -> orfail::Result<Vec<u8>> {
    let header = Header::from_reader(reader).or_fail()?;
    let mut buf = vec![0; header.content_length];
    reader.read_exact(&mut buf).or_fail()?;
    Ok(buf)
}

fn write_message<W: Write>(writer: &mut W, msg: &[u8]) -> orfail::Result<()> {
    Header::new(msg.len()).to_writer(writer).or_fail()?;
    writer.write_all(msg).or_fail()?;
    writer.flush().or_fail()?;
    log::debug!("Sent JSON: {}", std::str::from_utf8(msg).or_fail()?);
    Ok(())
}

#[derive(Debug)]
struct FileSystem;

impl erlls_core::fs::FileSystem for FileSystem {
    fn exists(&mut self, uri: &DocumentUri) -> Box<dyn Unpin + Future<Output = bool>> {
        log::info!("exists: {}", uri.path().display());
        Box::new(std::future::ready(uri.path().exists()))
    }

    fn read_file(
        &mut self,
        uri: &DocumentUri,
    ) -> Box<dyn Unpin + Future<Output = orfail::Result<String>>> {
        log::info!("read_file: {}", uri.path().display());
        let result = std::fs::read_to_string(uri.path())
            .or_fail_with(|e| format!("{e}: {}", uri.path().display()));
        Box::new(std::future::ready(result))
    }

    fn read_sub_dirs(
        &mut self,
        uri: &DocumentUri,
    ) -> Box<dyn Unpin + Future<Output = orfail::Result<Vec<DocumentUri>>>> {
        log::info!("read_sub_dirs: {}", uri.path().display());
        let f = || {
            let mut dirs = Vec::new();
            for entry in std::fs::read_dir(uri.path()).or_fail()? {
                let entry = entry.or_fail()?;
                if entry.file_type().or_fail()?.is_dir() {
                    if let Some(dir) = entry.path().to_str().and_then(|p| uri.join(p).ok()) {
                        dirs.push(dir);
                    }
                }
            }
            Ok(dirs)
        };
        Box::new(std::future::ready(f()))
    }
}

fn create_config() -> Config {
    let mut config = Config::default();
    if let Some(kernel_lib_dir) = std::process::Command::new("erl")
        .arg("-boot")
        .arg("start_clean")
        .arg("-noshell")
        .arg("-eval")
        .arg("io:format(code:lib_dir(kernel)).")
        .arg("-s")
        .arg("init")
        .arg("stop")
        .output()
        .ok()
        .and_then(|output| String::from_utf8(output.stdout).ok().map(PathBuf::from))
    {
        config
            .erl_libs
            .extend(kernel_lib_dir.parent().into_iter().map(|p| p.to_owned()));
        config.erl_libs.push(PathBuf::from("_checkouts/"));
        config.erl_libs.push(PathBuf::from("_build/default/lib"));
    }
    config
}
