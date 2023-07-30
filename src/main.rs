use erlls_core::{config::Config, header::Header, server::LanguageServer};
use orfail::OrFail;
use std::{
    io::{Read, Write},
    path::{Path, PathBuf},
};

fn main() -> orfail::Result<()> {
    env_logger::init();

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
        config.erl_libs.push(PathBuf::from("_build/default/lib"));
    }

    let mut server = LanguageServer::<FileSystem>::new(config);
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    let mut stdin = stdin.lock();
    let mut stdout = stdout.lock();
    let mut content_buf = Vec::new();
    loop {
        let header = Header::from_reader(&mut stdin).or_fail()?;
        log::debug!("Received header: {:?}", header);

        content_buf.resize(header.content_length as usize, 0);
        stdin.read_exact(&mut content_buf).or_fail()?;
        log::debug!(
            "Received JSON: {}",
            std::str::from_utf8(&content_buf).or_fail()?
        );

        server.handle_incoming_message(&content_buf);
        while let Some(msg) = server.take_outgoing_message() {
            write_message(&mut stdout, &msg).or_fail()?;
        }
    }
}

fn write_message<W: Write>(writer: &mut W, msg: &[u8]) -> orfail::Result<()> {
    Header::new(msg.len()).to_writer(writer).or_fail()?;
    writer.write_all(&msg).or_fail()?;
    writer.flush().or_fail()?;
    log::debug!("Sent JSON: {}", std::str::from_utf8(&msg).or_fail()?);
    Ok(())
}

#[derive(Debug)]
struct FileSystem;

impl erlls_core::fs::FileSystem for FileSystem {
    fn exists<P: AsRef<Path>>(path: P) -> bool {
        path.as_ref().exists()
    }

    fn read_file<P: AsRef<Path>>(path: P) -> orfail::Result<String> {
        std::fs::read_to_string(path).or_fail()
    }

    fn read_sub_dirs<P: AsRef<Path>>(path: P) -> orfail::Result<Vec<PathBuf>> {
        let mut dirs = Vec::new();
        for entry in std::fs::read_dir(path).or_fail()? {
            let entry = entry.or_fail()?;
            if entry.file_type().or_fail()?.is_dir() {
                dirs.push(entry.path());
            }
        }
        Ok(dirs)
    }
}
