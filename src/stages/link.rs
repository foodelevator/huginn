use std::{error::Error, fmt, fs, process::Command};

const LIBASM: &[u8] = include_bytes!("../../libasm.o");

pub fn link(object: Vec<u8>) -> Result<(), Box<dyn Error>> {
    let obj_name = "tmp-output.o";
    let lib_name = "tmp-libasm.o";
    fs::write(obj_name, &object)?;
    fs::write(lib_name, LIBASM)?;
    let output = Command::new("ld")
        .args(["-o", "a.out", obj_name, lib_name])
        .output()?;
    fs::remove_file(obj_name)?;
    fs::remove_file(lib_name)?;
    if output.status.code() != Some(0) {
        return Err(Box::new(LinkError {
            linker_output: String::from_utf8_lossy(&output.stderr).to_string(),
            code: output.status.code(),
        }));
    }

    Ok(())
}

#[derive(Debug)]
struct LinkError {
    pub linker_output: String,
    pub code: Option<i32>,
}

impl fmt::Display for LinkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\x1b[31mlinking error\x1b[0m:")?;
        if let Some(code) = self.code {
            write!(f, "exit code: {}\n{}", code, self.linker_output)
        } else {
            write!(f, "no exit code\n{}", self.linker_output)
        }
    }
}

impl Error for LinkError {}
