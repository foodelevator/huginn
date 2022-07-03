use std::{error::Error, fs, process::Command};

const LIBASM: &[u8] = include_bytes!("../../libasm.o");

pub fn link(object: Vec<u8>) -> Result<(), Box<dyn Error>> {
    let obj_name = "tmp-output.o";
    let lib_name = "tmp-libasm.o";
    fs::write(obj_name, &object)?;
    fs::write(lib_name, LIBASM)?;
    Command::new("ld")
        .args(["-o", "a.out", obj_name, lib_name])
        .output()?;
    fs::remove_file(obj_name)?;
    fs::remove_file(lib_name)?;

    Ok(())
}
