use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=libasm.s");

    let output = Command::new("nasm")
        .args(["-f", "elf64", "-o", "libasm.o", "libasm.s"])
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(0));
}
