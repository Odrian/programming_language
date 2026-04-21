use std::env;

fn main() {
    let target = env::var("TARGET")
        .expect("env variable `TARGET` should be set by rust");
    println!("cargo:rustc-env=BUILD_TARGET={}", target);
}
