use std::process::Command;

#[test]
fn test_corpus() {
    let test = "chap21_global";
    let success = Command::new("dart")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["tool/bin/test.dart", test, "-i", "target/debug/rlox"])
        .status()
        .expect("failed to run dart test.dart")
        .success();
    assert!(success);
}

// TODO: miri test
// replace interpreter with `cargo +nightly miri run` and disable isolation
