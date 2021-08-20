use std::process::Command;

const TEST: &str = "chap25_closures";

#[test]
fn test_corpus() {
    let success = Command::new("dart")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["tool/bin/test.dart", TEST, "-i", "target/debug/rlox"])
        .status()
        .expect("failed to run dart test.dart")
        .success();
    assert!(success);
}

// TODO: miri test
// replace interpreter with `cargo +nightly miri run` and disable isolation
