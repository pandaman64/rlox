use std::process::Command;

const TEST: &str = "chap30_optimization";

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

#[test]
fn test_corpus_stress_gc() {
    let success = Command::new("dart")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .env("RLOX_STRESS_GC", "1")
        .args(["tool/bin/test.dart", TEST, "-i", "target/debug/rlox"])
        .status()
        .expect("failed to run dart test.dart")
        .success();
    assert!(success);
}

// TODO: miri test
// replace interpreter with `cargo +nightly miri run` and disable isolation
#[test]
#[ignore = "running Miri is tooooooooo slow"]
fn test_corpus_miri() {
    let success = Command::new("dart")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .env("MIRIFLAGS", "-Zmiri-disable-isolation")
        .args([
            "tool/bin/test.dart",
            TEST,
            "-i",
            "cargo",
            "-a",
            "+nightly",
            "-a",
            "miri",
            "-a",
            "run",
            "-a",
            "--quiet",
            "-a",
            "--",
        ])
        .status()
        .expect("failed to run dart test.dart with miri")
        .success();
    assert!(success);
}
