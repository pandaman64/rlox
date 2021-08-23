use rlox::run;

#[test]
fn test_closure() {
    let input = r#"
fun greet(name) {
    fun hi() {
        print name;
    }
    return hi;
}
var john = greet("John");
var bob = greet("Bob");
john();
bob();
"#;
    run(input, &mut vec![]).unwrap();
}

#[test]
fn test_negate() {
    let input = r#"print -(3); // expect: -3
print --(3); // expect: 3
print ---(3); // expect: -3"#;
    run(input, &mut vec![]).unwrap();
}

#[test]
fn test_stress_gc() {
    let input = r#"fun f() {
    while (true) {
        var i = "i";
        return i;
    }
}

print f();
// expect: i"#;
    std::env::set_var("RLOX_LOG_GC", "1");
    std::env::set_var("RLOX_STRESS_GC", "1");
    let mut output = vec![];
    run(input, &mut output).unwrap();
    assert_eq!(output, b"i\n");
}

#[test]
fn test_class_decl() {
    let input = r#"class Brioche {}
    print Brioche;
    print Brioche();"#;
    std::env::set_var("RUST_LOG", "trace");
    std::env::set_var("RLOX_LOG_GC", "1");
    std::env::set_var("RLOX_STRESS_GC", "1");
    let mut output = vec![];
    run(input, &mut output).unwrap();
    assert_eq!(output, b"Brioche\nBrioche instance\n");
}
