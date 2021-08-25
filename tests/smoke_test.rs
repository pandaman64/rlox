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

#[test]
fn test_assignment() {
    let input = r#"a + (b.c = 100);"#;
    std::env::set_var("RUST_LOG", "trace");
    let mut output = vec![];
    assert!(run(input, &mut output).is_err());
}

#[test]
fn test_bound_method() {
    let input = r#"class Scone {
    topping(first, second) {
        print "scone with " + first + " and " + second;
    }
}

var scone = Scone();
scone.topping("berries", "cream");"#;
    std::env::set_var("RUST_LOG", "trace");
    std::env::set_var("RLOX_LOG_GC", "1");
    std::env::set_var("RLOX_STRESS_GC", "1");
    let mut output = vec![];
    run(input, &mut output).unwrap();
    assert_eq!(output, b"scone with berries and cream\n");
}

#[test]
fn test_this() {
    let input = r#"class Scone {
    topping() {
        print "scone with " + this.first + " and " + this.second;
    }
}

var scone = Scone();
scone.first = "berries";
scone.second = "cream";
scone.topping();"#;
    std::env::set_var("RUST_LOG", "trace");
    let mut output = vec![];
    run(input, &mut output).unwrap();
    assert_eq!(output, b"scone with berries and cream\n");
}

#[test]
fn test_inherit() {
    let input = r#"
class Super {
    foo() {
        print "foo";
    }
}

class Sub < Super {
    bar() {
        this.foo();
        print "bar";
    }
}

var sub = Sub();
sub.bar();"#;
    std::env::set_var("RUST_LOG", "trace");
    let mut output = vec![];
    run(input, &mut output).unwrap();
    assert_eq!(output, b"foo\nbar\n");
}
