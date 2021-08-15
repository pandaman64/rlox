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
