extern crate rambda;

fn main() {
    let mut expression = String::new();
    std::io::stdin()
        .read_line(&mut expression)
        .expect("should work");
    let ast = rambda::parse(&expression);

    println!("{:?}", &ast);
}
