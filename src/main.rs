#[derive(Debug)]
enum Lambda {
    Thing,
}

fn parse(_input: &str) -> Lambda {
    Lambda::Thing
}

fn main() {
    let mut expression = String::new();
    std::io::stdin()
        .read_line(&mut expression)
        .expect("should work");
    let ast = parse(&expression);

    println!("{:?}", &ast);
}
