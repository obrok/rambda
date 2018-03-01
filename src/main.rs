extern crate rambda;

use std::io::Read;

fn main() {
    let mut buffer = String::new();
    std::io::stdin()
        .read_to_string(&mut buffer)
        .expect("Couldn't read STDIN");

    match rambda::parse_statements(&buffer) {
        Ok(program) => {
            let results = rambda::eval_statements(&program);

            for (statement, result) in program.iter().zip(results) {
                println!(
                    "{}\n-> {}\n",
                    rambda::unparse_statement(&statement),
                    rambda::unparse(&result)
                );
            }
        }
        Err(why) => println!("{:?}", why),
    }
}
