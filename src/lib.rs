#[macro_use]
extern crate nom;

use nom::{alpha, space};
use Term::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Term<'a> {
    Var(&'a str),
    App(Box<Term<'a>>, Box<Term<'a>>),
    Abs(&'a str, Box<Term<'a>>),
}

fn var(name: &str) -> Box<Term> {
    Box::new(Var(name))
}

fn app<'a>(fun: Box<Term<'a>>, arg: Box<Term<'a>>) -> Box<Term<'a>> {
    Box::new(App(fun, arg))
}

named!(parse_term<Box<Term>>, alt_complete!(parse_app | parse_var));

named!(
    parse_app<Box<Term>>,
    do_parse!(fun: parse_fun >> space >> arg: parse_term >> (app(fun, arg)))
);

named!(
    parse_var<Box<Term>>,
    map_res!(alpha, |letters| { std::str::from_utf8(letters).map(var) })
);

named!(parse_fun<Box<Term>>, alt_complete!(parse_var));

pub fn parse(input: &str) -> Result<Box<Term>, String> {
    match parse_term(input.as_bytes()) {
        nom::IResult::Done(_input, output) => Ok(output),
        nom::IResult::Error(error) => Err(format!("Error {:?}", error)),
        nom::IResult::Incomplete(needed) => Err(format!("Incomplete {:?}", needed)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_variable() {
        assert_eq!(parse("var"), Ok(var("var")));
    }

    #[test]
    fn simple_application() {
        assert_eq!(parse("fun var"), Ok(app(var("fun"), var("var"))));
    }
}
