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

// named!(
//     parse_app<Box<Term>>,
//     do_parse!(fun: parse_term() >> arg: parse_term() >> app(fun, arg))
// );

named!(
    parse_var<Box<Term>>,
    map_res!(alpha, |letters| { std::str::from_utf8(letters).map(var) })
);

named!(parse_term<Box<Term>>, alt!(parse_var));

pub fn parse(input: &str) -> Box<Term> {
    parse_term(input.as_bytes()).unwrap().1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_variable() {
        assert_eq!(parse("var"), var("var"));
    }

    #[test]
    fn simple_application() {
        assert_eq!(parse("fun var"), app(var("fun"), var("var")));
    }
}
