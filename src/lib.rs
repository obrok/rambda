extern crate immutable_map;
#[macro_use]
extern crate nom;

use nom::{alpha, space};
use Term::*;
use immutable_map::TreeMap;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term<'a> {
    Var(&'a str),
    App(Box<Term<'a>>, Box<Term<'a>>),
    Fun(&'a str, Box<Term<'a>>),
}

fn var(name: &str) -> Box<Term> {
    Box::new(Var(name))
}

fn app<'a>(fun: Box<Term<'a>>, arg: Box<Term<'a>>) -> Box<Term<'a>> {
    Box::new(App(fun, arg))
}

fn fun<'a>(arg: &'a str, body: Box<Term<'a>>) -> Box<Term<'a>> {
    Box::new(Fun(arg, body))
}

named!(
    parse_term<Box<Term>>,
    alt_complete!(parse_app | parse_fun | parse_var)
);

named!(
    parse_app<Box<Term>>,
    do_parse!(fun: parse_fun >> space >> arg: parse_term >> (app(fun, arg)))
);

named!(
    parse_var<Box<Term>>,
    map_res!(alpha, |letters| { std::str::from_utf8(letters).map(var) })
);

named!(
    parse_fun<Box<Term>>,
    alt_complete!(parse_var | parse_fun_abstraction)
);

named!(
    parse_fun_abstraction<Box<Term>>,
    map_res!(
        do_parse!(
            tag!("(") >> opt!(space) >> arg: alpha >> opt!(space) >> tag!("->") >> opt!(space)
                >> body: parse_term >> opt!(space) >> tag!(")") >> ((arg, body))
        ),
        |(arg, body)| { std::str::from_utf8(arg).map(|arg| fun(arg, body)) }
    )
);



pub fn parse(input: &str) -> Result<Box<Term>, String> {
    match parse_term(input.as_bytes()) {
        nom::IResult::Done(_input, output) => Ok(output),
        nom::IResult::Error(error) => Err(format!("Error {:?}", error)),
        nom::IResult::Incomplete(needed) => Err(format!("Incomplete {:?}", needed)),
    }
}

pub fn eval(term: Box<Term>) -> Box<Term> {
    eval_with_env(term, &TreeMap::new())
}

fn eval_with_env<'a>(term: Box<Term<'a>>, env: &TreeMap<&'a str, Box<Term<'a>>>) -> Box<Term<'a>> {
    let term = *term;
    match term {
        App(fun, arg) => match *fun {
            Fun(argname, body) => {
                eval_with_env(body, &env.insert(argname, eval_with_env(arg, &env)))
            }
            other => Box::new(other),
        },
        Var(name) => env.get(name)
            .map(|value| eval_with_env(value.clone(), &env))
            .unwrap_or_else(|| Box::new(Var(name))),
        other => Box::new(other),
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

    #[test]
    fn simple_function() {
        assert_eq!(parse("( x  -> x)"), Ok(fun("x", var("x"))));
    }

    #[test]
    fn evaluating_variable() {
        assert_eq!(eval(var("x")), var("x"));
    }

    #[test]
    fn apply_function() {
        assert_eq!(eval(app(fun("x", var("x")), var("y"))), var("y"))
    }
}
