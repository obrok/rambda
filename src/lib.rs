extern crate immutable_map;
#[macro_use]
extern crate nom;

use nom::{alpha, space};
use Term::*;
use Statement::*;
use immutable_map::TreeMap;
use std::rc::Rc;
use std::borrow::Borrow;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Def { name: String, value: Rc<Term> },
    Expr(Rc<Term>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Term {
    Var(String),
    App(Rc<Term>, Rc<Term>),
    Fun(String, Rc<Term>),
}

pub type Env<'a> = TreeMap<&'a String, &'a Rc<Term>>;

fn var(name: &str) -> Rc<Term> {
    Rc::new(Var(String::from(name)))
}

fn app(fun: Rc<Term>, arg: Rc<Term>) -> Rc<Term> {
    Rc::new(App(fun, arg))
}

fn fun(arg: &str, body: Rc<Term>) -> Rc<Term> {
    Rc::new(Fun(String::from(arg), body))
}

named!(alpha_utf8<&str>, map_res!(alpha, std::str::from_utf8));

named!(
    parse_term<Rc<Term>>,
    alt_complete!(parse_app | parse_fun | parse_var)
);

named!(
    parse_app<Rc<Term>>,
    do_parse!(fun: parse_fun >> space >> arg: parse_term >> (app(fun, arg)))
);

named!(parse_var<Rc<Term>>, map!(alpha_utf8, var));

named!(
    parse_fun<Rc<Term>>,
    alt_complete!(parse_var | parse_fun_abstraction)
);

named!(
    parse_fun_abstraction<Rc<Term>>,
    do_parse!(
        tag!("(") >> opt!(space) >> arg: alpha_utf8 >> opt!(space) >> tag!("->") >> opt!(space)
            >> body: parse_term >> opt!(space) >> tag!(")") >> (fun(arg, body))
    )
);

named!(
    semicolon<()>,
    do_parse!(opt!(space) >> tag!(";") >> opt!(space) >> (()))
);


named!(
    parse_def_list<Vec<Statement>>,
    separated_list_complete!(semicolon, parse_statement)
);

named!(
    parse_statement<Statement>,
    alt_complete!(parse_def | parse_expr)
);

named!(
    parse_def<Statement>,
    do_parse!(
        opt!(space) >> name: alpha_utf8 >> opt!(space) >> tag!("=") >> opt!(space)
            >> value: parse_term >> (Def {
            name: String::from(name),
            value,
        })
    )
);

named!(parse_expr<Statement>, map!(parse_term, Expr));

pub fn parse_statements(input: &str) -> Result<Vec<Statement>, String> {
    normalize_error(parse_def_list(input.as_bytes()))
}

pub fn parse(input: &str) -> Result<Rc<Term>, String> {
    normalize_error(parse_term(input.as_bytes()))
}

fn normalize_error<I, O>(nom_result: nom::IResult<I, O>) -> Result<O, String> {
    match nom_result {
        nom::IResult::Done(_input, output) => Ok(output),
        nom::IResult::Error(error) => Err(format!("Error {:?}", error)),
        nom::IResult::Incomplete(needed) => Err(format!("Incomplete {:?}", needed)),
    }
}

pub fn eval(term: &Rc<Term>) -> &Rc<Term> {
    eval_with_env(term, &TreeMap::new())
}

pub fn eval_statements<'a>(statements: &'a Vec<Statement>) -> Vec<&'a Rc<Term>> {
    let mut results = Vec::new();
    let mut env = TreeMap::new();
    for statement in statements {
        let result_pair = eval_statement(statement, env);
        results.push(result_pair.0);
        env = result_pair.1;
    }
    results
}

pub fn eval_statement<'a>(statement: &'a Statement, env: Env<'a>) -> (&'a Rc<Term>, Env<'a>) {
    match statement {
        &Expr(ref expr) => (eval_with_env(&expr, &env), env),
        &Def {
            ref name,
            ref value,
        } => {
            let result = eval_with_env(&value, &env);
            (result, env.insert(name, value))
        }
    }
}

fn eval_with_env<'a>(term: &'a Rc<Term>, env: &TreeMap<&String, &'a Rc<Term>>) -> &'a Rc<Term> {
    match term.borrow() {
        &App(ref fun, ref arg) => match eval_with_env(fun, env).borrow() {
            &Fun(ref argname, ref body) => {
                eval_with_env(&body, &env.insert(&argname, eval_with_env(&arg, &env)))
            }
            _ => term,
        },
        &Var(ref name) => env.get(&name)
            .map(|value| eval_with_env(value.clone(), &env))
            .unwrap_or(term),
        _ => term,
    }
}

pub fn unparse(term: &Rc<Term>) -> String {
    match term.borrow() {
        &Var(ref x) => format!("{}", x),
        &App(ref f, ref x) => format!("{} {}", unparse(f), unparse(x)),
        &Fun(ref x, ref body) => format!("({} -> {})", x, unparse(body)),
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
        assert_eq!(eval(&var("x")), &var("x"));
    }

    #[test]
    fn apply_function() {
        assert_eq!(eval(&app(fun("x", var("x")), var("y"))), &var("y"))
    }

    #[test]
    fn parsing_definitions() {
        assert_eq!(
            parse_statements("f = (x -> x)"),
            Ok(vec![
                Def {
                    name: String::from("f"),
                    value: fun("x", var("x")),
                },
            ])
        )
    }

    #[test]
    fn parsing_expression_statements() {
        assert_eq!(
            parse_statements("x; y; (x -> x)"),
            Ok(vec![
                Expr(var("x")),
                Expr(var("y")),
                Expr(fun("x", var("x"))),
            ])
        )
    }

    #[test]
    fn unparsing() {
        assert_eq!(unparse(&app(fun("x", var("x")), var("y"))), "(x -> x) y")
    }

    #[test]
    fn evaluating_application_that_cannot_be_applied() {
        assert_eq!(eval(&app(var("f"), var("y"))), &app(var("f"), var("y")))
    }

    #[test]
    fn evaluating_statements() {
        assert_eq!(
            eval_statements(&vec![
                Expr(app(var("f"), var("y"))),
                Def {
                    name: String::from("f"),
                    value: fun("x", var("x")),
                },
                Expr(app(var("f"), var("y"))),
            ]),
            vec![&app(var("f"), var("y")), &fun("x", var("x")), &var("y")]
        )
    }
}
