extern crate immutable_map;
#[macro_use]
extern crate nom;

use nom::{is_alphanumeric, multispace};
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

named!(
    alpha_utf8<&str>,
    map_res!(take_while1!(is_identifier), std::str::from_utf8)
);

fn is_identifier(c: u8) -> bool {
    let underscore = 95;
    is_alphanumeric(c) || c == underscore
}

named!(
    parse_term<Rc<Term>>,
    alt_complete!(parse_app | parse_fun | parse_parens | parse_var)
);

named!(
    parse_app<Rc<Term>>,
    map_opt!(
        separated_list_complete!(whitespace, parse_fun),
        |terms: Vec<Rc<Term>>| {
            let mut terms = terms.clone();
            terms.reverse();
            let mut result = terms.pop();
            terms.reverse();

            for item in terms {
                result = result.map(|result| app(result, item))
            }
            result
        }
    )
);

named!(parse_var<Rc<Term>>, map!(alpha_utf8, var));

named!(
    parse_parens<Rc<Term>>,
    do_parse!(tag!("(") >> whitespace >> val: parse_term >> whitespace >> tag!(")") >> (val))
);

named!(
    parse_fun<Rc<Term>>,
    alt_complete!(parse_parens | parse_var | parse_fun_abstraction)
);

named!(
    parse_fun_abstraction<Rc<Term>>,
    do_parse!(
        tag!("(") >> whitespace >> arg: alpha_utf8 >> whitespace >> tag!("->") >> whitespace
            >> body: parse_term >> whitespace >> tag!(")") >> (fun(arg, body))
    )
);

named!(
    semicolon<()>,
    do_parse!(whitespace >> tag!(";") >> whitespace >> (()))
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
        whitespace >> name: alpha_utf8 >> whitespace >> tag!("=") >> whitespace >> value: parse_term
            >> (Def {
                name: String::from(name),
                value,
            })
    )
);

named!(whitespace<()>, map!(opt!(multispace), |_| ()));

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

pub fn eval(term: &Rc<Term>) -> Rc<Term> {
    eval_with_env(term, &TreeMap::new())
}

pub fn eval_statements<'a>(statements: &'a Vec<Statement>) -> Vec<Rc<Term>> {
    let mut results = Vec::new();
    let mut env = TreeMap::new();
    for statement in statements {
        let result_pair = eval_statement(statement, env);
        results.push(result_pair.0);
        env = result_pair.1;
    }
    results
}

pub fn eval_statement<'a>(statement: &'a Statement, env: Env<'a>) -> (Rc<Term>, Env<'a>) {
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

fn eval_with_env<'a>(term: &'a Rc<Term>, env: &'a Env) -> Rc<Term> {
    match term.borrow() {
        &App(ref fun, ref arg) => match eval_with_env(fun, env).borrow() {
            &Fun(ref argname, ref body) => {
                eval_with_env(&replace(&body, &argname, &eval_with_env(&arg, &env)), env)
            }
            _ => term.clone(),
        },
        &Var(ref name) => env.get(&name)
            .map(|value| eval_with_env(value, &env))
            .unwrap_or_else(|| Rc::clone(term)),
        _ => term.clone(),
    }
}

fn replace<'a>(term: &'a Rc<Term>, argname: &'a String, value: &'a Rc<Term>) -> Rc<Term> {
    match term.borrow() {
        &App(ref fun, ref arg) => app(replace(&fun, argname, value), replace(&arg, argname, value)),
        &Fun(ref arg, ref body) if arg == argname => fun(&arg, Rc::clone(body)),
        &Fun(ref arg, ref body) => fun(&arg, replace(&body, argname, value)),
        &Var(ref arg) if arg == argname => Rc::clone(value),
        &Var(_) => Rc::clone(term),
    }
}

pub fn unparse(term: &Rc<Term>) -> String {
    match term.borrow() {
        &Var(ref x) => format!("{}", x),
        &App(ref f, ref x) => format!("{} ({})", unparse(f), unparse(x)),
        &Fun(ref x, ref body) => format!("({} -> {})", x, unparse(body)),
    }
}

pub fn unparse_statement(statement: &Statement) -> String {
    match statement {
        &Expr(ref x) => unparse(x),
        &Def {
            ref name,
            ref value,
        } => format!("{} = {}", name, unparse(value)),
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
    fn variable_with_digits_and_underscores() {
        assert_eq!(parse("v_a_r_1"), Ok(var("v_a_r_1")))
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
        assert_eq!(eval(&var("x")), var("x"));
    }

    #[test]
    fn apply_function() {
        assert_eq!(eval(&app(fun("x", var("x")), var("y"))), var("y"))
    }

    #[test]
    fn application_is_left_associative() {
        assert_eq!(parse("a b c"), Ok(app(app(var("a"), var("b")), var("c"))))
    }

    #[test]
    fn parens() {
        assert_eq!(parse("a (b c)"), Ok(app(var("a"), app(var("b"), var("c")))))
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
        assert_eq!(unparse(&app(fun("x", var("x")), var("y"))), "(x -> x) (y)")
    }

    #[test]
    fn unparsing_statements() {
        assert_eq!(unparse_statement(&Expr(fun("x", var("x")))), "(x -> x)");
        assert_eq!(
            unparse_statement(&Def {
                name: String::from("f"),
                value: fun("x", var("x")),
            }),
            "f = (x -> x)"
        );
    }

    #[test]
    fn evaluating_application_that_cannot_be_applied() {
        assert_eq!(eval(&app(var("f"), var("y"))), app(var("f"), var("y")))
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
            vec![app(var("f"), var("y")), fun("x", var("x")), var("y")]
        )
    }

    #[test]
    fn evaluating_nested_function() {
        assert_eq!(parse_eval("(x -> (y -> x)) a b"), var("a"))
    }

    #[test]
    fn recursive_application() {
        assert_eq!(parse_eval("(y -> (f -> f y)) res (x -> x)"), var("res"))
    }

    #[test]
    fn name_shadowing() {
        assert_eq!(parse_eval("(x -> (x -> x)) y z"), var("z"))
    }

    fn parse_eval(input: &str) -> Rc<Term> {
        eval(&parse(input).expect("Invalid test expression"))
    }
}
