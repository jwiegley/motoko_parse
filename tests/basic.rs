#![recursion_limit = "1024"]

use combine::parser::char::spaces;
use combine::parser::Parser;
#[cfg(test)]
use combine::EasyParser;
use motoko_parse::parse::*;
// use rug::Integer;

fn to_int(x: u128) -> int {
    // Integer::from(x)
    x
}

fn nat(x: u128) -> exp_bin {
    exp_bin::un(exp_un::post(exp_post::nullary(exp_nullary::plain(
        exp_plain::lit(lit::nat(to_int(x))),
    ))))
}

fn var(s: &str) -> exp_bin {
    exp_bin::un(exp_un::post(exp_post::nullary(exp_nullary::id(
        s.to_string(),
    ))))
}

#[test]
fn test_parse_id() {
    pretty_assertions::assert_eq!(
        Ok(("HelloWorld".to_string(), "GoodbyeWorld")),
        parse_id(State::new()).easy_parse("HelloWorld GoodbyeWorld"),
    );
}

#[test]
fn test_parse_bool() {
    pretty_assertions::assert_eq!(
        Ok((true, ", false")),
        parse_bool(State::new()).easy_parse("true, false"),
    );
    pretty_assertions::assert_eq!(
        Ok((false, ", true")),
        parse_bool(State::new()).easy_parse("false, true"),
    );
}

#[test]
fn test_parse_nat() {
    pretty_assertions::assert_eq!(
        Ok((to_int(123), "")),
        parse_nat(State::new()).easy_parse("123"),
    );
    pretty_assertions::assert_eq!(
        Ok((to_int(123), "")),
        parse_nat(State::new()).easy_parse("0123"),
    );
}

/*
#[test]
fn test_parse_float() {
    pretty_assertions::assert_eq!(
        Ok((Float::with_val(53, 1.3), "")),
        parse_float(State::new()).easy_parse("1.3"),
    );
}
*/

#[test]
fn test_braces() {
    pretty_assertions::assert_eq!(
        Ok((to_int(100), "")),
        braces(parse_nat(State::new()).skip(spaces())).easy_parse("{ 100 }"),
    );
}

#[test]
fn test_parens() {
    pretty_assertions::assert_eq!(
        Ok((to_int(100), "")),
        parens(parse_nat(State::new()).skip(spaces())).easy_parse("( 100 )"),
    );
}

#[test]
fn test_brackets() {
    pretty_assertions::assert_eq!(
        Ok((to_int(100), "")),
        brackets(parse_nat(State::new()).skip(spaces())).easy_parse("[ 100 ]"),
    );
}

#[test]
fn test_angles() {
    pretty_assertions::assert_eq!(
        Ok((to_int(100), "")),
        angles(parse_nat(State::new()).skip(spaces())).easy_parse("< 100 >"),
    );
}

#[test]
fn test_keyword() {
    pretty_assertions::assert_eq!(
        Ok((("foo", "bar"), "(13)")),
        keyword("foo").and(keyword("bar")).easy_parse("foo bar(13)"),
    );
}

#[test]
fn test_parse_var_opt() {
    pretty_assertions::assert_eq!(
        Ok(((true, to_int(100)), "")),
        parse_var_opt(parse_nat(State::new()).skip(spaces())).easy_parse("var 100"),
    );
    pretty_assertions::assert_eq!(
        Ok(((false, to_int(100)), "")),
        parse_var_opt(parse_nat(State::new()).skip(spaces())).easy_parse("100"),
    );
}

#[test]
fn test_parse_obj_sort() {
    pretty_assertions::assert_eq!(
        Ok((obj_sort::object, "hello")),
        parse_obj_sort(State::new()).easy_parse("object hello"),
    );
}

#[test]
fn test_func_sort_opt() {
    pretty_assertions::assert_eq!(
        Ok((func_sort_opt::shared(false), "hello")),
        parse_func_sort_opt(State::new()).easy_parse("shared hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((func_sort_opt::shared(true), "hello")),
        parse_func_sort_opt(State::new()).easy_parse("shared query hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((func_sort_opt::query, "hello")),
        parse_func_sort_opt(State::new()).easy_parse("query hello"),
    );
}

#[test]
fn test_shared_pat_opt() {
    pretty_assertions::assert_eq!(
        Ok((
            shared_pat_opt::shared(false, Some(pat_plain::underscore)),
            "hello"
        )),
        parse_shared_pat_opt(State::new()).easy_parse("shared _ hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((shared_pat_opt::shared(false, None), "{} hello")),
        parse_shared_pat_opt(State::new()).easy_parse("shared{} hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((
            shared_pat_opt::shared(true, Some(pat_plain::underscore)),
            "hello"
        )),
        parse_shared_pat_opt(State::new()).easy_parse("shared query _ hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((shared_pat_opt::shared(true, None), "{} hello")),
        parse_shared_pat_opt(State::new()).easy_parse("shared query{} hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((shared_pat_opt::query(Some(pat_plain::underscore)), "hello")),
        parse_shared_pat_opt(State::new()).easy_parse("query _ hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((shared_pat_opt::query(None), "{} hello")),
        parse_shared_pat_opt(State::new()).easy_parse("query{} hello"),
    );
}

#[test]
fn test_parse_lit() {
    pretty_assertions::assert_eq!(
        Ok((lit::null, "hello")),
        parse_lit(State::new()).easy_parse("null hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((lit::bool_(true), "hello")),
        parse_lit(State::new()).easy_parse("true hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((lit::nat(to_int(123)), "hello")),
        parse_lit(State::new()).easy_parse("123 hello"),
    );
    // jww (2021-11-20): Not working yet
    // pretty_assertions::assert_eq!(
    //     Ok((lit::float(Float::with_val(53, 1.3)), "hello")),
    //     parse_lit(State::new()).easy_parse("1.3 hello"),
    // );
    pretty_assertions::assert_eq!(
        Ok((lit::char_('c'), "hello")),
        parse_lit(State::new()).easy_parse("'c' hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((lit::text("string".to_string()), "hello")),
        parse_lit(State::new()).easy_parse("\"string\" hello"),
    );
}

#[test]
fn test_parse_exp_nondec() {
    pretty_assertions::assert_eq!(
        Ok((
            exp_nondec::return_(Some(exp::exp_nonvar(Box::new(exp_nonvar::exp_nondec(
                exp_nondec::bin(exp_bin::un(exp_un::post(exp_post::nullary(
                    exp_nullary::plain(exp_plain::exp(vec![exp::exp_nonvar(Box::new(
                        exp_nonvar::exp_nondec(exp_nondec::bin(exp_bin::binop(
                            Box::new(var("name")),
                            binop::add,
                            Box::new(nat(20)),
                        )))
                    ))]))
                ))))
            ))))),
            "",
        )),
        parse_exp_nondec(State::new()).easy_parse("return (name + 20)"),
    );
}

#[test]
fn test_parse_exp() {
    pretty_assertions::assert_eq!(
        Ok((
            exp::exp_nonvar(Box::new(exp_nonvar::exp_nondec(exp_nondec::bin(nat(1))))),
            "",
        )),
        parse_exp(State::new()).easy_parse("1"),
    );
    pretty_assertions::assert_eq!(
        Ok((
            exp::exp_nonvar(Box::new(exp_nonvar::exp_nondec(exp_nondec::bin(
                exp_bin::binop(Box::new(nat(1)), binop::add, Box::new(nat(3)))
            )))),
            ""
        )),
        parse_exp(State::new()).easy_parse("1 + 3"),
    );
    pretty_assertions::assert_eq!(
        Ok((
            exp::exp_nonvar(Box::new(exp_nonvar::exp_nondec(exp_nondec::bin(
                exp_bin::binop(Box::new(var("name")), binop::add, Box::new(nat(3)))
            )))),
            ""
        )),
        parse_exp(State::new()).easy_parse("name + 3"),
    );
    pretty_assertions::assert_eq!(
        Ok((
            exp::exp_nonvar(Box::new(exp_nonvar::exp_nondec(exp_nondec::bin(
                exp_bin::binop(
                    Box::new(exp_bin::un(exp_un::post(exp_post::nullary(
                        exp_nullary::plain(exp_plain::exp(vec![exp::exp_nonvar(Box::new(
                            exp_nonvar::exp_nondec(exp_nondec::bin(exp_bin::binop(
                                Box::new(nat(1)),
                                binop::add,
                                Box::new(nat(3)),
                            )))
                        ))]))
                    )))),
                    binop::mul,
                    Box::new(exp_bin::relop(
                        Box::new(nat(4)),
                        relop::eq,
                        Box::new(exp_bin::binop(
                            Box::new(nat(72)),
                            binop::div,
                            Box::new(nat(2))
                        ))
                    ))
                )
            )))),
            "",
        )),
        parse_exp(State::new()).easy_parse("(1 + 3) * 4 == 72 / 2"),
    );
}

#[test]
fn test_parse_dec_nonvar() {
    pretty_assertions::assert_eq!(
        Ok((
            dec_nonvar::func {
                shared_pat: None,
                id: None,
                binds: None,
                plain: pat_plain::bins(vec![pat_bin::colon(
                    Box::new(pat_bin::un(pat_un::nullary(pat_nullary::plain(
                        pat_plain::id("name".to_string())
                    )))),
                    typ::nobin(Box::new(typ_nobin::pre(typ_pre::un(typ_un::nullary(
                        typ_nullary::dot(vec!["nat".to_string()], None)
                    ))))),
                )]),
                typ: Some(typ::nobin(Box::new(typ_nobin::pre(typ_pre::un(
                    typ_un::question(Box::new(typ_un::nullary(typ_nullary::dot(
                        vec!["nat".to_string()],
                        None
                    ))))
                ))))),
                body: func_body::block(block {
                    decs: vec![dec::nondec(exp_nondec::return_(Some(exp::exp_nonvar(
                        Box::new(exp_nonvar::exp_nondec(exp_nondec::bin(exp_bin::un(
                            exp_un::post(exp_post::nullary(exp_nullary::plain(exp_plain::exp(
                                vec![exp::exp_nonvar(Box::new(exp_nonvar::exp_nondec(
                                    exp_nondec::bin(exp_bin::binop(
                                        Box::new(var("name")),
                                        binop::add,
                                        Box::new(nat(20)),
                                    ))
                                )))]
                            )))),
                        ))))
                    ))))],
                }),
            },
            "",
        )),
        parse_dec_nonvar(State::new()).easy_parse("func(name: nat): ?nat { return (name + 20); }"),
    );
}
