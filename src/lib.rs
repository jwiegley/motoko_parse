#![recursion_limit = "1024"]
#![allow(non_camel_case_types)]

use combine::parser::char::{char, spaces, string};
use combine::parser::combinator::Either;
#[cfg(test)]
use combine::EasyParser;
use combine::{
    attempt, between, choice, easy, many, many1, optional, parser, satisfy, sep_by, sep_by1,
    sep_end_by, sep_end_by1, struct_parser, unexpected_any, value, Parser,
};
use rug::{Float, Integer};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct State {
    pub depth: usize,
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    pub fn new() -> Self {
        State { depth: 0 }
    }

    pub fn indent(&self) {
        print!("{: <1$}", "", self.depth * 2);
    }

    pub fn incr(&self) -> Self {
        State {
            depth: self.depth + 1,
        }
    }
}

pub type id = String;
pub type nat = Integer;
pub type float = Float;
pub type text = String;

fn is_alphabetic(c: char) -> bool {
    c.is_alphabetic()
}

fn is_alphanumeric(c: char) -> bool {
    c.is_alphanumeric()
}

fn cons((x, xs): (char, Vec<char>)) -> String {
    vec![x].into_iter().chain(xs.into_iter()).collect()
}

pub fn parse_id_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = id> {
    st.indent();
    println!("parse_id");
    satisfy(is_alphabetic)
        .and(many(satisfy(is_alphanumeric)))
        .map(cons)
}

parser! {
    pub fn parse_id['a](st: State)(easy::Stream<&'a str>) -> id
    where []
    {
        parse_id_(*st)
    }
}

#[test]
fn test_parse_id() {
    pretty_assertions::assert_eq!(
        Ok(("HelloWorld".to_string(), " GoodbyeWorld")),
        parse_id(State::new()).easy_parse("HelloWorld GoodbyeWorld"),
    );
}

pub fn parse_bool_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = bool> {
    st.indent();
    println!("parse_bool");
    keyword("true")
        .with(value(true))
        .or(keyword("false").with(value(false)))
}

parser! {
    pub fn parse_bool['a](st: State)(easy::Stream<&'a str>) -> bool
    where []
    {
        parse_bool_(*st)
    }
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

fn is_numeric(c: char) -> bool {
    c.is_numeric()
}

fn parse_integer<'a>(xs: Vec<char>) -> impl Parser<easy::Stream<&'a str>, Output = Integer> {
    match Integer::parse(xs.into_iter().collect::<String>()).map(Integer::from) {
        Ok(int) => value(int).left(),
        Err(_err) => unexpected_any("nat").right(),
    }
}

pub fn parse_nat_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = nat> {
    st.indent();
    println!("parse_nat");
    many(satisfy(is_numeric)).skip(spaces()).then(parse_integer)
}

parser! {
    pub fn parse_nat['a](st: State)(easy::Stream<&'a str>) -> nat
    where []
    {
        parse_nat_(*st)
    }
}

#[test]
fn test_parse_nat() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(123), "")),
        parse_nat(State::new()).easy_parse("123"),
    );
    pretty_assertions::assert_eq!(
        Ok((Integer::from(123), "")),
        parse_nat(State::new()).easy_parse("0123"),
    );
}

// fn parse_float_match<'a, F: Iterator<Item = Input::Range>>(x: F) -> Float {
//     Float::with_val(
//         53,
//         Float::parse(x[0].into_iter().collect()).expect("failed to parse
// floating point"),     )
// }

// jww (2021-11-20): Finish this
fn parse_rug_float<'a>(
    (xs, _ys): (Vec<char>, Vec<char>),
) -> impl Parser<easy::Stream<&'a str>, Output = Float> {
    fn with_53<T>(v: T) -> Float
    where
        Float: rug::Assign<T>,
    {
        Float::with_val(53, v)
    }
    match Float::parse(xs.into_iter().collect::<String>()).map(with_53) {
        Ok(f) => value(f).left(),
        Err(_err) => unexpected_any("float").right(),
    }
}

pub fn parse_float_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = float> {
    st.indent();
    println!("parse_float");
    many(satisfy(is_numeric))
        .skip(char('.'))
        .and(many1(satisfy(is_numeric)))
        .skip(spaces())
        .then(parse_rug_float)
}

parser! {
    pub fn parse_float['a](st: State)(easy::Stream<&'a str>) -> float
    where []
    {
        parse_float_(*st)
    }
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

fn not_quote(c: char) -> bool {
    c != '\''
}

// jww (2021-11-18): What is the grammer here?
pub fn parse_char_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = char> {
    st.indent();
    println!("parse_char");
    char('\'')
        .with(satisfy(not_quote))
        .skip(char('\'').skip(spaces()))
}

parser! {
    pub fn parse_char['a](st: State)(easy::Stream<&'a str>) -> char
    where []
    {
        parse_char_(*st)
    }
}

fn not_double_quote(c: char) -> bool {
    c != '"'
}

// jww (2021-11-18): What is the grammer here?
pub fn parse_text_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = text> {
    st.indent();
    println!("parse_text");
    char('"')
        .with(many(satisfy(not_double_quote)))
        .skip(char('"'))
        .skip(spaces())
}

parser! {
    pub fn parse_text['a](st: State)(easy::Stream<&'a str>) -> text
    where []
    {
        parse_text_(*st)
    }
}

pub fn braces<'a, T>(
    p: impl Parser<easy::Stream<&'a str>, Output = T>,
) -> impl Parser<easy::Stream<&'a str>, Output = T> {
    between(symbol("{"), symbol("}"), p)
}

#[test]
fn test_braces() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(100), "")),
        braces(parse_nat(State::new()).skip(spaces())).easy_parse("{ 100 }"),
    );
}

pub fn parens<'a, T>(
    p: impl Parser<easy::Stream<&'a str>, Output = T>,
) -> impl Parser<easy::Stream<&'a str>, Output = T> {
    between(symbol("("), symbol(")"), p)
}

#[test]
fn test_parens() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(100), "")),
        parens(parse_nat(State::new()).skip(spaces())).easy_parse("( 100 )"),
    );
}

pub fn brackets<'a, T>(
    p: impl Parser<easy::Stream<&'a str>, Output = T>,
) -> impl Parser<easy::Stream<&'a str>, Output = T> {
    between(symbol("["), symbol("]"), p)
}

#[test]
fn test_brackets() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(100), "")),
        brackets(parse_nat(State::new()).skip(spaces())).easy_parse("[ 100 ]"),
    );
}

pub fn angles<'a, T>(
    p: impl Parser<easy::Stream<&'a str>, Output = T>,
) -> impl Parser<easy::Stream<&'a str>, Output = T> {
    between(symbol("<"), symbol(">"), p)
}

#[test]
fn test_angles() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(100), "")),
        angles(parse_nat(State::new()).skip(spaces())).easy_parse("< 100 >"),
    );
}

pub fn keyword<'a>(s: &'static str) -> impl Parser<easy::Stream<&'a str>, Output = &str> {
    attempt(string(s)).skip(spaces())
}

#[test]
fn test_keyword() {
    pretty_assertions::assert_eq!(
        Ok((("foo", "bar"), "(13)")),
        keyword("foo").and(keyword("bar")).easy_parse("foo bar(13)"),
    );
}

pub fn symbol<'a>(s: &'static str) -> impl Parser<easy::Stream<&'a str>, Output = &str> {
    attempt(string(s)).skip(spaces())
}

pub fn operator<'a>(s: &'static str) -> impl Parser<easy::Stream<&'a str>, Output = &str> {
    attempt(string(s)).skip(spaces())
}

fn first_is_some<U, V>((x, y): (Option<U>, V)) -> (bool, V) {
    (x.is_some(), y)
}

pub fn parse_var_opt<'a, T>(
    p: impl Parser<easy::Stream<&'a str>, Output = T>,
) -> impl Parser<easy::Stream<&'a str>, Output = (bool, T)> {
    optional(keyword("var")).and(p).map(first_is_some)
}

#[test]
fn test_parse_var_opt() {
    pretty_assertions::assert_eq!(
        Ok(((true, Integer::from(100)), "")),
        parse_var_opt(parse_nat(State::new()).skip(spaces())).easy_parse("var 100"),
    );
    pretty_assertions::assert_eq!(
        Ok(((false, Integer::from(100)), "")),
        parse_var_opt(parse_nat(State::new()).skip(spaces())).easy_parse("100"),
    );
}

/*
<list(X, SEP)> ::=
    <empty>
    X
    X SEP <list(X, SEP)>
*/

// This is parsed using sep_by
pub type list<T> = Vec<T>;

/*
<list1(X, SEP)> ::=
    X
    X SEP <list(X, SEP)>
*/

// This doesn't guarantee that it's not always empty, but it's way more
// convenient to work with.
//
// This is parsed using sep_by1
pub type list1<T> = Vec<T>;

/*
<obj_sort> ::=
    'object'
    'actor'
    'module'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum obj_sort {
    object,
    actor,
    module,
}

pub fn parse_obj_sort_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = obj_sort> {
    st.indent();
    println!("parse_obj_sort");
    choice([
        attempt(keyword("object")).with(value(obj_sort::object)),
        attempt(keyword("actor")).with(value(obj_sort::actor)),
        attempt(keyword("module")).with(value(obj_sort::module)),
    ])
}

parser! {
    pub fn parse_obj_sort['a](st: State)(easy::Stream<&'a str>) -> obj_sort
    where []
    {
        parse_obj_sort_(*st)
    }
}

#[test]
fn test_parse_obj_sort() {
    pretty_assertions::assert_eq!(
        Ok((obj_sort::object, "hello")),
        parse_obj_sort(State::new()).easy_parse("object hello"),
    );
}

/*
<func_sort_opt> ::=
    <empty>
    'shared' 'query'?
    'query'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum func_sort_opt {
    shared(bool),
    query,
}

impl func_sort_opt {
    fn func_sort_opt_shared<U>(q: Option<U>) -> Self {
        func_sort_opt::shared(q.is_some())
    }
}

pub fn parse_func_sort_opt_<'a>(
    st: State,
) -> impl Parser<easy::Stream<&'a str>, Output = func_sort_opt> {
    st.indent();
    println!("parse_func_sort_opt");
    keyword("shared")
        .with(optional(keyword("query")))
        .map(func_sort_opt::func_sort_opt_shared)
        .or(keyword("query").with(value(func_sort_opt::query)))
}

parser! {
    pub fn parse_func_sort_opt['a](st: State)(easy::Stream<&'a str>) -> func_sort_opt
    where []
    {
        parse_func_sort_opt_(*st)
    }
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

/*
 * TYPES
 */

/*
<shared_pat_opt> ::=
    <empty>
    'shared' 'query'? <pat_plain>?
    'query' <pat_plain>?
*/

#[derive(Clone, Debug, PartialEq)]
pub enum shared_pat_opt {
    shared(bool, Option<pat_plain>),
    query(Option<pat_plain>),
}

impl shared_pat_opt {
    fn shared_pat_opt_shared<U>((q, p): (Option<U>, Option<pat_plain>)) -> Self {
        shared_pat_opt::shared(q.is_some(), p)
    }
}

pub fn parse_shared_pat_opt_<'a>(
    st: State,
) -> impl Parser<easy::Stream<&'a str>, Output = shared_pat_opt> {
    st.indent();
    println!("parse_shared_pat_opt");
    keyword("shared")
        .with(optional(keyword("query")))
        .and(optional(parse_pat_plain(st.incr())))
        .map(shared_pat_opt::shared_pat_opt_shared)
        .or(keyword("query")
            .with(optional(parse_pat_plain(st.incr())))
            .map(shared_pat_opt::query))
}

parser! {
    pub fn parse_shared_pat_opt['a](st: State)(easy::Stream<&'a str>) -> shared_pat_opt
    where []
    {
        parse_shared_pat_opt_(*st)
    }
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

/*
<typ_obj> ::=
    '{' <list(<typ_field>, ';')> '}'
*/

#[derive(Clone, Debug, PartialEq)]
pub struct typ_obj {
    pub fields: list<typ_field>,
}

impl typ_obj {
    fn new(fields: list<typ_field>) -> typ_obj {
        typ_obj { fields }
    }
}

pub fn parse_typ_obj_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ_obj> {
    st.indent();
    println!("parse_typ_obj");
    sep_end_by(parse_typ_field(st.incr()), symbol(";")).map(typ_obj::new)
}

parser! {
    pub fn parse_typ_obj['a](st: State)(easy::Stream<&'a str>) -> typ_obj
    where []
    {
        parse_typ_obj_(*st)
    }
}

/*
<typ_variant> ::=
    '{' '#' '}'
    '{' <list1(<typ_tag>, ';')> '}'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum typ_variant {
    hash,
    variants(list1<typ_tag>),
}

pub fn parse_typ_variant_<'a>(
    st: State,
) -> impl Parser<easy::Stream<&'a str>, Output = typ_variant> {
    st.indent();
    println!("parse_typ_variant");
    braces(
        symbol("#").with(value(typ_variant::hash)).or(sep_end_by1(
            parse_typ_tag(st.incr()),
            symbol(";"),
        )
        .map(typ_variant::variants)),
    )
}

parser! {
    pub fn parse_typ_variant['a](st: State)(easy::Stream<&'a str>) -> typ_variant
    where []
    {
        parse_typ_variant_(*st)
    }
}

/*
<typ_nullary> ::=
    '(' <list(<typ_item>, ',')> ')'
    <id> ('.' <id>)* <typ_args>?
    '[' 'var'? <typ> ']'
    <typ_obj>
    <typ_variant>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum typ_nullary {
    list(list<typ_item>),
    dot(list1<id>, Option<typ_args>),
    bracket(bool, typ),
    obj(typ_obj),
    variant(typ_variant),
}

impl typ_nullary {
    fn typ_nullary_dot((ids, args): (list1<id>, Option<typ_args>)) -> Self {
        typ_nullary::dot(ids, args)
    }

    fn typ_nullary_bracket((var, typ): (bool, typ)) -> Self {
        typ_nullary::bracket(var, typ)
    }
}

pub fn parse_typ_nullary_<'a>(
    st: State,
) -> impl Parser<easy::Stream<&'a str>, Output = typ_nullary> {
    st.indent();
    println!("parse_typ_nullary");
    parens(sep_by(parse_typ_item(st.incr()), symbol(",")))
        .map(typ_nullary::list)
        .or(brackets(parse_var_opt(parse_typ(st.incr()))).map(typ_nullary::typ_nullary_bracket))
        .or(attempt(parse_typ_obj(st.incr())).map(typ_nullary::obj))
        .or(attempt(parse_typ_variant(st.incr())).map(typ_nullary::variant))
        .or(sep_by1(parse_id(st.incr()), symbol("."))
            .and(optional(parse_typ_args(st.incr())))
            .map(typ_nullary::typ_nullary_dot))
}

parser! {
    pub fn parse_typ_nullary['a](st: State)(easy::Stream<&'a str>) -> typ_nullary
    where []
    {
        parse_typ_nullary_(*st)
    }
}

/*
<typ_un> ::=
    <typ_nullary>
    '?' <typ_un>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum typ_un {
    nullary(typ_nullary),
    question(Box<typ_un>),
}

impl typ_un {
    fn typ_un_question(u: typ_un) -> Self {
        typ_un::question(Box::new(u))
    }
}

pub fn parse_typ_un_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ_un> {
    st.indent();
    println!("parse_typ_un");
    symbol("?")
        .with(parse_typ_un(st.incr()))
        .map(typ_un::typ_un_question)
        .or(parse_typ_nullary(st.incr()).map(typ_un::nullary))
}

parser! {
    pub fn parse_typ_un['a](st: State)(easy::Stream<&'a str>) -> typ_un
    where []
    {
        parse_typ_un_(*st)
    }
}

/*
<typ_pre> ::=
    <typ_un>
    'async' <typ_pre>
    <obj_sort> <typ_obj>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum typ_pre {
    un(typ_un),
    async_(Box<typ_pre>),
    obj(obj_sort, typ_obj),
}

impl typ_pre {
    fn typ_pre_async_(t: typ_pre) -> Self {
        typ_pre::async_(Box::new(t))
    }

    fn typ_pre_obj((o, t): (obj_sort, typ_obj)) -> Self {
        typ_pre::obj(o, t)
    }
}

pub fn parse_typ_pre_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ_pre> {
    st.indent();
    println!("parse_typ_pre");
    parse_typ_un(st.incr())
        .map(typ_pre::un)
        .or(keyword("async")
            .with(parse_typ_pre(st.incr()))
            .map(typ_pre::typ_pre_async_))
        .or(parse_obj_sort(st.incr())
            .and(parse_typ_obj(st.incr()))
            .map(typ_pre::typ_pre_obj))
}

parser! {
    pub fn parse_typ_pre['a](st: State)(easy::Stream<&'a str>) -> typ_pre
    where []
    {
        parse_typ_pre_(*st)
    }
}

/*
<typ_nobin> ::=
    <typ_pre>
    <func_sort_opt> ('<' <list(<typ_bind>, ',')> '>')? <typ_un> '->' <typ_nobin>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum typ_nobin {
    pre(typ_pre),
    func {
        sort: Option<func_sort_opt>,
        binds: Option<list<typ_bind>>,
        typ: typ_un,
        ret: Box<typ_nobin>,
    },
}

impl typ_nobin {
    fn typ_nobin_func(
        (((sort, binds), typ), ret): (
            ((Option<func_sort_opt>, Option<list<typ_bind>>), typ_un),
            typ_nobin,
        ),
    ) -> Self {
        typ_nobin::func {
            sort,
            binds,
            typ,
            ret: Box::new(ret),
        }
    }
}

pub fn parse_typ_nobin_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ_nobin> {
    st.indent();
    println!("parse_typ_nobin");
    parse_typ_pre(st.incr())
        .map(typ_nobin::pre)
        .or(optional(parse_func_sort_opt(st.incr()))
            .and(optional(angles(sep_by(
                parse_typ_bind(st.incr()),
                symbol(","),
            ))))
            .and(parse_typ_un(st.incr()))
            .skip(keyword("->"))
            .and(parse_typ_nobin(st.incr()))
            .map(typ_nobin::typ_nobin_func))
}

parser! {
    pub fn parse_typ_nobin['a](st: State)(easy::Stream<&'a str>) -> typ_nobin
    where []
    {
        parse_typ_nobin_(*st)
    }
}

/*
<typ> ::=
    <typ_nobin>
    <typ> 'and' <typ>
    <typ> 'or' <typ>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum typ {
    nobin(Box<typ_nobin>),
    and(Box<typ_nobin>, Box<typ>),
    or(Box<typ_nobin>, Box<typ>),
}

pub fn parse_typ_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ> {
    st.indent();
    println!("parse_typ");
    fn and_or<'a>((l, r_opt): (typ_nobin, Option<(bool, typ)>)) -> typ {
        match r_opt {
            Some((true, r)) => typ::or(Box::new(l), Box::new(r)),
            Some((false, r)) => typ::or(Box::new(l), Box::new(r)),
            None => typ::nobin(Box::new(l)),
        }
    }
    parse_typ_nobin(st.incr())
        .and(optional(
            keyword("and")
                .with(value(true))
                .or(keyword("or").with(value(false)))
                .and(parse_typ(st.incr())),
        ))
        .skip(spaces())
        .map(and_or)
}

parser! {
    pub fn parse_typ['a](st: State)(easy::Stream<&'a str>) -> typ
    where []
    {
        parse_typ_(*st)
    }
}

/*
<typ_item> ::=
    <id> ':' <typ>
    <typ>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum typ_item {
    colon(id, typ),
    typ(typ),
}

impl typ_item {
    fn typ_item_colon((id, typ): (id, typ)) -> Self {
        typ_item::colon(id, typ)
    }
}

pub fn parse_typ_item_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ_item> {
    st.indent();
    println!("parse_typ_item");
    parse_id(st.incr())
        .skip(symbol(":"))
        .and(parse_typ(st.incr()))
        .map(typ_item::typ_item_colon)
        .or(parse_typ(st.incr()).map(typ_item::typ))
}

parser! {
    pub fn parse_typ_item['a](st: State)(easy::Stream<&'a str>) -> typ_item
    where []
    {
        parse_typ_item_(*st)
    }
}

/*
<typ_args> ::=
    '<' <list(<typ>, ',')> '>'

*/

#[derive(Clone, Debug, PartialEq)]
pub struct typ_args {
    pub args: list<typ>,
}

impl typ_args {
    fn new(args: list<typ>) -> Self {
        typ_args { args }
    }
}

pub fn parse_typ_args_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ_args> {
    st.indent();
    println!("parse_typ_args");
    angles(sep_by(parse_typ(st.incr()), symbol(","))).map(typ_args::new)
}

parser! {
    pub fn parse_typ_args['a](st: State)(easy::Stream<&'a str>) -> typ_args
    where []
    {
        parse_typ_args_(*st)
    }
}

/*
<typ_field> ::=
    'var'? <id> ':' <typ>
    <id> ('<' <list(<typ_bind>, ',')> '>')? <typ_nullary> ':' <typ>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum typ_field {
    colon(bool, id, typ),
    field {
        id: id,
        binds: Option<list<typ_bind>>,
        nullary: typ_nullary,
        typ: typ,
    },
}

impl typ_field {
    fn typ_field_colon(((v, id), t): ((bool, id), typ)) -> Self {
        typ_field::colon(v, id, t)
    }

    fn typ_field_field(
        (((id, binds), nullary), typ): (((id, Option<list<typ_bind>>), typ_nullary), typ),
    ) -> Self {
        typ_field::field {
            id,
            binds,
            nullary,
            typ,
        }
    }
}

pub fn parse_typ_field_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ_field> {
    st.indent();
    println!("parse_typ_field");
    parse_var_opt(parse_id(st.incr()))
        .skip(symbol(":"))
        .and(parse_typ(st.incr()))
        .map(typ_field::typ_field_colon)
        .or(parse_id(st.incr())
            .and(optional(angles(sep_by(
                parse_typ_bind(st.incr()),
                symbol(","),
            ))))
            .and(parse_typ_nullary(st.incr()))
            .skip(symbol(":"))
            .and(parse_typ(st.incr()))
            .map(typ_field::typ_field_field))
}

parser! {
    pub fn parse_typ_field['a](st: State)(easy::Stream<&'a str>) -> typ_field
    where []
    {
        parse_typ_field_(*st)
    }
}

/*
<typ_tag> ::=
    '#' <id> (':' <typ>)?

*/

#[derive(Clone, Debug, PartialEq)]
pub struct typ_tag {
    pub tag: id,
    pub typ: Option<Box<typ>>,
}

impl typ_tag {
    fn new((tag, typ): (id, Option<typ>)) -> Self {
        typ_tag {
            tag,
            typ: typ.map(Box::new),
        }
    }
}

pub fn parse_typ_tag_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ_tag> {
    st.indent();
    println!("parse_typ_tag");
    symbol("#")
        .with(parse_id(st.incr()))
        .and(optional(symbol(":").with(parse_typ(st.incr()))))
        .map(typ_tag::new)
}

parser! {
    pub fn parse_typ_tag['a](st: State)(easy::Stream<&'a str>) -> typ_tag
    where []
    {
        parse_typ_tag_(*st)
    }
}

/*
<typ_bind> ::=
    <id> '<:' <typ>
    <id>
*/

#[derive(Clone, Debug, PartialEq)]
pub struct typ_bind {
    pub id: id,
    pub typ: Option<typ>,
}

pub fn parse_typ_bind_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = typ_bind> {
    st.indent();
    println!("parse_typ_bind");
    struct_parser! {
        typ_bind {
            id: parse_id(st.incr()),
            typ: optional(keyword("<:").with(parse_typ(st.incr())))
        }
    }
}

parser! {
    pub fn parse_typ_bind['a](st: State)(easy::Stream<&'a str>) -> typ_bind
    where []
    {
        parse_typ_bind_(*st)
    }
}

/*
 * LITERALS
 */

/*
<lit> ::=
    'null'
    <bool>
    <nat>
    <float>
    <char>
    <text>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum lit {
    null,
    bool_(bool),
    nat(nat),
    float(float),
    char_(char),
    text(text),
}

pub fn parse_lit_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = lit> {
    st.indent();
    println!("parse_lit");
    keyword("null")
        .with(value(lit::null))
        .or(parse_bool(st.incr()).map(lit::bool_))
        .or(attempt(parse_float(st.incr())).map(lit::float))
        .or(parse_nat(st.incr()).map(lit::nat))
        .or(parse_char(st.incr()).map(lit::char_))
        .or(parse_text(st.incr()).map(lit::text))
}

parser! {
    pub fn parse_lit['a](st: State)(easy::Stream<&'a str>) -> lit
    where []
    {
        parse_lit_(*st)
    }
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
        Ok((lit::nat(Integer::from(123)), "hello")),
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

/*
 * OPERATORS
 */

/*
<unop> ::=
    '+'
    '-'
    '^'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum unop {
    plus,
    minus,
    caret,
}

pub fn parse_unop_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = unop> {
    st.indent();
    println!("parse_unop");
    choice([
        attempt(operator("+")).with(value(unop::plus)),
        attempt(operator("-")).with(value(unop::minus)),
        attempt(operator("^")).with(value(unop::caret)),
    ])
}

parser! {
    pub fn parse_unop['a](st: State)(easy::Stream<&'a str>) -> unop
    where []
    {
        parse_unop_(*st)
    }
}

/*
<binop> ::=
    '+'
    '-'
    '*'
    '/'
    '%'
    '**'
    '+%'
    '-%'
    '*%'
    '**%'
    '&'
    '|'
    '^'
    '<<'
    ' >>'
    '<<>'
    '<>>'
    '#'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum binop {
    add,
    sub,
    mul,
    div,
    modulo,
    exp,
    add_mod,
    sub_mod,
    mul_mod,
    exp_mod,
    and,
    or,
    xor,
    shl,
    shr,
    rol,
    ror,
    cat,
}

pub fn parse_binop_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = binop> {
    st.indent();
    println!("parse_binop");
    choice([
        attempt(operator("+")).with(value(binop::add)),
        attempt(operator("-")).with(value(binop::sub)),
        attempt(operator("*")).with(value(binop::mul)),
        attempt(operator("/")).with(value(binop::div)),
        attempt(operator("%")).with(value(binop::modulo)),
        attempt(operator("**")).with(value(binop::exp)),
        attempt(operator("+%")).with(value(binop::add_mod)),
        attempt(operator("-%")).with(value(binop::sub_mod)),
        attempt(operator("*%")).with(value(binop::mul_mod)),
        attempt(operator("**%")).with(value(binop::exp_mod)),
        attempt(operator("&")).with(value(binop::and)),
        attempt(operator("|")).with(value(binop::or)),
        attempt(operator("^")).with(value(binop::xor)),
        attempt(operator("<<")).with(value(binop::shl)),
        attempt(operator(">>")).with(value(binop::shr)),
        attempt(operator("<<>")).with(value(binop::rol)),
        attempt(operator("<>>")).with(value(binop::ror)),
        attempt(operator("#")).with(value(binop::cat)),
    ])
}

parser! {
    pub fn parse_binop['a](st: State)(easy::Stream<&'a str>) -> binop
    where []
    {
        parse_binop_(*st)
    }
}

/*
<relop> ::=
    '=='
    '!='
    ' < '
    '<='
    ' > '
    '>='
*/

#[derive(Clone, Debug, PartialEq)]
pub enum relop {
    eq,
    ne,
    lt,
    le,
    gt,
    ge,
}

pub fn parse_relop_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = relop> {
    st.indent();
    println!("parse_relop");
    choice([
        attempt(operator("==")).with(value(relop::eq)),
        attempt(operator("!=")).with(value(relop::ne)),
        attempt(operator("<")).with(value(relop::lt)),
        attempt(operator("<=")).with(value(relop::le)),
        attempt(operator(">")).with(value(relop::gt)),
        attempt(operator(">=")).with(value(relop::ge)),
    ])
}

parser! {
    pub fn parse_relop['a](st: State)(easy::Stream<&'a str>) -> relop
    where []
    {
        parse_relop_(*st)
    }
}

/*
<unassign> ::=
    '+='
    '-='
    '^='
*/

#[derive(Clone, Debug, PartialEq)]
pub enum unassign {
    add,
    sub,
    caret,
}

pub fn parse_unassign_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = unassign> {
    st.indent();
    println!("parse_unassign");
    choice([
        attempt(operator("+=")).with(value(unassign::add)),
        attempt(operator("-=")).with(value(unassign::sub)),
        attempt(operator("^=")).with(value(unassign::caret)),
    ])
}

parser! {
    pub fn parse_unassign['a](st: State)(easy::Stream<&'a str>) -> unassign
    where []
    {
        parse_unassign_(*st)
    }
}

/*
<binassign> ::=
    '+='
    '-='
    '*='
    '/='
    '%='
    '**-'
    '+%='
    '-%='
    '*%='
    '**%='
    '&='
    '|='
    '^='
    '<<='
    '>>='
    '<<>='
    '<>>='
    '@='
*/

#[derive(Clone, Debug, PartialEq)]
pub enum binassign {
    add,
    sub,
    mul,
    div,
    modulo,
    exp,
    add_mod,
    sub_mod,
    mul_mod,
    exp_mod,
    and,
    or,
    xor,
    shl,
    shr,
    rol,
    ror,
    at,
}

pub fn parse_binassign_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = binassign> {
    st.indent();
    println!("parse_binassign");
    choice([
        attempt(operator("+=")).with(value(binassign::add)),
        attempt(operator("-=")).with(value(binassign::sub)),
        attempt(operator("*=")).with(value(binassign::mul)),
        attempt(operator("/=")).with(value(binassign::div)),
        attempt(operator("%=")).with(value(binassign::modulo)),
        attempt(operator("**-")).with(value(binassign::exp)),
        attempt(operator("+%=")).with(value(binassign::add_mod)),
        attempt(operator("-%=")).with(value(binassign::sub_mod)),
        attempt(operator("*%=")).with(value(binassign::mul_mod)),
        attempt(operator("**%=")).with(value(binassign::exp_mod)),
        attempt(operator("&=")).with(value(binassign::and)),
        attempt(operator("|=")).with(value(binassign::or)),
        attempt(operator("^=")).with(value(binassign::xor)),
        attempt(operator("<<=")).with(value(binassign::shl)),
        attempt(operator(">>=")).with(value(binassign::shr)),
        attempt(operator("<<>=")).with(value(binassign::rol)),
        attempt(operator("<>>=")).with(value(binassign::ror)),
        attempt(operator("@=")).with(value(binassign::at)),
    ])
}

parser! {
    pub fn parse_binassign['a](st: State)(easy::Stream<&'a str>) -> binassign
    where []
    {
        parse_binassign_(*st)
    }
}

/*
 * EXPRESSIONS
 */

/*
<exp_obj> ::=
    '{' <list(<exp_field>, ';')> '}'

*/

#[derive(Clone, Debug, PartialEq)]
pub struct exp_obj {
    pub fields: list<exp_field>,
}

impl exp_obj {
    fn new(fields: list<exp_field>) -> Self {
        exp_obj { fields }
    }
}

pub fn parse_exp_obj_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp_obj> {
    st.indent();
    println!("parse_exp_obj");
    braces(sep_end_by(parse_exp_field(st.incr()), symbol(";")).map(exp_obj::new))
}

parser! {
    pub fn parse_exp_obj['a](st: State)(easy::Stream<&'a str>) -> exp_obj
    where []
    {
        parse_exp_obj_(*st)
    }
}

/*
<exp_plain> ::=
    <lit>
    '(' <list(<exp>, ',')> ')'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum exp_plain {
    lit(lit),
    exp(list<exp>),
}

pub fn parse_exp_plain_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp_plain> {
    st.indent();
    println!("parse_exp_plain");
    parse_lit(st.incr())
        .map(exp_plain::lit)
        .or(parens(sep_by(parse_exp(st.incr()), symbol(","))).map(exp_plain::exp))
}

parser! {
    pub fn parse_exp_plain['a](st: State)(easy::Stream<&'a str>) -> exp_plain
    where []
    {
        parse_exp_plain_(*st)
    }
}

/*
<exp_nullary> ::=
    <exp_obj>
    <exp_plain>
    <id>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum exp_nullary {
    obj(exp_obj),
    plain(exp_plain),
    id(id),
}

pub fn parse_exp_nullary_<'a>(
    st: State,
) -> impl Parser<easy::Stream<&'a str>, Output = exp_nullary> {
    st.indent();
    println!("parse_exp_nullary");
    parse_exp_obj(st.incr())
        .map(exp_nullary::obj)
        .or(parse_exp_plain(st.incr()).map(exp_nullary::plain))
        .or(parse_id(st.incr()).map(exp_nullary::id))
}

parser! {
    pub fn parse_exp_nullary['a](st: State)(easy::Stream<&'a str>) -> exp_nullary
    where []
    {
        parse_exp_nullary_(*st)
    }
}

/*
<exp_post> ::=
    <exp_nullary>
    '[' 'var'? <list(<exp_nonvar>, ',')> ']'
    <exp_post> '[' <exp> ']'
    <exp_post> '.'<nat>
    <exp_post> '.' <id>
    <exp_post> ('<' <list(<typ>, ',')> '>')? <exp_nullary>
    <exp_post> BANG
*/

#[derive(Clone, Debug, PartialEq)]
pub enum exp_post {
    nullary(exp_nullary),
    nonvars(bool, list<exp_nonvar>),
    bracket(Box<exp_post>, exp),
    dot(Box<exp_post>, nat),
    id(Box<exp_post>, id),
    typ(Box<exp_post>, Option<list<typ>>, exp_nullary),
    bang(Box<exp_post>),
}

impl exp_post {
    fn exp_post_nonvars((v, e): (bool, list<exp_nonvar>)) -> Self {
        exp_post::nonvars(v, e)
    }

    #[allow(non_snake_case)]
    fn match_on_exp_post__post((pre, post): (exp_post, Option<exp_post__post>)) -> Self {
        match post {
            None => pre,
            Some(exp_post__post::bracket(exp)) => exp_post::bracket(Box::new(pre), exp),
            Some(exp_post__post::dot(nat)) => exp_post::dot(Box::new(pre), nat),
            Some(exp_post__post::id(id)) => exp_post::id(Box::new(pre), id),
            Some(exp_post__post::typ(typs, nullary)) => exp_post::typ(Box::new(pre), typs, nullary),
            Some(exp_post__post::bang) => exp_post::bang(Box::new(pre)),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum exp_post__post {
    bracket(exp),
    dot(nat),
    id(id),
    typ(Option<list<typ>>, exp_nullary),
    bang,
}

impl exp_post__post {
    #[allow(non_snake_case)]
    fn exp_post__post_typ((typs, nullary): (Option<list<typ>>, exp_nullary)) -> Self {
        exp_post__post::typ(typs, nullary)
    }
}

pub fn parse_exp_post_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp_post> {
    st.indent();
    println!("parse_exp_post");
    brackets(
        parse_var_opt(sep_by(parse_exp_nonvar(st.incr()), symbol(",")))
            .map(exp_post::exp_post_nonvars),
    )
    .or(parse_exp_nullary(st.incr()).map(exp_post::nullary))
    .and(optional(
        brackets(parse_exp(st.incr()))
            .map(exp_post__post::bracket)
            .or(symbol(".")
                .with(parse_nat(st.incr()))
                .map(exp_post__post::dot))
            .or(symbol(".")
                .with(parse_id(st.incr()))
                .map(exp_post__post::id))
            .or(optional(angles(sep_by(parse_typ(st.incr()), symbol(","))))
                .and(parse_exp_nullary(st.incr()))
                .map(exp_post__post::exp_post__post_typ))
            .or(symbol("!").with(value(exp_post__post::bang))),
    ))
    .map(exp_post::match_on_exp_post__post)
}

parser! {
    pub fn parse_exp_post['a](st: State)(easy::Stream<&'a str>) -> exp_post
    where []
    {
        parse_exp_post_(*st)
    }
}

/*
<exp_un> ::=
    <exp_post>
    '#' <id>
    '#' <id> <exp_nullary>
    '?' <exp_un>
    <unop> <exp_un>
    <unassign> <exp_un>
    'actor' <exp_plain>
    'not' <exp_un>
    'debug_show' <exp_un>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum exp_un {
    post(exp_post),
    hash(id, Option<exp_nullary>),
    question(Box<exp_un>),
    unop(unop, Box<exp_un>),
    unassign(unassign, Box<exp_un>),
    actor(exp_plain),
    not(Box<exp_un>),
    debug_show(Box<exp_un>),
}

impl exp_un {
    fn exp_un_hash((id, nullary): (id, Option<exp_nullary>)) -> Self {
        exp_un::hash(id, nullary)
    }

    fn exp_un_question(un: exp_un) -> Self {
        exp_un::question(Box::new(un))
    }

    fn exp_un_unop((unop, un): (unop, exp_un)) -> Self {
        exp_un::unop(unop, Box::new(un))
    }

    fn exp_un_unassign((unassign, un): (unassign, exp_un)) -> Self {
        exp_un::unassign(unassign, Box::new(un))
    }

    fn exp_un_not(un: exp_un) -> Self {
        exp_un::not(Box::new(un))
    }

    fn exp_un_debug_show(un: exp_un) -> Self {
        exp_un::debug_show(Box::new(un))
    }
}

pub fn parse_exp_un_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp_un> {
    st.indent();
    println!("parse_exp_un");
    symbol("#")
        .with(parse_id(st.incr()))
        .and(optional(parse_exp_nullary(st.incr())))
        .map(exp_un::exp_un_hash)
        .or(symbol("?")
            .with(parse_exp_un(st.incr()))
            .map(exp_un::exp_un_question))
        .or(keyword("actor")
            .with(parse_exp_plain(st.incr()))
            .map(exp_un::actor))
        .or(keyword("not")
            .with(parse_exp_un(st.incr()))
            .map(exp_un::exp_un_not))
        .or(keyword("debug_show")
            .with(parse_exp_un(st.incr()))
            .map(exp_un::exp_un_debug_show))
        .or(parse_unop(st.incr())
            .and(parse_exp_un(st.incr()))
            .map(exp_un::exp_un_unop))
        .or(parse_unassign(st.incr())
            .and(parse_exp_un(st.incr()))
            .map(exp_un::exp_un_unassign))
        .or(parse_exp_post(st.incr()).map(exp_un::post))
}

parser! {
    pub fn parse_exp_un['a](st: State)(easy::Stream<&'a str>) -> exp_un
    where []
    {
        parse_exp_un_(*st)
    }
}

/*
<exp_bin> ::=
    <exp_un>
    <exp_bin> <binop> <exp_bin>
    <exp_bin> <relop> <exp_bin>
    <exp_bin> 'and' <exp_bin>
    <exp_bin> 'or' <exp_bin>
    <exp_bin> ':' <typ_nobin>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum exp_bin {
    un(exp_un),
    binop(Box<exp_bin>, binop, Box<exp_bin>),
    relop(Box<exp_bin>, relop, Box<exp_bin>),
    and(Box<exp_bin>, Box<exp_bin>),
    or(Box<exp_bin>, Box<exp_bin>),
    colon(Box<exp_bin>, typ_nobin),
}

impl exp_bin {
    #[allow(non_snake_case)]
    fn match_on_exp_bin__post((un, post): (exp_un, Option<exp_bin__post>)) -> Self {
        let pre = Box::new(exp_bin::un(un));
        match post {
            None => *pre,
            Some(exp_bin__post::binop(op, x)) => exp_bin::binop(pre, op, x),
            Some(exp_bin__post::relop(op, x)) => exp_bin::relop(pre, op, x),
            Some(exp_bin__post::and(x)) => exp_bin::and(pre, x),
            Some(exp_bin__post::or(x)) => exp_bin::or(pre, x),
            Some(exp_bin__post::colon(x)) => exp_bin::colon(pre, x),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum exp_bin__post {
    binop(binop, Box<exp_bin>),
    relop(relop, Box<exp_bin>),
    and(Box<exp_bin>),
    or(Box<exp_bin>),
    colon(typ_nobin),
}

impl exp_bin__post {
    #[allow(non_snake_case)]
    fn exp_bin__post_binop((binop, bin): (binop, exp_bin)) -> Self {
        exp_bin__post::binop(binop, Box::new(bin))
    }

    #[allow(non_snake_case)]
    fn exp_bin__post_relop((relop, bin): (relop, exp_bin)) -> Self {
        exp_bin__post::relop(relop, Box::new(bin))
    }

    #[allow(non_snake_case)]
    fn exp_bin__post_and(r: exp_bin) -> Self {
        exp_bin__post::and(Box::new(r))
    }

    #[allow(non_snake_case)]
    fn exp_bin__post_or(r: exp_bin) -> Self {
        exp_bin__post::or(Box::new(r))
    }
}

pub fn parse_exp_bin_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp_bin> {
    st.indent();
    println!("parse_exp_bin");
    parse_exp_un(st.incr())
        .and(optional(
            parse_binop(st.incr())
                .and(parse_exp_bin(st.incr()))
                .map(exp_bin__post::exp_bin__post_binop)
                .or(parse_relop(st.incr())
                    .and(parse_exp_bin(st.incr()))
                    .map(exp_bin__post::exp_bin__post_relop))
                .or(keyword("and")
                    .with(parse_exp_bin(st.incr()))
                    .map(exp_bin__post::exp_bin__post_and))
                .or(keyword("or")
                    .with(parse_exp_bin(st.incr()))
                    .map(exp_bin__post::exp_bin__post_or))
                .or(symbol(":")
                    .with(parse_typ_nobin(st.incr()))
                    .map(exp_bin__post::colon)),
        ))
        .map(exp_bin::match_on_exp_bin__post)
}

parser! {
    pub fn parse_exp_bin['a](st: State)(easy::Stream<&'a str>) -> exp_bin
    where []
    {
        parse_exp_bin_(*st)
    }
}

/*
<exp_nondec> ::=
    <exp_bin>
    <exp_bin> ':=' <exp>
    <exp_bin> <binassign> <exp>
    'return' <exp>?
    'async' <exp_nest>
    'await' <exp_nest>
    'assert' <exp_nest>
    'label' <id> (':' <typ>)? <exp_nest>
    'break' <id> <exp_nullary>?
    'continue' <id>
    'debug' <exp_nest>
    'if' <exp_nullary> <exp_nest>
    'if' <exp_nullary> <exp_nest> 'else' <exp_nest>
    'try' <exp_nest> <catch>
    'throw' <exp_nest>
    'switch' <exp_nullary> '{' <list(<case>, ';')> '}'
    'while' <exp_nullary> <exp_nest>
    'loop' <exp_nest>
    'loop' <exp_nest> 'while' <exp_nest>
    'for' '(' <pat> 'in' <exp> ')' <exp_nest>
    'ignore' <exp_nest>
    'do' <block>
    'do' '?' <block>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum exp_nondec {
    bin(exp_bin),
    assign(exp_bin, exp),
    binassign(exp_bin, binassign, exp),
    return_(Option<exp>),
    async_(exp_nest),
    await_(exp_nest),
    assert(exp_nest),
    label(id, Option<typ>, exp_nest),
    break_(id, Option<exp_nullary>),
    continue_(id),
    debug(exp_nest),
    if_(exp_nullary, exp_nest),
    if_else(exp_nullary, exp_nest, exp_nest),
    try_(exp_nest, catch),
    throw(exp_nest),
    switch(exp_nullary, list<case>),
    while_(exp_nullary, exp_nest),
    loop_(exp_nest),
    loop_while(exp_nest, exp_nest),
    for_(pat, exp, exp_nest),
    ignore(exp_nest),
    do_(block),
    do_question(block),
}

impl exp_nondec {
    fn exp_nondec_bin_assign_binassign(
        (bin, v): (exp_bin, Option<Either<exp, (binassign, exp)>>),
    ) -> Self {
        match v {
            None => exp_nondec::bin(bin),
            Some(Either::Left(exp)) => exp_nondec::assign(bin, exp),
            Some(Either::Right((assn, exp))) => exp_nondec::binassign(bin, assn, exp),
        }
    }

    fn exp_nondec_label(((id, ty), body): ((id, Option<typ>), exp_nest)) -> Self {
        exp_nondec::label(id, ty, body)
    }

    fn exp_nondec_break_((id, nullary): (id, Option<exp_nullary>)) -> Self {
        exp_nondec::break_(id, nullary)
    }

    fn exp_nondec_if_else(
        ((cond, then), melse_): ((exp_nullary, exp_nest), Option<exp_nest>),
    ) -> Self {
        match melse_ {
            None => exp_nondec::if_(cond, then),
            Some(else_) => exp_nondec::if_else(cond, then, else_),
        }
    }

    fn exp_nondec_try_((nest, catch): (exp_nest, catch)) -> Self {
        exp_nondec::try_(nest, catch)
    }

    fn exp_nondec_switch((scrutinee, cases): (exp_nullary, list<case>)) -> Self {
        exp_nondec::switch(scrutinee, cases)
    }

    fn exp_nondec_while_((cond, body): (exp_nullary, exp_nest)) -> Self {
        exp_nondec::while_(cond, body)
    }

    fn exp_nondec_loop_while((cond, mbody): (exp_nest, Option<exp_nest>)) -> Self {
        match mbody {
            None => exp_nondec::loop_(cond),
            Some(body) => exp_nondec::loop_while(cond, body),
        }
    }

    fn exp_nondec_for_(((pat, exp), body): ((pat, exp), exp_nest)) -> Self {
        exp_nondec::for_(pat, exp, body)
    }
}

pub fn parse_exp_nondec_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp_nondec> {
    st.indent();
    println!("parse_exp_nondec");
    keyword("return")
        .with(optional(parse_exp(st.incr())).map(exp_nondec::return_))
        .or(keyword("async").with(parse_exp_nest(st.incr()).map(exp_nondec::async_)))
        .or(keyword("await").with(parse_exp_nest(st.incr()).map(exp_nondec::await_)))
        .or(keyword("assert").with(parse_exp_nest(st.incr()).map(exp_nondec::assert)))
        .or(keyword("label")
            .with(parse_id(st.incr()))
            .and(optional(symbol(":").with(parse_typ(st.incr()))))
            .and(parse_exp_nest(st.incr()))
            .map(exp_nondec::exp_nondec_label))
        .or(keyword("break")
            .with(parse_id(st.incr()))
            .and(optional(parse_exp_nullary(st.incr())))
            .map(exp_nondec::exp_nondec_break_))
        .or(keyword("continue").with(parse_id(st.incr()).map(exp_nondec::continue_)))
        .or(keyword("debug").with(parse_exp_nest(st.incr()).map(exp_nondec::debug)))
        .or(keyword("if")
            .with(parse_exp_nullary(st.incr()))
            .and(parse_exp_nest(st.incr()))
            .and(optional(keyword("else").with(parse_exp_nest(st.incr()))))
            .map(exp_nondec::exp_nondec_if_else))
        .or(keyword("try")
            .with(parse_exp_nest(st.incr()))
            .and(parse_catch(st.incr()))
            .map(exp_nondec::exp_nondec_try_))
        .or(keyword("throw").with(parse_exp_nest(st.incr()).map(exp_nondec::throw)))
        .or(keyword("switch")
            .with(parse_exp_nullary(st.incr()))
            .and(braces(sep_end_by(parse_case(st.incr()), symbol(";"))))
            .map(exp_nondec::exp_nondec_switch))
        .or(keyword("while")
            .with(parse_exp_nullary(st.incr()))
            .and(parse_exp_nest(st.incr()))
            .map(exp_nondec::exp_nondec_while_))
        .or(keyword("loop")
            .with(parse_exp_nest(st.incr()))
            .and(optional(keyword("while").with(parse_exp_nest(st.incr()))))
            .map(exp_nondec::exp_nondec_loop_while))
        .or(keyword("for")
            .with(parens(
                parse_pat(st.incr())
                    .skip(keyword("in"))
                    .and(parse_exp(st.incr())),
            ))
            .and(parse_exp_nest(st.incr()))
            .map(exp_nondec::exp_nondec_for_))
        .or(keyword("ignore").with(parse_exp_nest(st.incr()).map(exp_nondec::ignore)))
        .or(keyword("do").with(
            symbol("?")
                .with(parse_block(st.incr()).map(exp_nondec::do_question))
                .or(parse_block(st.incr()).map(exp_nondec::do_)),
        ))
        .or(parse_exp_bin(st.incr())
            .and(optional(
                keyword(":=")
                    .with(parse_exp(st.incr()))
                    .map(Either::Left)
                    .or(parse_binassign(st.incr())
                        .and(parse_exp(st.incr()))
                        .map(Either::Right)),
            ))
            .map(exp_nondec::exp_nondec_bin_assign_binassign))
}

parser! {
    pub fn parse_exp_nondec['a](st: State)(easy::Stream<&'a str>) -> exp_nondec
    where []
    {
        parse_exp_nondec_(*st)
    }
}

/*
<exp_nonvar> ::=
    <exp_nondec>
    <dec_nonvar>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum exp_nonvar {
    exp_nondec(exp_nondec),
    dec_nonvar(dec_nonvar),
}

pub fn parse_exp_nonvar_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp_nonvar> {
    st.indent();
    println!("parse_exp_nonvar");
    parse_exp_nondec(st.incr())
        .map(exp_nonvar::exp_nondec)
        .or(parse_dec_nonvar(st.incr()).map(exp_nonvar::dec_nonvar))
}

parser! {
    pub fn parse_exp_nonvar['a](st: State)(easy::Stream<&'a str>) -> exp_nonvar
    where []
    {
        parse_exp_nonvar_(*st)
    }
}

/*
<exp> ::=
    <exp_nonvar>
    <dec_var>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum exp {
    exp_nonvar(Box<exp_nonvar>),
    dec_var(Box<dec_var>),
}

impl exp {
    fn exp_exp_nonvar(e: exp_nonvar) -> Self {
        exp::exp_nonvar(Box::new(e))
    }

    fn exp_dec_var(d: dec_var) -> Self {
        exp::dec_var(Box::new(d))
    }
}

pub fn parse_exp_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp> {
    st.indent();
    println!("parse_exp");
    parse_exp_nonvar(st.incr())
        .map(exp::exp_exp_nonvar)
        .or(parse_dec_var(st.incr()).map(exp::exp_dec_var))
}

parser! {
    pub fn parse_exp['a](st: State)(easy::Stream<&'a str>) -> exp
    where []
    {
        parse_exp_(*st)
    }
}

#[cfg(test)]
fn nat(x: usize) -> exp_bin {
    exp_bin::un(exp_un::post(exp_post::nullary(exp_nullary::plain(
        exp_plain::lit(lit::nat(Integer::from(x))),
    ))))
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
        ),),
        parse_exp(State::new()).easy_parse("1 + 3"),
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

/*
<exp_nest> ::=
    <block>
    <exp>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum exp_nest {
    block(block),
    exp(exp),
}

pub fn parse_exp_nest_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp_nest> {
    st.indent();
    println!("parse_exp_nest");
    parse_block(st.incr())
        .map(exp_nest::block)
        .or(parse_exp(st.incr()).map(exp_nest::exp))
}

parser! {
    pub fn parse_exp_nest['a](st: State)(easy::Stream<&'a str>) -> exp_nest
    where []
    {
        parse_exp_nest_(*st)
    }
}

/*
 * STATEMENTS
 */

/*
<block> ::=
    '{' <list(<dec>, ';')> '}'

*/

#[derive(Clone, Debug, PartialEq)]
pub struct block {
    pub decs: list<dec>,
}

impl block {
    fn new(decs: list<dec>) -> Self {
        block { decs }
    }
}

pub fn parse_block_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = block> {
    st.indent();
    println!("parse_block");
    braces(sep_end_by(parse_dec(st.incr()), symbol(";"))).map(block::new)
}

parser! {
    pub fn parse_block['a](st: State)(easy::Stream<&'a str>) -> block
    where []
    {
        parse_block_(*st)
    }
}

/*
<case> ::=
    'case' <pat_nullary> <exp_nest>

*/

#[derive(Clone, Debug, PartialEq)]
pub struct case {
    pub nullary: pat_nullary,
    pub nest: exp_nest,
}

impl case {
    fn new((nullary, nest): (pat_nullary, exp_nest)) -> Self {
        case { nullary, nest }
    }
}

pub fn parse_case_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = case> {
    st.indent();
    println!("parse_case");
    keyword("case")
        .with(parse_pat_nullary(st.incr()))
        .and(parse_exp_nest(st.incr()))
        .map(case::new)
}

parser! {
    pub fn parse_case['a](st: State)(easy::Stream<&'a str>) -> case
    where []
    {
        parse_case_(*st)
    }
}

/*
<catch> ::=
    'catch' <pat_nullary> <exp_nest>

*/

#[derive(Clone, Debug, PartialEq)]
pub struct catch {
    pub nullary: pat_nullary,
    pub nest: exp_nest,
}

impl catch {
    fn new((nullary, nest): (pat_nullary, exp_nest)) -> Self {
        catch { nullary, nest }
    }
}

pub fn parse_catch_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = catch> {
    st.indent();
    println!("parse_catch");
    keyword("catch")
        .with(parse_pat_nullary(st.incr()))
        .and(parse_exp_nest(st.incr()))
        .map(catch::new)
}

parser! {
    pub fn parse_catch['a](st: State)(easy::Stream<&'a str>) -> catch
    where []
    {
        parse_catch_(*st)
    }
}

/*
<exp_field> ::=
    'var'? <id> (':' <typ>)?
    'var'? <id> (':' <typ>)? '=' <exp>
*/

#[derive(Clone, Debug, PartialEq)]
pub struct exp_field {
    pub var: bool,
    pub id: id,
    pub typ: Option<typ>,
    pub exp: Option<exp>,
}

impl exp_field {
    fn new((((var, id), typ), exp): (((bool, id), Option<typ>), Option<exp>)) -> Self {
        exp_field { var, id, typ, exp }
    }
}

pub fn parse_exp_field_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = exp_field> {
    st.indent();
    println!("parse_exp_field");
    parse_var_opt(parse_id(st.incr()))
        .and(optional(symbol(":").with(parse_typ(st.incr()))))
        .and(optional(symbol("=").with(parse_exp(st.incr()))))
        .map(exp_field::new)
}

parser! {
    pub fn parse_exp_field['a](st: State)(easy::Stream<&'a str>) -> exp_field
    where []
    {
        parse_exp_field_(*st)
    }
}

/*
<dec_field> ::=
    <vis> <stab> <dec>

*/

#[derive(Clone, Debug, PartialEq)]
pub struct dec_field {
    pub vis: Option<vis>,
    pub stab: Option<stab>,
    pub dec: dec,
}

pub fn parse_dec_field_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = dec_field> {
    st.indent();
    println!("parse_dec_field");
    struct_parser! {
        dec_field {
            vis: optional(parse_vis(st.incr())),
            stab: optional(parse_stab(st.incr())),
            dec: parse_dec(st.incr())
        }
    }
}

parser! {
    pub fn parse_dec_field['a](st: State)(easy::Stream<&'a str>) -> dec_field
    where []
    {
        parse_dec_field_(*st)
    }
}

/*
<vis> ::=
    <empty>
    'private'
    'public'
    'system'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum vis {
    private,
    public,
    system,
}

pub fn parse_vis_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = vis> {
    st.indent();
    println!("parse_vis");
    choice([
        attempt(keyword("private")).with(value(vis::private)),
        attempt(keyword("public")).with(value(vis::public)),
        attempt(keyword("system")).with(value(vis::system)),
    ])
}

parser! {
    pub fn parse_vis['a](st: State)(easy::Stream<&'a str>) -> vis
    where []
    {
        parse_vis_(*st)
    }
}

/*
<stab> ::=
    <empty>
    'flexible'
    'stable'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum stab {
    flexible,
    stable,
}

pub fn parse_stab_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = stab> {
    st.indent();
    println!("parse_stab");
    choice([
        attempt(keyword("flexible")).with(value(stab::flexible)),
        attempt(keyword("stable")).with(value(stab::stable)),
    ])
}

parser! {
    pub fn parse_stab['a](st: State)(easy::Stream<&'a str>) -> stab
    where []
    {
        parse_stab_(*st)
    }
}

/*
 * PATTERNS
 */

/*
<pat_plain> ::=
    '_'
    <id>
    <lit>
    '(' <list(<pat_bin>, ',')> ')'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum pat_plain {
    underscore,
    id(id),
    lit(lit),
    bins(list<pat_bin>),
}

pub fn parse_pat_plain_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = pat_plain> {
    st.indent();
    println!("parse_pat_plain");
    symbol("_")
        .with(value(pat_plain::underscore))
        .or(parse_id(st.incr()).map(pat_plain::id))
        .or(parse_lit(st.incr()).map(pat_plain::lit))
        .or(parens(sep_by(parse_pat_bin(st.incr()), symbol(","))).map(pat_plain::bins))
}

parser! {
    pub fn parse_pat_plain['a](st: State)(easy::Stream<&'a str>) -> pat_plain
    where []
    {
        parse_pat_plain_(*st)
    }
}

/*
<pat_nullary> ::=
    <pat_plain>
    '{' <list(<pat_field>, ';')> '}'
*/

#[derive(Clone, Debug, PartialEq)]
pub enum pat_nullary {
    plain(pat_plain),
    field(list<pat_field>),
}

pub fn parse_pat_nullary_<'a>(
    st: State,
) -> impl Parser<easy::Stream<&'a str>, Output = pat_nullary> {
    st.indent();
    println!("parse_pat_nullary");
    braces(sep_end_by(parse_pat_field(st.incr()), symbol(";")))
        .map(pat_nullary::field)
        .or(parse_pat_plain(st.incr()).map(pat_nullary::plain))
}

parser! {
    pub fn parse_pat_nullary['a](st: State)(easy::Stream<&'a str>) -> pat_nullary
    where []
    {
        parse_pat_nullary_(*st)
    }
}

/*
<pat_un> ::=
    <pat_nullary>
    '#' <id>
    '#' <id> <pat_nullary>
    '?' <pat_un>
    <unop> <lit>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum pat_un {
    nullary(pat_nullary),
    hash(id, Option<pat_nullary>),
    question(Box<pat_un>),
    unop_lit(unop, lit),
}

impl pat_un {
    fn pat_un_hash((id, n): (id, Option<pat_nullary>)) -> Self {
        pat_un::hash(id, n)
    }

    fn pat_un_question(u: pat_un) -> Self {
        pat_un::question(Box::new(u))
    }

    fn pat_un_unop_lit((u, l): (unop, lit)) -> Self {
        pat_un::unop_lit(u, l)
    }
}

pub fn parse_pat_un_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = pat_un> {
    st.indent();
    println!("parse_pat_un");
    symbol("#")
        .with(parse_id(st.incr()))
        .and(optional(parse_pat_nullary(st.incr())))
        .map(pat_un::pat_un_hash)
        .or(symbol("?")
            .with(parse_pat_un(st.incr()))
            .map(pat_un::pat_un_question))
        .or(parse_unop(st.incr())
            .and(parse_lit(st.incr()))
            .map(pat_un::pat_un_unop_lit))
        .or(parse_pat_nullary(st.incr()).map(pat_un::nullary))
}

parser! {
    pub fn parse_pat_un['a](st: State)(easy::Stream<&'a str>) -> pat_un
    where []
    {
        parse_pat_un_(*st)
    }
}

/*
<pat_bin> ::=
    <pat_un>
    <pat_bin> 'or' <pat_bin>
    <pat_bin> ':' <typ>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum pat_bin {
    un(pat_un),
    or(Box<pat_bin>, Box<pat_bin>),
    colon(Box<pat_bin>, typ),
}

impl pat_bin {
    #[allow(non_snake_case)]
    fn match_on_pat_bin__post((un, post): (pat_un, Option<pat_bin__post>)) -> Self {
        let pre = Box::new(pat_bin::un(un));
        match post {
            None => *pre,
            Some(pat_bin__post::or(x)) => pat_bin::or(pre, x),
            Some(pat_bin__post::colon(x)) => pat_bin::colon(pre, x),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum pat_bin__post {
    or(Box<pat_bin>),
    colon(typ),
}

impl pat_bin__post {
    #[allow(non_snake_case)]
    fn pat_bin__post_or(b: pat_bin) -> Self {
        pat_bin__post::or(Box::new(b))
    }
}

pub fn parse_pat_bin_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = pat_bin> {
    st.indent();
    println!("parse_pat_bin");
    parse_pat_un(st.incr())
        .and(optional(
            keyword("or")
                .with(parse_pat_bin(st.incr()))
                .map(pat_bin__post::pat_bin__post_or)
                .or(symbol(":")
                    .with(parse_typ(st.incr()))
                    .map(pat_bin__post::colon)),
        ))
        .map(pat_bin::match_on_pat_bin__post)
}

parser! {
    pub fn parse_pat_bin['a](st: State)(easy::Stream<&'a str>) -> pat_bin
    where []
    {
        parse_pat_bin_(*st)
    }
}

/*
<pat> ::=
    <pat_bin>
*/

#[derive(Clone, Debug, PartialEq)]
pub struct pat {
    pub bin: pat_bin,
}

pub fn parse_pat_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = pat> {
    st.indent();
    println!("parse_pat");
    struct_parser! {
        pat {
            bin: parse_pat_bin(st.incr())
        }
    }
}

parser! {
    pub fn parse_pat['a](st: State)(easy::Stream<&'a str>) -> pat
    where []
    {
        parse_pat_(*st)
    }
}

/*
<pat_field> ::=
    <id> (':' <typ>)?
    <id> (':' <typ>)? '=' <pat>
*/

#[derive(Clone, Debug, PartialEq)]
pub struct pat_field {
    pub id: id,
    pub typ: Option<typ>,
    pub pat: Option<pat>,
}

pub fn parse_pat_field_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = pat_field> {
    st.indent();
    println!("parse_pat_field");
    struct_parser! {
        pat_field {
            id: parse_id(st.incr()),
            typ: optional(symbol(":").with(parse_typ(st.incr()))),
            pat: optional(symbol("=").with(parse_pat(st.incr())))
        }
    }
}

parser! {
    pub fn parse_pat_field['a](st: State)(easy::Stream<&'a str>) -> pat_field
    where []
    {
        parse_pat_field_(*st)
    }
}

/*
 * DECLARATIONS
 */

/*
<dec_var> ::=
    'var' <id> (':' <typ>)? '=' <exp>

*/

#[derive(Clone, Debug, PartialEq)]
pub struct dec_var {
    pub id: id,
    pub typ: Option<typ>,
    pub exp: exp,
}

pub fn parse_dec_var_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = dec_var> {
    st.indent();
    println!("parse_dec_var");
    struct_parser! {
        dec_var {
            id: keyword("var").with(parse_id(st.incr())),
            typ: optional(symbol(":").with(parse_typ(st.incr()))),
            exp: symbol("=").with(parse_exp(st.incr())),
        }
    }
}

parser! {
    pub fn parse_dec_var['a](st: State)(easy::Stream<&'a str>) -> dec_var
    where []
    {
        parse_dec_var_(*st)
    }
}

/*
<dec_nonvar> ::=
    'let' <pat> '=' <exp>
    'type' <id> ('<' <list(<typ_bind>, ',')> '>')? '=' <typ>
    <obj_sort> <id>? '='? <obj_body>
    <shared_pat_opt> 'func' <id>? ('<' <list(<typ_bind>, ',')> '>')? <pat_plain> (':' <typ>)? <func_body>
    <shared_pat_opt> <obj_sort>? 'class' <id>? ('<' <list(<typ_bind>, ',')> '>')? <pat_plain> (':' <typ>)? <class_body>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum dec_nonvar {
    let_dec(pat, Box<exp>),
    type_dec(id, Option<list<typ_bind>>, typ),
    obj_dec(obj_sort, Option<id>, bool, obj_body),
    func {
        shared_pat: Option<shared_pat_opt>,
        id: Option<id>,
        binds: Option<list<typ_bind>>,
        plain: pat_plain,
        typ: Option<typ>,
        body: func_body,
    },
    class {
        shared_pat: Option<shared_pat_opt>,
        sort: Option<obj_sort>,
        id: Option<id>,
        binds: Option<list<typ_bind>>,
        plain: pat_plain,
        typ: Option<typ>,
        body: class_body,
    },
}

impl dec_nonvar {
    fn dec_nonvar_let_dec((pat, exp): (pat, exp)) -> Self {
        dec_nonvar::let_dec(pat, Box::new(exp))
    }

    fn dec_nonvar_type_dec(((id, binds), typ): ((id, Option<list<typ_bind>>), typ)) -> Self {
        dec_nonvar::type_dec(id, binds, typ)
    }

    fn dec_nonvar_obj_dec<U>(
        (((sort, id), eq), body): (((obj_sort, Option<id>), Option<U>), obj_body),
    ) -> Self {
        dec_nonvar::obj_dec(sort, id, eq.is_some(), body)
    }

    fn dec_nonvar_func(
        (((((shared_pat, id), binds), plain), typ), body): (
            (
                (
                    ((Option<shared_pat_opt>, Option<id>), Option<list<typ_bind>>),
                    pat_plain,
                ),
                Option<typ>,
            ),
            func_body,
        ),
    ) -> Self {
        dec_nonvar::func {
            shared_pat,
            id,
            binds,
            plain,
            typ,
            body,
        }
    }

    fn dec_nonvar_class(
        ((((((shared_pat, sort), id), binds), plain), typ), body): (
            (
                (
                    (
                        ((Option<shared_pat_opt>, Option<obj_sort>), Option<id>),
                        Option<list<typ_bind>>,
                    ),
                    pat_plain,
                ),
                Option<typ>,
            ),
            class_body,
        ),
    ) -> Self {
        dec_nonvar::class {
            shared_pat,
            sort,
            id,
            binds,
            plain,
            typ,
            body,
        }
    }
}

pub fn parse_dec_nonvar_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = dec_nonvar> {
    st.indent();
    println!("parse_dec_nonvar");
    keyword("let")
        .with(parse_pat(st.incr()))
        .and(symbol("=").with(parse_exp(st.incr())))
        .map(dec_nonvar::dec_nonvar_let_dec)
        .or(keyword("type")
            .with(parse_id(st.incr()))
            .and(optional(angles(sep_by(
                parse_typ_bind(st.incr()),
                symbol(","),
            ))))
            .skip(symbol("="))
            .and(parse_typ(st.incr()))
            .map(dec_nonvar::dec_nonvar_type_dec))
        .or(attempt(
            parse_obj_sort(st.incr())
                .and(optional(parse_id(st.incr())))
                .and(optional(symbol("=")))
                .and(parse_obj_body(st.incr())),
        )
        .map(dec_nonvar::dec_nonvar_obj_dec))
        .or(attempt(
            optional(parse_shared_pat_opt(st.incr()))
                .skip(keyword("func"))
                .and(optional(parse_id(st.incr())))
                .and(optional(angles(sep_by(
                    parse_typ_bind(st.incr()),
                    symbol(","),
                ))))
                .and(parse_pat_plain(st.incr()))
                .and(optional(symbol(":").with(parse_typ(st.incr()))))
                .and(parse_func_body(st.incr())),
        )
        .map(dec_nonvar::dec_nonvar_func))
        .or(optional(parse_shared_pat_opt(st.incr()))
            .and(optional(parse_obj_sort(st.incr())))
            .skip(keyword("class"))
            .and(optional(parse_id(st.incr())))
            .and(optional(angles(sep_by(
                parse_typ_bind(st.incr()),
                symbol(","),
            ))))
            .and(parse_pat_plain(st.incr()))
            .and(optional(symbol(":").with(parse_typ(st.incr()))))
            .and(parse_class_body(st.incr()))
            .map(dec_nonvar::dec_nonvar_class))
}

parser! {
    pub fn parse_dec_nonvar['a](st: State)(easy::Stream<&'a str>) -> dec_nonvar
    where []
    {
        parse_dec_nonvar_(*st)
    }
}

#[test]
fn test_parse_dec_nonvar() {
    pretty_assertions::assert_eq!(
        Ok((
            dec_nonvar::let_dec(
                pat {
                    bin: pat_bin::un(pat_un::nullary(pat_nullary::plain(pat_plain::underscore)))
                },
                Box::new(exp::exp_nonvar(Box::new(exp_nonvar::exp_nondec(
                    exp_nondec::bin(exp_bin::binop(
                        Box::new(nat(1)),
                        binop::add,
                        Box::new(nat(3)),
                    ))
                ))))
            ),
            "",
        )),
        parse_dec_nonvar(State::new()).easy_parse("func(name: nat): ?nat { return (name + 20); }"),
    );
}

/*
<dec> ::=
    <dec_var>
    <dec_nonvar>
    <exp_nondec>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum dec {
    var(dec_var),
    nonvar(dec_nonvar),
    nondec(exp_nondec),
}

pub fn parse_dec_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = dec> {
    st.indent();
    println!("parse_dec");
    attempt(parse_exp_nondec(st.incr()))
        .map(dec::nondec)
        .or(attempt(parse_dec_var(st.incr())).map(dec::var))
        .or(parse_dec_nonvar(st.incr()).map(dec::nonvar))
}

parser! {
    pub fn parse_dec['a](st: State)(easy::Stream<&'a str>) -> dec
        where []
    {
        parse_dec_(*st)
    }
}

/*
 * DEFINITIONS
 */

/*
<func_body> ::=
    '=' <exp>
    <block>
*/

#[derive(Clone, Debug, PartialEq)]
pub enum func_body {
    exp(exp),
    block(block),
}

pub fn parse_func_body_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = func_body> {
    st.indent();
    println!("parse_func_body");
    symbol("=")
        .with(parse_exp(st.incr()).map(func_body::exp))
        .or(parse_block(st.incr()).map(func_body::block))
}

parser! {
    pub fn parse_func_body['a](st: State)(easy::Stream<&'a str>) -> func_body
    where []
    {
        parse_func_body_(*st)
    }
}

/*
<obj_body> ::=
    '{' <list(<dec_field>, ';')> '}'

*/

#[derive(Clone, Debug, PartialEq)]
pub struct obj_body {
    pub fields: list<dec_field>,
}

pub fn parse_obj_body_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = obj_body> {
    st.indent();
    println!("parse_obj_body");
    struct_parser! {
        obj_body {
            fields: braces(sep_end_by(parse_dec_field(st.incr()), symbol(";")))
        }
    }
}

parser! {
    pub fn parse_obj_body['a](st: State)(easy::Stream<&'a str>) -> obj_body
    where []
    {
        parse_obj_body_(*st)
    }
}

/*
<class_body> ::=
    '=' <id>? <obj_body>
    <obj_body>
*/

#[derive(Clone, Debug, PartialEq)]
pub struct class_body {
    pub equal: bool,
    pub id: Option<id>,
    pub body: obj_body,
}

impl class_body {
    fn new((id, body): (Option<Option<id>>, obj_body)) -> Self {
        class_body {
            equal: id.is_some(),
            id: id.flatten(),
            body,
        }
    }
}

pub fn parse_class_body_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = class_body> {
    st.indent();
    println!("parse_class_body");
    optional(symbol("=").with(optional(parse_id(st.incr()))))
        .and(parse_obj_body(st.incr()))
        .map(class_body::new)
}

parser! {
    pub fn parse_class_body['a](st: State)(easy::Stream<&'a str>) -> class_body
    where []
    {
        parse_class_body_(*st)
    }
}

/*
<imp> ::=
    'import' <id>? '='? <text>

*/

#[derive(Clone, Debug, PartialEq)]
pub struct imp {
    pub id: Option<id>,
    pub equal: bool,
    pub name: text,
}

fn is_some<U>(o: Option<U>) -> bool {
    o.is_some()
}

pub fn parse_imp_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = imp> {
    st.indent();
    println!("parse_imp");
    struct_parser! {
        imp {
            _: keyword("import"),
            id: optional(parse_id(st.incr())),
            equal: optional(symbol("=")).map(is_some),
            name: parse_text(st.incr()),
        }
    }
}

parser! {
    pub fn parse_imp['a](st: State)(easy::Stream<&'a str>) -> imp
    where []
    {
        parse_imp_(*st)
    }
}

/*
<prog> ::=
    <list(<imp>, ';')> <list(<dec>, ';')>

*/

#[derive(Clone, Debug, PartialEq)]
pub struct prog {
    pub imps: list<imp>,
    pub decs: list<dec>,
}

pub fn parse_prog_<'a>(st: State) -> impl Parser<easy::Stream<&'a str>, Output = prog> {
    st.indent();
    println!("parse_prog");
    struct_parser! {
        prog {
            imps: sep_end_by(parse_imp(st.incr()), symbol(";")),
            decs: sep_end_by(parse_dec(st.incr()), symbol(";"))
        }
    }
}

parser! {
    pub fn parse_prog['a](st: State)(easy::Stream<&'a str>) -> prog
    where []
    {
        parse_prog_(*st)
    }
}
