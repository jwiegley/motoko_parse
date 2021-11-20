#![allow(non_camel_case_types)]

use combine::parser::char::{char, spaces, string};
use combine::parser::combinator::Either;
use combine::{
    attempt, between, choice, many, optional, parser, satisfy, sep_by, sep_end_by, sep_end_by1,
    struct_parser, unexpected_any, value, Parser, StdParseResult,
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

pub fn parse_id_<'a>(st: State) -> impl Parser<&'a str, Output = id> {
    st.indent();
    println!("parse_id");
    satisfy(|c: char| c.is_alphabetic())
        .and(many(satisfy(|c: char| c.is_alphanumeric())))
        .map(|(x, xs): (char, Vec<char>)| vec![x].into_iter().chain(xs.into_iter()).collect())
}

parser! {
    fn parse_id['a](st: State)(&'a str) -> id
    where []
    {
        parse_id_(*st)
    }
}

#[test]
fn test_parse_id() {
    pretty_assertions::assert_eq!(
        Ok(("HelloWorld".to_string(), " GoodbyeWorld")),
        parse_id(State::new()).parse("HelloWorld GoodbyeWorld"),
    );
}

pub fn parse_bool_<'a>(st: State) -> impl Parser<&'a str, Output = bool> {
    st.indent();
    println!("parse_bool");
    keyword("true")
        .with(value(true))
        .or(keyword("false").with(value(false)))
}

parser! {
    fn parse_bool['a](st: State)(&'a str) -> bool
    where []
    {
        parse_bool_(*st)
    }
}

#[test]
fn test_parse_bool() {
    pretty_assertions::assert_eq!(
        Ok((true, ", false")),
        parse_bool(State::new()).parse("true, false"),
    );
    pretty_assertions::assert_eq!(
        Ok((false, ", true")),
        parse_bool(State::new()).parse("false, true"),
    );
}

pub fn parse_nat_<'a>(st: State) -> impl Parser<&'a str, Output = nat> {
    st.indent();
    println!("parse_nat");
    many(satisfy(|c: char| c.is_numeric()))
        .skip(spaces())
        .then(|xs: Vec<char>| {
            match Integer::parse(xs.into_iter().collect::<String>()).map(Integer::from) {
                Ok(int) => value(int).left(),
                Err(_err) => unexpected_any("nat").right(),
            }
        })
}

parser! {
    fn parse_nat['a](st: State)(&'a str) -> nat
    where []
    {
        parse_nat_(*st)
    }
}

#[test]
fn test_parse_nat() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(123), "")),
        parse_nat(State::new()).parse("123"),
    );
    pretty_assertions::assert_eq!(
        Ok((Integer::from(123), "")),
        parse_nat(State::new()).parse("0123"),
    );
}

// fn parse_float_match<'a, F: Iterator<Item = Input::Range>>(x: F) -> Float {
//     Float::with_val(
//         53,
//         Float::parse(x[0].into_iter().collect()).expect("failed to parse
// floating point"),     )
// }

pub fn parse_float_<'a>(st: State) -> impl Parser<&'a str, Output = float> {
    st.indent();
    println!("parse_float");
    many(satisfy(|c: char| c.is_numeric()))
        .skip(spaces())
        .then(|xs: Vec<char>| {
            match Float::parse(xs.into_iter().collect::<String>()).map(|v| Float::with_val(53, v)) {
                Ok(f) => value(f).left(),
                Err(_err) => unexpected_any("float").right(),
            }
        })
    // static REGEX: Lazy<Regex> =
    //     Lazy::new(||
    // Regex::new("[-+]?(([0-9]*[.][0-9]+([ed][-+]?[0-9]+)?)|(inf)|(nan))").
    // unwrap()); captures(&*REGEX).skip(spaces()).map(parse_float_match)
}

parser! {
    fn parse_float['a](st: State)(&'a str) -> float
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
        parse_float(State::new()).parse("1.3"),
    );
}
*/

// jww (2021-11-18): What is the grammer here?
pub fn parse_char_<'a>(st: State) -> impl Parser<&'a str, Output = char> {
    st.indent();
    println!("parse_char");
    char('\'')
        .with(satisfy(|c: char| c != '\''))
        .skip(char('\''))
}

parser! {
    fn parse_char['a](st: State)(&'a str) -> char
    where []
    {
        parse_char_(*st)
    }
}

// jww (2021-11-18): What is the grammer here?
pub fn parse_text_<'a>(st: State) -> impl Parser<&'a str, Output = text> {
    st.indent();
    println!("parse_text");
    char('"')
        .with(many(satisfy(|c: char| c != '"')))
        .skip(char('"'))
}

parser! {
    fn parse_text['a](st: State)(&'a str) -> text
    where []
    {
        parse_text_(*st)
    }
}

pub fn braces<'a, T>(p: impl Parser<&'a str, Output = T>) -> impl Parser<&'a str, Output = T> {
    between(symbol("{"), symbol("}"), p)
}

#[test]
fn test_braces() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(100), "")),
        braces(parse_nat(State::new()).skip(spaces())).parse("{ 100 }"),
    );
}

pub fn parens<'a, T>(p: impl Parser<&'a str, Output = T>) -> impl Parser<&'a str, Output = T> {
    between(symbol("("), symbol(")"), p)
}

#[test]
fn test_parens() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(100), "")),
        parens(parse_nat(State::new()).skip(spaces())).parse("( 100 )"),
    );
}

pub fn brackets<'a, T>(p: impl Parser<&'a str, Output = T>) -> impl Parser<&'a str, Output = T> {
    between(symbol("["), symbol("]"), p)
}

#[test]
fn test_brackets() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(100), "")),
        brackets(parse_nat(State::new()).skip(spaces())).parse("[ 100 ]"),
    );
}

pub fn angles<'a, T>(p: impl Parser<&'a str, Output = T>) -> impl Parser<&'a str, Output = T> {
    between(symbol("<"), symbol(">"), p)
}

#[test]
fn test_angles() {
    pretty_assertions::assert_eq!(
        Ok((Integer::from(100), "")),
        angles(parse_nat(State::new()).skip(spaces())).parse("< 100 >"),
    );
}

pub fn keyword<'a>(s: &'static str) -> impl Parser<&'a str, Output = &str> {
    string(s).skip(spaces())
}

#[test]
fn test_keyword() {
    pretty_assertions::assert_eq!(
        Ok((("foo", "bar"), "(13)")),
        keyword("foo").and(keyword("bar")).parse("foo bar(13)"),
    );
}

pub fn symbol<'a>(s: &'static str) -> impl Parser<&'a str, Output = &str> {
    string(s).skip(spaces())
}

pub fn operator<'a>(s: &'static str) -> impl Parser<&'a str, Output = &str> {
    string(s).skip(spaces())
}

pub fn parse_var_opt<'a, T>(
    p: impl Parser<&'a str, Output = T>,
) -> impl Parser<&'a str, Output = (bool, T)> {
    optional(keyword("var"))
        .and(p)
        .map(|(var, x)| (var.is_some(), x))
}

#[test]
fn test_parse_var_opt() {
    pretty_assertions::assert_eq!(
        Ok(((true, Integer::from(100)), "")),
        parse_var_opt(parse_nat(State::new()).skip(spaces())).parse("var 100"),
    );
    pretty_assertions::assert_eq!(
        Ok(((false, Integer::from(100)), "")),
        parse_var_opt(parse_nat(State::new()).skip(spaces())).parse("100"),
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

pub fn parse_obj_sort_<'a>(st: State) -> impl Parser<&'a str, Output = obj_sort> {
    st.indent();
    println!("parse_obj_sort");
    choice([
        attempt(keyword("object")).with(value(obj_sort::object)),
        attempt(keyword("actor")).with(value(obj_sort::actor)),
        attempt(keyword("module")).with(value(obj_sort::module)),
    ])
}

parser! {
    fn parse_obj_sort['a](st: State)(&'a str) -> obj_sort
    where []
    {
        parse_obj_sort_(*st)
    }
}

#[test]
fn test_parse_obj_sort() {
    pretty_assertions::assert_eq!(
        Ok((obj_sort::object, "hello")),
        parse_obj_sort(State::new()).parse("object hello"),
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

pub fn parse_func_sort_opt_<'a>(st: State) -> impl Parser<&'a str, Output = func_sort_opt> {
    st.indent();
    println!("parse_func_sort_opt");
    keyword("shared")
        .with(optional(keyword("query")))
        .map(|q| func_sort_opt::shared(q.is_some()))
        .or(keyword("query").with(value(func_sort_opt::query)))
}

parser! {
    fn parse_func_sort_opt['a](st: State)(&'a str) -> func_sort_opt
    where []
    {
        parse_func_sort_opt_(*st)
    }
}

#[test]
fn test_func_sort_opt() {
    pretty_assertions::assert_eq!(
        Ok((func_sort_opt::shared(false), "hello")),
        parse_func_sort_opt(State::new()).parse("shared hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((func_sort_opt::shared(true), "hello")),
        parse_func_sort_opt(State::new()).parse("shared query hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((func_sort_opt::query, "hello")),
        parse_func_sort_opt(State::new()).parse("query hello"),
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

pub fn parse_shared_pat_opt_<'a>(st: State) -> impl Parser<&'a str, Output = shared_pat_opt> {
    keyword("shared")
        .with(optional(keyword("query")))
        .and(optional(parse_pat_plain(st.incr())))
        .map(|(q, p)| shared_pat_opt::shared(q.is_some(), p))
        .or(keyword("query")
            .with(optional(parse_pat_plain(st.incr())))
            .map(shared_pat_opt::query))
}

parser! {
    fn parse_shared_pat_opt['a](st: State)(&'a str) -> shared_pat_opt
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
        parse_shared_pat_opt(State::new()).parse("shared _ hello"),
    );
    /*
        pretty_assertions::assert_eq!(
            Ok((shared_pat_opt::shared(false, None), "{} hello")),
            parse_shared_pat_opt(State::new()).parse("shared{} hello"),
        );
        pretty_assertions::assert_eq!(
            Ok((
                shared_pat_opt::shared(true, Some(pat_plain::underscore)),
                "hello"
            )),
            parse_shared_pat_opt(State::new()).parse("shared query _ hello"),
        );
        pretty_assertions::assert_eq!(
            Ok((shared_pat_opt::shared(true, None), "{} hello")),
            parse_shared_pat_opt(State::new()).parse("shared query{} hello"),
        );
        pretty_assertions::assert_eq!(
            Ok((shared_pat_opt::query(Some(pat_plain::underscore)), "hello")),
            parse_shared_pat_opt(State::new()).parse("query _ hello"),
        );
        pretty_assertions::assert_eq!(
            Ok((shared_pat_opt::query(None), "{} hello")),
            parse_shared_pat_opt(State::new()).parse("query{} hello"),
        );
    */
}

/*
<typ_obj> ::=
    '{' <list(<typ_field>, ';')> '}'
*/

#[derive(Clone, Debug, PartialEq)]
pub struct typ_obj {
    pub fields: list<typ_field>,
}

pub fn parse_typ_obj_<'a>(st: State) -> impl Parser<&'a str, Output = typ_obj> {
    sep_end_by(parse_typ_field(st.incr()), symbol(";")).map(|fields| typ_obj { fields })
}

parser! {
    fn parse_typ_obj['a](st: State)(&'a str) -> typ_obj
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

pub fn parse_typ_variant_<'a>(st: State) -> impl Parser<&'a str, Output = typ_variant> {
    braces(
        symbol("#").with(value(typ_variant::hash)).or(sep_end_by1(
            parse_typ_tag(st.incr()),
            symbol(";"),
        )
        .map(typ_variant::variants)),
    )
}

parser! {
    fn parse_typ_variant['a](st: State)(&'a str) -> typ_variant
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

pub fn parse_typ_nullary_<'a>(st: State) -> impl Parser<&'a str, Output = typ_nullary> {
    parens(sep_by(parse_typ_item(st.incr()), symbol(",")))
        .map(typ_nullary::list)
        .or(brackets(parse_var_opt(parse_typ(st.incr())))
            .map(|(var, typ)| typ_nullary::bracket(var, typ)))
        .or(parse_typ_obj(st.incr()).map(typ_nullary::obj))
        .or(parse_typ_variant(st.incr()).map(typ_nullary::variant))
        .or(sep_by(parse_id(st.incr()), symbol("."))
            .and(optional(parse_typ_args(st.incr())))
            .map(|(ids, args)| typ_nullary::dot(ids, args)))
}

parser! {
    fn parse_typ_nullary['a](st: State)(&'a str) -> typ_nullary
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

pub fn parse_typ_un_<'a>(st: State) -> impl Parser<&'a str, Output = typ_un> {
    parse_typ_nullary(st.incr())
        .map(typ_un::nullary)
        .or(symbol("?")
            .with(parse_typ_un(st.incr()))
            .map(|u| typ_un::question(Box::new(u))))
}

parser! {
    fn parse_typ_un['a](st: State)(&'a str) -> typ_un
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

pub fn parse_typ_pre_<'a>(st: State) -> impl Parser<&'a str, Output = typ_pre> {
    parse_typ_un(st.incr())
        .map(typ_pre::un)
        .or(keyword("async")
            .with(parse_typ_pre(st.incr()))
            .map(|t| typ_pre::async_(Box::new(t))))
        .or(parse_obj_sort(st.incr())
            .and(parse_typ_obj(st.incr()))
            .map(|(o, t)| typ_pre::obj(o, t)))
}

parser! {
    fn parse_typ_pre['a](st: State)(&'a str) -> typ_pre
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

pub fn parse_typ_nobin_<'a>(st: State) -> impl Parser<&'a str, Output = typ_nobin> {
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
            .map(|(((sort, binds), typ), ret)| typ_nobin::func {
                sort,
                binds,
                typ,
                ret: Box::new(ret),
            }))
}

parser! {
    fn parse_typ_nobin['a](st: State)(&'a str) -> typ_nobin
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

pub fn parse_typ_<'a>(st: State) -> impl Parser<&'a str, Output = typ> {
    parse_typ_nobin(st.incr())
        .and(optional(
            keyword("and").or(keyword("or")).and(parse_typ(st.incr())),
        ))
        .map(|(l, r_opt)| match r_opt {
            Some(("and", r)) => typ::or(Box::new(l), Box::new(r)),
            Some((_, r)) => typ::or(Box::new(l), Box::new(r)),
            None => typ::nobin(Box::new(l)),
        })
}

parser! {
    fn parse_typ['a](st: State)(&'a str) -> typ
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

pub fn parse_typ_item_<'a>(st: State) -> impl Parser<&'a str, Output = typ_item> {
    parse_id(st.incr())
        .skip(symbol(":"))
        .and(parse_typ(st.incr()))
        .map(|(id, t)| typ_item::colon(id, t))
        .or(parse_typ(st.incr()).map(typ_item::typ))
}

parser! {
    fn parse_typ_item['a](st: State)(&'a str) -> typ_item
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

pub fn parse_typ_args_<'a>(st: State) -> impl Parser<&'a str, Output = typ_args> {
    angles(sep_by(parse_typ(st.incr()), symbol(","))).map(|args| typ_args { args })
}

parser! {
    fn parse_typ_args['a](st: State)(&'a str) -> typ_args
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

pub fn parse_typ_field_<'a>(st: State) -> impl Parser<&'a str, Output = typ_field> {
    parse_var_opt(parse_id(st.incr()))
        .skip(symbol(":"))
        .and(parse_typ(st.incr()))
        .map(|((v, id), t)| typ_field::colon(v, id, t))
        .or(parse_id(st.incr())
            .and(optional(angles(sep_by(
                parse_typ_bind(st.incr()),
                symbol(","),
            ))))
            .and(parse_typ_nullary(st.incr()))
            .skip(symbol(":"))
            .and(parse_typ(st.incr()))
            .map(|(((id, binds), nullary), typ)| typ_field::field {
                id,
                binds,
                nullary,
                typ,
            }))
}

parser! {
    fn parse_typ_field['a](st: State)(&'a str) -> typ_field
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

pub fn parse_typ_tag_<'a>(st: State) -> impl Parser<&'a str, Output = typ_tag> {
    symbol("#")
        .with(parse_id(st.incr()))
        .and(optional(symbol(":").with(parse_typ(st.incr()))))
        .map(|(tag, typ)| typ_tag {
            tag,
            typ: typ.map(Box::new),
        })
}

parser! {
    fn parse_typ_tag['a](st: State)(&'a str) -> typ_tag
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

pub fn parse_typ_bind_<'a>(st: State) -> impl Parser<&'a str, Output = typ_bind> {
    struct_parser! {
        typ_bind {
            id: parse_id(st.incr()),
            typ: optional(keyword("<:").with(parse_typ(st.incr())))
        }
    }
}

parser! {
    fn parse_typ_bind['a](st: State)(&'a str) -> typ_bind
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

pub fn parse_lit_<'a>(st: State) -> impl Parser<&'a str, Output = lit> {
    st.indent();
    println!("parse_lit");
    keyword("null")
        .with(value(lit::null))
        .or(parse_bool(st.incr()).map(lit::bool_))
        .or(parse_float(st.incr()).map(lit::float))
        .or(parse_nat(st.incr()).map(lit::nat))
        .or(parse_char(st.incr()).map(lit::char_))
        .or(parse_text(st.incr()).map(lit::text))
}

parser! {
    fn parse_lit['a](st: State)(&'a str) -> lit
    where []
    {
        parse_lit_(*st)
    }
}

/*
#[test]
fn test_parse_lit() {
    pretty_assertions::assert_eq!(
        Ok((lit::null, "hello")),
        parse_lit(State::new()).parse("null hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((lit::bool_(true), "hello")),
        parse_lit(State::new()).parse("true hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((lit::nat(Integer::from(123)), "hello")),
        parse_lit(State::new()).parse("123 hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((lit::float(Float::with_val(53, 1.3)), "hello")),
        parse_lit(State::new()).parse("1.3 hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((lit::char_('c'), "hello")),
        parse_lit(State::new()).parse("'c' hello"),
    );
    pretty_assertions::assert_eq!(
        Ok((lit::text("string".to_string()), "hello")),
        parse_lit(State::new()).parse("\"string\" hello"),
    );
}
*/

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

pub fn parse_unop_<'a>(st: State) -> impl Parser<&'a str, Output = unop> {
    st.indent();
    println!("parse_unop");
    choice([
        attempt(operator("+")).with(value(unop::plus)),
        attempt(operator("-")).with(value(unop::minus)),
        attempt(operator("^")).with(value(unop::caret)),
    ])
}

parser! {
    fn parse_unop['a](st: State)(&'a str) -> unop
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

pub fn parse_binop_<'a>(st: State) -> impl Parser<&'a str, Output = binop> {
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
    fn parse_binop['a](st: State)(&'a str) -> binop
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

pub fn parse_relop_<'a>(st: State) -> impl Parser<&'a str, Output = relop> {
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
    fn parse_relop['a](st: State)(&'a str) -> relop
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

pub fn parse_unassign_<'a>(st: State) -> impl Parser<&'a str, Output = unassign> {
    st.indent();
    println!("parse_unassign");
    choice([
        attempt(operator("+=")).with(value(unassign::add)),
        attempt(operator("-=")).with(value(unassign::sub)),
        attempt(operator("^=")).with(value(unassign::caret)),
    ])
}

parser! {
    fn parse_unassign['a](st: State)(&'a str) -> unassign
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

pub fn parse_binassign_<'a>(st: State) -> impl Parser<&'a str, Output = binassign> {
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
    fn parse_binassign['a](st: State)(&'a str) -> binassign
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

pub fn parse_exp_obj_<'a>(st: State) -> impl Parser<&'a str, Output = exp_obj> {
    st.indent();
    println!("parse_exp_obj");
    braces(sep_end_by(parse_exp_field(st.incr()), symbol(";")).map(|fields| exp_obj { fields }))
}

parser! {
    fn parse_exp_obj['a](st: State)(&'a str) -> exp_obj
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

pub fn parse_exp_plain_<'a>(st: State) -> impl Parser<&'a str, Output = exp_plain> {
    st.indent();
    println!("parse_exp_plain");
    parse_lit(st.incr())
        .map(exp_plain::lit)
        .or(parens(sep_by(parse_exp(st.incr()), symbol(","))).map(exp_plain::exp))
}

parser! {
    fn parse_exp_plain['a](st: State)(&'a str) -> exp_plain
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

pub fn parse_exp_nullary_<'a>(st: State) -> impl Parser<&'a str, Output = exp_nullary> {
    st.indent();
    println!("parse_exp_nullary");
    parse_exp_obj(st.incr())
        .map(exp_nullary::obj)
        .or(parse_exp_plain(st.incr()).map(exp_nullary::plain))
        .or(parse_id(st.incr()).map(exp_nullary::id))
}

parser! {
    fn parse_exp_nullary['a](st: State)(&'a str) -> exp_nullary
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

#[derive(Clone, Debug, PartialEq)]
pub enum exp_post__post {
    bracket(exp),
    dot(nat),
    id(id),
    typ(Option<list<typ>>, exp_nullary),
    bang,
}

pub fn parse_exp_post_<'a>(st: State) -> impl Parser<&'a str, Output = exp_post> {
    st.indent();
    println!("parse_exp_post");
    brackets(
        parse_var_opt(sep_by(parse_exp_nonvar(st.incr()), symbol(",")))
            .map(|(v, e)| exp_post::nonvars(v, e)),
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
                .map(|(typ, nullary)| exp_post__post::typ(typ, nullary)))
            .or(symbol("!").with(value(exp_post__post::bang))),
    ))
    .map(|(pre, post)| match post {
        None => pre,
        Some(exp_post__post::bracket(exp)) => exp_post::bracket(Box::new(pre), exp),
        Some(exp_post__post::dot(nat)) => exp_post::dot(Box::new(pre), nat),
        Some(exp_post__post::id(id)) => exp_post::id(Box::new(pre), id),
        Some(exp_post__post::typ(typs, nullary)) => exp_post::typ(Box::new(pre), typs, nullary),
        Some(exp_post__post::bang) => exp_post::bang(Box::new(pre)),
    })
}

parser! {
    fn parse_exp_post['a](st: State)(&'a str) -> exp_post
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

pub fn parse_exp_un_<'a>(st: State) -> impl Parser<&'a str, Output = exp_un> {
    st.indent();
    println!("parse_exp_un");
    symbol("#")
        .with(parse_id(st.incr()))
        .and(optional(parse_exp_nullary(st.incr())))
        .map(|(id, nullary)| exp_un::hash(id, nullary))
        .or(symbol("?")
            .with(parse_exp_un(st.incr()))
            .map(|un| exp_un::question(Box::new(un))))
        .or(keyword("actor")
            .with(parse_exp_plain(st.incr()))
            .map(exp_un::actor))
        .or(keyword("not")
            .with(parse_exp_un(st.incr()))
            .map(|un| exp_un::not(Box::new(un))))
        .or(keyword("debug_show")
            .with(parse_exp_un(st.incr()))
            .map(|un| exp_un::debug_show(Box::new(un))))
        .or(parse_unop(st.incr())
            .and(parse_exp_un(st.incr()))
            .map(|(unop, un)| exp_un::unop(unop, Box::new(un))))
        .or(parse_unassign(st.incr())
            .and(parse_exp_un(st.incr()))
            .map(|(unop, un)| exp_un::unassign(unop, Box::new(un))))
        .or(parse_exp_post(st.incr()).map(exp_un::post))
}

parser! {
    fn parse_exp_un['a](st: State)(&'a str) -> exp_un
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

#[derive(Clone, Debug, PartialEq)]
pub enum exp_bin__post {
    binop(binop, Box<exp_bin>),
    relop(relop, Box<exp_bin>),
    and(Box<exp_bin>),
    or(Box<exp_bin>),
    colon(typ_nobin),
}

pub fn parse_exp_bin_<'a>(st: State) -> impl Parser<&'a str, Output = exp_bin> {
    st.indent();
    println!("parse_exp_bin");
    parse_exp_un(st.incr())
        .and(optional(
            parse_binop(st.incr())
                .and(parse_exp_bin(st.incr()))
                .map(|(binop, bin)| exp_bin__post::binop(binop, Box::new(bin)))
                .or(parse_relop(st.incr())
                    .and(parse_exp_bin(st.incr()))
                    .map(|(relop, bin)| exp_bin__post::relop(relop, Box::new(bin))))
                .or(keyword("and")
                    .with(parse_exp_bin(st.incr()))
                    .map(|r| exp_bin__post::and(Box::new(r))))
                .or(keyword("or")
                    .with(parse_exp_bin(st.incr()))
                    .map(|r| exp_bin__post::or(Box::new(r))))
                .or(symbol(":")
                    .with(parse_typ_nobin(st.incr()))
                    .map(exp_bin__post::colon)),
        ))
        .map(|(un, post)| {
            let pre = Box::new(exp_bin::un(un));
            match post {
                None => *pre,
                Some(exp_bin__post::binop(op, x)) => exp_bin::binop(pre, op, x),
                Some(exp_bin__post::relop(op, x)) => exp_bin::relop(pre, op, x),
                Some(exp_bin__post::and(x)) => exp_bin::and(pre, x),
                Some(exp_bin__post::or(x)) => exp_bin::or(pre, x),
                Some(exp_bin__post::colon(x)) => exp_bin::colon(pre, x),
            }
        })
}

parser! {
    fn parse_exp_bin['a](st: State)(&'a str) -> exp_bin
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

pub fn parse_exp_nondec_<'a>(st: State) -> impl Parser<&'a str, Output = exp_nondec> {
    st.indent();
    println!("parse_exp_nondec");
    parse_exp_bin(st.incr())
        .and(optional(
            keyword(":=")
                .with(parse_exp(st.incr()))
                .map(Either::Left)
                .or(parse_binassign(st.incr())
                    .and(parse_exp(st.incr()))
                    .map(Either::Right)),
        ))
        .map(|(bin, v)| match v {
            None => exp_nondec::bin(bin),
            Some(Either::Left(exp)) => exp_nondec::assign(bin, exp),
            Some(Either::Right((assn, exp))) => exp_nondec::binassign(bin, assn, exp),
        })
        .or(keyword("return").with(optional(parse_exp(st.incr())).map(exp_nondec::return_)))
        .or(keyword("async").with(parse_exp_nest(st.incr()).map(exp_nondec::async_)))
        .or(keyword("await").with(parse_exp_nest(st.incr()).map(exp_nondec::await_)))
        .or(keyword("assert").with(parse_exp_nest(st.incr()).map(exp_nondec::assert)))
        .or(keyword("label")
            .with(parse_id(st.incr()))
            .and(optional(symbol(":").with(parse_typ(st.incr()))))
            .and(parse_exp_nest(st.incr()))
            .map(|((id, ty), body)| exp_nondec::label(id, ty, body)))
        .or(keyword("break")
            .with(parse_id(st.incr()))
            .and(optional(parse_exp_nullary(st.incr())))
            .map(|(id, nullary)| exp_nondec::break_(id, nullary)))
        .or(keyword("continue").with(parse_id(st.incr()).map(exp_nondec::continue_)))
        .or(keyword("debug").with(parse_exp_nest(st.incr()).map(exp_nondec::debug)))
        .or(keyword("if")
            .with(parse_exp_nullary(st.incr()))
            .and(parse_exp_nest(st.incr()))
            .and(optional(keyword("else").with(parse_exp_nest(st.incr()))))
            .map(|((cond, then), melse_)| match melse_ {
                None => exp_nondec::if_(cond, then),
                Some(else_) => exp_nondec::if_else(cond, then, else_),
            }))
        .or(keyword("try")
            .with(parse_exp_nest(st.incr()))
            .and(parse_catch(st.incr()))
            .map(|(nest, catch)| exp_nondec::try_(nest, catch)))
        .or(keyword("throw").with(parse_exp_nest(st.incr()).map(exp_nondec::throw)))
        .or(keyword("switch")
            .with(parse_exp_nullary(st.incr()))
            .and(braces(sep_end_by(parse_case(st.incr()), symbol(";"))))
            .map(|(scrutinee, cases)| exp_nondec::switch(scrutinee, cases)))
        .or(keyword("while")
            .with(parse_exp_nullary(st.incr()))
            .and(parse_exp_nest(st.incr()))
            .map(|(cond, body)| exp_nondec::while_(cond, body)))
        .or(keyword("loop")
            .with(parse_exp_nest(st.incr()))
            .and(optional(keyword("while").with(parse_exp_nest(st.incr()))))
            .map(|(cond, mbody)| match mbody {
                None => exp_nondec::loop_(cond),
                Some(body) => exp_nondec::loop_while(cond, body),
            }))
        .or(keyword("for")
            .with(parens(
                parse_pat(st.incr())
                    .skip(keyword("in"))
                    .and(parse_exp(st.incr())),
            ))
            .and(parse_exp_nest(st.incr()))
            .map(|((pat, exp), body)| exp_nondec::for_(pat, exp, body)))
        .or(keyword("ignore").with(parse_exp_nest(st.incr()).map(exp_nondec::ignore)))
        .or(keyword("do").with(
            symbol("?")
                .with(parse_block(st.incr()).map(exp_nondec::do_question))
                .or(parse_block(st.incr()).map(exp_nondec::do_)),
        ))
}

parser! {
    fn parse_exp_nondec['a](st: State)(&'a str) -> exp_nondec
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

pub fn parse_exp_nonvar_<'a>(st: State) -> impl Parser<&'a str, Output = exp_nonvar> {
    st.indent();
    println!("parse_exp_nonvar");
    parse_exp_nondec(st.incr())
        .map(exp_nonvar::exp_nondec)
        .or(parse_dec_nonvar(st.incr()).map(exp_nonvar::dec_nonvar))
}

parser! {
    fn parse_exp_nonvar['a](st: State)(&'a str) -> exp_nonvar
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

pub fn parse_exp_<'a>(st: State) -> impl Parser<&'a str, Output = exp> {
    st.indent();
    println!("parse_exp");
    parse_exp_nonvar(st.incr())
        .map(|e| exp::exp_nonvar(Box::new(e)))
        .or(parse_dec_var(st.incr()).map(|d| exp::dec_var(Box::new(d))))
}

parser! {
    fn parse_exp['a](st: State)(&'a str) -> exp
    where []
    {
        parse_exp_(*st)
    }
}

/*
#[test]
fn test_parse_exp() {
    println!("exp = {:#?}", parse_exp(State::new()).parse("1"));
    // println!("exp = {:#?}", parse_exp().parse("1 + 3"));
    // println!("exp = {:#?}", parse_exp().parse("(1 + 3) * 4 == 72 / 2"));
    // pretty_assertions::assert_eq!(
    //     Ok((todo!(), "")),
    //     parse_exp().parse("(1 + 3) * 4 == 72 / 2"),
    // );
}
*/

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

pub fn parse_exp_nest_<'a>(st: State) -> impl Parser<&'a str, Output = exp_nest> {
    parse_block(st.incr())
        .map(exp_nest::block)
        .or(parse_exp(st.incr()).map(exp_nest::exp))
}

parser! {
    fn parse_exp_nest['a](st: State)(&'a str) -> exp_nest
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

pub fn parse_block_<'a>(st: State) -> impl Parser<&'a str, Output = block> {
    braces(sep_end_by(parse_dec(st.incr()), symbol(";"))).map(|decs| block { decs })
}

parser! {
    fn parse_block['a](st: State)(&'a str) -> block
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

pub fn parse_case_<'a>(st: State) -> impl Parser<&'a str, Output = case> {
    keyword("case")
        .with(parse_pat_nullary(st.incr()))
        .and(parse_exp_nest(st.incr()))
        .map(|(nullary, nest)| case { nullary, nest })
}

parser! {
    fn parse_case['a](st: State)(&'a str) -> case
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

pub fn parse_catch_<'a>(st: State) -> impl Parser<&'a str, Output = catch> {
    keyword("catch")
        .with(parse_pat_nullary(st.incr()))
        .and(parse_exp_nest(st.incr()))
        .map(|(nullary, nest)| catch { nullary, nest })
}

parser! {
    fn parse_catch['a](st: State)(&'a str) -> catch
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

pub fn parse_exp_field_<'a>(st: State) -> impl Parser<&'a str, Output = exp_field> {
    parse_var_opt(parse_id(st.incr()))
        .and(optional(symbol(":").with(parse_typ(st.incr()))))
        .and(optional(symbol("=").with(parse_exp(st.incr()))))
        .map(|(((var, id), typ), exp)| exp_field { var, id, typ, exp })
}

parser! {
    fn parse_exp_field['a](st: State)(&'a str) -> exp_field
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

pub fn parse_dec_field_<'a>(st: State) -> impl Parser<&'a str, Output = dec_field> {
    struct_parser! {
        dec_field {
            vis: optional(parse_vis(st.incr())),
            stab: optional(parse_stab(st.incr())),
            dec: parse_dec(st.incr())
        }
    }
}

parser! {
    fn parse_dec_field['a](st: State)(&'a str) -> dec_field
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

pub fn parse_vis_<'a>(st: State) -> impl Parser<&'a str, Output = vis> {
    st.indent();
    println!("parse_vis");
    choice([
        attempt(keyword("private")).with(value(vis::private)),
        attempt(keyword("public")).with(value(vis::public)),
        attempt(keyword("system")).with(value(vis::system)),
    ])
}

parser! {
    fn parse_vis['a](st: State)(&'a str) -> vis
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

pub fn parse_stab_<'a>(st: State) -> impl Parser<&'a str, Output = stab> {
    st.indent();
    println!("parse_stab");
    choice([
        attempt(keyword("flexible")).with(value(stab::flexible)),
        attempt(keyword("stable")).with(value(stab::stable)),
    ])
}

parser! {
    fn parse_stab['a](st: State)(&'a str) -> stab
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

pub fn parse_pat_plain_<'a>(st: State) -> impl Parser<&'a str, Output = pat_plain> {
    symbol("_")
        .with(value(pat_plain::underscore))
        .or(parse_id(st.incr()).map(pat_plain::id))
        .or(parse_lit(st.incr()).map(pat_plain::lit))
        .or(parens(sep_by(parse_pat_bin(st.incr()), symbol(","))).map(pat_plain::bins))
}

parser! {
    fn parse_pat_plain['a](st: State)(&'a str) -> pat_plain
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

pub fn parse_pat_nullary_<'a>(st: State) -> impl Parser<&'a str, Output = pat_nullary> {
    braces(sep_end_by(parse_pat_field(st.incr()), symbol(";")))
        .map(pat_nullary::field)
        .or(parse_pat_plain(st.incr()).map(pat_nullary::plain))
}

parser! {
    fn parse_pat_nullary['a](st: State)(&'a str) -> pat_nullary
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

pub fn parse_pat_un_<'a>(st: State) -> impl Parser<&'a str, Output = pat_un> {
    symbol("#")
        .with(parse_id(st.incr()))
        .and(optional(parse_pat_nullary(st.incr())))
        .map(|(id, n)| pat_un::hash(id, n))
        .or(symbol("?")
            .with(parse_pat_un(st.incr()))
            .map(|u| pat_un::question(Box::new(u))))
        .or(parse_unop(st.incr())
            .and(parse_lit(st.incr()))
            .map(|(u, l)| pat_un::unop_lit(u, l)))
        .or(parse_pat_nullary(st.incr()).map(pat_un::nullary))
}

parser! {
    fn parse_pat_un['a](st: State)(&'a str) -> pat_un
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

#[derive(Clone, Debug, PartialEq)]
pub enum pat_bin__post {
    or(Box<pat_bin>),
    colon(typ),
}

pub fn parse_pat_bin_<'a>(st: State) -> impl Parser<&'a str, Output = pat_bin> {
    parse_pat_un(st.incr())
        .and(optional(
            keyword("or")
                .with(parse_pat_bin(st.incr()))
                .map(|b| pat_bin__post::or(Box::new(b)))
                .or(symbol(":")
                    .with(parse_typ(st.incr()))
                    .map(pat_bin__post::colon)),
        ))
        .map(|(un, post)| {
            let pre = Box::new(pat_bin::un(un));
            match post {
                None => *pre,
                Some(pat_bin__post::or(x)) => pat_bin::or(pre, x),
                Some(pat_bin__post::colon(x)) => pat_bin::colon(pre, x),
            }
        })
}

parser! {
    fn parse_pat_bin['a](st: State)(&'a str) -> pat_bin
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

pub fn parse_pat_<'a>(st: State) -> impl Parser<&'a str, Output = pat> {
    struct_parser! {
        pat {
            bin: parse_pat_bin(st.incr())
        }
    }
}

parser! {
    fn parse_pat['a](st: State)(&'a str) -> pat
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

pub fn parse_pat_field_<'a>(st: State) -> impl Parser<&'a str, Output = pat_field> {
    struct_parser! {
        pat_field {
            id: parse_id(st.incr()),
            typ: optional(symbol(":").with(parse_typ(st.incr()))),
            pat: optional(symbol("=").with(parse_pat(st.incr())))
        }
    }
}

parser! {
    fn parse_pat_field['a](st: State)(&'a str) -> pat_field
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

pub fn parse_dec_var_<'a>(st: State) -> impl Parser<&'a str, Output = dec_var> {
    struct_parser! {
        dec_var {
            id: keyword("var").with(parse_id(st.incr())),
            typ: optional(symbol(":").with(parse_typ(st.incr()))),
            exp: symbol("=").with(parse_exp(st.incr())),
        }
    }
}

parser! {
    fn parse_dec_var['a](st: State)(&'a str) -> dec_var
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

pub fn parse_dec_nonvar_<'a>(st: State) -> impl Parser<&'a str, Output = dec_nonvar> {
    keyword("let")
        .with(parse_pat(st.incr()))
        .and(symbol("=").with(parse_exp(st.incr())))
        .map(|(pat, exp)| dec_nonvar::let_dec(pat, Box::new(exp)))
        .or(keyword("type")
            .with(parse_id(st.incr()))
            .and(optional(angles(sep_by(
                parse_typ_bind(st.incr()),
                symbol(","),
            ))))
            .skip(symbol("="))
            .and(parse_typ(st.incr()))
            .map(|((id, binds), typ)| dec_nonvar::type_dec(id, binds, typ)))
        .or(parse_obj_sort(st.incr())
            .and(optional(parse_id(st.incr())))
            .and(optional(symbol("=")))
            .and(parse_obj_body(st.incr()))
            .map(|(((sort, id), eq), body)| dec_nonvar::obj_dec(sort, id, eq.is_some(), body)))
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
                .and(parse_func_body(st.incr()))
                .map(
                    |(((((shared_pat, id), binds), plain), typ), body)| dec_nonvar::func {
                        shared_pat,
                        id,
                        binds,
                        plain,
                        typ,
                        body,
                    },
                ),
        ))
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
            .map(
                |((((((shared_pat, sort), id), binds), plain), typ), body)| dec_nonvar::class {
                    shared_pat,
                    sort,
                    id,
                    binds,
                    plain,
                    typ,
                    body,
                },
            ))
}

parser! {
    fn parse_dec_nonvar['a](st: State)(&'a str) -> dec_nonvar
    where []
    {
        parse_dec_nonvar_(*st)
    }
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

pub fn parse_dec_<'a>(st: State) -> impl Parser<&'a str, Output = dec> {
    parse_dec_var(st.incr())
        .map(dec::var)
        .or(parse_dec_nonvar(st.incr()).map(dec::nonvar))
        .or(parse_exp_nondec(st.incr()).map(dec::nondec))
}

parser! {
    fn parse_dec['a](st: State)(&'a str) -> dec
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

pub fn parse_func_body_<'a>(st: State) -> impl Parser<&'a str, Output = func_body> {
    symbol("=")
        .with(parse_exp(st.incr()).map(func_body::exp))
        .or(parse_block(st.incr()).map(func_body::block))
}

parser! {
    fn parse_func_body['a](st: State)(&'a str) -> func_body
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

pub fn parse_obj_body_<'a>(st: State) -> impl Parser<&'a str, Output = obj_body> {
    struct_parser! {
        obj_body {
            fields: braces(sep_end_by(parse_dec_field(st.incr()), symbol(";")))
        }
    }
}

parser! {
    fn parse_obj_body['a](st: State)(&'a str) -> obj_body
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

pub fn parse_class_body_<'a>(st: State) -> impl Parser<&'a str, Output = class_body> {
    optional(symbol("=").with(optional(parse_id(st.incr()))))
        .and(parse_obj_body(st.incr()))
        .map(|(id, body)| class_body {
            equal: id.is_some(),
            id: id.flatten(),
            body,
        })
}

parser! {
    fn parse_class_body['a](st: State)(&'a str) -> class_body
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

pub fn parse_imp_<'a>(st: State) -> impl Parser<&'a str, Output = imp> {
    struct_parser! {
        imp {
            _: keyword("import"),
            id: optional(parse_id(st.incr())),
            equal: optional(symbol("=")).map(|x| x.is_some()),
            name: parse_text(st.incr()),
        }
    }
}

parser! {
    fn parse_imp['a](st: State)(&'a str) -> imp
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

pub fn parse_prog_<'a>(st: State) -> impl Parser<&'a str, Output = prog> {
    struct_parser! {
        prog {
            imps: sep_end_by(parse_imp(st.incr()), symbol(";")),
            decs: sep_end_by(parse_dec(st.incr()), symbol(";"))
        }
    }
}

parser! {
    fn parse_prog['a](st: State)(&'a str) -> prog
    where []
    {
        parse_prog_(*st)
    }
}
