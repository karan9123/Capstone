// #![allow(warnings)]
use std::rc::Rc;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum BoolExpr {
    Var(String),
    Const(bool),
    Not(Rc<BoolExpr>),
    And(Rc<BoolExpr>, Rc<BoolExpr>),
    Or(Rc<BoolExpr>, Rc<BoolExpr>),
}

use BoolExpr::*;

pub fn parse_bool_expr(input: &str) -> Result<BoolExpr, String> {
    let tokens: Vec<&str> = input.split_whitespace().collect();
    parse_expr(&tokens, 0).map(|(expr, _)| expr)
}

fn parse_expr(tokens: &[&str], mut pos: usize) -> Result<(BoolExpr, usize), String> {
    match tokens.get(pos) {
        Some(&"Var") => {
            pos += 1;
            let name = tokens
                .get(pos)
                .ok_or("Unexpected end of input")?
                .to_string();
            Ok((Var(name), pos + 1))
        }
        Some(&"Const(true)") => Ok((Const(true), pos + 1)),
        Some(&"Const(true),") => Ok((Const(true), pos + 1)),
        Some(&"Const(false)") => Ok((Const(false), pos + 1)),
        Some(&"Const(false),") => Ok((Const(false), pos + 1)),
        Some(&"Not") => {
            let (expr, new_pos) = parse_expr(tokens, pos + 1)?;
            Ok((Not(Rc::new(expr)), new_pos))
        }
        Some(&"And") => {
            let (left, pos) = parse_expr(tokens, pos + 1)?;
            let (right, new_pos) = parse_expr(tokens, pos)?;
            Ok((And(Rc::new(left), Rc::new(right)), new_pos))
        }
        Some(&"Or") => {
            let (left, pos) = parse_expr(tokens, pos + 1)?;
            let (right, new_pos) = parse_expr(tokens, pos)?;
            Ok((Or(Rc::new(left), Rc::new(right)), new_pos))
        }
        Some(&",") => parse_expr(tokens, pos + 1),
        _ => Err("Unexpected token".to_string()),
    }
}
