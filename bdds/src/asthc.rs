use hash_cons::{Hc, HcTable};

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum BoolExprHc {
    Var(String),
    Const(bool),
    Not(Hc<BoolExprHc>),
    And(Hc<BoolExprHc>, Hc<BoolExprHc>),
    Or(Hc<BoolExprHc>, Hc<BoolExprHc>),
}

use BoolExprHc::*;

pub fn parse_hc_bool_expr(input: &str, table: HcTable<BoolExprHc>) -> Result<BoolExprHc, String> {
    let tokens: Vec<&str> = input.split_whitespace().collect();
    parse_expr(&tokens, 0, table).map(|(expr, _)| expr)
}

fn parse_expr(
    tokens: &[&str],
    mut pos: usize,
    table: HcTable<BoolExprHc>,
) -> Result<(BoolExprHc, usize), String> {
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
            let (expr, new_pos) = parse_expr(tokens, pos + 1, table.clone())?;
            Ok((Not(table.clone().hashcons(expr)), new_pos))
        }
        Some(&"And") => {
            let (left, pos) = parse_expr(tokens, pos + 1, table.clone())?;
            let (right, new_pos) = parse_expr(tokens, pos, table.clone())?;
            Ok((
                And(table.clone().hashcons(left), table.hashcons(right)),
                new_pos,
            ))
        }
        Some(&"Or") => {
            let (left, pos) = parse_expr(tokens, pos + 1, table.clone())?;
            let (right, new_pos) = parse_expr(tokens, pos, table.clone())?;
            Ok((
                Or(table.clone().hashcons(left), table.hashcons(right)),
                new_pos,
            ))
        }
        Some(&",") => parse_expr(tokens, pos + 1, table),
        _ => Err("Unexpected token".to_string()),
    }
}
