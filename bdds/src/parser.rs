use hash_cons::{Hc, HcTable};

#[derive(Debug, PartialEq, Eq, Clone)]
enum Token {
    And,
    Or,
    Not,
    Xor,
    Variable(String),
    OpenParen,
    CloseParen,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum AST {
    Variable(String),
    And(Hc<AST>, Hc<AST>),
    Or(Hc<AST>, Hc<AST>),
    Not(Hc<AST>),
    Xor(Hc<AST>, Hc<AST>),
    True,
    False,
}

fn tokenize(input: &str) -> Vec<Token> {
    input
        .split_whitespace()
        .map(|s| match s.to_uppercase().as_str() {
            "AND" => Token::And,
            "OR" => Token::Or,
            "NOT" => Token::Not,
            "XOR" => Token::Xor,
            "(" => Token::OpenParen,
            ")" => Token::CloseParen,
            var => Token::Variable(var.to_string()),
        })
        .collect()
}

// Implement the parse function
fn parse(tokens: &[Token], table: HcTable<AST>) -> Result<AST, String> {
    parse_or(&mut tokens.iter().peekable(), table)
}

// Parse OR expressions
fn parse_or(
    tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>,
    table: HcTable<AST>,
) -> Result<AST, String> {
    let mut node = parse_and(tokens, table.clone())?;

    while let Some(&&Token::Or) = tokens.peek() {
        tokens.next(); // Consume 'OR'
        let right = parse_and(tokens, table.clone())?;
        node = AST::Or(table.hashcons(node), table.hashcons(right));
    }

    Ok(node)
}

// Parse AND expressions
fn parse_and(
    tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>,
    table: HcTable<AST>,
) -> Result<AST, String> {
    let mut node = parse_xor(tokens, table.clone())?;

    while let Some(&&Token::And) = tokens.peek() {
        tokens.next(); // Consume 'AND'
        let right = parse_xor(tokens, table.clone())?;
        node = AST::And(table.hashcons(node), table.hashcons(right));
    }

    Ok(node)
}

// Parse XOR expressions
fn parse_xor(
    tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>,
    table: HcTable<AST>,
) -> Result<AST, String> {
    let mut node = parse_not(tokens, table.clone())?;

    while let Some(&&Token::Xor) = tokens.peek() {
        tokens.next(); // Consume 'XOR'
        let right = parse_not(tokens, table.clone())?;
        node = AST::Xor(table.hashcons(node), table.hashcons(right));
    }

    Ok(node)
}

// Parse NOT expressions
fn parse_not(
    tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>,
    table: HcTable<AST>,
) -> Result<AST, String> {
    let table_clone = table.clone();
    if let Some(&&Token::Not) = tokens.peek() {
        tokens.next(); // Consume 'NOT'
        let expr = parse_not(tokens, table)?;
        return Ok(AST::Not(table_clone.hashcons(expr)));
    }
    parse_primary(tokens, table.clone())
}

// Parse primary expressions (variables and parentheses)
fn parse_primary(
    tokens: &mut std::iter::Peekable<std::slice::Iter<Token>>,
    table: HcTable<AST>,
) -> Result<AST, String> {
    match tokens.next() {
        Some(&Token::Variable(ref var)) => Ok(AST::Variable(var.clone())),
        Some(&Token::OpenParen) => {
            let expr = parse_or(tokens, table);
            if tokens.next() == Some(&&Token::CloseParen) {
                expr
            } else {
                Err("Mismatched parentheses".to_string())
            }
        }
        _ => Err("Unexpected token".to_string()),
    }
}

pub fn parse_boolean_expression(input: &str, table: HcTable<AST>) -> Result<AST, String> {
    let tokens = tokenize(input);
    parse(&tokens, table)
}
