mod parser;

use hash_cons::{Hc, HcTable};
use parser::{parse_boolean_expression, AST};

#[derive(Hash, PartialEq, Eq, Clone)]
enum TerminalNode {
    True,
    False,
}

#[derive(Hash, PartialEq, Eq, Clone)]
struct NonTerminalNode {
    var: u32,
    low: Hc<BddNode>,
    high: Hc<BddNode>,
}

#[derive(Hash, PartialEq, Eq, Clone)]
enum BddNode {
    Terminal(TerminalNode),
    NonTerminal(NonTerminalNode),
}

impl BddNode {
    fn new_false_terminal() -> BddNode {
        BddNode::Terminal(TerminalNode::False)
    }

    fn new_true_terminal() -> BddNode {
        BddNode::Terminal(TerminalNode::True)
    }

    fn new_non_terminal(var: u32, low: Hc<BddNode>, high: Hc<BddNode>) -> BddNode {
        BddNode::NonTerminal(NonTerminalNode { var, low, high })
    }

    fn _is_terminal(&self) -> bool {
        match self {
            BddNode::Terminal(_) => true,
            BddNode::NonTerminal(_) => false,
        }
    }
}
use std::fmt;

impl fmt::Debug for BddNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BddNode::Terminal(TerminalNode::True) => write!(f, "Terminal: True"),
            BddNode::Terminal(TerminalNode::False) => write!(f, "Terminal: False"),
            BddNode::NonTerminal(node) => {
                write!(f, "var: {}\n", node.var)?;
                write!(f, "Low: {:?}\n", node.low)?;
                write!(f, "High: {:?}", node.high)
            }
        }
    }
}

#[derive(Hash, PartialEq, Eq, Clone)]
struct Bdd {
    root: Hc<BddNode>,
}

impl Bdd {
    fn new(root: Hc<BddNode>) -> Bdd {
        Bdd { root: root }
    }
}

fn main() {
    // let table: HcTable<BddNode> = HcTable::new();

    // // to show a or b
    // let hc_false = table.hashcons(BddNode::new_false_terminal());

    // let hc_true = table.hashcons(BddNode::new_true_terminal());

    // let hc_node_b = table.hashcons(BddNode::new_non_terminal(
    //     2,
    //     hc_false.clone(),
    //     hc_true.clone(),
    // ));

    // let hc_node_a = table.hashcons(BddNode::new_non_terminal(
    //     1,
    //     hc_node_b.clone(),
    //     hc_true.clone(),
    // ));

    // let root_bdd = Bdd::new(hc_node_a);

    // println!("{:?}", root_bdd.root);

    let table: HcTable<parser::AST> = HcTable::new();
    let expression = "x AND ( y OR NOT z )";
    match parse_boolean_expression(expression, table) {
        Ok(ast) => println!("\n\n{:?}\n\n", ast),
        Err(e) => println!("Error parsing expression: {}", e),
    }
}
fn variable_to_u32(var_name: &String) -> u32 {
    // Implementation of variable_to_u32 function
    // ...
    // Return the corresponding u32 value for the variable name
    // For example, you can use a match statement to map variable names to u32 values
    match var_name {
        x if x == "x" => 0,
        y if y == "y" => 0,
        z if z == "z" => 0,
        _ => panic!("Unknown variable name: {}", var_name),
    }
}
fn ast_to_bdd(ast: parser::AST, table: &mut HcTable<BddNode>, var_order: &[String]) -> BddNode {
    if var_order.is_empty() {
        panic!("Variable order is empty");
    }

    let top_var = &var_order[0];
    let table_clone = table.clone();
    let (f_x, f_not_x) = apply_shannons_expansion(&ast, top_var, table_clone);

    let bdd_low = ast_to_bdd(f_not_x, table, &var_order[1..]);
    let bdd_high = ast_to_bdd(f_x, table, &var_order[1..]);

    let low_node = table.hashcons(bdd_low);
    let high_node = table.hashcons(bdd_high);

    BddNode::new_non_terminal(
        variable_to_u32(top_var), // Assuming a function to map variable name to u32
        low_node,
        high_node,
    )

    // Bdd::new(table.hashcons(bdd_node))
}

fn apply_shannons_expansion(
    ast: &parser::AST,
    top_var: &str,
    table: &HcTable<parser::AST>,
) -> (parser::AST, parser::AST) {
    match ast {
        parser::AST::Variable(var) if var == top_var => (parser::AST::True, parser::AST::False),
        parser::AST::Variable(_) | parser::AST::True | parser::AST::False => {
            (ast.clone(), ast.clone())
        }
        parser::AST::And(left, right) => {
            let (left_true, left_false) = apply_shannons_expansion(left, top_var, table);
            let (right_true, right_false) = apply_shannons_expansion(right, top_var, table);

            let new_left_true = table.hashcons(left_true);
            let new_right_true = table.hashcons(right_true);
            let new_left_false = table.hashcons(left_false);
            let new_right_false = table.hashcons(right_false);

            (
                parser::AST::And(new_left_true, new_right_true),
                parser::AST::And(new_left_false, new_right_false),
            )
        }
        parser::AST::Or(left, right) => {
            let (left_true, left_false) = apply_shannons_expansion(left, top_var, table);
            let (right_true, right_false) = apply_shannons_expansion(right, top_var, table);

            let new_left_true = table.hashcons(left_true);
            let new_right_true = table.hashcons(right_true);
            let new_left_false = table.hashcons(left_false);
            let new_right_false = table.hashcons(right_false);

            (
                parser::AST::And(new_left_true, new_right_true),
                parser::AST::And(new_left_false, new_right_false),
            )
        }
        parser::AST::Not(expr) => {
            let (expr_true, expr_false) = apply_shannons_expansion(expr, top_var, table);
            let new_expr_true = table.hashcons(expr_true);
            let new_expr_false = table.hashcons(expr_false);

            (
                parser::AST::Not(new_expr_true),
                parser::AST::Not(new_expr_false),
            )
        }
        parser::AST::Xor(left, right) => {
            let (left_true, left_false) = apply_shannons_expansion(left, top_var, table);
            let (right_true, right_false) = apply_shannons_expansion(right, top_var, table);

            let new_left_true = table.hashcons(left_true);
            let new_right_true = table.hashcons(right_true);
            let new_left_false = table.hashcons(left_false);
            let new_right_false = table.hashcons(right_false);

            (
                parser::AST::And(new_left_true, new_right_true),
                parser::AST::And(new_left_false, new_right_false),
            )
        }
    }
}
