use hash_cons::Hc;

#[derive(Hash, PartialEq, Eq, Clone)]
enum TerminalNode {
    True,
    False,
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct NonTerminalNode {
    pub var: u32,
    pub low: Hc<BddNode>,
    pub high: Hc<BddNode>,
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub enum BddNode {
    Terminal(TerminalNode),
    NonTerminal(NonTerminalNode),
}

impl BddNode {
    pub fn new_false_terminal() -> BddNode {
        BddNode::Terminal(TerminalNode::False)
    }

    pub fn new_true_terminal() -> BddNode {
        BddNode::Terminal(TerminalNode::True)
    }

    pub fn new_non_terminal(var: u32, low: Hc<BddNode>, high: Hc<BddNode>) -> BddNode {
        BddNode::NonTerminal(NonTerminalNode { var, low, high })
    }

    pub fn _is_terminal(&self) -> bool {
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
pub struct Bdd {
    pub root: Hc<BddNode>,
}

impl Bdd {
    pub fn new(root: Hc<BddNode>) -> Bdd {
        Bdd { root: root }
    }
}
