
### Description

- **Definition**: BDDs are data structures used to represent `Boolean functions`.
- **Structure**: They are a type of a rooted, directed acyclic graph (DAG) consisting of nodes and edges.
- **Nodes**: There are two types of nodes – decision nodes and terminal nodes. 
	- **Decision** nodes represent `Boolean variables`.
	- **Terminal** nodes represent `Boolean values` (true or false).

### Why use BDDs?

- **Efficiency**: They can efficiently represent complex Boolean functions, especially when these functions are reducible.
- **Canonical Form**: Under a fixed [[#Variable Ordering]], every Boolean function has a unique BDD representation, making comparison and manipulation of functions straightforward.

### Basic Concepts

#### Components of BDDs

- **Variables**: Represented as `nodes` in the graph.
- **Edges**: Two edges come out of each decision node, corresponding to the cases where the variable is true or false .
- **Terminal Nodes**: Typically, two terminal nodes represent the Boolean values 0 (false) and 1 (true).

#### Variable Ordering

- The order in which variables appear from the root to the leaves.

### Operations on BDDs

#### Parsing

- A BDD Parser which will convert the string expression to a boolean function.
#### Creating BDDs

- A BDD is constructed for a given Boolean function by decomposing the function recursively based on the [[Shannon Expansion]].

#### Basic Operations

1. **AND**: Intersection of two BDDs.
2. **OR**: Union of two BDDs.
3. **NOT**: Negation of a BDD.
4. **XOR**: Exclusive disjunction of two BDDs.

#### Advanced Operations

- **Restrict**: Restricts a variable to a specific value (true/false).
- **Existential/Universal Quantification**: Used to express “there exists” or “for all” conditions over variables.

### Reduction Rules

BDDs often employ reduction rules to simplify the graph:

1. **Merge Identical Subgraphs**: Nodes that represent the same function are merged.
2. **Eliminate Redundant Nodes**: Nodes that do not affect the function (where both edges point to the same node) are removed.

### Examples

Let's consider a simple Boolean function for illustration: f(a, b, c) = a AND (b OR c).

#### Step-by-Step Construction

1. **Start with Variable `a`**: Two branches, `a = 0` (false) and `a = 1` (true).
2. **Next, Variable `b`**: Two branches from each of the `a` branches.
3. **Finally, Variable `c`**: Two branches from each `b` branch, leading to terminal nodes.

#### Reduction

- Merge any isomorphic subgraphs.
- Eliminate redundant nodes.

### Complex Logic Example

Consider a more complex function, like a majority function, which is true if and only if the majority of the inputs are true.

#### Function Definition

maj(a, b, c) = (a AND b) OR (a AND c) OR (b AND c)

#### BDD Construction

The construction will involve decomposing this function based on the variables, similar to the previous example, but with more branches and a more complex structure.

### Practical Considerations

- **Variable Ordering**: Crucial for size and efficiency; finding an optimal ordering is NP-complete.
- **Dynamic Reordering**: Dynamically changing the variable order during BDD manipulation can optimize size.
- **Model Checking**: In verifying hardware and software, BDDs are used to represent and manipulate state spaces.
- **Applications**: BDDs are widely used in formal verification and complex digital circuit design.



### Limitations

- Using `u32` to store var in `Non-terminal Node`, which means we can have maximum of 2<sup>32</sup> -1 Non-terminal Nodes.