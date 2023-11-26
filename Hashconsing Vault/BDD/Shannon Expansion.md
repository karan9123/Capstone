
**Shannon expansion** is a way to express a Boolean function based on the value of one of its variables. It decomposes a Boolean function into simpler parts. For a given Boolean function \( f(x_1, x_2, ..., x_n) and a variable ( x_i ), Shannon expansion is given by:

\[ f(x_1, x_2, ..., x_n) = (x_i \land f_{x_i=1}) \lor (\lnot x_i \land f_{x_i=0}) \]

where:
- \( f_{x_i=1} \) is the function \( f \) with \( x_i \) set to 1.
- \( f_{x_i=0} \) is the function \( f \) with \( x_i \) set to 0.

### Use of Shannon Expansion in BDDs

1. **BDD Construction**:
   - The Shannon expansion is used to recursively build a BDD.
   - Starting from the first variable in the order, the function is split into two sub-functions using Shannon expansion: one where the variable is true and one where it is false.
   - This process is repeated for each sub-function and each subsequent variable until terminal conditions are reached (variables are either 1 or 0).

2. **Node Representation**:
   - In a BDD, each non-terminal node represents a decision based on a variable \( x_i \).
   - The node has two branches: one corresponding to \( x_i = 1 \) (usually represented by a solid line) and one for \( x_i = 0 \) (represented by a dotted line).

3. **Reducing BDD Size**:
   - As the BDD is built, sub-functions that are identical are merged due to the canonical nature of BDDs. This is where the efficiency of BDDs comes in, effectively storing complex functions compactly.

### Example of Shannon Expansion in BDDs

Consider a simple Boolean function \( f(a, b) = a \lor b \).

1. **Applying Shannon Expansion to 'a'**:
   - \( f_{a=1} = 1 \lor b = 1 \) (since anything ORed with true is true)
   - \( f_{a=0} = 0 \lor b = b \)

2. **Building the BDD**:
   - Start with a node for variable 'a'.
   - Two branches from 'a': 
     - The branch for \( a = 1 \) leads directly to a terminal node 1.
     - The branch for \( a = 0 \) leads to another decision node for 'b'.
   - The node for 'b' then branches to terminal nodes: '1' if \( b = 1 \) and '0' if \( b = 0 \).