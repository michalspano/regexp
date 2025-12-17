---
theme           : default
_class          : lead
paginate        : true
color           : #fff
backgroundColor : #2e3440
marp            : true
---

![bg left:40% 60%](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fcdn.creazilla.com%2Ficons%2F3210593%2Fregex-icon-md.png&f=1&nofb=1&ipt=9377d59d98594a1b55c189f64cf627366b05632204e475da6388414747ba2546)

# TDA452 Functional programming

### Project - Regex Parsing

Chalmers University of Technology
$\cdot$ Michal Spano, Philipp-Immanuel Holst

---

## What Is This?

- A regex **parser \& interpreter** for a minimal language (subset of `ISO XXX`):
    ```
    r1,r2 ::= ε | . | a | r1r2 | r1* | r1+
    ```
    - *Note*: difficulty parsing  `r1 | r2`
- Matches input text on regex in a *repl* (CLI)
    ```txt
    λ> main
    >> (ab.)*ab
    >? abxabyab
    Checking "abxabyab" on "(ab.)*ab" results in: True
    ...
    ```
- Library for further extension
- Visualizer for underlying datastructure using a markup language `dot`

---

# General structure of our project


- Our project reads, *parses* and *evaluates* a regular expression
- The parsing and evaluation of a regular expression are the two main obstacles
    - Parsing: Tokenizing the string, given a correct syntax
    - Evaluation: Building and running a state machine that is equivalent to the given string


**Our Goal**: An interface like `check :: Pattern -> String -> Bool` that checks if a given input-string matches a given regex-pattern

--- 
# Parsing of a regular expression
## Syntax
- Our regex-parser allows the following syntax:
    - Literals (alphanumerical)
    - Epsilon (the empty string)
    - Kleene 
    - Plus
    - Concatenation
- A valid expression could be `(ab.)*ab`
- The subset avoids infix-operators like *or* `a|b`

--- 
# Parsing of a regular expression
## Internal representation
- The strings are parsed via the `Parsing.hs` library
- Module: `Parser` exposes the interface `parseReg :: String -> Maybe Regex`
- The parsing results in a tokenization via the recursive datatype `Regex` 
```haskell 
data Regex = 
      Epsilon
    | Literal Char
    | Kleene Regex
    | Concat Regex Regex
    | Dot
```

- Earlier problems of left-recursion are avoided by using specific parsing functions for each tokens and disjoining them

--- 

# Evaluating a `Regex`
## Generalized process
- Following the tokenization of a string into a `Regex`, the tokens are compiled into a deterministic finite automaton (*DFA*)
- This process requires different steps:
1. Creating an (Epsilon-) NFA from a Regex
2. creating a DFA from an (Epsilon-) NFA
2.1 Creating a cleaned up DFA from a DFA
3. Running an input string on a DFA

--- 
# Evaluating a `Regex`
## General datastructures 
- State machines are represented as graphs with entry points, accepting states and labeled transitions
- States are internally represented as integers, transitions are maps from one state to another given some character-literal
- Datatypes slightly differ between different automatons according to their specification
- Datatypes are factored out to a module `DataTypes`
- Heavy use of self-made auxiliary data structure `DefaultMap`

---

# Evaluating a `Regex`
## Creating an (Epsilon-) NFA
- Module `NFA` that exposes an interface `fromRegex :: Regex -> NFA`
- An `NFA` is a graph with exactly one entry point, one accepting state and with labeled transitions:
    - `data NFA = NFA State State NFATransitions`
    - `type NFATransitions = DefaultMap State (DefaultMap Char [State])`
- The `NFA` is created via the *Thompson-construction* 
- The result is an Epsilon-NFA equivalent to the regular expression

--- 

# Evaluating a `Regex`
## Creating a DFA
- A `DFA` has exactly one entry point but a list of accepting states

    - `data DFA = DFA State [State] DFATransitions`
    - `type DFATransitions = DefaultMap State (Map Char State)`
- Module `DFA` that exposes an interface `fromNFA :: NFA -> DFA`
- But: `fromNFA = flattenToDFA . fromNFAMulti `
- All the work is done via the *powerset-construction* in `fromNFAMulti`

---
# Evaluating a `Regex`
## Creating a DFA: `PowerSetDFA`

- The *powerset-construction* results in an intermediate DFA that we named `PowerSetDFA` that can be created via `fromNFAMulti`
    - `data PowerSetDFA = PowerSetDFA MultiState [MultiState] PowerSetDFATransitions`
    - `type PowerSetDFATransitions = DefaultMap MultiState (Map Char MultiState)`
- Difference: A `MultiState` represents a set of states, as per *powerset-construction*
- Which is why it is flattened into a regular `DFA`


---
# Evaluating a `Regex`
## Evaluating a DFA with input
Reminder: `check :: Pattern -> String -> Bool`

```haskell
match :: RegPattern -> String -> Bool
match p s = case P.parseReg p of
    Just reg -> match1 reg s
    _        -> False

match1 :: P.Regex -> String -> Bool
match1 pattern input = let dfa = (DFA.fromNFA . NFA.fromRegex) pattern in 
    check dfa input
```
---
# Evaluating a `Regex`
## Evaluating a DFA with input
Reminder: `check :: Pattern -> String -> Bool`

- `check` runs a simple traversal on our `DFA` and checks whether it can read the whole input and by that end up in an accepting state
- Since we handle `.` explicitly in our datastructures, we have to branch out whenever we have the choice between a dot and a literal
- Bonus: additional checking function `checkWithTrace` that, using a state, additionally traces the route that was taken when evaluating the `DFA`
    - `checkWithTrace :: DFA -> String -> (Bool, [(State, Bool)]) `
- This will be useful in the future!

--- 


# **Example**:
# Let's run: `a*.+b`


--- 

# Why is this a nice project?

- Very algorithm-heavy project
- Requires a lot of monadic action (`IO`, `State`, `Maybe`) to function elegantly
- Lots of datastructures used (Custom recursive datatypes, self made datastructures like `DefaultMap`, Records to use as state)
- Was very funny
- Kind of related to our major 
- Easily extendable in the future

---

## Future Plans

- Package the source via `Cabal`
- Extend testing to `QuickCheck`
- Extend the language (e.g., `reg1 | reg2`) + update parsing
- Build a frontend (website) - embed graphs
    - Animation for step-by-step traversal
    - Talk to server (this module); will emit graph resources
    - Useful for intricate patterns (e.g. for students)

---

![bg](./frontend_demo.png)

---

# Thanks & Questions?