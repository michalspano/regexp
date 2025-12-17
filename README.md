## Regex Parser

This repository features a minimal regex parser \& pattern matcher. The regex is
translated to a DFA (deterministic finite automaton) which is then traversed to
find if a string matches the (regex) pattern.

## Installation

The source code contains two modules: `Regex` and `Testing`. Both feature `main`
functions:
1. `Regex.main` will ask the user for a pattern and a string to be matched;
it'll evaluate them and show the result.
> A single pass will create `nfa.dot`, `powerSetDFA.dot`, and `dfa.dot`,
> featuring the automata in the [dot](https://graphviz.org/docs/layouts/dot/)
> syntax. These can be then transpiled to e.g. `PNG`, `SVG`.
2. `Testing.main` will run predefined test suites on the implementation.

Note: `Regex.main` requires `containers`, `mtl`. Proceed with:
```ghci
:set -package containers
:set -package mtl
```

***

**TODO**: use `Cabal` to package this project \& tidy up the structure.