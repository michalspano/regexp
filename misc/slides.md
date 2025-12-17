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
$\cdot$ Michal Spano, Phillip-Immanuel Holst

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

TODO

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

# Thanks