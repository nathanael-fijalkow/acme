# ACME: Automata with Counters, Monoids and Equivalence

ACME is a tool implementing algebraic techniques to solve decision problems from automata theory.
The core generic algorithm takes as input an automaton and computes its stabilization monoid,
which is a generalization of its transition monoid.

ACME has been written in OCaml, by [Nathana&euml;l Fijalkow](https://nathanael-fijalkow.github.io/)
and [Denis Kuperberg](https://perso.ens-lyon.fr/denis.kuperberg/).

ACME uses <a href=http://www.graphviz.org/>graphviz</a> to visualize the automata and monoids produced.

* [ATVA'2014 tool paper](https://github.com/nathanael-fijalkow/acme/blob/master/ATVA_2014_ACME.pdf)
* [Manual (PDF)](https://github.com/nathanael-fijalkow/acme/tree/master/Manual/main.pdf).

### Installation and Use

* Linux: Type "make linux" to compile, it produces the executable acme.
* Windows: You need the "make" utility package, which you can get <a href=http://gnuwin32.sourceforge.net/packages/make.htm>here</a>. Use "make win" to compile, it produces acme.exe.

### Features

* **-sm**: Computes the stabilization monoid of a B-automaton and minimizes it.
* **-equ**: Checks whether two B-automata are equivalent.
* **-mma**: Runs the Markov Monoid algorithm on a probabilistic automaton.
* **-fpp**: Checks whether a classical automaton has the finite power property, i.e. whether there exists n such that L^* = L^0 + L^1 + ... + L^n.
* **-dotty**: Produces a file output.gv (DOT format) and output.ps, which can be visualized using <a href=http://www.graphviz.org/>graphviz</a>.

