# ACME: Automata with Counters, Monoids and Equivalence

ACME is a tool implementing algebraic techniques to solve decision problems from automata theory.
The core generic algorithm takes as input an automaton and computes its stabilization monoid,
which is a generalization of its transition monoid.

ACME has been written in OCaml, by [Nathana&euml;l Fijalkow](http://games-automata-play.com/)
and [Denis Kuperberg](https://perso.ens-lyon.fr/denis.kuperberg/).

ACME uses <a href=http://www.graphviz.org/>graphviz</a> to visualize the automata and monoids produced.

See the [ATVA'2014 tool paper](https://hal.archives-ouvertes.fr/hal-02101510)

### Installation and Use

* Linux: Type "make linux" to compile, it produces the executable acme.
* Windows: You need the "make" utility package, which you can get <a href=http://gnuwin32.sourceforge.net/packages/make.htm>here</a>. Use "make win" to compile, it produces acme.exe.

### Format for input files

We describe it line by line:
* the first line is the size of the automaton
* the second line is the type of the automaton:
 > c: classical
 > p: probabilistic
 > n (int): a B-automaton with n counters
* the third line is the alphabet. Each character is a letter, they should not be separated
* the fourth line is the initial states. Each state should be separated by spaces
* the fifth line is the final states. Each state should be separated by spaces
* the next lines are the transition matrices, one for each letter in the input order.
A transition matrix is given by actions (like IE,RI, OO, \_\_) separated by spaces.
Each matrix is preceded by a single character line, the letter (for readability and checking purposes).
 
For classical automata (without counters), the transition matrix contains only 1 and \_.

The blank lines and lines starting with "%" are ignored.

### Available Functions

* Computing the Stabilization Monoid of a $B$-Automaton
**-sm**: Computes the stabilization monoid of a B-automaton and minimizes it.

 > in text verbose mode:
<div class="highlighter-rouge">
	<div class="highlight">
		<pre class="highlight">
			<code>
acme -sm Examples/test\_sm.txt -text
			</code>
		</pre>
	</div>
</div>

 > to visualize it using Graphviz:
<div class="highlighter-rouge">
	<div class="highlight">
		<pre class="highlight">
			<code>
acme -sm Examples/test\_sm.txt -dotty
			</code>
		</pre>
	</div>
</div>

* Checking the Equivalence of two $B$-Automata
**-equ**: Checks whether two B-automata are equivalent

 > in text verbose mode:
<div class="highlighter-rouge">
	<div class="highlight">
		<pre class="highlight">
			<code>
acme -equ Examples/test\_equ.txt -text
			</code>
		</pre>
	</div>
</div>

 > to visualize them using Graphviz:
<div class="highlighter-rouge">
	<div class="highlight">
		<pre class="highlight">
			<code>
acme -equ Examples/test\_equ.txt -dotty
			</code>
		</pre>
	</div>
</div>

* Running the Markov Monoid Algorithm on a Probabilistic Automaton
**-mma**: Runs the Markov Monoid algorithm on a probabilistic automaton

 > in text verbose mode:
<div class="highlighter-rouge">
	<div class="highlight">
		<pre class="highlight">
			<code>
acme -mma Examples/test\_mma.txt -text
			</code>
		</pre>
	</div>
</div>

 > to visualize it using Graphviz:
<div class="highlighter-rouge">
	<div class="highlight">
		<pre class="highlight">
			<code>
acme -mma Examples/test\_mma.txt -dotty
			</code>
		</pre>
	</div>
</div>


* Checking whether a Classical Automaton has the Finite Power Property
**-fpp**: Checks whether a classical automaton has the finite power property, i.e. whether there exists n such that L^* = L^0 + L^1 + ... + L^n

 > in text verbose mode:
<div class="highlighter-rouge">
	<div class="highlight">
		<pre class="highlight">
			<code>
acme -fpp Examples/test\_fpp\_true.txt -text
			</code>
		</pre>
	</div>
</div>

 > To visualize it using Graphviz:
<div class="highlighter-rouge">
	<div class="highlight">
		<pre class="highlight">
			<code>
acme -fpp Examples/test\_fpp\_true.txt -dotty
			</code>
		</pre>
	</div>
</div>

* Drawing an automaton using Graphviz
**-dotty**: Produces a file output.gv (DOT format) and output.ps, which can be visualized using <a href=http://www.graphviz.org/>graphviz</a>

<div class="highlighter-rouge">
	<div class="highlight">
		<pre class="highlight">
			<code>
acme -dotty Examples/test\_sm.txt -text
			</code>
		</pre>
	</div>
</div>
