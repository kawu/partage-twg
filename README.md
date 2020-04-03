Partage for TWG
===============

**Partage for TWG** is a Haskell library and tool dedicated to parsing *tree
wrapping grammars* (TWGs) using the A\* algorithm [3,4,5].  This version is a
fork of [Partage for TAGs](https://github.com/kawu/partage) and it's currently
under development.

<!---
It implements two kinds of parsers -\- an Earley-style, bottom-up parser [1]
with special focus on structure (and, hence, computation) sharing [2], and an
A\* parser [3,4,5].
-->
    
<!---
[![Build Status](https://travis-ci.org/kawu/partage.svg?branch=master)](https://travis-ci.org/kawu/partage)
-->

<!---
#### Earley-style parser

The Earley-style parser implements two flavors of grammar compression:

  * Subtrees common to different elementary trees are shared.  The input TAG is
    in fact transformed into an equivalent directed acyclic graph (DAG).
  * Flat production grammar rules representing the individual parts of the DAG
    are compressed into an FSA.  The default choice is a prefix-tree
    compression, although other forms of compression are also possible (e.g.,
    minimal FSA).
-->

#### A\* parser

The A\* parser works best on the results of dependency-aware supertagging in
which:

  * To each position in the input sentence a distribution of TWG elementary
    trees (supertags) which can be anchored at this position is assigned.  This
    comes from classic statistical supertagging.
  * To each position in the sentence the distribution of the possible heads is
    also assigned.  This can result from statistical dependency parsing.

The probability of a TWG derivation in this setting is defined as the product
of the probabilities of the participating supertags multiplied by the product
of the probabilities of the entailed dependency arcs.  The A\* parser
guarantees to find a most probable derivation, without searching the entire
space of the possible derivations.

<!---
Grammar compression is also used in the A\* parser, but to a very limited
extent.
-->

<!---
##### Correctness

Some correctness-related properties (notably, monotonicity) of the A\* parser
are verified using the [Coq][coq] proof assistant.  See the
[proofs](proofs#proofs) subfolder for more details.
-->


Installation
------------

First you will need to download and install the [Haskell Tool Stack][stack].
Then run the following commands:

    git clone https://github.com/kawu/partage-twg.git
    cd partage-twg
    stack install

The last command builds the `partage-twg` command-line tool and (on Linux)
places it in the `~/.local/bin/` directory by default. Installation on Windows,
even though not tested, should be also possible.

You can also run the following command in the local repository to make sure
that all the tests succeed:

    stack test


Data format
-----------

Partage works with tab-separated values (`.tsv`) files, with the individual
sentences separated by blank lines. Each non-blank line corresponds to a token
in the corresponding sentence and contains the following columns:

  * ID of the token
  * word form
  * head distribution
  * 1st supertag
  * 2nd supertag
  * ...

The head distribution entry has the form of `|` separated pairs, each pair consisting
of a head token ID and the corresponding probability (separated by a colon).
If this entry is empty, no head dependency constraints are assumed for the
corresponding position.

Each supertag entry consists of a supertag represented in the bracketed format
and the corresponding probability (also separated by a colon).  Here is an
example of how an input sentence represented in this format can look like.

```
1	John	2:1.0	(NP (N <>)):1.0
2	eats	0:1.0	(SENT (NP) (VP (V <>))):0.6	(SENT (NP) (VP (V <>) (NP))):0.4
3	an	4:0.5|1:0.5	(NP* (D <>)):1.0
4	apple	2:0.5|0:0.5	(NP (N <>)):1.0
```

In general, the token IDs have to correspond to the range `1..n` where `n` is
the sentence length.  ID `0` is reserved for the special dummy token
representing the root.  Another, larger example can be found in
`examples/french.tsv`.

The anchor of the trees is represented with `<>`.  For example, `(SENT (NP) (VP
(V <>)))` is transformed to `(SENT (NP) (VP (V eats)))` in the example above
before parsing takes place.

<!---
#### Adjunction vs. sister-adjunction

Auxiliary trees (which attach to other trees via adjunction) are represented by
marking the *foot* node with a star, e.g. `(VP (V <>)(VP* ))`.

Sister trees (which attach to other trees via sister-adjunction) are
represented by marking the *root* node with a star, e.g. `(NP* (D <>))`.
-->

#### Wrapping substitution

To mark a d-daughter node, use `#WRAP#`, as in the following example:
```
1       we      2:1     (NP (PRO <>)):1
2       keep    3:1     (CLAUSE (CORE (NP ) (NUC (NUC (V <>)) (NUC )))):1
3       wondering       0:1     (SENTENCE (CLAUSE (NUC#WRAP# (V <>)) (CLAUSE ))):1
4       what    9:1     (PrCS (NP-WH (PRO-WH <>))):1
5       mr.     6:1     (NUC_N* (N-PROP <>)):1
6       gates   7:1     (NP (CORE_N (NUC_N (N-PROP <>)))):1
7       wanted  9:1     (CLAUSE (CORE (CORE (NP ) (NUC (V <>))) (CORE ))):1
8       to      9:1     (CORE* (CLM <>)):1
9       say     3:1     (CLAUSE (PrCS ) (CORE#WRAP# (NUC (V <>)))):1
```

#### Empty terminals

To represent an empty terminal, use the special `-NONE-` terminal symbol, e.g
`(S (NP )(VP (V <>)(NP -NONE-)))`.


Usage
-----

Use the following command to parse an input `test.tsv` file using A\*:

    partage-twg astar -i test.tsv

or:

    cat test.tsv | partage-twg astar

Assuming the first example sentence above, both commands should produce:

```
1	John	2	(NP (N John))
2	eats	0	(SENT (NP) (VP (V eats) (NP)))
3	an	4	(NP* (D an))
4	apple	2	(NP (N apple))
```

Run `partage-twg astar --help` to learn more about the possible parsing options.

#### Output representation

You can retrieve the parsed (derived) trees instead of the selected supertags
and dependency heads using the `-p` option:

    partage-twg astar -i test.tsv -p

You can also use `-v` (`--verbosity`) to get additional information on the
resulting parses, such as their weights (`-v 1`) or the corresponding
derivation trees (`-v 2`).

#### Start symbol

The `-s` option allows to specify the start symbol, i.e., the label of the root
of the resulting parse, e.g.:

    partage-twg astar -i test.tsv -s SENT

To use several start symbols, specify them separated by spaces between double
quotation marks, as in the example below:

    partage-twg astar -i test.tsv -s "SENT FRAG"

Alternatively, you can use the `-s` option several times:

    partage-twg astar -i test.tsv -s SENT -s FRAG

#### Number of supertags and dependency heads

It is possible to restrict the number of supertags (`-t` or `--max-tags`) and
dependency heads (`-d` or `--max-deps`) used for parsing.  For instance, to
restrict both numbers to `5`:

    partage-twg astar -i test.tsv -s SENT -t 5 -d 5

This will normally speed up parsing, but there is a price to pay -- additional
restrictions may make the parser fail for some sentences.  In such situations,
the parser will explore the full parsing hypergraph and actually work slower.

The related `--beta` parameter alows to determine the number of supertags and
heads for each token dynamically, based on the corresonding probabilities.  For
instance, with `--beta 0.01` and the highest supertag probability of `0.9`, all
the supertags with the probabilities lower than `0.01 x 0.9` will get discarded.

<!---
#### Earley

In order to run the Earley-style parser instead of A\*, use:

    partage-twg earley -i test.tsv -s SENT

Note that the Earley-style parser ignores the dependency-related constraints
(third column of the input file).  It also requires that the start symbol(s) be
specified.   The output is also different, it consists of the set of parsed
trees.  For the example sentence above:

```
(SENT (NP (N John)) (VP (V eats) (NP (D an) (N apple))))
```

Run `partage-twg earley -\-help` to learn more about the possible parsing options.
-->


References
----------

[1] Miguel A. Alonso, David Cabrero, Eric de la Clergerie and Manuel
Vilares, *Tabular Algorithms for TAG Parsing*, in Proc. of EACL'99,
Ninth Conference of the European Chapter of the Association for
Computational Linguistics, pp. 150-157, Bergen, Norway, 1999.

[2] Jakub Waszczuk, Agata Savary, Yannick Parmentier, *Enhancing Practical TAG
Parsing Efficiency by Capturing Redundancy*, 21st International Conference on
Implementation and Application of Automata (CIAA 2016), Seoul, South Korea,
2016, ([PDF](https://hal.archives-ouvertes.fr/hal-01309598v2/document)).

[3] Jakub Waszczuk, Agata Savary, and Yannick Parmentier, *Promoting multiword
expressions in A\* TAG parsing*, 26th International Conference on Computational
Linguistics (COLING 2016), Osaka, Japan, 2016,
([PDF](https://aclweb.org/anthology/C/C16/C16-1042.pdf)).

[4] Jakub Waszczuk, Agata Savary, and Yannick Parmentier, *Multiword
Expression-Aware A\* TAG Parsing Revisited*, 13th International Workshop on
Tree Adjoining Grammars and Related Formalisms, Umeå, Sweden, 2017.
([PDF](http://www.aclweb.org/anthology/W17-6209)).

[5] Jakub Waszczuk, *Leveraging MWEs in practical TAG parsing: towards the best
of the two worlds*, PhD Thesis, Tours, 2017,
([PDF](http://www.applis.univ-tours.fr/theses/2017/jakub.waszczuk_6706.pdf))


[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
[coq]: https://coq.inria.fr/ "Coq proof assistant"
