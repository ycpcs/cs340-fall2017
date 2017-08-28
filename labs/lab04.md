---
layout: default
title: "Lab 4: Context-free grammars"
---

# Context-free grammars

This is a pencil and paper lab.

## First part

Specify a context-free grammar (CFG) that generates all strings of terminal symbols chosen from

> **a** **\[** **\]** **(** **)**

where the square brackets and parentheses are properly
balanced.

Examples of strings in the language:

> ε
>
> a 
>
> \[a\]
>
> (a)\[a\]
>
> a((a))\[a\]

Examples of strings not in the language:

> \[a)
>
> (\[a\]
>
> )a\[a\]
>
> \[(a\])
>
> a\]\[a

Hint: make sure delimiters (brackets and parentheses) are added in balanced pairs.
Be sure to specify which nonterminal symbol is the start symbol. Each production should have a
single nonterminal symbol on the left hand side.

## Second part

Show a derivation for the string

> a((a))\[a\]

See [Lecture 4](../lectures/lecture04.html) for examples of derivations.  Each step in the derivation shows how to apply a production to expand one nonterminal symbol in the working string.

<!--
## Before you leave

Ask the instructor to check your work.

You can also check out a [solution](lab04soln.html), although note that this is definitely not the *only* solution.
-->
