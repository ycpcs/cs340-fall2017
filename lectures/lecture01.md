---
layout: default
title: "Lecture 1: Syntax, Regular Languages and Regular Expressions"
---

Syntax
======

The *syntax* of a programming language is the set of rules that describe which sequences of characters form legal programs, and which do not.

There are two components to programming language syntax:

1.  **Lexical structure**. The lexical structure of a language determines how the overall sequence of characters in the program text is broken up into *lexemes*. Lexemes are the "words" of the program.

2.  **Syntactic structure**. The syntactic structure of a language determines how the lexemes (words) are grouped into more complex structures ("sentences"). Syntactic structure is often referred to as *grammar*, and is exactly analogous to grammar in human languages.

Program text that violates either the lexical or syntactic rules of the language is not a legal program.

Note that just because program text obeys both the lexical and syntactic structure of the language doesn't mean that the program is *meaningful*. For example, in English, it's possible to construct sentences such as

> Colorless green ideas sleep furiously. (Chomsky, 1957)

which is valid lexically and grammatically, but is meaningless (or at least nonsensical). Here is an example of a syntactically valid but semantically meaningless snippet of Java code:

    int count = "Hello, world";

This statement has no meaning because a string value cannot be legally assigned to an int variable.

Tokens
======

A *token* is a lexeme which falls into a particular "category" of lexemes. For example, most programming languages have a notion of an *identifier*, a name which can refer to a construct such as a variable, procedure, data type, etc. There are a many possible ways to form an identifier, and thus many lexemes which fall into the category "identifier". For example, in C and Java, the following lexemes are all identifiers:

    foo
    bar
    sheep
    LinkedList

The notion of a token is important for describing the syntactic structure of a language. For example, there are many possible ways that identifiers can be used in Java: variable declarations, method declarations, etc. If we had to specify the rules for (let's say) declaring variables separately for each possible identifier, an enormous number of rules would be required. Instead, we define the rule for variable declarations once, stating that *any* identifier may be used to name the variable.

So, the *grammar* of a language is described by specifying what *tokens* may be legally used in a particular syntactic construct.

Formal Languages, Recognizers and Generators
============================================

A *formal language* is a mathematical abstraction that is useful for modeling the lexical and syntactic structures of a programming language.

A formal language is defined as

> a set of strings of symbols

A *symbol* could be a single character chosen from some *alphabet* of possible symbols, which would be useful in the case of describing the lexical structure of a programming language.

A symbol could be a token in the case of describing a grammar to define the syntactic structure of a programming language.

A *string* is a sequence of zero or more symbols.

So, a formal language is simply the set (possibly infinite) of all sequences of symbols which are members of the language.

There are many ways to specify formal languages. There are two strategies for describing formal languages that are particularly important, *recognizers* and *generators*.

> ![image](img/Recognizer.jpg)
>
> A recognizer

A *recognizer* for a language is an automaton (machine) which, when fed a string of symbols, will issue a Yes/No decision stating whether or not the string of symbols is a member of a particular language.

A *generator* for a language is a set of rules describing how to generate all possible members of a particular language.

Regular Languages
=================

*Regular* languages are a useful model for the lexical structure of a programming language.

We will learn about two formalisms, *regular expressions* and *finite automata*, for specifying regular languages precisely:

1.  Regular expression - a generator for a regular language
2.  Finite automaton - a recognizer for a regular language

Regular Expressions
===================

A regular expression "generates" all possible strings in a regular language.

Specifying a regular expression

> Any symbol in the alphabet is a regular expression generating itself
>
> The special symbol &epsilon; (epsilon) is a regular expression representing the empty string

Rules for combining regular expressions

> More complex regular expressions are built out of simpler regular expressions.
>
> Note that parentheses can be used for grouping.
>
> **Concatenation**
>
> > If x and y are regular expressions generating regular languages Lx and Ly, then xy is a regular expression generating the language containing all strings resulting from concatenating one member of Lx with one member of Ly.
>
> **Alternation**
>
> > If x and y are regular expressions generating regular languages Lx and Ly, then (x\|y) is a regular expression generating the language Lx ∪ Ly. (I.e., the union of Lx and Ly.)
>
> **Repetition**
>
> > If x is a regular expression generating regular language Lx, then (x)\* is a regular expression generating the language of all strings formed by concatenating 0 or more of members of Lx. (Kleene star operator.)
> >
> > If x is a regular expression generating regular language Lx, then (x)+ is a regular expression generating the language of all strings formed by concatenating 1 or more of members of Lx. (Kleene plus operator.)

This may seem complicated, but some examples should clarify things.

In the examples below, the alphabet is {a, b}.

> Regular expression | Language (set of strings)
> ------------------ | -------------------------
> a | { a }
> aa | { aa }
> a\* | { &epsilon;, a, aa, aaa, ... }
> aa\* | { a, aa, aaa, ... }
> a+ | { a, aa, aaa, ...}
> ba+ | { ba, baa, baaa, ...}
> (ba)+ | { ba, baba, bababa, ...}
> (a&#124;b) | { a, b }
> a&#124;b\* | { a, &epsilon;, b, bb, bbb, ... }
> (a&#124;b)\* | { &epsilon;, a, b, aa, ab, ba, bb, ... }
> aa(ba)\*bb | { aabb, aababb, aabababb, ... }

Application to programming languages
------------------------------------

The various types of tokens legal in a programming language are typically specified as a series of regular expressions, each regular expression specifying the possible lexemes for one particular kind of token.

For example, the following regular expression generates all legal decimal integer literals in C and C++:

> (-\|&epsilon;)(0\|1\|2\|3\|4\|5\|6\|7\|8\|9)+

I.e., an integer literal is an optional minus sign ("-"), followed by a sequence of one or more digits.

# Strategy

Sometimes it isn't obvious how to create a single regular expression that generates precisely the language you want.

The good news is that you don't need to tackle the entire language all at once.  An excellent strategy is to work on a regular expression that generates *part* of the language.  Then work on another, and so on, until together they generate all of the strings in the language.  Then, you can use the alternation operator (\|) to combine them.

For example, let's say you want a regular expression for language *Z*.  <i>R<sub>Z</sub></i> is the regular expression which will generate the language *Z*.  But you don't yet know what <i>R<sub>Z</sub></i> should be.

So, you create regular expressions <i>R<sub>P</sub></i>, <i>R<sub>Q</sub></i>, and <i>R<sub>R</sub></i>, which generate languages *P*, *Q*, and *R*, such that

> *Z* = *P* ∪ *Q* ∪ *R*

That means that <i>R<sub>Z</sub></i> should be

> <i>R<sub>P</sub></i> \| <i>R<sub>Q</sub></i> \| <i>R<sub>R</sub></i>

Note that for this example, it is not necessary for languages *P*, *Q*, and *R* to be non-overlapping.  In other words, there might be some strings that are in more than one of *P*, *Q*, and *R*.  That is fine!  The only requirement is that all of the strings in *Z* are in the union of *P*, *Q*, and *R*.

As a specific example, let's consider Language 2 from [Lab 1](../labs/lab01.html), which is the language of all strings of strictly alternating **a**'s and **b**'s.  Let's break this language down into four subsets:

* Even length strings starting with **a**
* Odd length strings starting with **a**
* Even length strings starting with **b**
* Odd length strings starting with **b**

Here are four regular expressions which generate each of these subsets:

* (ab)\*
* (ab)\*a
* (ba)\*
* (ba)\*b

Now we can put them all together as a single regular expression:

> (ab)\*\|(ab)\*a\|(ba)\*\|(ba)\*b
