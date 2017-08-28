---
layout: default
title: "Lab 5: Recursive Descent Parsing"
---

# Getting Started

Download [RecursiveDescentJava.zip](../lectures/RecursiveDescentJava.zip).  Import it into your Eclipse workspace.

# Your task

Modify the parser so that it supports parenthesized expressions.

Do so by adding the following production:

> F &rarr; ( E )

You will need to modify the lexer so that it supports left and right parentheses as terminal symbols:

* Modify the definition of **LEGAL** in **Lexer.java** to include the **(** and **)** characters
* Modify **Symbol.java** to define new terminal symbols for left and right parentheses
* Modify the definition of the **fromCharacter** method in **Symbol.java** to handle **(** and **)** characters

You will also need to modify the **parseF** method in the parser class so that it applies the **F &rarr; ( E )** production when appropriate.

## Testing

Try running the program and entering the expression

> (a + b) * 3

<!--
## Solution

You can see a solution here: [RecursiveDescentSoln.zip](RecursiveDescentSoln.zip)
-->
