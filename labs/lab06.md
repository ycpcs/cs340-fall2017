---
layout: default
title: "Lab 6: Precedence Climbing"
---

Getting Started
===============

Download the example implementation of precedence climbing:

> [PrecedenceClimbingJava.zip](../lectures/PrecedenceClimbingJava.zip)

Import it into Eclipse.  You should see a project called **PrecedenceClimbing**.

Your Task
=========

You have two tasks:

1.  Add support for the **^** operator, meaning exponentiation. It should have a higher precedence than the already-supported operators, and should be right-associative. Note that you will need to modify the lexer to recognize **^** as a token.
2.  Allow parentheses to be used to explicitly specify the order of operation. To do this, modify **parsePrimary** so that if it sees a left parenthesis, it consumes it, calls **parse** to parse an expression, and then consumes a right parenthesis. Test by entering expressions such as **(a + b) \* 3** and verify that the parse tree reflects the intended order of operations.

You will need to modify the lexer so that it supports the exponentiation operator (^) and left and right parentheses as terminal symbols:

* Modify the definition of **LEGAL** in **Lexer.java** to include the **^**, **(**, and **)** characters
* Modify **Symbol.java** to define new terminal symbols for exponentiation and left and right parentheses
* Modify the definition of the **fromCharacter** method in **Symbol.java** to handle **^**, **(**, and **)** characters

You will also need to modify the **parsePrimary** method in the parser class so that it applies the **F &rarr; ( E )** production when appropriate.

<!--
## Solution

A solution is available: [PrecedenceClimbingSoln.zip](PrecedenceClimbingSoln.zip)
-->
