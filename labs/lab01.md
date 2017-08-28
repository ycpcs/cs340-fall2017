---
layout: default
title: "Lab 1: Regular Expressions"
---

Regular Expressions
===================

Start a terminal.

To get started, run the following commands:

    cd $HOME
    mkdir -p CS340/CS340_Lab01
    cd CS340/CS340_Lab01
    wget http://ycpcs.github.io/cs340-fall2016/resources/regexChecker.jar
    wget http://ycpcs.github.io/cs340-fall2016/resources/regexEquivalenceChecker.jar

You can run the **regexChecker.jar** program with the command

    java -jar regexChecker.jar

The program allows you to type in a regular expression and then match it against some strings. For each string, the program will tell you whether or not the string matches the regular expression.

You can run the **regexEquivalenceChecker.jar** program with the command

    java -jar regexEquivalenceChecker.jar

This program allows you to enter two regular expressions, and will tell you whether or not they are equivalent. If they are not equivalent, it will give examples of strings that are generated by one regular expression, but not the other.

There is some incredibly awesome theory behind this program (and the algorithm it uses) that we will learn about later in the semester.

Your Task
=========

Find regular expressions that match the following languages.  Create a text file in the **CS340/CS340\_Lab01** directory and save your answers in it.  (This is for your own record: you don't have to submit it.)

You can use the **regexChecker.jar** program to test your regular expressions to make sure they accept and reject the example strings as appropriate.  *However*: testing a regular expression does not guarantee that the regular expression is correct.

Note that ε (epsilon) denotes the empty string.

When you are done you can check your answers against the [solutions](lab01soln.html).

Language 1
----------

The language of all strings starting with **abb** and ending with an even number of **a**'s.

Example strings to try:

> In the language | Not in the language
> --------------- | -------------------
> abb | ε
> abbaa | abba
> abbaaaa | abbaaa
> abbaaaaaa | ab
> abbaaaaaaaa | babb

Language 2
----------

The language of all strings of strictly alternating **a**'s and **b**'s.

Example strings to try:

> In the language | Not in the language
> --------------- | -------------------
> ε | abb
> a | baa
> aba | abba
> baba | ababaa
> ababab | bba

Language 3
----------

The language of all strings of **a**'s and **b**'s which contain an odd number of **b**'s.

Example strings to try:

> In the language | Not in the language
> --------------- | -------------------
> b | ε
> baaabb | a
> aabaaaabaaaabaa | abb
> bbbbab | bab
> aaba | aaaabaaba