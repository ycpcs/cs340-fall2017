---
layout: default
title: "Lab 2: Finite Automata using JFLAP"
---

# Getting Started

Start a terminal.  Create a lab directory and download JFLAP:

    cd $HOME
    mkdir -p CS340/CS340_Lab02
    cd CS340/CS340_Lab02
    wget http://ycpcs.github.io/cs340-fall2016/resources/JFLAP.jar

Start JFLAP:

    java -jar JFLAP.jar

As you create automata in JFLAP, save them (as **.jff** files) in the **CS340/CS340\_Lab02** directory.

# Your Task

In JFLAP, implement finite automata for the following four regular languages.

You should execute your finite automata on example input strings. Make sure that the automata accept the example strings which are in the language, and reject the example strings which are not in the language.

Languages 1-3 use the alphabet {**a, b**}. Language 4 uses the alphabet {**a, b, c**}.

Language 1
----------

The first language contains strings starting with **aa**, followed by 0 or more *b*'s, and ending with **a**.

Example strings which are in the language:

    aaa
    aaba
    aabba
    aabbba
    aabbbba

Example strings which are not in the language:

    aa
    aabaa
    baaa
    aaab
    aba

Language 2
----------

The second language contains strings which do not start with **ba**.

Example strings which are in the language:

    ε (the empty string)
    a
    b
    aa
    bb
    bba

Example strings which are not in the language:

    ba
    baa
    bab
    baba

Language 3
----------

The third language contains strings which have an even number of **a**'s.

Example strings which are in the language:

    ε (the empty string)
    b
    bbb
    baba
    aababba
    baa
    baabaa

Example strings which are not in the language:

    a
    bab
    aaba
    bbbabb

Language 4
----------

The fourth language contains all strings (over the alphabet {**a**, **b**, **c**}) that do not contain the substring **bc**.

Avoiding the generation of a substring using a regular expression is difficult. However, matching a substring (and not accepting strings that contain it) using a regular expression is pretty easy!

Example strings which are in the language:

    ε (the empty string)
    abac
    abb
    bac
    bacca

Example strings which are not in the language:

    bc
    bca
    bcb
    bcc
    abc
    bbabca
