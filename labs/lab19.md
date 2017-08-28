---
layout: default
title: "Lab 19: MiniVM programming"
---

# Getting started

Download [CS340\_Lab19.zip](CS340_Lab19.zip).  Unzip it somewhere (e.g., in your `CS340` directory).  The lab files are in a directory called `CS340_Lab19`.

# MiniVM

[MiniVM](https://github.com/daveho/MiniVM) is a toy stack-based bytecode virtual machine.  It's a little bit like the Java virtual machine, although greatly simplified.

The [documentation](https://github.com/daveho/MiniVM/blob/master/Documentation.md) gives a general overview of how MiniVM works.  You can look at the [instruction set](https://github.com/daveho/MiniVM/blob/master/InstructionSet.md) documents each supported instruction.

You can also browse some [example programs](https://github.com/daveho/MiniVM/tree/master/t).

# Your task

Write a program to compute the sum of the first *N* integers and print out the sum.  Since MiniVM doesn't support reading user input, you will need to hard code *N* in the program.

The program is in the file `sumints.mvm`.  You can execute it interactively with the command

    ./MiniVM.rb -x -i sumints.mvm

In interactive mode, each time you press Enter, one instruction will be executed.

You can execute it noninteractively with the command

    ./MiniVM.rb -x sumints.mvm

Interactive execution is useful for understanding the exact behavior of the program.  Noninteractive execution is useful if you want to test the program for larger values of *N* (where tracing the entire execution of the program would be tedious.)

## If you have time

Try modifying the program to use a procedure to compute the sum of 1..N.

<!--
# Solution

[sumints.mvm](https://github.com/ycpcs/cs340-fall2016/blob/gh-pages/labs/sumints.mvm), [sumints\_proc.mvm](https://github.com/ycpcs/cs340-fall2016/blob/gh-pages/labs/sumints_proc.mvm) (version using a procedure)
-->
