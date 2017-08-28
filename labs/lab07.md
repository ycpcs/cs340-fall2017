---
layout: default
title: "Lab 7: Turing Machines"
---

Your Task
=========

Using JFLAP, create Turing Machines to perform the tasks described below.

First Task
----------

Create a Turing Machine that increments a binary number.

The initial tape will be a sequence of ones and zeroes specifying a binary (base 2) number. The final contents of the tape should be a sequence of ones and zeros specifying a binary number that is one greater than the original number.

For example, if the initial sequence is

> 10010111

Then the final sequence (once the Turing Machine has halted) should be

> 10011000

Another example, if the initial sequence is

> 111

then the final sequence should be

> 1000

### Hints

The Turing Machine should start by moving to the end of the input string, then start moving to the left.

If the Turing Machine sees a 0 on its scan to the left, it can change it to a 1 and halt. If it sees a 1, it should change the 1 to a zero and continue to the left (essentially carrying the result of adding 1 to 1 one place to the left.)

Don't forget that the result can overflow past the left side of the string (as in the second example above.)

Second Task
-----------

Create a Turing Machine which *decrements* (subtracts one from) the binary number on the input tape.

The process will be similar to incrementing.

For example, if the initial string is

> 1001

then the final string should be

> 1000

Another example: if the initial string is

> 1000

then the final string should be

> 0111

(If you would like to trim unnecesssary 0's from the left side of the string, you can, but it's not required.)
