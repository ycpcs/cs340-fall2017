---
layout: default
title: "Regular Language Practice Problems"
---

# Your task

Create a regular expression and deterministic finite automaton (DFA) for each of the languages described below.

**Language 1**.  The language of all strings over {a, b} such that the as and bs are strictly alternating.

Examples of strings in the language: ε, a, b, ab, ba, aba, bab, abab, baba

Examples of strings not in the language: bb, aa, abba

**Language 2**.  The language of all strings over {a, b} that begin with "aba" and end with "bb".

Examples of strings in the language: ababb, ababb, ababbb, ababbababb

Examples of strings not in the language: ε, aba, abab, abb

**Language 3**.  The language of all strings over {a, b} such that the as always occur in groups of 2 or more.

Examples of strings in the language: ε, b, bbbb, aa, aaa, aabaaabbaa, bbaabaaabaabaaa

Examples of strings not in the language: a, aba, abba, aabaaaaba

**Language 4**.  The language of all strings over {a, b, c} which start with "a" and which do not contain the substring "cb".  (Note that this one is challenging as a regular expression.)

Examples of strings in the language: a, ab, abc, ac, acc, accab

Examples of strings not in the language: ε, b, c, acb, abbabcba

**Language 5**.  The language of all strings over {a, b} such that the number of as is even and the number of bs is odd.  (Note that this one is challenging as a regular expression.)

Examples of strings in the language: b, aab, baa, aba, aaaba, aababab

Examples of strings not in the language: ε, aa, abba, abbbabab

## Solutions

Regular expressions: [reglang-regex.txt](reglang-regex.txt).  Note that `e` is used to mean epsilon.

DFAs: [reglang-fa1.jff](reglang-fa1.jff), [reglang-fa2.jff](reglang-fa2.jff), [reglang-fa3.jff](reglang-fa3.jff), [reglang-fa4.jff](reglang-fa4.jff), [reglang-fa5.jff](reglang-fa5.jff).
