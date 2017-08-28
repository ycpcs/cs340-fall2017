---
layout: default
title: "Lab 4: Solution"
---

CFG (start symbol is X):

> X &rarr; ε
>
> X &rarr; L
>
> L &rarr; E
>
> L &rarr; E L
>
> E &rarr; a
>
> E &rarr; ( E )
>
> E &rarr; \[ E \]

# Derivation

String | Production
------ | ----------
X | X &rarr; L
L | L &rarr; E L
E L | E &rarr; a
a L | L &rarr; E L
a E L | E &rarr; ( E )
a ( E ) L | E &rarr; ( E )
a ( ( E ) ) L | E &rarr; a
a ( ( a ) ) L | L &rarr; E
a ( ( a ) ) E | E &rarr; \[ E \]
a ( ( a ) ) \[ E \] | E &rarr; a
a ( ( a ) ) \[ a \] | 
