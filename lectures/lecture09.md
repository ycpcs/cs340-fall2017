---
layout: default
title: "Lecture 9: Decidability of Regular Languages"
---

Regular Languages are Decidable
===============================

Regular languages are decidable: given any two regular languages A and B, an algorithm can determine whether A and B contain the same strings.

The decision algorithm exploits the fact that *set operations* can be performed on regular languages, based on transformations of finite automata. Because a regular language in any form (regular expression, DFA, and NFA) can be freely converted to any other form, these operations on automata are fully general.

Because these set operations yield a finite automaton as a result, the results of set operations on regular languages are also regular languages.

Let's say that A and B are finite automata recognizing regular languages A and B. (I.e., we use "A" to mean both a regular language and a finite automaton that recognizes language "A".) We can transform the automata A and/or B to construct an automaton that recognizes the languages

> A ∪ B, the union of A and B
>
> A<sup>C</sup>, the complement of A, meaning all strings (over some alphabet) that are *not* in A
>
> A ∩ B, the intersection of A and B
>
> A - B (set difference), all strings that are in A but not in B

In addition to the possibility of performing set operations on finite automata, it is also possible to check any finite automaton to find out whether or not it recognizes a nonempty set of strings. Using this check, we can find out, for any regular language, whether or not that language is empty.

A decision procedure to check the equivalence of regular languages A and B is

> (A = B) ≡ ( (A - B = ∅) and (B - A = ∅) )

In other words, A is equivalent to B if and only if the set differences (A - B) and (B - A) are both empty.

Constructions for Set Operations
================================

Here are constructions for set operations on regular languages.

**Union (A ∪ B)**: The basic idea is to create new start and accepting states, connect the new start state to the start states for A and B using epsilon transitions, and connect the accepting states of A and B to the new accepting state using epsilon transitions. After these modifications are done, all of the original start and accepting states of A and B become non-start, non-accepting states. The resulting NFA recognizes the union of A and B.

<b>Complement (A<sup>C</sup>)</b>: Given a DFA recognizing language A, create an explicit "reject" state, which is a non-accepting state. In all states of A where there is no explicit transition on a particular input symbol, create an explicit transition to the reject state on that symbol. In the reject state, self-transitions on each possible input symbol lead back to the reject state. The result of these transformations is a DFA that recognizes the same language as the original DFA, but every state has a transition on every input symbol. By changing each accepting state to a non-accepting state, and changing each non-accepting state to an accepting state, we create a DFA that recognize the complement of A.

**Intersection (A ∩ B)**: Intersection can be synthesized using the constructions for union and complement:

> A ∩ B = ( (A<sup>C</sup>) ∪ (B<sup>C</sup>) )<sup>C</sup>

In other words, the intersection of A and B is the complement of the union of the complements of A and B.

**Difference (A - B)**: Difference can be synthesized using the constructions for intersection and complement

> A - B = A ∩ B<sup>C</sup>

In other words, A - B is the set of all strings that are in A and also in the complement of B.

Determining If An Automaton Recognizes a Nonempty Language
==========================================================

An automaton recognizes a nonempty language if and only if an accepting state is reachable from the start state.

The following algorithm determines if an accepting state is reachable from the start state:

    public boolean recognizesNonEmptyLanguage(FiniteAutomaton fa) {
        // If any path from the start state leads to an accepting state,
        // then the automaton accepts at least one string.

        LinkedList<State> workList = new LinkedList<State>();
        Set<State> seen = new TreeSet<State>();

        workList.add(fa.getStartState());

        while (!workList.isEmpty()) {
            State s = workList.removeFirst();
            seen.add(s);

            if (s.isAccepting()) {
                return true;
            }

            for (Transition t : fa.getTransitions(s)) {
                if (!seen.contains(t.getToState())) {
                    workList.addLast(t.getToState());
                }
            }
        }

        return false;
    }
