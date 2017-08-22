---
layout: default
title: "Lecture 19: Virtual Machines"
---

# Language implementation strategies

An implementation of a programming language must provide some mechanism for executing programs.  There are several common strategies.

An *interpreter* converts the program into a data structure (often an abstract syntax tree), and then executes the program by evaluating the data structure, typically with the support of other run-time data structures such as an activation record stack and a heap for dynamic allocation.  Interpreters are (generally) easy to implement, and permit significant flexibility in what kinds of language features can be implemented.  Dynamic languages are often implemented using interpreters: this is true of the standard implementations of Ruby and Python.  Interpreters typically impose some overhead compared to machine code executed directly on the CPU: for example, it is not unusual for a program executed on an interpreter to take 10 times longer to complete a computationally-intensive task than an equivalent machine language program executing directly on the CPU.

A *compiler* converts the program into executable machine language.  The program is then loaded directly into memory by the operating system and executed directly on the CPU.  The operating system and a "runtime library" provide support for run-time data structures such as the stack and heap.  Compiled programs, if carefully optimized, can get arbitrarily close to achieving the maximum possible performance.  C and C++ are usually implemented by compilation.

A *virtual machine* permits a hybrid approach.  A virtual machine is essentially an interpreter for a "virtual" machine language: this virtual machine language is often called "bytecode".  The virtual machine may execute bytecode

* by interpreting it
* by translating it to machine code at run-time ("Just In Time" compilation)
* by using both interpretation and JIT compilation (many JVMs, including the default Hotspot JVM, work this way)

Java, Scala, Clojure, and C# are all typically implemented by compilation to virtual machine bytecode.  JavaScript is also usually implemented on a virtual machine (using interpretation and JIT compilation), although typically without an explicit bytecode representation.

Virtual machines using JIT compilation can achieve performance comparable to the performance of natively compiled code.

Note that there is another meaning for the term "Virtual Machine", which is an environment which virtualizes an entire computer to permit executing of a guest operating system within a host operating system.  Examples of this type of "system-level" VM include VMWare, VirtualBox, KVM, Xen, QEMU, and many others.  There are important similarities between system-level and language VMs, and in many cases they share common implementation strategies.

# Benefits of virtual machines

Virtual Machines offer a number of benefits.

They can permit the same program to be run on different operating systems and CPU architectures.  The Java Virtual Machine (JVM) has been fairly successful in providing a consistent runtime environment on many OSes and CPUs.

They can enforce safety features.  For example, the JVM does not permit a program to access arbitrary memory or execute arbitrary machine code (except in the case of bugs in the JVM implementation.)  This can eliminate broad classes of bugs, such as stack overflows (which are a common cause of security vulnerabilities for C and C++ programs.)

# Drawbacks of virtual machines

The main drawback of virtual machines is memory and CPU overhead compared to natively compiled programs.  A virtual machine using JIT compilation requires CPU time to translate bytecode into optimized machine code, and requires memory for run-time structures (representations of the program code) that are not needed for native executables.  For long-running programs on computers with ample CPU and memory resources, this is often not a concern.  For resource-constrained environments, this may be more problematic.

<!-- vim:set wrap: ­-->
<!-- vim:set linebreak: -->
<!-- vim:set nolist: -->
