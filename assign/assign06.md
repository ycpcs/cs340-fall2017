---
layout: default
title: "Assignment 6: Parsing"
---

**Due**: Friday, Nov 17th by 11:59 PM

# Getting Started

Download [cs340-assign06.zip](cs340-assign06.zip).

If you are using Counterclockwise under Eclipse, you can import the zipfile as an Eclipse project.  After importing it, you should see a project called **cs340-assign06** in your workspace.

# Your Task

The source file `parser2.clj` implements a recursive descent parser for the following context-free grammar:

> *unit* → *statement\_list*
>
> *statement\_list* → *statement* *statement\_list* \| *statement*
>
> *statement* → *var\_decl\_statement* \| *expression\_statement*
>
> *var\_decl\_statement* → **var** **identifier** **;**
>
> *expression\_statement* → *expression* **;**
>
> *expression* → see below
>
> *primary* → **identifier** \| **int\_literal** \| **str\_literal**

The names and symbols in **bold** are terminal symbols (tokens), while the names in *italics* are nonterminal symbols.

Expressions are infix expressions with the := (assignment), = (equality), !=, \<, \>, \<=, \>=, +, -, \*, /, and ^ (exponentiation) operators.  They are parsed by [precedence climbing](../lectures/lecture06.html).

Your task is to modify the parser to support the following productions:

> *primary* → **(** *expression* **)**
>
> *statement* → *if\_statement*
>
> *statement* →  *while\_statement*
>
> *if\_statement* →  **if** **(** *expression* **)** **{** *statement\_list* **}**
>
> *while\_statement* → **while** **(** *expression* **)** **{** *statement\_list* **}**

Supporting the first production (allowing parenthesized subexpressions) will require modifying the **parse-primary** function.  Supporting the other two productions will involve modifying the **parse-statement** function.

## Recursive descent parsing in a functional language

Implementing a recursive descent parser in a functional language is not really that hard.  However, there is one fundamental issue that requires some thought: the lexical analyzer (lexer) cannot implement *stateful* operations.

In our previous work with parsing (such as [Assignment 3](assign03.html)), we assumed that the lexer's **next()** method consumed a token, *removing it from the input sequence*, such that a subsequent call to **next()** would return a different token.  Because **next()** modified the lexer's internal state, it is a "stateful" operation.  When writing programs in a functional language, where destructive modifications of data are not possible, our operations can't be stateful.

From the standpoint of implementing a recursive descent parser, the issue is that each parse function needs to consume some number of tokens from the input sequence.  Thus, each time a parse function is called, we need to know how many tokens were consumed, and which tokens remain.  This turns out to be pretty easy if we have each parse function return *two* values: a parse node, and a sequence containing the remaining tokens.  We can have the parse functions return two values by having them return a *record* containing a parse node and the sequence containing the remaining tokens.

## Record data types

Record types in Clojure are very much like struct data types in C, except that record instances are immutable.

The parser defines two record types:

{% highlight clojure %}
; Result of expanding a single right-hand-side symbol:
; A single parse node, and a sequence containing the remaining
; input tokens.
(defrecord SingleParseResult [node tokens])

; Result of partially or completely applying a production:
; A sequence of 0 or more parse nodes, and a sequence containing
; the remaining input tokens.
(defrecord ParseResult [nodes tokens])
{% endhighlight %}

The `node` module defines one additional record type, which is used to represent parse tree nodes (as well as other kinds of nodes in future assignments):

{% highlight clojure %}
; Record for parse tree, AST, and augmented AST nodes.
;
; symbol indicates the (terminal or nonterminal) symbol.
;
; value is the "value" of the node, which for terminal nodes
; is the lexeme of the token, and for nonterminal nodes
; is the list of child nodes.
;
; props is a map containing property values; these are used
; in the augmented AST for things like register numbers for
; variables.
;
(defrecord Node [symbol value props])
{% endhighlight %}

Creating an instance of a record type is done by using the name of the type as a constructor function.  For example, here is the definition of the **make-node** function in the `node` module:

{% highlight clojure %}
; Make a node without any properties.
;
; Parameters:
;   symbol - the symbol (keyword value) used to label the node
;   value  - the value of the node: for parent nodes, it should
;            be a sequence (list or vector) containing the child
;            nodes
;
; Returns: the node
;
(defn make-node [symbol value]
  (Node. symbol value {}))
{% endhighlight %}

This function creates a **Node** record with the specified symbol and value, and with an empty properties map.

Accessing a field of a record is done by applying a keyword naming the field that you want to retrieve as a function on the record instance.  For exmaple, if **n** is a **Node** record instance, then the expression

{% highlight clojure %}
(:symbol n)
{% endhighlight %}

would retrieve the value of **n**'s **symbol** field.

## Tokens, Parse functions

The parser takes its inputs as a sequence of tokens.  Each token is a two-element vector, where the first element is the token's lexeme, and the second element is the token's symbol, represented as a Clojure keyword value.  Here are the various types of tokens that the lexer produces:

> Symbol | Meaning
> ------ | -------
> **:var** | **var** keyword
> **:func** | **func** keyword
> **:if** | **if** keyword
> **:while** | **while** keyword
> **:identifier** | an identifier
> **:str\_literal** | a string literal
> **:int\_literal** | an integer literal
> **:op\_assign** | the assignment operator, **:=**
> **:op\_plus** | the addition operator, **+**
> **:op\_minus** | the subtraction operator, **-**
> **:op\_mul** | the multiplication operator, <b>*</b>
> **:op\_div** | the division operator, **/**
> **:op\_exp** | the exponentiation operator, **^**
> **:op\_lte** | less than or equal to, **\<=**
> **:op\_lt** | less than, **\<**
> **:op\_gte** | greater than or equal to, **\>=**
> **:op\_gt** | greater than, **\>**
> **:op\_eq** | equal to, **=**
> **:op\_neq** | not equal to, **!=**
> **:semicolon** | semicolon, **;**
> **:lparen** | left parenthesis, **(**
> **:rparen** | right parenthesis, **)**
> **:lbrace** | left brace, **{**
> **:rbrace** | right brace, **}**

Parse functions in the parser take a sequence of tokens and return a **SingleParseResult** record containing a **Node** (a parse tree representing the result of the parse), and a sequence containing the remaining input tokens.

## Symbol application functions

The parser uses *symbol application functions* to make progress.  Each symbol application function applies a single symbol (terminal or nonterminal) on the right-hand side of a production.  All of the parse functions are symbol application functions.  The **expect** function creates a symbol application function which consumes a particular terminal symbol.

The parser has some helper functions for applying productions: **do-production**, **apply-production**, and **complete-production**.

The **do-production** function is the easiest of the three helpers: it applies a complete production and returns a **SingleParseResult**.  For example, here is how the **parse-unit** function is implemented:

{% highlight clojure %}
(defn parse-unit [token-seq]
  ; unit -> ^ statement_list
  (do-production :unit [parse-statement-list] token-seq))
{% endhighlight %}

If the right-hand side of a production has one or more terminal symbols, then the **expect** function can be used to return a symbol application function for consuming a particular type of terminal symbol (token).  For example, here is the implementation of the **parse-var-decl-statement** function:

{% highlight clojure %}
(defn parse-var-decl-statement [token-seq]
  ; var_decl_statement -> ^ var identifier ;
  (do-production :var_decl_statement
                 [(expect :var) (expect :identifier) (expect :semicolon)]
                 token-seq))
{% endhighlight %}

The only drawback to **do-production** is that it requires that you apply a complete production.  If you are only going to apply part of a production (consuming only some of the symbols on the right-hand side of a production), then you can use the **apply-production** function.  It takes a **ParseResult** and a list of symbol application functions, and returns an extended **ParseResult** created by applying the specified symbol application functions.  The **complete-production** function takes a nonterminal symbol (keyword) and a **ParseResult**, and returns a **SingleParseResult** containing a single parse node with all parse nodes in the **ParseResult**.  The **apply-production** and **complete-production** functions can be used together to apply a production in stages.  You can see an example of this in **parse-statement-list**.  (You will mostly likely not need to use **apply-production** or **complete-production**, but understanding how they work will be helpful in understanding how the parser works.)

## Testing

A good way to test your work (as you modify the parser) is to print the parse trees that are created as a result of parsing.  The **pp/pretty-print** function takes a **Node** (the root of the parse tree) and prints out a text representation of the parse tree.

In `parser2.clj` there is a "Testing" section at the bottom of the file.  If you uncomment the two lines at the bottom of the file (defining variables called **testprog** and **prog**), when you load the parser it will do a parse of a test input (**testprog**), saving the resulting parse tree in **prog**.  So, after loading the module, evaluating

{% highlight clojure %}
(pp/pretty-print prog)
{% endhighlight %}

in a REPL will produce the following output:

    :unit
    +--:statement_list
       +--:statement
       |  +--:var_decl_statement
       |     +--:var["var"]
       |     +--:identifier["a"]
       |     +--:semicolon[";"]
       +--:statement_list
          +--:statement
          |  +--:var_decl_statement
          |     +--:var["var"]
          |     +--:identifier["b"]
          |     +--:semicolon[";"]
          +--:statement_list
             +--:statement
             |  +--:expression_statement
             |     +--:op_assign
             |     |  +--:primary
             |     |  |  +--:identifier["a"]
             |     |  +--:primary
             |     |     +--:int_literal["5"]
             |     +--:semicolon[";"]
             +--:statement_list
                +--:statement
                   +--:expression_statement
                      +--:op_assign
                      |  +--:primary
                      |  |  +--:identifier["b"]
                      |  +--:op_div
                      |     +--:primary
                      |     |  +--:identifier["a"]
                      |     +--:op_exp
                      |        +--:primary
                      |        |  +--:identifier["a"]
                      |        +--:op_exp
                      |           +--:primary
                      |           |  +--:int_literal["4"]
                      |           +--:primary
                      |              +--:int_literal["5"]
                      +--:semicolon[";"]

This parse tree corresponds to the input

    var a; var b; a := 5; b := a / a^4^5;

As you work on the parser, you can modify **testprog** to test other inputs to ensure they parse correctly.

## Example inputs and parse trees

Here are some example inputs you can try, along with their expected parse trees.  (Copy the example inputs into the contents of the string assigned to **testprog**.)

Example input:

    a * (b + 3);

Expected output:

    :unit
    +--:statement_list
       +--:statement
          +--:expression_statement
             +--:op_mul
             |  +--:primary
             |  |  +--:identifier["a"]
             |  +--:primary
             |     +--:lparen["("]
             |     +--:op_plus
             |     |  +--:primary
             |     |  |  +--:identifier["b"]
             |     |  +--:primary
             |     |     +--:int_literal["3"]
             |     +--:rparen[")"]
             +--:semicolon[";"]

Example input:

    while (a < b) { a := a * 4; }

Expected parse tree:

    :unit
    +--:statement_list
       +--:statement
          +--:while_statement
             +--:while["while"]
             +--:lparen["("]
             +--:op_lt
             |  +--:primary
             |  |  +--:identifier["a"]
             |  +--:primary
             |     +--:identifier["b"]
             +--:rparen[")"]
             +--:lbrace["{"]
             +--:statement_list
             |  +--:statement
             |     +--:expression_statement
             |        +--:op_assign
             |        |  +--:primary
             |        |  |  +--:identifier["a"]
             |        |  +--:op_mul
             |        |     +--:primary
             |        |     |  +--:identifier["a"]
             |        |     +--:primary
             |        |        +--:int_literal["4"]
             |        +--:semicolon[";"]
             +--:rbrace["}"]

Example input:

    if ( (a + b) / 5 >= 44 ) { c := c ^ (2 + d); }

Expected parse tree:

    :unit
    +--:statement_list
       +--:statement
          +--:if_statement
             +--:if["if"]
             +--:lparen["("]
             +--:op_gte
             |  +--:op_div
             |  |  +--:primary
             |  |  |  +--:lparen["("]
             |  |  |  +--:op_plus
             |  |  |  |  +--:primary
             |  |  |  |  |  +--:identifier["a"]
             |  |  |  |  +--:primary
             |  |  |  |     +--:identifier["b"]
             |  |  |  +--:rparen[")"]
             |  |  +--:primary
             |  |     +--:int_literal["5"]
             |  +--:primary
             |     +--:int_literal["44"]
             +--:rparen[")"]
             +--:lbrace["{"]
             +--:statement_list
             |  +--:statement
             |     +--:expression_statement
             |        +--:op_assign
             |        |  +--:primary
             |        |  |  +--:identifier["c"]
             |        |  +--:op_exp
             |        |     +--:primary
             |        |     |  +--:identifier["c"]
             |        |     +--:primary
             |        |        +--:lparen["("]
             |        |        +--:op_plus
             |        |        |  +--:primary
             |        |        |  |  +--:int_literal["2"]
             |        |        |  +--:primary
             |        |        |     +--:identifier["d"]
             |        |        +--:rparen[")"]
             |        +--:semicolon[";"]
             +--:rbrace["}"]

# Submitting

When you are done, submit the lab to the Marmoset server using either of the methods below.

> **Important**: after you submit, log into the submission server and verify that the correct files were uploaded. You are responsible for ensuring that you upload the correct files. I may assign a grade of 0 for an incorrectly submitted assignment.

From a web browser
------------------

Create a zip file containing your completed project.  (If you are in Eclipse, you can use **File &rarr; Export... &rarr; General &rarr; Archive File**.)

Upload the saved zip file to the Marmoset server as **assign06**. The server URL is

> [https://cs.ycp.edu/marmoset/](https://cs.ycp.edu/marmoset/)

From the command line
---------------------

From the command line, run the command

    make submit

Type your Marmoset username and password when prompted.
