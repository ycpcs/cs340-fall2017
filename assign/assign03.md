---
layout: default
title: "Assignment 3: JSON Parser"
---

**Due**: Monday, Sept 25th by 11:59 PM

Getting Started
===============

Download [CS340\_Assign03.zip](CS340_Assign03.zip) and unzip it.

Import it into Eclipse **Using File&rarr;Import...&rarr;General&rarr;Existing projects into workspace&rarr;Archive File**.  You should see a project called **CS340\_Assign03** in your Eclipse workspace.

Your Task
=========

Your task is to modify the **JSONParser** class so that it implements a parser for subset of [JSON](http://en.wikipedia.org/wiki/JSON). JSON is a standard format for representing structured data in text format, and is frequently used by web applications to transfer data.

The grammar
-----------

Here is the grammar for the JSON subset your parser should handle:

> *Value* → *StringLiteral* \| *IntLiteral* \| *Object* \| *Array*
> 
> *Object* → "{" *OptFieldList* "}"
> 
> *OptFieldList* → *FieldList* \| ε
> 
> *FieldList* → *Field* \| *Field* "," *FieldList*
> 
> *Field* → *StringLiteral* ":" *Value*
> 
> *Array* → "[" *OptValueList* "]"
> 
> *OptValueList* → *ValueList* \| ε
> 
> *ValueList* → *Value* \| *Value* "," *ValueList*

Some explanations:

-   The italicized words (*Value*, *Object*, etc.) are nonterminal symbols, except for *StringLiteral* and *IntLiteral*, which refer to **STRING\_LITERAL** and **INT\_LITERAL** tokens
-   The items in double quotes ("{", ":", etc.) are terminal symbols (tokens)
-   The vertical bar (\|) indicates alternate productions on the same nonterminal symbol

Approach
--------

Your parser should have one parse function for each nonterminal symbol.

A **parseValue** function is provided to help you get started. It uses the lexer (lexical analyzer) to "peek" ahead in the input sequence of tokens. Based on the type of token seen, it either directly parses a string literal or integer value, or calls the **parseObject** or **parseArray** functions as appropriate.

The main challenge in implementing each parse function is deciding what production to apply. For some parse functions, such as **parseObject** and **parseArray**, handle nonterminal symbols for which there is only a single production, so they will be relatively simple. (The **parseObject** function is also provided for you, since it demonstrates how to consume terminal symbols using the **expect** method.)

The "interesting" parse functions will be for the *OptFieldList*, *FieldList*, *OptValueList*, and *ValueList* nonterminal symbols.

Hints and Specifications
------------------------

For the **parseOptFieldList** and **parseOptValueList** methods, the issue is determining whether or not to apply the epsilon production. The *OptFieldList* and *OptValueList* nonterminals, which are used in the productions for *FieldList* and *ValueList*, generate strings of symbols which will be terminated with the "}" and "]" symbols, respectively. So, these functions can use the lexer's **peek** method to find out what the next token will be, and if it is "}" (*OptFieldList*) or "]" (*OptValueList*), then the epsilon production should be applied.

For the **parseFieldList** and **parseValueList** methods, the issue is whether a single item (*Field* or *Value*) will be generated, or whether more than one item will be generated. A good approach is to start by expanding one item (i.e., call **parseField** or **parseValue**), then check to see whether then next token is a comma. A comma will indicate that the list continues past the first item, and so a recursive call to **parseFieldList**/**parseValueList** should be made.

**Important**: Your parser must reject inputs that can't be derived from the grammar by throwing a **ParserException** (or **LexerException**, either is fine).  For example, note how the **parseValue** method raises an exception if the input string is empty.  Make sure your parse functions raise an exception if an unexpected token is seen.  (Using the **expect** method may be useful.)

Testing
-------

To test your parser, run the **Main** class as a Java application.

Here are some example inputs that you can try, along with their expected parse trees:

First example:

    "abc"

Parse tree:

    VALUE
    +--STRING_LITERAL(""abc"")

Second example:

    123

Parse tree:

    VALUE
    +--INT_LITERAL("123")

Third example:

    { "abc" : 123 }

Parse tree:

    VALUE
    +--OBJECT
       +--LBRACE("{")
       +--OPT_FIELD_LIST
       |  +--FIELD_LIST
       |     +--FIELD
       |        +--STRING_LITERAL(""abc"")
       |        +--COLON(":")
       |        +--VALUE
       |           +--INT_LITERAL("123")
       +--RBRACE("}")

Fourth example:

    [ 1, 2, "a", 3, "bcd" ]

Parse tree:

    VALUE
    +--ARRAY
       +--LBRACKET("[")
       +--OPT_VALUE_LIST
       |  +--VALUE_LIST
       |     +--VALUE
       |     |  +--INT_LITERAL("1")
       |     +--COMMA(",")
       |     +--VALUE_LIST
       |        +--VALUE
       |        |  +--INT_LITERAL("2")
       |        +--COMMA(",")
       |        +--VALUE_LIST
       |           +--VALUE
       |           |  +--STRING_LITERAL(""a"")
       |           +--COMMA(",")
       |           +--VALUE_LIST
       |              +--VALUE
       |              |  +--INT_LITERAL("3")
       |              +--COMMA(",")
       |              +--VALUE_LIST
       |                 +--VALUE
       |                    +--STRING_LITERAL(""bcd"")
       +--RBRACKET("]")

Fifth example:

    { "abc" : [ 1, 2 ], "def" : { "hey" : "yo" } }

Parse tree:

    VALUE
    +--OBJECT
       +--LBRACE("{")
       +--OPT_FIELD_LIST
       |  +--FIELD_LIST
       |     +--FIELD
       |     |  +--STRING_LITERAL(""abc"")
       |     |  +--COLON(":")
       |     |  +--VALUE
       |     |     +--ARRAY
       |     |        +--LBRACKET("[")
       |     |        +--OPT_VALUE_LIST
       |     |        |  +--VALUE_LIST
       |     |        |     +--VALUE
       |     |        |     |  +--INT_LITERAL("1")
       |     |        |     +--COMMA(",")
       |     |        |     +--VALUE_LIST
       |     |        |        +--VALUE
       |     |        |           +--INT_LITERAL("2")
       |     |        +--RBRACKET("]")
       |     +--COMMA(",")
       |     +--FIELD_LIST
       |        +--FIELD
       |           +--STRING_LITERAL(""def"")
       |           +--COLON(":")
       |           +--VALUE
       |              +--OBJECT
       |                 +--LBRACE("{")
       |                 +--OPT_FIELD_LIST
       |                 |  +--FIELD_LIST
       |                 |     +--FIELD
       |                 |        +--STRING_LITERAL(""hey"")
       |                 |        +--COLON(":")
       |                 |        +--VALUE
       |                 |           +--STRING_LITERAL(""yo"")
       |                 +--RBRACE("}")
       +--RBRACE("}")

# Grading

Your parser will be graded according the following criteria:

* Parsing string literal values: 5%
* Parsing integer literal values: 5%
* Parsing empty object: 10%
* Parsing object with one field: 10%
* Parsing object with multiple fields: 15%
* Parsing empty array: 10%
* Parsing array with one element: 15%
* Parsing array with multiple elements: 15%
* Illegal input is rejected with a **ParserException**: 15%

# Submitting

When you are done, submit the lab to the Marmoset server using one of the methods below.

## From Eclipse

If you have the [Simple Marmoset Uploader Plugin](../resources/index.html) installed, select the project (**CS340\_Assign03**) in the package explorer and then press the blue up arrow button in the toolbar. Enter your Marmoset username and password when prompted.

This is the recommended way to submit your work.

## From a web browser

Save the project (**CS340\_Assign03**) to a zip file by right-clicking it and choosing

> **Export...&rarr;Archive File**

Upload the saved zip file to the Marmoset server as **assign03**. The server URL is

> <https://cs.ycp.edu/marmoset/>

Use this method only if there is some reason why you can't use the plugin.
