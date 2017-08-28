---
layout: default
title: "Lab 12: Tic Tac Toe"
---

# Getting started

Download [cs340-lab12.zip](cs340-lab12.zip).

Unzip it in your `CS340` directory, e.g.

    cd CS340
    unzip cs340-lab12.zip

Open `src/cs340_lab12/core.clj` in an editor (e.g., Kate) or import it into Eclipse as follows:

1. **File** &rarr; **New** &rarr; **Project...** &rarr; **General** &rarr; **Project**
2. Enter `cs340-lab12` as the project name
3. Uncheck "Use default location", click **Browse**, then choose the `cs340-lab12` directory
4. Click **Finish**

You should see a project called **cs340-lab12** in your workspace.  You can now open `src/cs340_lab12/core.clj`.  (Note that these steps will only work if you have Counterclockwise installed.)

If you do not see **Leiningen dependencies** in your **cs340-lab12** project, right-click on the project and choose **Configure** &rarr; **Convert to Leiningen project**.  If the project is not configured as a Leiningen project, you won't be able to start a REPL from within Eclipse.

# Your task

Your task is to implement each of the following functions:

* `check-x`
* `check-o`
* `make-check-fn`
* `get-row`
* `get-col`

Each function has a detailed comment indicating the parameter(s) the function takes and what value should be returned, along with example invocations and expected results.

Run the `lein test` command to run the unit tests.  You can see the code for the unit tests in the file `test/cs340_lab12/core_test.clj`.

## Hints

You may find the `mapv` function to be useful in implementing `get-col`.

`make-check-fn` is a higher-order function (a function which returns a function).  Make sure you understand what the examples shown in the comment are doing.

If you get all of the functions working, examine the `is-win?` function.  See if you can improve it.

<!--
# Solution

When you are done, you can compare your solution to mine:

> [cs340-lab12-solution.zip](cs340-lab12-solution.zip)
-->
