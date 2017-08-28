---
layout: default
title: "Lab 11: Clojure functions"
---

# Getting started

Download [cs340-lab11.zip](cs340-lab11.zip).

Unzip it in your `CS340` directory, e.g.

    cd CS340
    unzip cs340-lab11.zip

Open `src/cs340_lab11/core.clj` in an editor (e.g., Kate) or import it into Eclipse as follows:

1. **File** &rarr; **New** &rarr; **Project...** &rarr; **General** &rarr; **Project**
2. Enter `cs340-lab11` as the project name
3. Uncheck "Use default location", click **Browse**, then choose the `cs340-lab11` directory
4. Click **Finish**

You should see a project called **cs340-lab11** in your workspace.  You can now open `src/cs340_lab11/core.clj`.  (Note that these steps will only work if you have Counterclockwise installed.)

# Your task

Your task is to implement each of the following functions:

* `tally-item`
* `invoice-total`
* `swapsies`
* `mulv3`
* `mulv`

Each function has a detailed comment indicating the parameter(s) the function takes and what value should be returned, along with example invocations and expected results.

Run the `lein test` command to run the unit tests.  You can see the code for the unit tests in the file `test/cs340_lab11/core_test.clj`.

## Hints

You don't need to do the functions in order (other than that you will want to implement `tally-item` before `invoice-total`.)

You may find that destructuring is helpful for `tally-item`, `swapsies`, and `mulv3`.

<!--
# Solution

When you are done, you can compare your solution to mine:

> [cs340-lab11-solution.zip](cs340-lab11-solution.zip)
-->
