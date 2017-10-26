---
layout: default
title: "Assignment 4: Clojure MOOC"
---

**Due**:

* Milestone 1 is due Friday, Oct 13th
* Milestone 2 is due Wednesday, Oct 25th
* Milestone 3 is due Wednesday, Nov 1st

# Learning Clojure

We will be using [Clojure](http://clojure.org/) for a series of programming assignments.

The [Functional programming with Clojure](http://mooc.fi/courses/2014/clojure/index.html) MOOC at the University of Helsinki is a truly excellent way to learn Clojure.  In this assignment you will work on the first eight chapters.  There are three Milestones:

* Milestone 1: Complete **Basic tools**, **Training day**, and **I am a horse in the land of booleans**
* Milestone 2: Complete **Structured data** and **P-P-P-Pokerface** (look over **Style** as well, although there are no problems in that chapter)
* Milestone 3: Complete **Predicates** and **Recursion**

You can find the chapters on the [Material and course content](http://iloveponies.github.io/120-hour-epic-sax-marathon/index.html) page.

# Programming environment

The **Basic tools** chapter covers the software you will need.

I recommend using Eclipse with the [Counterclockwise](https://github.com/ccw-ide/ccw/wiki/GoogleCodeHome) plugin.  You can install this through the Eclipse marketplace (search for "counterclockwise").

As an alternative to Eclipse and Counterclockwise, you can try using [Light Table](http://www.lighttable.com/).  You can also use [Emacs](https://www.gnu.org/software/emacs/) as described in the textbook.

The computers in KEC 119 have Eclipse with Counterclockwise and Local Terminal.

# Using Git

The programming activities for each chapter are in a Git repository on GitHub.  The recommended way of starting each chapter is to fork the repository, and then clone your fork.

For example, after forking the **training-day** repository, I would clone my fork using the command

    cd
    cd git
    git clone git@github.com:daveho/training-day

This would place the repository in the `git` subdirectory of my home directory (which is the recommended place to put local Git repositories.)

# Importing the repository into your Eclipse workspace

If you are planning to use Eclipse and Counterclockwise, you will need to import the repository as an Eclipse project.  To do so:

1. Go the to Git perspective in Eclipse.
2. In the Git Repositories view, click the "Add an existing local Git repository" button, and choose the local Git repository (e.g., **training-day**).
3. Switch back to the Java perspective.
4. Choose **File&rarr;New&rarr;Project...&rarr;General&rarr;Project**.
5. Uncheck the "Use default location" checkbox.  Use "Browse" to choose the directory containing your local Git repository (e.g., **training-day**).  Enter the project name (e.g., **training-day**.)
6. Click "Finish".  You should now see the project in your Eclipse workspace.  The Clojure file you will need to edit will be in the **src** directory.
7. Right-click on the project and choose **Configure&rarr;Convert to Leiningen Project**.  (The "Progress Information" dialog may hang: just cancel it if this happens.)

Once you have set up the Eclipse project, you should commit your changes and push them to your fork.  In general, it is a good idea to commit and push your changes to your GitHub repository as you work: your GitHub repository will serve as a record of your progress.

# Testing your work

To run the unit tests for a programming activity:

1. In a terminal, run the command <code>cd $HOME && cd <i>path-to-repo</i></code>, where <i>path-to-repo</i> is the path to the local git repository containing your work.  (E.g., `git/training-day`.)
2. Next, run the command <code>lein midje</code>.  The output will indicate how many tests passed and which tests (if any) failed.

If you get an error from Leiningen about `midje` not being a valid task, try deleting the `.lein` directory (in your home directory) and running the command again.

To test your functions interactively (highly recommended!):

1. Start a terminal and cd into the repository directory (as described above).
2. Run the command `lein repl` to start a read-eval-print loop.
3. Enter <code>(use '<i>namespace</i>)</code>, where *namespace* is the namespace defined for the chapter, e.g., `i-am-a-horse-in-the-land-of-booleans`
4. Now you can call functions defined in your source code.  If you modify your code, you can run <code>(use '<i>namespace</i> :reload)</code> to reload your code in the REPL session.

# Sign-off

Please see me for each milestone so I can sign off on your progress.
