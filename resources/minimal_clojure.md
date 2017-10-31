---
layout: default
title: "Minimal Clojure environment"
---

# Minimal Clojure environment

This document describes how to set up a minimal Clojure environment under Linux or MacOS.

## Java

First, make sure that Java 1.8 or higher is installed.  If you run

```bash
java -version
```

from the command line, you should see something like the following:

```
openjdk version "1.8.0_131"
OpenJDK Runtime Environment (build 1.8.0_131-8u131-b11-2ubuntu1.16.04.3-b11)
OpenJDK 64-Bit Server VM (build 25.131-b11, mixed mode)
```

The exact output isn't important as long as the version number starts with `1.8` or `1.9`.

## Leiningen

Start a terminal and run the following commands:

```
cd $HOME
mkdir -p bin
cd bin
[ -e lein ] && mv lein lein.OLD
wget https://raw.githubusercontent.com/technomancy/leiningen/2.7.1/bin/lein
chmod a+x lein
```

## Configure .bashrc

Edit the `.bashrc` file in your home directory to add the following lines at the bottom of the file:

```
export PATH=$HOME/bin:$PATH
```

Close your existing terminal (if any), start a new terminal, and then run the command

```bash
echo $PATH
```

Make sure you see something like <tt><i>homedir</i>/bin</tt> in your `$PATH`.  For example, my home directory is `/home/dhovemey`, so I would expect to see `/home/dhovemey/bin` in my `$PATH`.

## Verify leiningen

In your terminal, run the command

```bash
which lein
```

Make sure the output is something like <tt><i>homedir</i>/bin/lein</tt>, where <tt><i>homedir</i></tt> is your home directory.  When I run this command, I see the output

```
/home/dhovemey/bin/lein
```

because my home directory is `/home/dhovemey`.  **Important**: make sure you do *not* see `/usr/bin/lein`.

Next, run the command

```bash
lein -version
```

You should see something like

```
Leiningen 2.7.1 on Java 1.8.0_131 OpenJDK 64-Bit Server VM
```

Finally, run the following commands:

```bash
cd $HOME
rm -rf .lein
```

These last commands will clear out any previously cached dependencies and ensure you get a completely fresh Clojure environment.

## Using Leiningen

Now run

```bash
lein repl
```

You should be in a Clojure REPL.  If this works, you should be able to run Leiningen commands from the command line.  For example, let's say your work on the `structured-data` MOOC chapter is in `$HOME/git/structured-data`.  To run the tests, you'd do something like

```bash
cd $HOME
cd git/structured-data
lein midje
```
