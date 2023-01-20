Setup & Installation Instructions
---------------------------------

## Overview

This document contains instructions for setting up and installing the recommended toolchain for building and running the code examples in this repository. The instructions are written for a Unix-like environment (e.g., macOS and Linux), but are easily adaptable to Windows (in particular, the [Windows Subsystem for Linux](https://learn.microsoft.com/en-us/windows/wsl/about)).

Here's a list of the software you'll be installing:

- GHCup, a tool for installing and managing GHC and related software
- GHC, the Glasgow Haskell Compiler
- HLS, the Haskell Language Server
- Stack, a build tool for Haskell projects
- Microsoft Visual Studio Code, a free, open-source editor

## Prerequisites

You need to know the basics of how to navigate files and directories using a Unix shell (via a terminal application). If you're not familiar with this, you can learn the basics by reading through the first two lessons at  [Software Carpentry Unix Shell](https://swcarpentry.github.io/shell-novice/).

You will need a working Git installation. If you don't already have one, you can download and install Git from [https://git-scm.com/downloads](https://git-scm.com/downloads).

On macOS, you will need to have command line tools installed. You can install them by running (at a Terminal):

    xcode-select --install

## Video Tutorial (for the next two sections)

If you prefer to watch a video tutorial, you can visit <https://youtu.be/9lF7JpvHsEk>

## Installing GHCup

To install GHCup, run the following command in a terminal:

    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

In order, answer 'P', 'Y', and 'Y' to the prompts, in order to prepend GHCup to your PATH, install HLS, and use the GHC installed by GHCup.  After the installation is complete, you will need to restart your terminal session (or run `source ~/.ghcup/env`).

You can confirm that GHCup is installed by running:

    ghcup --version

## Cloning and Building the Lecture Repository

To clone the lecture repository, run the following command in a terminal:

    git clone https://github.com/cs340ppp/lectures

After this completes, you will have a directory called `lectures` in your current directory. It contains, among other things, all of the lecture source files and build configurations for the containing project.

To build the lecture files, `cd` into the directory and run the following command in a terminal:

    stack build

The first time you run this command, it will take a while to download and install the necessary dependencies. After that, it should be much faster.

If the command completes successfully, congratulations! You've installed the recommended toolchain for building and running the code examples in this repository, and compiled all the lecture source files.

To run the main function in the lecture files, run the following command in a terminal:

    stack exec cs340-exe

To run the tests, run the following command in a terminal:

    stack test

To load the lecture files into GHCi, run the following command in a terminal:

    stack ghci

To load a specific lecture's (e.g., lecture 1) source file, run the following command from within GHCi:

    :l src/Lect01.lhs

## Installing the Recommended Editor

We recommend using [Visual Studio Code](https://code.visualstudio.com/) as your editor. It is a free, open-source editor that is easy to use and has a large community of users. It is also the editor that the course staff uses.

To install Visual Studio Code, download and run the installer from [https://code.visualstudio.com/](https://code.visualstudio.com/). 

After installing and opening Visual Studio Code, go to the Extensions tab (on the left side of the window) and search for extension named "Haskell". Install the extension by clicking the "Install" button.

You should now be able to open the lecture files in Visual Studio Code and have them automatically loaded into the Haskell Language Server (HLS). You can confirm that HLS is working by opening a lecture file and hovering over a function name. You should see a tooltip with the type of the function.

That's it! Be sure to get in touch with the TA or instructor if you have any questions or run into any problems. See you in class!
