# Course Overview

## Agenda

- Faculty and Staff
- About the Class
- Resources and Tools
- Administrivia

## Faculty and Staff

Prof:

- *Michael Lee* lee@iit.edu
- Homepage: http://moss.cs.iit.edu
- Hours: TBA

TA:

- TBA

## About the Class

- What?
- Why?
- How?

### What?

"Programming *Patterns* and *Paradigms*"

- *Paradigm*: Model for how to *organize*, *express*, or *execute* a program

- *Pattern*: Reusable *blueprint* for solving a common class of problems

#### Paradigms

- Imperative / Functional / Logic
- Procedural / Object-Oriented
- Concurrent / Asynchronous / Distributed

#### Patterns

- Data Structures
- Recursion
- HOFs
- Monads
- DSLs
- Testing

### Why?

- *Industry adoption*: FP concepts now standard in JavaScript, Python, Rust, Swift, Kotlin
- *Think differently*: Strong typing and immutability prevent entire classes of bugs
- *Modern frameworks*: React, Redux, async/await all rooted in FP patterns
- *Better code*: Write more modular, testable, and maintainable programs
- *Career edge*: Stand out by understanding abstractions most developers only use superficially

Patterns You'll See Everywhere

- Monads → Promises, Optionals, Result types
- Async abstractions → Node.js, Python asyncio, Rust tokio
- ADTs → Rust enums, TypeScript discriminated unions
- HOFs → map/filter/reduce in every language
- DSLs → SQL, React JSX, configuration languages

### How?

- Language: Haskell (clean syntax, powerful type system, pure functional)
- Progression: Fundamentals → data structures → effects → concurrency → advanced patterns
- Hands-on practice: 6-8 machine problems to apply concepts immediately
- Build up gradually: Each topic scaffolds to the next
- Focus on understanding: Less memorization, more pattern recognition
- Real applications: Every concept connects to tools you'll use professionally

## Learning Outcomes

After successfully completing the class, students should be able to:

- Write *substantial, well-typed programs* using Haskell

- Apply functional programming techniques such as *recursion*, *higher-order functions*, and *pattern matching* to solve problems

- Design and implement *reusable, polymorphic data types* and *functional data structures*

- Use *monads* to compose and manage *computational effects*

- Write *asynchronous* and *concurrent* programs using functional abstractions

- Design and implement *domain-specific languages* for solving structured problems

- Write and use *property-based tests* to verify program correctness

## Resources and Tools

- Haskell Toolchain
- Git, GitHub, and GitHub Classroom
- Lecture Repository

### Haskell Toolchain

Toolchain setup instructions (writeup and video) on Canvas

- `GHCUp`: installer for Haskell development tools

- `GHC`: Glasgow Haskell Compiler

  - `GHCi`: GHC's interactive environment (REPL)

- `Stack`: a Haskell build tool

### Git, GitHub, and GitHub Classroom

Most course content is version-controlled

- `git` for tracking course content and your own work

- *GitHub* for hosting public and private shared repositories

- *GitHub Classroom* for handling assignment distribution and GitHub <-> @IllinoisTech username mapping

### Lecture Repository

Located at `https://github.com/cs340ppp/lectures`

- Two branches: `main` and `demo`

  - `demo` contains "starter" code for lectures, and `main` contains "completed" code (spoilers!)

- `lectures/slides`: Markdown formatted slides

  - I use [Presenterm][presenterm] to present them in class

- `lectures/src`: Haskell source files

[presenterm]: https://mfontanini.github.io/presenterm/

## Administrivia

- Prerequisites
- Grading
- Assessments
- References

### Prerequisites

### Grading

- 50%	Machine Problems
- 25%	Midterm Exam
- 25%	Final Exam

The grade scale is:

- A ≥ 90%
- B ≥ 80%
- C ≥ 70%
- D ≥ 60%
- E < 60%

#### Exams

There will be two exams, covering concepts and practical skills. Exams will be synchronous, in-person, and closed-device/closed-notes. The midterm exam will take place on or around March 13th (just before spring break), and the final exam will take place during finals week (May 4-9).

At my discretion, I may apply a linear formula to normalize exam scores such that the maximum and average scores are adjusted to 100% and 75%.

### Assessments

### Late Policy

### References

- Learn You a Haskell for Great Good
- Graham Hutton, *Programming in Haskell*
- O'Sullivan, Stewart, Goerzen, *Real World Haskell*

## Before Next Class

Read Hughes's "Why Functional Programming Matters" (at least sections 1 & 2, if you can get further, great!)
