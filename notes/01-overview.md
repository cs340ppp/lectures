# Course Overview

## Agenda

- Faculty & Staff
- Course Overview
- Administrivia

## Faculty & Staff

Prof: *Michael Lee*

- Email: <lee@iit.edu>
- Homepage: <https://moss.cs.iit.edu>
- Hours: Tue/Thu 12:00-14:00

Office hours are by appointment only -- make appointments on my homepage. You can also reach out anytime via MS Teams for asynchronous help.

TA: Shuichi Maruyama <smaruyam@hawk.illinoistech.edu>

The TA will grade all machine problems and is the first point of contact for questions about grading.

## Course Overview

### What is it about?

"Programming *Patterns* and *Paradigms*"

#### Paradigms

*Paradigm*: Model for how to *organize*, *express*, or *execute* a program. A language's paradigm is the water we swim in as we program with it -- we may not even be aware of how it guides our thinking!

A given paradigm typically imposes some syntactic/semantic conventions or limits on programs.

Examples:

- Computation: Imperative / Declarative (Functional / Logic)

- Modularization: Monolithic / Procedural / Object-Oriented

- Concurrency: Concurrent / Asynchronous / Distributed

Most modern languages support *multiple paradigms* simultaneously, but tend to default to the *imperative* paradigm. For this reason, we will be use a *functional* language.

#### Patterns

*Pattern*: Reusable *blueprint* for solving a common class of problems

The "wheels" of software development.

Some OOP/Imperative Patterns:

- Loops/Iterators

- Encapsulation (setters & getters)

- Singleton & Factory

- Observer pattern, aka Publish/Subscribe

We will encounter their functional counterparts, and lots more!

E.g., recursion in place of iteration

### Why is it important?

Functional programming is exploding in popularity, with good reason!

- *Industry adoption*: FP concepts now standard in JavaScript, Python, Rust, Swift, Kotlin

- *Modern frameworks*: React, Redux, async/await all rooted in FP patterns

- *Better code*: Strong typing and immutability prevent entire classes of bugs, and lets us write more modular, testable, and maintainable programs

- *Cutting edge features*: Many new programming language features are developed and tested in functional PLs first

Identifying and learning common patterns is important so that we:

- know what tools we have available to us (if all you have is a hammer, everything looks like a nail)

- don't waste time reinventing them

We'll encounter patterns that pop up (though sometimes disguised) everywhere:

- Monads -> Promises, Optionals, Result types

- Async abstractions -> Node.js, Python asyncio, Rust tokio

- ADTs -> Rust enums, TypeScript discriminated unions

- HOFs -> map/filter/reduce in every language

- DSLs -> SQL, React JSX, configuration languages

### How are we going to learn all this?

- Language: Haskell

  - Clean, elegant syntax

  - Powerful type system

  - *Purely* functional

  - Be forewarned: it will feel like a straitjacket until you get used to it!

- Progression: Fundamentals -> data structures -> effects -> concurrency -> advanced patterns

- Focus on understanding: Less memorization, more pattern recognition

- Real applications: Every concept connects to tools you'll use professionally

#### A Taste of Haskell

```haskell
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes = filterPrime [2..] 
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

quicksort :: Ord a => [a] -> [a]
quicksort []      = []
quicksort (p:xs)  = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

## Learning Outcomes

After successfully completing the class, students should be able to:

- Write *substantial, well-typed programs* using Haskell

- Apply functional programming techniques such as *recursion*, *higher-order functions*, and *pattern matching* to solve problems

- Design and implement *reusable, polymorphic data types* and *functional data structures*

- Use *monads* to compose and manage *computational effects*

- Write *asynchronous* and *concurrent* programs using functional abstractions

- Design and implement *domain-specific languages* for solving structured problems

- Write and use *property-based tests* to verify program correctness

## Topics

- Types and Type Systems

- Pattern Matching

- Currying and Partial Evaluation

- Recursion

- Lazy Evaluation

- Higher-Order Functions

- Parametric and Ad Hoc Polymorphism

- Algebraic Data Types and Functional Data Structures

- Monads

- Property-Based Testing

- Asynchronous and Concurrent Programming

## Resources and Tools

- Haskell Toolchain
- Git, GitHub, and GitHub Classroom
- Lecture Repository

### Haskell Toolchain

Toolchain setup (instructions on Canvas)

- `GHCUp`: installer for Haskell development tools

- `GHC`: Glasgow Haskell Compiler

  - `GHCi`: GHC's interactive environment (REPL)

- `Stack`: a Haskell build tool

Setup instructions will be posted to Canvas, and I'll start demonstrating their use in class on Friday.

### Git, GitHub, and GitHub Classroom

Most course content is version-controlled

- `git` for tracking course content and your own work

- *GitHub* for hosting public and private shared repositories

- *GitHub Classroom* for handling assignment distribution and GitHub <-> @IllinoisTech username mapping

### Lecture Repository

Located at `https://github.com/cs340ppp/lectures`

- Two branches: `main` and `demo`

  - `demo` contains "starter" code for lectures, and `main` contains "completed" code (spoilers!)

  - I will be updating both branches as the semester progresses. My recommended workflow is to clone the repository and create your own branch off of `demo`, and edit it to add your own notes and code, merging in changes that I make periodically.

- `lectures/notes`: Markdown formatted notes

  - Lecture slides are distilled from these

- `lectures/src`: Haskell source files

## Administrivia

- Prerequisites
- Grading
- Assessments
- References

### Prerequisites

Students are expected to be familiar with an imperative, statically-typed procedural or object-oriented language, and to have written reasonably sophisticated programs (500+ lines of code) with it.

Officially, CS 116/201 are prereqs.

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

There will be two exams, covering concepts and practical skills. Exams will be synchronous, in-person, and closed-device/closed-notes. The midterm exam will take place on or around March 4th, and the final exam will take place during finals week (May 4-9).

At my discretion, I may apply a linear formula to normalize exam scores such that the maximum and average scores are adjusted to 100% and 75%.

#### Machine Problems

Each machine problem (MP) will ask you to implement a working software application based on a high-level specification and set of functional requirements. MPs will be released via GitHub Classroom, and submission will be via shared, private Git repositories. Each MP writeup will detail its grading criteria and point breakdown. MPs are weighted proportionally according to their allotted points.

#### Late Policy

In general, machine problems will not be accepted late for credit. I understand, however, that circumstances may arise in life, school, and work that get in the way of a timely submission. If you have a valid reason (i.e., not just procrastination or time mismanagement) for needing an extra day or two, please get in touch with me **before an assignment is due**, and we can discuss the possibility of an extension.

### References

- Learn You a Haskell for Great Good
- Graham Hutton, *Programming in Haskell*
- O'Sullivan, Stewart, Goerzen, *Real World Haskell*

## Before Next Class

Read Hughes's "Why Functional Programming Matters" (at least sections 1 & 2, if you can get further, great!)
