Self-hosted implementation
==========================

An attempt at re-implementing Garlic in itself. This would make the name of the language **G**arlic's **A** _**R**ecursive_ **L**isp **I**mplementation **C**ompiler.

Success for this project entails running the full test suite for the current compiler, only using the self-hosted compiler. This includes all the small language features, like numbers, strings, functions, etc., as well as larger ones such as the full module system.

Quick Start
-----------

```sh
# From inside the "recursive" directory

# Build the recursive implementation
make

# Run it against the included test file. Because the current implementation
# is an interpreter instead of a compiler, this will evaluate the test file
# directly instead of creating another executable.
./garlic test.scm
```

Why?
----

One of the goals of Garlic has always been "to implement a compiler that is useful for writing programs." A compiler written in Garlic would demonstrate that the language is useful. Indeed, in developing the self-hosted version, I've found many deficiencies in the language and added support for useful features.

Current Status
--------------

The self-hosted implementation is in its early stages. The current version of the implementation is not a compiler, but a meta-circular interpreter. Support for basic s-expressions, including defining values and functions, as well as calling functions is present. This is sufficient for a simple and powerful, but extremely unwieldy, language.

Some of the major work to be done:

- Better error messages. There are many missing features, but one of the worst problems is that these missing features don't result in clear errors. For example, comments are not supported, but including a comment may cause nothing to be evaluated, instead of a message stating that the lexing failed at a particular location. Or, instead of signalling an unreferenced variable, the interpreter will simply segfault.

- Support more tokens in the lexer:

  - Strings
  - Comments
  - Negative numbers
  - Floats
  - Booleans
  - Quoted list and atoms

- Support a wider variety of language constructs

  - `let` bindings
  - Dotted lists
  - `begin`
  - Conditionals
  - Variadic functions

- Implement a module system

- Implement a compiler backend so that native binaries are produced. One of the goals with such a backend is finally adding macro support.
