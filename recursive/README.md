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

I'm also working on a small library for generating ELF files. While I'm developing the library, the module for the library is itself a program that generates a static, hard-coded ELF file. To run that program:

```sh
# From inside the "recursive" directory

../garlic -o elf-test elf-x86-64-linux-gnu.scm
./elf-test
chmod +x generated-elf
./generated-elf
echo $?  # Should print "42"
```

Why?
----

One of the goals of Garlic has always been "to implement a compiler that is useful for writing programs." A compiler written in Garlic would demonstrate that the language is useful. Indeed, in developing the self-hosted version, I've found many deficiencies in the language and added support for useful features.

Current Status
--------------

The self-hosted implementation is in its early stages. The current version of the implementation is not a compiler, but a meta-circular interpreter. Support for basic s-expressions, including defining values and functions, as well as calling functions is present. This is sufficient for a simple and powerful, but extremely unwieldy, language.

Some of the major work to be done:

- Better error messages. Actually, recent changes to the recursive implementation have yielded clearer error messages than even the Ruby implementation, in certain scenarios. However, more work is required, especially for runtime errors.

- Support more tokens in the lexer:

  - Floats
  - Dotted lists

- Support a wider variety of language constructs

  - `let` bindings
  - `begin`
  - Variadic functions
  - Recursive functions

- Implement a module system

- Implement a compiler backend so that native binaries are produced. One of the goals with such a backend is finally adding macro support.
