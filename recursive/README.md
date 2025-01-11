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

# Compile a simple "program" that just outputs an integer
./garlic test-numbers.scm  # Writes a file named "main"
chmod +x main
./main
echo $?  # Should print an integer (will depend on latest state of repo)
```

Note that you can change the name of the output executable:

```sh
./garlic -O executable-name test-numbers.scm
./garlic --output executable-name test-numbers.scm
```

### Running in interpreted mode

The code generation is lagging the furthest behind currently, but the lexer/parser supports many constructs already. To test the latter, the recursive implementation can be run in interpreted mode, which supports all the features the lexer/parser do:

```sh
# From inside the "recursive" directory
make

# Interpret a much more full featured program. The intepreter will output
# anything to be displayed on the standard output. Also output it to a file to
# compare against the reference output.
./garlic --interpret test.scm | tee out.txt
diff out.txt test.scm.result  # Ensure the output is correct
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
