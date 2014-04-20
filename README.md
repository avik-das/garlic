scheme-compiler
===============

(I need to come up with a better name.)

A small, self-contained compiler for a Scheme-like language written in Ruby.

Quick Start
-----------

```sh
git clone https://github.com/avik-das/scheme-compiler.git
./s-expr.rb test.scm
gcc -s compiled.s stdlib.c hashmap.c # later, this will be automated
./a.out
```
