The Garlic Programming Language
===============================

Garlic--short for **G**arlic's **A** **R**uby **L**isp **I**mplementation **C**ompiler--is programming language in the Lisp family. As the name implies, it has an ahead-of-time compiler written in Ruby. The compiler targets the x86-64 processor architecture, with the System V AMD64 ABI calling convention used by GCC.

Instead of emitting a final binary, the compiler generates assembly code compatible with the GNU assembler. The assembly code is then handed to `gcc`, along with the runtime and any C files accessed via the foreign-function interface, in order to create a suitable executable for the current platform, which may be either Linux or OS X.

Quick Start
-----------

```sh
git clone https://github.com/avik-das/scheme-compiler.git
cd scheme-compiler
bundle install

# Compile and run an example. Ensure that gcc is installed.
./s-expr.rb test/success/full.scm
./main

# Run the tests
./run-tests.sh

# Run an example using the SDL2 library. Ensure that the necessary SDL2
# developement packages (libsdl2-dev and libsdl2-image-dev, or the
# corresponding frameworks on OS X).
cd sdl2-test
make
./marley
make clean
```

Why?
----

This is a personal project of mine. I'm using this as an exercise to play around with language features, and as an excuse to provide myself with a fun challenge. This alone should explain the design and architectural choices I've made.

High-level features
-------------------

* Scheme-inspired language
* No macros (yet!)
* Module system
* Foreign-function interface allowing implementation of garlic modules in C

The language
------------

### Basics

Garlic uses s-expressions as its main syntactic element. The first element of each list denotes the function being called, and the remainder are the arguments to that function.

As an example, `display` is a unary (one-argument) function that prints its argument to the screen. In the code below, the argument itself is a nested function call, where the function being called is `+`, the addition function. This function is a variadic (variable number of arguments) function, with the example showing two arguments.

Finally, `newline` is a function that takes no arguments.

```scheme
(display (+ 1 2))
(newline)
```

### Data Types

Data types in garlic consist of atoms and s-expressions.

The first type of atom is the integer. An integer can be positive or negative, and currently, may only be within the range of `-2^62` to `2^62 - 1`. In the future, integers outside of this range may be promoted to some type of arbitrary-precision integer.

Floating point and fractional numbers are not supported at this time.

```scheme
1
2
100
0
-3
-4
-200
```

The next type of atom is the boolean.

```scheme
#t ; true
#f ; false
```

Strings are also considered atoms. Strings are denoted by double-quotes, and double quotes may be escaped inside the string by a backslash. The standard escape sequences such as `\n` and `\t` are supported.

```scheme
"a string"
"some numbers and symbols: 1 2 3 # @"
"a string with\na newline"
"\ta string with an embedded tab"
"a string with \"escaped\" quotes"
"no need to 'escape' single quotes"
""
```

Aside from atoms, there are also `cons` cells, consisting of two parts: a `car` and a `cdr`. They can be formed with the `cons` function, and they can be nested.

```scheme
(cons 1 2)
(car (cons 1 2)) ; 1
(cdr (cons 1 2)) ; 2

(car (cdr (cons (cons 1 2) (cons 3 4)))) ; 3
```

Any valid identifier can be used as symbol by quoting it, as can any number. One special symbol is `nil`, denoted by `'()`.

```scheme
'a-quoted-value
'1
'-1
'() ; special value, nil
```

With these building blocks, we can form _lists_, a data structure consisting of nested `cons` cells ending in `nil`. A list can also be formed by the `list` function, or by quoting an s-expression. In the latter case, all elements of the list are quoted automatically. In this way, `nil` is itself a list, albeit an emty one.

```scheme
; The following are equivalent:
(cons 'a (cons 'b (cons 'c (cons 'd '()))))
(list 'a 'b 'c 'd)
'(a b c d)

; You can use car and cdr on lists:
(car '(1 2 3)) ; 1
(cdr '(1 2 3)) ; '(2 3)
```

Note that if non-`nil` terminated `cons` cell is desired, it can be expressed as an _improper_ list:

```scheme
; The following are equivalent:
'(1 . 2)
(cons 1 2)
```

### Functions

The fundamental means of abstraction in garlic is the lambda, a way of defining a function that can be called later.

Here, the lambda takes three arguments, `a`, `b` and `c`, and returns `a + b - c`. Given a lambda, it can then be used as the first item of an sexp, since it is itself a function.

```scheme
; (a b c) -> a + b -c
(lambda (a b c) (- (+ a b) c))

; evaluates to 8
((lambda (x) (* 2 x)) 4)
```

The body of a lambda can consist of multiple statements.

```scheme
(lambda (x)
  (display "[INFO] ")
  (display x)
  (newline))
```

All values, lambdas or otherwise, can be assigned to a name using `define`. Note that definitions can refer to each other, but the values are assigned in the order they appear in the source. However, definitions are "hoisted" to the top of a scope, meaning it's possible to refer to names defined after its use.

```scheme
; evaluates to 7
(g 3)

(define c 2)
(define f (lambda (x) (* c x)))     ; x -> c * x = 2 * x
(define g (lambda (x) (+ (f x) 1))) ; x -> f(x) + 1 = 2 * x + 1
```

Since naming a lambda is such a common pattern, the `define` syntax can be made terser for function definitions:

```scheme
; This...
(define (g a b c)
  (+ a b c))

; ...is equivalent to
(define g
  (lambda (a b c)
    (+ a b c)))
```

A function introduces a new scope. This means that all defined identifiers within the function body, including the function parameters, are only visible within the function body and not outside. Furthermore, the function "closes over" all the identifiers in the function's lexical scope, i.e. all the identifiers visible to the function. Note that it's possible to shadow a closed over variable within the function body, but it does not affect the value of the identifier outside the body.

```scheme
(define a 1)
(define c 2)

; notice the definition hoisting inside the function body
(define (f a b)
  (+ a b c)
  (define c 5))

; evaluates to 12
(f 3 4)

; evaluates to 1 and 2 respectively
a
c

; b is not visible outside the function body
```

Functions can take a variable number of arguments. These functions are called "variadic" functions, and they can optionally have a sequence of required arguments preceding the variadic arguments. All arguments after the required ones are collected into a single list.

In the following example, the function `f` takes two required arguments `a` and `b` and the remaining arguments are packaged into `rest`. Thus, `f` must always be called with at least two arguments, possibly more.

Meanwhile, the function `g` takes no required arguments, so it might be called with any number of arguments, even none.

```scheme
(define (f a b . rest)
  (+ a b (length rest)))

(f 1 2)       ; -> 3
(f 1 2 3)     ; -> 4
(f 1 2 3 4 5) ; -> 6

(define (g . rest)
  (null? rest))

(g)     ; -> #t
(g 1)   ; -> #f
(g 1 2) ; -> #f
```

Note that anonymous `lambda`s support variable numbers of arguments as well, though the syntax for the no required arguments case is slightly different. `f` and `g` above could be defined as follows:

```scheme
; similar to a function definition
(lambda (a b . rest)
  (+ a b (length rest)))

; note that the parameter list is not enclosed in parentheses
(lambda rest
  (null? rest))
```

### Local variable bindings

* let
* let\*
* letrec

### Modules

Garlic supports a full module system. A module consists of a single garlic file that defines a set of identifiers it wants to mark as visible to other garlic files. In the following module, `f` and `g` are exported by the `module-export` syntax, but `h` is not. By putting it a file called `my-module.scm`, this file defines the `my-module` module.

```scheme
; my-module.scm

(define (f x) (+ x 1))
(define (g x) (h (+ x 2)))
(define (h x) (+ x 3))

(module-export
  f
  g)
```

Given such a module, another garlic file can import the module and refer to the public identifiers exported by the module. This is done by using the `require` syntax, where the name of the module is a string denoting the relative path to the module file, without the `.scm` file extension. Once the module is `require`d, any references to identifiers within the module are preceded by the module name and a colon:

```scheme
(require "my-module")

; if the module were in different directly, you would require:
;   (require "../other/directory/my-module")

(my-module:f 2) ; -> 3
(my-module:g 2) ; -> 7

; there is no my-module:h
```

There are a few modules are part of the standard library, such as `display-helpers`. Instead of specifying the full path to these modules, it is enough to refer to them by name, without the double-quotes.

```scheme
(require display-helpers)

(display-helpers:display-with-tag "INFO" "display-with-tag")

; displays:
; [INFO] display-with-tag
```

 - module initialization
 - cyclical dependencies
 - renaming
 - import all
   - original name
 - stdlib

### Foreign-function interface

 - require
 - defining C functions usable in Lisp
 - exports
 - scm.h
