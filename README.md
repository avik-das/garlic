The Garlic Programming Language
===============================

Garlic--short for **G**arlic's **A** **R**uby **L**isp **I**mplementation **C**ompiler--is programming language in the Lisp family. As the name implies, it has an ahead-of-time compiler written in Ruby. The compiler targets the x86-64 processor architecture, with the System V AMD64 ABI calling convention used by GCC.

Instead of emitting a final binary, the compiler generates assembly code compatible with the GNU assembler. The assembly code is then handed to `gcc`, along with the runtime and any C files accessed via the foreign function interface, in order to create a suitable executable for the current platform, which may be either Linux or OS X.

Quick Start
-----------

```sh
git clone https://github.com/avik-das/garlic.git
cd garlic
bundle install

# Compile and run an example. Ensure that gcc is installed.
./garlic test/success/full.scm
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
* Foreign function interface allowing implementation of garlic modules in C

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

### Conditionals

Use an `if` expression to conditionally evaluate one of two statements. An `if` expression has three parts: a condition, an expression to evaluate if the condition is "true-like" and an expression to evaluate otherwise. Note that _any_ value other than `#f` is considered "true-like," even `nil` and `0`.

```scheme
(define my-not
  (lambda (val) (if val #f #t)))

; all of the below will evaluate to 'correct
(if #t 'correct 'wrong)
(if #f 'wrong 'correct)
(if '() 'correct 'wrong) ; nil is truthy
(if 0 'correct 'wrong)   ; 0 is truthy
(if 1 'correct 'wrong)
(if (my-not #f) 'correct 'wrong)
(if (my-not #t) 'wrong 'correct)
(if (my-not 0) 'wrong 'correct)
(if (my-not 1) 'wrong 'correct)
```

To evaluate a chain of conditions, `if` expressions must be nested in the "else" expressions. Alternatively, you can use a `cond` expression, which allows for a series of conditions and associated expressions to evaluate.

```scheme
(define (to-word n)
  (cond ((= n 1) 'one)
        ((= n 2) 'two)
        ((= n 3) 'three)
        (else 'unknown)))

(to-word 1) ; one
(to-word 2) ; two
(to-word 3) ; three
(to-word 4) ; unknown
```

The `else` clause is optional. However, if there is no `else` clause and no clause matches, then the `cond` expression evaluates to `nil`.

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

Functions are convenient because they provide variable bindings that are visible only in a limited scope. `let` bindings provide the same benefit without the overhead of defining and calling a function.

The basic facility for local variable bindings is the `let` block. It takes a list of pairs, where the first element is an identifier and the second is an expression that evaluates to its value. These bindings are followed by a list of statements in which the bound variables are visible. Again, definitions within this block are hoisted to the top.

```scheme
(define a 1) ; accessible inside the let block
(define b 2) ; will be shadowed by a let binding
(define c 3) ; will be shadowed by a let binding
(define f 4) ; will be shadowed by define

(let ((b 5)
      (c 6)
      (d 7))
  (+ a b c d e f) ; 1 + 5 + 6 + 7 + 8 + 9 = 36
  (define e 8)
  (define f 9))

a ; 1
b ; 2
c ; 3
f ; 4

; d and e are not visible here
```

It's important to note that within the list of bindings, one bound expression cannot refer to any of the other bound variables. In essense, all the binding values are computed, and all the bindings are then created simultaneously.

One alternative is `let*`, which executes the bindings in sequential order, allowing one expression to refer to the previous bindings. However, previous bindings can't refer to later ones.

```scheme
; Not possible using "let"
(let* ((a 1)
       (b (+ 1 a))
       (c (+ a b)))
  c) ; 1 + 2 = 3
```

Finally, there is `letrec`, which makes all bindings available immediately. Note that _initialization_ of the bindings still happens in sequential order, so it's not possible to use the values of later bindings immediately. However, one binding may _refer_ to a later binding in a delayed context, such as a `lambda`. This allows for mutual recursion.

```scheme
; This example is adapted from the Racket language documentation.
(letrec ((is-even? (lambda (x)
                     (if (= x 0)
                       #t
                       (is-odd? (- x 1))) ))
         (is-odd? (lambda (x)
                    (if (= x 0)
                      #f
                      (is-even? (- x 1))) )))
  (is-odd? 11)) ; #t
```

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

`require` statements can only appear at the top level of a file, so it's not possible to conditionally import a module at run time.

When importing a module, a different module name can be chosen to represent it in the scope of the file using the renaming syntax. Typically, this is done to shorten the name of the module within the scope of file that depends on it.

```scheme
(require "my-module" => mm)

(mm:f 2) ; -> 3
(mm:g 2) ; -> 7
```

Once a module is imported using the renaming syntax, it is an error to prefix any identifiers with the original name. This is to avoid polluting the namespace with too many module names.

The module name prefix can be completely eliminated by using the "import all" syntax, after which all identifiers in the imported module become part of the current namespace.

```scheme
(require "my-module" *)

(f 2) ; -> 3
(g 2) ; -> 7
```

When importing using this syntax, it is still possible to refer to identifiers in the imported module using the original module name. This is to disambiguate in the case of a local definition shadowing one that was imported into the local namespace.

```scheme
(require "my-module" *)

(define (f n)
  (* n 2))

(f 2) ; -> 4, using the local function
(g 2) ; -> 7, using the imported function

(my-module:f 2) ; -> 3, using the imported function
(my-module:g 2) ; -> 7, using the imported function
```

In any program, it can be assumed that an implitic `(require stdlib *)` is present at the top of all modules. This means any functions defined in the standard library are available without any prefixing in all modules. As a consequence of using the "import all" version of the import, it is also possible to refer to standard library functions using the `stdlib` prefix. Again, this is useful if a local definition shadows a standard library definition.

```scheme
; notice the lack of an stdlib import

(not #t)
(stdlib:not #t)
```

It's possible for two modules to depend on each other, forming cycles in a prograam's dependency graph. This can be used for mutual recursion, but in general, such closely related functions should be defined in one module.

Any top-level statements in a module are considered part of the module's initialization routine. All modules required by a program are initialized exactly once at the start of the program's execution, regardless of where in the importing file it was imported.

The order of module initialization is undefined, except that a module will always be initialized before any module that depends on it. The exception is in the case of cyclical dependencies where the order is again undefined.

### Foreign function interface

When importing a module, if instead of a `module-name.scm` file there is a `module-name.c`, the latter file can be imported as if it's a Garlic module. This is the basis of the FFI, or Foreign Function Interface, in Garlic.

First, the C module being imported needs to have a few properties:

- The file must `#include` the `garlic.h` header. When `gcc` is called, this header will be part of the includes search path, so there's no need to worry about where it resides.

- All functions that are to be exported by the C module must accept and return values of type `garlic_value_t`. This type is defined in `garlic.h`, and there are functions declared in the header to manipulate and work with these values.

  A function may accept no arguments, and any function that only has a side-effect (and thus no meaningful return value) should return `nil`, represented by `NIL_VALUE`.

- All exported functions must be declared in special exports structure, where each exported function is given a name to be used in Garlic, a pointer to the C function and the number of arguments it accepts.

```c
// in garlic_c.c

#include <garlic.h>

garlic_value_t add(garlic_value_t a, garlic_value_t b) {
    // Note the use of functions/macros to work with Garlic values, coercing
    // them back and forth between Garlic values and C values.
    return int_to_garlicval(garlicval_to_int(a) + garlicval_to_int(b));
}

// The exports structure is a NULL-terminated array. Some caveats:
//
// - The name of the structure must be <module-name>_exports.
// - The structure is statically parsed by the Garlic compiler for static
//   analysis purposes, so it cannot be generated by macros, or even contain
//   inline comments. It's best to keep it simple.
garlic_native_export_t garlic_c_exports[] = {
    {"add", add, 2},
    0
};
```

Once such a C module exists, it can be imported and used like any other module.

```scheme
(require "garlic_c")

(garlic_c:add 1 2) ; -> 3
```

Like any other import, importing C modules supports renaming and the "import all" syntax.
