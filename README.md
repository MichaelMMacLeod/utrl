# Untitled Term Rewriting Language (Haskell, 2024)

This repository contains a compiler/interpreter for `utrl`, a simple untyped [purely-functional](https://en.wikipedia.org/wiki/Purely_functional_programming) [term-rewriting](https://en.wikipedia.org/wiki/Rewriting) programming language.

## Table of Contents

- [Motivation](#motivation)
- [Examples](#examples)
  - [Natural Numbers](#natural-numbers)
  - [Mergesort](#mergesort)
  - [Brainfuck interpreter](#brainfuck-interpreter)
- [Installation Instructions](#installation-instructions)
- [Error Code Index](#error-code-index)

## Motivation

Several languages nowadays such as [Racket](https://racket-lang.org/) and [Rust](https://www.rust-lang.org/) support ['by-example'](https://dl.acm.org/doi/abs/10.1145/41625.41632)-style macros.`utrl` is an experiment in designing a language with this feature as its *only* feature; in `utrl`, the sole construct is `def`, a [syntax-case](https://docs.racket-lang.org/guide/syntax-case.html)-style term-rewriting rule definition. Unsurprisingly, the `utrl` interpreter resembles a conventional *macro expander*, but with the following caveats:

1. `utrl` definitions are more flexible in that they need not start with a symbol:
    ```scheme
    (def ($x I_return_x) $x)

    // Evaluation of '(Hello_world! I_return_x)':
    //
    // 0. (Hello_world! I_return_x)
    // 1. Hello_world!
    // Hello_world!
     ``` 
2. If no definition immediately matches, inner terms will be expanded first. This allows `utrl` definitions to automatically evaluate their subterms when needed:

    ```scheme
    (def (if true  then $t else $e) $t)
    (def (if false then $t else $e) $e)
    (def (A == A) true)

    // Evaluation of '(if (A == A) then ok else fail)':
    //
    // 0. (if (A == A) then ok else fail)
    // 1. (if true then ok else fail)
    // 2. ok
    // ok
    ```
Similar to macro definition syntax in other languages, `utrl` supports ellipses denoted via two dots (`..`):

- Ellipses can extract multiple parts of a term:
  ```scheme
  (def (unzip (list (pair $x $y) ..))
    (pair (list $x ..)
          (list $y ..)))

  // Evaluation of '(unzip (list (pair 1 a) (pair 2 b) (pair 3 c)))':
  //
  // 0. (unzip (list (pair 1 a) (pair 2 b) (pair 3 c)))
  // 1. (pair (list 1 2 3) (list a b c))
  // (pair (list 1 2 3) (list a b c))
  ```
- Ellipses can flatten terms:
  ```scheme
  (def (flatten (list (list $xs ..) ..))
    (list $xs .. ..))

  // Evaluation of '(flatten (list (list 1 2 3) (list) (list a b c)))':
  //
  // 0. (flatten (list (list 1 2 3) (list) (list a b c)))
  // 1. (list 1 2 3 a b c)
  // (list 1 2 3 a b c)
  ```

I consider `utrl` more of an art project than a useful tool. It's asthetically pleasing, but a royal pain to use. `utrl` lacks almost every built-in feature one would expect out of a programming language including types, numbers, booleans, structs, and so on. That being said, it *is* possible with enough effort to write some [interesting programs](#examples). `utrl` is Turing-complete, and while I do not have a rigorous proof of this, I have written a [brainfuck](https://en.wikipedia.org/wiki/Brainfuck) interpreter in it, which can be found [here](./test/programs/brainfuck.defs).

Possibly the only advantage `utrl` has over other languages is that it produces [helpful error messages](#error-code-index) at the (arguably) *correct* time. Errors with `utrl` definitions are detected when definitions are compiled as opposed to when they are used. This is different from Rust and Racket which both (as of 2024) detect some errors only when a macro is *used*, and not when it is *defined*:

- Rust
  ```rust
  macro_rules! bad_list {
      ( ($( $x:expr ),*) ($( $y:expr ),*) ) => {
          {
              let mut temp_vec = Vec::<(i32, &'static str)>::new();
              $(
                  temp_vec.push(($x, $y));
              )*
              temp_vec
          }
      };
  }

  pub fn main() {
      bad_list!((1, 2, 3) ("a", "b", "c", "d", "e"));
  }
  ```
  Rust emits the following error only when `bad_list` is used with two lists of differing lengths. If the source contains only a definition of `bad_list`, no error will be signalled.
  ```
  error: meta-variable `x` repeats 3 times, but `y` repeats 5 times
   --> <source>:5:14
    |
  5 |               $(
    |  ______________^
  6 | |                 temp_vec.push(($x, $y));
  7 | |             )*
    | |_____________^
  ```
- Racket
  ```scheme
  #lang racket

  (define-syntax (bad-list stx)
    (syntax-case stx ()
      [(_ (x ...) (y ...))
       #'(list '(x . y) ...)]))

  (bad-list (1 2 3) ("a" "b" "c" "d" "e"))
  ```
  Racket emits the following error only when `bad-list` is used with two lists of differing lengths. If the source contains only a definition of `bad-list`, no error will be signalled.
  ```
  test.rkt:5:13: syntax: incompatible ellipsis match counts for template
    at: ...
    in: ((quote (x . y)) ...)
    location...:
    test.rkt:5:13
  ```
- `utrl`
  ```scheme
  (def (bad-list ($x ..) ($y ..))
    (list ($x $y) ..))
  ```
  `utrl` emits the following error when `bad-list` is defined. The program does not compile successfully, so it cannot be run.
  ```
  error[E003]: variables matched under different ellipses used with same ellipsis
  ./bad-list.defs:1:20
     |
   1 | (def (bad-list ($x ..) ($y ..))
     |                    ^^ $x matched under this ellipsis
  ./bad-list.defs:1:28
     |
   1 | (def (bad-list ($x ..) ($y ..))
     |                            ^^ $y matched under this ellipsis
  ./bad-list.defs:2:17
     |
   2 |   (list ($x $y) ..))
     |                 ^^ both used with this ellipsis
  help: variables matched under different ellipses can't be used with the same ellipsis
  ```
## Examples

### Natural Numbers

We can use `utrl` to define simple operations on [Peano natural numbers](https://en.wikipedia.org/wiki/Peano_axioms):

```scheme
// File 'add.defs'

/* Addition */
(def ($n + 0)      $n)
(def ($n + (S $m)) (S ($n + $m)))

/* Equality */
(def (0      == 0)      true)
(def ((S $n) == 0)      false)
(def (0      == (S $m)) false)
(def ((S $n) == (S $m)) ($n == $m))

/* Peano natural numbers 1-5 */
(def 1 (S 0))
(def 2 (S 1))
(def 3 (S 2))
(def 4 (S 3))
(def 5 (S 4))
```
```scheme
// File 'add.input'

(5 == (2 + 3)) /* this should evaluate to 'true' */
```
```scheme
bash$ utrl --defs ./add.defs --input ./add.input
true
bash$ # Good, that is what we wanted. '--trace' will show the steps:
bash$ utrl --defs ./add.defs --input ./add.input --trace
0. (5 == (2 + 3))
1. ((S 4) == (2 + 3))
2. ((S (S 3)) == (2 + 3))
3. ((S (S 3)) == ((S 1) + 3))
4. ((S (S 3)) == ((S 1) + (S 2)))
5. ((S (S 3)) == (S ((S 1) + 2)))
6. ((S 3) == ((S 1) + 2))
7. ((S (S 2)) == ((S 1) + 2))
8. ((S (S 2)) == ((S 1) + (S 1)))
9. ((S (S 2)) == (S ((S 1) + 1)))
10. ((S 2) == ((S 1) + 1))
11. ((S (S 1)) == ((S 1) + 1))
12. ((S (S 1)) == ((S 1) + (S 0)))
13. ((S (S 1)) == (S ((S 1) + 0)))
14. ((S 1) == ((S 1) + 0))
15. ((S 1) == (S 1))
16. (1 == 1)
17. ((S 0) == 1)
18. ((S 0) == (S 0))
19. (0 == 0)
20. true
true
```

### Mergesort

With a bit of effort, it's possible to write a [mergesort](https://en.wikipedia.org/wiki/Merge_sort) algorithm using `utrl`.

```scheme
// File 'mergesort.defs'

// Sorts a list of natural numbers via merge sort
//
// sort : List Nat -> List Nat
(def (sort $xs)
  (sort-rec (length $xs) nil $xs))

// Helper function for 'sort'. Partitions its third
// argument (a list) into two pieces, accumulating the
// first piece in the second argument. Returns the
// result of 'merge'ing the final two pieces together.
// The integer argument counts twice the number of
// list elements to split off.
//
// sort-rec : Nat -> List Nat -> List Nat -> List Nat
(def (sort-rec 0 nil nil)
  nil)
(def (sort-rec 0 ($a :: nil) nil)
  ($a :: nil))
(def (sort-rec 0 nil ($x :: nil))
  ($x :: nil))
(def (sort-rec 0 ($a :: nil) ($x :: nil))
  (merge ($a :: nil)
         ($x :: nil)))
(def (sort-rec 0 ($a :: nil) ($x :: ($y :: nil)))
  (merge ($a :: nil)
         (sort ($x :: ($y :: nil)))))
(def (sort-rec 0 ($a :: ($b :: $as)) ($x :: ($y :: $ys)))
  (merge (sort ($a :: ($b :: $as)))
         (sort ($x :: ($y :: $ys)))))
(def (sort-rec (S 0) $as $xs)
  (sort-rec 0 $as $xs))
(def (sort-rec (S (S $n)) $as ($x :: $xs))
  (sort-rec $n ($x :: $as) $xs))

// merge : List Nat -> List Nat -> List Nat
//
// Combines two lists, both in ascending order, into
// a single list in ascending order.
(def (merge nil ($y :: $ys))
  ($y :: $ys))
(def (merge ($x :: $xs) nil)
  ($x :: $xs))
(def (merge ($x :: $xs) ($y :: $ys))
  (if ($x < $y)
    then ($x :: (merge $xs ($y :: $ys)))
    else ($y :: (merge ($x :: $xs) $ys))))

// Returns the length of a list
//
// length : List a -> Nat
(def (length nil)
  0)
(def (length ($x :: $xs))
  (S (length $xs)))

// 'If' statement syntax
(def (if true then $then else $else) $then)
(def (if false then $then else $else) $else)

// A couple translations of arabic numerals into Peano numbers.
// See: https://en.wikipedia.org/wiki/Peano_axioms
(def 1 (S 0))
(def 2 (S 1))
(def 3 (S 2))
(def 4 (S 3))
(def 5 (S 4))
(def 6 (S 5))
(def 7 (S 6))
(def 8 (S 7))
(def 9 (S 8))

// Equality of natural numbers
//
// (==) : Nat -> Nat -> Bool
(def (0 == 0)
  true)
(def ((S $n) == 0)
  false)
(def (0 == (S $m))
  false)
(def ((S $n) == (S $m))
  ($n == $m))

// Equality of lists
//
// (==) : List a -> List a -> Bool
(def (nil == nil)
  true)
(def (($x :: $xs) == nil)
  false)
(def (nil == ($y :: $ys))
  false)
(def (($x :: $xs) == ($y :: $ys))
  (($x == $y) && ($xs == $ys)))

// Logical operator 'AND'
//
// (&&) : Bool -> Bool -> Bool
(def (true && $x) $x)

// 'less-than' operator on natural numbers
//
// (<) : Nat -> Nat -> Bool
(def (0 < 0)
  false)
(def ((S $n) < 0)
  false)
(def (0 < (S $m))
  true)
(def ((S $n) < (S $m))
  ($n < $m))

// List construction syntax: creates cons cells
// from a flat list.
(def (list)
  nil)
(def (list $x $xs ..)
  ($x :: (list $xs ..)))
```
```scheme
// File 'mergesort.input'

((list 1 2 3) == (sort (list 3 2 1)))
```
```scheme
bash$ time utrl --defs ./mergesort.defs --input ./mergesort.input
true

real    0m0.014s
user    0m0.008s
sys     0m0.004s
bash$ # Using '--trace' here produces a lot of output!
bash$ utrl --defs ./mergesort.defs --input ./mergesort.input --trace
0. ((list 1 2 3) == (sort (list 3 2 1)))
1. ((1 :: (list 2 3)) == (sort (list 3 2 1)))
2. ((1 :: (list 2 3)) == (sort-rec (length (list 3 2 1)) nil (list 3 2 1)))
3. (((S 0) :: (list 2 3)) == (sort-rec (length (list 3 2 1)) nil (list 3 2 1)))
4. (((S 0) :: (2 :: (list 3))) == (sort-rec (length (list 3 2 1)) nil (list 3 2 1)))
5. (((S 0) :: (2 :: (list 3))) == (sort-rec (length (list 3 2 1)) nil (3 :: (list 2 1))))

Intermediate steps ellided for brevity. '--trace' will actually
print every single step. Try it for yourself and see all the text
fly by!

60. (((S 2) :: nil) == ((S (S 1)) :: nil))
61. (((S 2) == (S (S 1))) && (nil == nil))
62. ((2 == (S 1)) && (nil == nil))
63. ((2 == (S 1)) && true)
64. (((S 1) == (S 1)) && true)
65. ((1 == 1) && true)
66. (((S 0) == 1) && true)
67. (((S 0) == (S 0)) && true)
68. ((0 == 0) && true)
69. (true && true)
70. true
true
```

### Brainfuck interpreter

To help demonstrate [Turing completeness](https://en.wikipedia.org/wiki/Turing_completeness), I wrote a [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) interpreter in `utrl`. The definitions file can be found in the [test directory](./test/programs/brainfuck.defs). It can be used to sort a list [via quicksort written in Brainfuck](./test/programs/brainfuck.input).

## Installation Instructions

This project uses [Nix](https://nixos.org/download/) to manage its build process and dependencies. Nix version 2.8 (released on 2022-04-19) or greater is required. Use `nix --version` to check if you're up to date.

To build and test the interpreter use:
```
nix --extra-experimental-features 'nix-command flakes' build 
```
This installs the interpreter to `./result/bin/utrl`. To run it, use:
```
./result/bin/utrl --help
```
A development environment containing [VSCodium](https://vscodium.com/) with Haskell/Nix extensions pre-installed, along with the Haskell build tool [cabal](https://cabal.readthedocs.io/en/stable/), and Haskell compiler [ghc](https://www.haskell.org/ghc/) can be entered using:
```
nix --extra-experimental-features 'nix-command flakes' develop
```
These are some of the available commands inside the development environment:
```
codium                   # open VSCodium IDE
cabal build              # build interpreter
cabal test               # run tests
cabal run utrl -- --help # run interpreter
```
It's a good idea when developing to use `cabal build` instead of `nix build`; the former will selectively build only the parts of the code that have changed, while the latter will always perform a full-rebuild.

## Error Code Index

### `E001`

There are unbalanced parentheses or unbalanced block-comments (`/*` and `*/`).

#### Example (missing right parenthesis)

```
(def A B
```
```
error[E001]: bad syntax
./test/programs/error-missing-right-paren.defs:1:9
   |
 1 | (def A B
   |         ^ unexpected end of input
   |           expecting '(' or ')'

```

### `E002`

There is a definition with a variable used in the constructor with a different number of ellipses (`..`) than it was matched with in the pattern.

#### Example (too few ellipses)

```
(def ($x ..) $x)
```
```
error[E002]: too few ellipses
./test/programs/error-ellipses-1-0.defs:1:7
   |
 1 | (def ($x ..) $x)
   |       ^^ matched with 1 ellipsis in pattern
./test/programs/error-ellipses-1-0.defs:1:14
   |
 1 | (def ($x ..) $x)
   |              ^^ used with 0 ellipses in constructor
help: variables must be used with the same number of ellipses they were matched with
```

#### Example (too many ellipses)

```
(def (flatten (list (list $x ..) ..))
  (list $x .. .. ..))
```
```
error[E002]: too many ellipses
./test/programs/error-ellipses-2-3.defs:1:27
   |
 1 | (def (flatten (list (list $x ..) ..))
   |                           ^^ matched with 2 ellipses in pattern
./test/programs/error-ellipses-2-3.defs:2:9
   |
 2 |   (list $x .. .. ..))
   |         ^^ used with 3 ellipses in constructor
help: variables must be used with the same number of ellipses they were matched with
```

### `E003`

There are two variables that are used with the same ellipsis, but not matched under the same ellipsis.

#### Example

```
(def (capture ($x ..) ($y ..))
  (result ($x $y) ..))
```
```
error[E003]: variables matched under different ellipses used with same ellipsis
./test/programs/error-captures-1-level.defs:1:19
   |
 1 | (def (capture ($x ..) ($y ..))
   |                   ^^ $x matched under this ellipsis
./test/programs/error-captures-1-level.defs:1:27
   |
 1 | (def (capture ($x ..) ($y ..))
   |                           ^^ $y matched under this ellipsis
./test/programs/error-captures-1-level.defs:2:19
   |
 2 |   (result ($x $y) ..))
   |                   ^^ both used with this ellipsis
help: variables matched under different ellipses can't be used with the same ellipsis
```

#### Example (multiple ellipses)

```
(def (capture (($x ..) ..) (($y ..) ..))
  (result ($x .. $y ..) ..))
```
```
error[E003]: variables matched under different ellipses used with same ellipsis
./test/programs/error-captures-2-level-inside.defs:1:24
   |
 1 | (def (capture (($x ..) ..) (($y ..) ..))
   |                        ^^ $x matched under this ellipsis
./test/programs/error-captures-2-level-inside.defs:1:37
   |
 1 | (def (capture (($x ..) ..) (($y ..) ..))
   |                                     ^^ $y matched under this ellipsis
./test/programs/error-captures-2-level-inside.defs:2:25
   |
 2 |   (result ($x .. $y ..) ..))
   |                         ^^ both used with this ellipsis
help: variables matched under different ellipses can't be used with the same ellipsis
```

### `E004`

An ellipsis follows a symbol. Ellipses may only follow variables or lists which contain one or more variables. 

#### Example

```
(def (copy (list x ..))
  (list x ..))
```
```
error[E004]: ellipsis following symbol
./test/programs/error-ellipsis-on-symbol.defs:1:18
   |
 1 | (def (copy (list x ..))
   |                  ^ this symbol doesn't begin with a dollar-sign ('$'),
   |                    so it is not considered a variable
help: perhaps you meant '$x'?

error[E004]: ellipsis following symbol
./test/programs/error-ellipsis-on-symbol.defs:2:9
   |
 2 |   (list x ..))
   |         ^ this symbol doesn't begin with a dollar-sign ('$'),
   |           so it is not considered a variable
help: perhaps you meant '$x'?
```

### `E005`

There is more than one ellipsis in a list of some definition's pattern.

#### Example

```
(def ($x .. $y .. $z ..) constructor)
```
```
error[E005]: too many ellipses in single term of pattern
./test/programs/error-pattern-3-ellipses.defs:1:6
   |
 1 | (def ($x .. $y .. $z ..) constructor)
   |      ^^^^^^^^^^^^^^^^^^^ this term has more than one ellipsis
./test/programs/error-pattern-3-ellipses.defs:1:16
   |
 1 | (def ($x .. $y .. $z ..) constructor)
   |                ^^ ellipsis #2
./test/programs/error-pattern-3-ellipses.defs:1:22
   |
 1 | (def ($x .. $y .. $z ..) constructor)
   |                      ^^ ellipsis #3
help: each term in a definitions's pattern may have at most one ellipsis
```

It is okay to have multiple ellipses in a definition's pattern, but there cannot ever be more than one in each list in the pattern. The following is perfectly acceptable, as no single list contains more than one ellipsis:

```
(def (($x ..) ($y ..) $z ..) constructor)
//   (___________________..)     See how each
//    (___..)                 list contains only
//            (___..)         a single ellipsis?
```

This rule does not apply to constructors, which may have as many ellipses per list as desired. For example, here is a valid definition that uses three ellipses in a single list in its constructor:

```
(def
  // All the ellipses here must be in different lists,
  // as this is the definition's pattern.
  (flatten3 (list (list (list $x ..) ..) ..))

  // All the ellipses here can be in the same list,
  // as this is the definition's constructor.
  (list $x .. .. ..))
```

### `E006`

The same variable occurs more than once in a definition's pattern.

#### Example

```
(def (twice $x $x) constructor)
```
```
error[E006]: variable occurs more than once in pattern
./test/programs/error-var-pattern-use-2.defs:1:13
   |
 1 | (def (twice $x $x) constructor)
   |             ^^ use #1
./test/programs/error-var-pattern-use-2.defs:1:16
   |
 1 | (def (twice $x $x) constructor)
   |                ^^ use #2
help: a variable may occur at most once in a definition's pattern
```

### `E007`

Two definitions exist which match the same term(s). A given input term may be matched by at most one definition.

#### Example (exactly the same pattern)

```
(def A Y)
(def A Z)
```
```
error[E007]: overlapping patterns
./test/programs/error-overlapping-patterns-simple.defs:1:6
   |
 1 | (def A Y)
   |      ^ this pattern may match the same term as ...
./test/programs/error-overlapping-patterns-simple.defs:2:6
   |
 2 | (def A Z)
   |      ^ ... this other pattern
help: patterns possibly matching the same term are not allowed
```

#### Example (overlapping via variables)

```
(def ($n 0) _)
(def (0 $m) _)
```
```
error[E007]: overlapping patterns
./test/programs/error-overlapping-patterns-variables.defs:1:6
   |
 1 | (def ($n 0) _)
   |      ^^^^^^ this pattern may match the same term as ...
./test/programs/error-overlapping-patterns-variables.defs:2:6
   |
 2 | (def (0 $m) _)
   |      ^^^^^^ ... this other pattern
help: patterns possibly matching the same term are not allowed
```

#### Example (overlapping via variables and ellipses)

```
(def (a b $x ..) _)
(def (a b $x .. c) _)
```
```
error[E007]: overlapping patterns
./test/programs/error-overlapping-patterns-multiple.defs:1:6
   |
 1 | (def (a b $x ..) _)
   |      ^^^^^^^^^^^ this pattern may match the same term as ...
./test/programs/error-overlapping-patterns-multiple.defs:2:6
   |
 2 | (def (a b $x .. c) _)
   |      ^^^^^^^^^^^^^ ... this other pattern
help: patterns possibly matching the same term are not allowed
```

### `E008`

A term in either the pattern or constructor is followed by ellipses but contains no variables.

#### Example

```
(def pattern
  ((no vars here) ..))
```
```
error[E008]: no variables in term preceding ellipsis
./test/programs/error-no-vars-in-ellipsis-constructor.defs:2:19
   |
 2 |   ((no vars here) ..))
   |                   ^^ the ellipsis
./test/programs/error-no-vars-in-ellipsis-constructor.defs:2:4
   |
 2 |   ((no vars here) ..))
   |    ^^^^^^^^^^^^^^ the term preceding the ellipsis
help: there must be at least one variable in the term preceding an ellipsis
```

### `E009`

A symbol is present at the top-level of the definitions.

#### Example

```
(def I_am_okay because_I_am_a_definition)
I_am_a_symbol_not_a_definition
```
```
error[E009]: expected definition, found symbol
./test/programs/error-def-is-symbol.defs:2:1
   |
 2 | I_am_a_symbol_not_a_definition
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ this should be a definition
help: definitions look like this: '(def <pattern> <constructor>)'
```

### `E010`

A definition was found with too many or too few terms. Each definition must have exactly three terms: `(def[1] <pattern>[2] <constructor>[3])`

#### Example (too many terms)

```
(def a b c)
```
```
error[E010]: definition has too many terms
./test/programs/error-def-4-terms.defs:1:1
   |
 1 | (def a b c)
   | ^^^^^^^^^^^ has 4 terms, but should have 3
help: definitions look like this: '(def <pattern> <constructor>)'
```

### Example (too few terms)

```
(def (blah (blah) blah))
```
```
error[E010]: definition has too few terms
./test/programs/error-def-2-terms.defs:1:1
   |
 1 | (def (blah (blah) blah))
   | ^^^^^^^^^^^^^^^^^^^^^^^^ has 2 terms, but should have 3
help: definitions look like this: '(def <pattern> <constructor>)'
```

## `E011`

A definition did not start with `def`. All definitions must start with the symbol `def`.

### Example

```
(DEF A B)
```
```
error[E011]: definition does not start with 'def'
./test/programs/error-def-missing-def.defs:1:2
   |
 1 | (DEF A B)
   |  ^^^ this term should be 'def'
help: definitions look like this: '(def <pattern> <constructor>)'
```

## `E012`

A variable was used in the constructor that was not matched in the pattern. This may be due to a missing dollar-sign (`$`) prefix somewhere in the pattern.

### Example

```
(def (copy x) $x)
```

```
error[E012]: variable not matched in pattern
./test/programs/error-var-not-matched.defs:1:15
   |
 1 | (def (copy x) $x)
   |               ^^ the variable
./test/programs/error-var-not-matched.defs:1:6
   |
 1 | (def (copy x) $x)
   |      ^^^^^^^^ the pattern
help: variables cannot be used without first being matched
```