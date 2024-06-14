# Untitled Term Rewriting Language (Haskell, 2024)

## Building, Testing, and Running

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

An ellipsis (`..`) follows a symbol. Ellipses may only follow variables or lists which contain one or more variables. 

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

Two definitions can match the same term. A given input term may be matched by at most one definition.

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