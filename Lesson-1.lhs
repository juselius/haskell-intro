# Lesson 1

## Functional programming

* Functional programming dates back to 1930:s, and the work on Lambda calculus by Alonzo Church
* Functional progamming is experiencing a second coming:
    * Reduced code complexity
    * Reduced number of bugs
    * Increased code flexibility
* Key concepts in functional programming are:
    * Pure functions
    * Immutable data
    * Functions are first class, function == data
    * Composition of functions
    * Composition of data

## Haskell

* Haskell:
    * Is a purely functional language
    * Is statically typed
    * Has a powerful type system, with type interference
    * Is lazy
    * Has confined IO
    * Is elegant can concise
    * Is fun!

### Ecosystem

* ghc: compiler
* ghci: REPL (Read-Eval-Print-Loop)
* runhaskell: scripting
* cabal: package manager and build system
* haddock: documentation
* hackage: package repository
* hoogle: search engine
* hlint: syntax checking

### Running Haskell

* ``ghci`` and ``IHaskell notebook``:
    * REPL: {expr} -> print result
    * ``:l file.hs`` -> load external Haskell source file
* ``runhaskell file.hs``: run Haskell source file like a script
    * (also: ``#!/usr/bin/env runhaskell``)
* ``ghc --make file.hs -o foo.bin``: compile Haskell source file to binary
## Basic Haskell syntax

> :option no-lint


### Simple expressions

> 1 + 1
> 1 - 2.0
> 2 / 3
> (10 + 5.5) / (2 * 3)


### Booleans

> True == False
> True /= False
> not (True && True) || True
> "foo" == "bar"


### Function calls
* Function calls are without parenthesis

> min 1 2
> max 1 2
> succ 'a'
> pred 1


#### Operators
* Binary functions can be used as infix operators
* Operators are automatically infix, but can be used as prefix functions too

> 1 `min` 2
> 42 `div` 21
> (+) 1 2


### Constant definition
* Constant identifiers **must** start with a lower case letter
* Definitions may contain ':s

> a  = 42
> a' = 42.0
> o'Toole = "Actor"


## Function definition
* Function definitions without parenthesis
* Function names **must** start with a lower case letter (except operators)
* Pure functions take parameters as input and return a result (no explicit return)

> add x y = x + y
> add a a'
> a + 2 `add` a
> (+/) x y = (x + y) / y
> a +/ a'



### Lambda functions
* Lambda functions are "anonymous" functions, and they are fundamental to functional programming
* Lambda functions can be bound to a name, thus creating a "normal" function

> addA = \x -> x + a


### Local definitions
* Using ``where`` and ``let`` we can make define constants and functions in the local scope
* Prefer ``where`` over ``let``
* Order is not important

> myFunc a =
>     a + b
>     where
>         b = a^2 + 1
>
> myFunc' a =
>     let b = b' + 1
>         b' = a^2
>     in a + b
>
> myFunc 2
> myFunc' 2


## Conditionals
### if
* ``if`` is **always** followed by ``else``

> abs' x =
>     if x > 0
>         then x
>         else abs x
>
> abs' 1
> abs' (-1)


### case
* ``case`` expressions are better than nested ``if - then - else``

> hello a = case a of
>     "hw" -> "Hello world"
>     "HW" -> "HELLO WORLD!"
>     "sl" -> "Sing song, so long."
>     _    -> "Que?"
>
> hello "hw"
> hello "hello"


### Guards

* Guards are a compact and preferred alternative to ``if`` statements
* Guards are like ``case`` statements, but with explicit truth value testing

> bmi weight height
>     | weight / height ^ 2 <= 18.5 = "You're underweight, you emo!"
>     | weight / height ^ 2 <= 25.0 = "You're supposedly normal."
>     | weight / height ^ 2 <= 30.0 = "You're a heavy weight champion!"
>     | otherwise                   = "You're a whale, congratulations!"
>
> bmi' weight height
>     | idx <= skinny = "You're underweight, you emo!"
>     | idx <= normal = "You're supposedly normal."
>     | idx <= heavy  = "You're a heavy weight champion!"
>     | otherwise     = "You're a whale, congratulations!"
>     where
>         idx = weight / height ^ 2
>         skinny = 18.5
>         normal = 25.0
>         heavy  = 30.0
>
> bmi' 93.0 1.85


## Indentation
* Haskell relies on indentation to define block structure
* The indentation rules are somewhat complicated, but pretty simple in practice
* Code which is part of some expression should be indented further in than the beginning of that expression
* Or: indentation marks the continuation of an expression
* We also indent:
    * Multiline functions
    * After ``do, let, where, of``
    * Stylistically after ``if, in``
## Comments

* Line comments: ``--``
* Block comments: ``{- ... -}``
* Haddock comments: ``-- | ``

