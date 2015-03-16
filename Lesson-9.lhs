# Lesson 9

## Modules, imports and cabal

### Modules

Haskell code is organized into modules. Directory structure follows module
naming, i.e. ``Foo.Bar -> Foo/Bar.hs``.

> module Foo.Bar (
>     foo
>   , bar
>   , Foo(..)
> ) where
>
> data Foo = Foo Int Int | Bar Int
>
> foo :: Int -> Foo
> foo x = Foo x x
>
> bar :: Int -> Foo
> bar x = Bar x
>

### Importing modules

> import Foo.Bar
> import qualified Foo.Bar
> import qualified Foo.Bar as B
> import Foo.Bar (foo)
> import Foo.Bar  hiding (bar)

### Cabal

    $ cabal init
    $ vim foo.cabal
    $ cabal configure
    $ cabal build
    $ ./dist/build/foo/foo
    $ cabal install --prefix=/some/where
