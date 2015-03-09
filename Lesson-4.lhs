# Lesson 4

* So far Haskell has looked more or less like a dynamically typed language:
    * We have relied heavily on Haskell's _type inference_ engine
* Haskell is statically typed
* Haskell has a very rich type system:
    * Type level programming
    * Generalized algebraic types
    * Dependent types
    * ...

## Types

* Every entity in Haskell has a type
* Types can be specified explicitly using the ``::`` type operator ("_has type of_")
* Explicit type annotations are sometimes necessary, when the type inference is ambiguous
* Often type annotations are more important as guides for the reader
* **NB!** It takes time and effort to get used to reading and understanding types
Using the ``:t`` function in ``ghci`` we can query the type:

> :option no-lint



> :t 'a'



> :t True



> :t "Hello"



> :t ('a', True)



> :t [True,False,True]


#### Inlined type specifiers

> a = 1 :: Integer
> b = (1 :: Float) + 2.0 :: Float


### Functions

> f :: Int -> Double
> f x = 2 * x



> f :: Int -> Double
> f x = 2 * fromIntegral x



> g :: Char -> Char -> String
> g a b = [a,b]


#### Generic functions
* Type variables
* Determined at compile time

> h :: [a] -> a
> h (x:xs) = x


## Abstract data types

* So far we have only used predefined types
* Haskell's type system is rich and powerful, and we will only scratch the surface
* Haskell allows us to define our own types using the ``data`` keyword
    * The ``data`` keyword defines *type constructors*
    * Every type can have zero or more *value (data) constructors* associated with it 
    * A *data (value) constructor* is a function which generates an *instance* of a *type*
    * Type and data constructors are *always* capitalized
#### Single-valued types

> data Singleton  = Singleton 
> data Simpleton  = Simpleton Int
> data FunnyShape = NamedPoint String Int Int
> 
> :t Singleton
> :t Simpleton
> :t NamedPoint


Use of ADTs in functions, note the use of *pattern matching*:

> f :: Int -> Simpleton
> f x = Simpleton (x + 1)
> 
> g :: Simpleton -> Int
> g (Simpleton x) = x
> 
> g (f 1)


#### Multi-valued types

> data Shape = Point Float Float | Rect Float Float Float Float | Circle Float Float Float
> 
> f :: Shape -> String
> f (Point a b) = "Point at " ++ show a ++ ", " ++ show b
> f (Rect _ _ _ _) = "Rectangle"
> f (Circle _ _ r ) = "Circle, r = " ++ show r
> 
> f (Point 0.0 0.0) 
> f (Circle 1.0 2.0 3.0)



### Type parameters
* Type parameters allow us to define types with "type holes" in them
* A *parametrized type* becomes a *concrete type* when the "hole" is replaced with another concrete type
* Parametrized data types are somewhat similar to C++ templates or Java/C# generics

> data Maybe a = Nothing | Just a deriving (Show)
> data Either a b = Left a | Right b deriving (Show)
> 
> f :: Float -> Maybe Float
> f x = if x < 0.0 then Nothing else Just (sqrt x)
> 
> f 2.0
> f (-1.0)
> 
> g :: Float -> Either String Float
> g x = if x < 0.0 then Left "Fail!" else Right (sqrt x)
> 
> g 2.0
> g (-1.0)


### Record syntax
* Acessing value attributes is a hassle, forcing us to write accessor functions using pattern matching
* Fortunately, Haskell can automate this daunting task using *record syntax*

> getPointX :: Shape -> Float
> getPointX (Point x _) = x
> 
> data Shape = Point {
>       posX :: Float
>     , posY :: Float  
>     } 
>     | Circle {
>       posX :: Float
>     , posY :: Float
>     , radius :: Float
>     } deriving (Show)
> 
> p = Point 1.0 2.0
> c = Circle 1.0 2.0 3.0
> posX p
> posY p
> 
> posX c
> posY c
> radius c


### Record updates
* Record syntax can also be used to "update" or "change" records (a new record is returned, and immutability is preserved!)

> c = Circle 1.0 2.0 3.0
> c' = c { radius = -1.0 }
> c'


### Type aliases

``type`` creates a type alias which can be used interchangably with the aliased type

> type String = [Char]
> 
> f :: String -> Int
> f = length
> 
> f' :: [Char] -> Int
> f' = length
> 
> f ['f', 'o', 'o']
> f' "foo"


``newtype`` creates a new *type constructor`` aliasing the original type, but they cannot be used interchangably

> newtype Stringy = Stringy [Char]
> 
> f :: Stringy -> Int
> f (Stringy x) = length x
> 
> f (Stringy "foo")
> f (Stringy ['f', 'o', 'o'])
> f ['f', 'o', 'o']


### Recursive types (adv.)
* Types can be recursive, i.e. contain themselves
* Recursive types are e.g. used to define lists, trees, etc.

> data Lst a = Nil | Lst a (Lst a) 
> l = Lst 1 (Lst 2 (Lst 3 Nil))


## Type classes
* Typeclasses are interfaces for some behavior
* Typeclasses are contracts for types
* If a type implements a typeclass, it promises to behave accoring to the typeclass specification
* Typeclasses allow us to overload functions and operators for types  

> :t 1



> :t 1.0



> :t (==)


#### Defining type classes

> class Hello a where
>     hello :: a -> String
>     world :: a -> String
>     world a = hello a ++ " World!"


#### Implementing type class instances

> data Foo = Foo Int
> data Bar = Bar String
> 
> instance Hello Foo where
>     hello (Foo n) = unwords (replicate n "Hello")
> 
> instance Hello Bar where
>     hello (Bar s) = "Hello " ++ s ++ ", " ++ s
> 
> f :: Hello a => a -> String
> f x = world x
> 
> f (Foo 5)
> f (Bar "silly")


### Derived type classes
* Haskell can automatically implement many common type classes for us, using the ``deriving`` clause
* Some important type classes are ``Eq, Ord, Num, Read, Functor, Monoid, Monad, Applicative``
## Types and generic functions (adv.)
Types can tell us a lot about what functions *can* do. The following function has only *one, single, possible* implementation. The function must hold *for all* types of a, and the only common value of all types is *bottom*.

> f :: a -> a
> f x = x


Or explicitly:

> {-# LANGUAGE ExplicitForAll #-}
> f' :: forall a. a -> a
> f' x = x


#### Parametric algebraic data types give us more possibilities
We can still not do anything about *a*:

> data Foo a = Foo Int a
> data Bar a = Bar Float a
> 
> f :: Foo a -> Bar a
> f (Foo x y) = Bar (fromIntegral x) y


#### Typeclasses 
Adding a type constraint allows us to use generic types in a controlled manner

> f :: (Eq a, Show a) => a -> a -> String
> f x y = if x == y then "ok" else show x ++ " /= " ++ show y
> 
> f "hello" "world"
> f 42 42
> f 1.0 2.0


### Polymorphic return types
* Using typeclasses we can define functions which are polymorphic in the return type
* This technique is used with great success in e.g. the Haskell Regexp library

> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> import Data.Char
> class Wow a where
>     g :: String -> a
> 
> instance Wow String where
>     g s = s ++ ", " ++ s
> 
> instance Wow Int where
>     g s = length s
>     
> instance Wow Bool where
>     g s = if length s > 0 then True else False
> 
> instance Wow Float where
>     g s = fromIntegral . foldl (\acc x -> acc + ord x) 0 $ s
> 
> g "hello" :: String
> g "hello" :: Int
> g "hello" :: Bool
> g "hello" :: Float


## Kinds (optional)
* What is the type of a type? It's a *kind*!
* Haskell has only two basic kinds:
    * Boxed: ``*``
    * Unboxed: ``#``
* We can inspect the kind of a type (value constructor) in ``ghci`` using the ``:k`` command
* Type constructors can be thought of as lambda functions in kind space 

> data Simple = Simple Int Float String
> data Param a b = Param a b
> 
> :k Simple
> :k Param


