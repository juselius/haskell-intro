 # Lesson 5
 ## Higher-order functions

> :option no-lint


 ### Currying
* Currying is also called *partial function application*
* Currying is the act of calling a function with
fewer arguments than the *arity* of the function
* The result of partially applying a function is:
    * A new function
    * Closure (storing free variables as parametric values inside a function)

> madd3 :: Int -> Int -> Int -> Int
> madd3 x y z = x * 3 + y * 2 + z
> madd3 11 4 1
>
> madd2 = madd3 11
> madd2 4 1
>
> madd1 = madd3 11 4
> madd1 1
>
> madd1' = madd2 4
> madd1' 1
>
> :t madd3
> :t madd2
> :t madd1
> :t madd1'



 ### Functions as function arguments
Functions can be passed as arguments to functions:

> twice :: (a -> a) -> a -> a
> twice f x = f (f x)
>
> twice (+1) 2
> twice (\x -> x^2-1) 4
>
> cube :: (a -> a -> a) -> a -> a
> cube f x = f x (f x x)
>
> cube (*) 2


 ### Returning functions
Functions can return functions:

> fret :: Int -> (Float -> Float)
> fret x = \y -> fromIntegral x * y
>
> g = fret 21
> g 2.0
> :t g


 ### Function transformations
Functions can take functions as arguments and return  new functions:

> i2f :: (Int -> Int) -> (Float -> Float)
> i2f h = \x -> fromIntegral (h (round x))
>
> f :: Int -> Int
> f x = 2 * x
>
> g x = (i2f f) x
> :t g


 ### ETA reduction
* Haskell functions can often be simplified in an algebraic manner:
    * ``(\x -> abs x) == abs``
    * ``(f x = abs x) == f = abs``

> f :: Int -> Int
> f = (*2)
>
> g = i2f f
> :t g



 ## Function composition
* Function composition is a powerfult technique for building complexity from simpler units
* Function composition is common in many languages:
```
    print(sin(exp(x)))
```
```
    $ cat data | sed 's/hello/world/' | sort | uniq | head
```
* Function compostion clean, since Haskell doesn't use parenthesis
* Function composition is expressed using the ``(.)`` operator
* A useful aide when doing composition is the *function application* operator ``($)``
    * An useful mnemonic for ``$`` is *parenthesis until end-of-statement*

> f    x = negate (abs x)
> f'   x = (negate . abs) x
> f''  x = negate . abs $ x
> f''' x = negate . abs


 #### Composition combinators
* Combinators are functions which combine functions (of some sort, in some way) to procuce new functions
* Combinators are a very powerful techique and are much used, in particular in embedded domain specific languages (EDSLs)

 #### Composition and currying
We can easily curry functions in function compostition:

> clever = "There are two ways of constructing a software design:\nOne way is to make it so simple that there are obviously no deficiencies\nThe other way is to make it so complicated that there are no obvious deficiencies. -- C.A.R. Hoare\n\nOne of my most productive days was throwing away 1000 lines of code. -- Ken Thompson\n\nIt's a curious thing about our industry:\nNot only do we not learn from our mistakes, we also don't learn from our successes. -- Keith Braithwaite\n\nAnd folks, let’s be honest. Sturgeon was an optimist. Way more than 90% of code is crap. -- Al Viro"
>
> firstParagraph = unlines . takeWhile (not . null) . dropWhile null . lines
> skipParagraph = unlines . dropWhile (not . null) . dropWhile null . lines
>
> p1 = firstParagraph clever
> p2 = firstParagraph . skipParagraph $ clever
> p3 = firstParagraph . skipParagraph . skipParagraph $ clever
>
> putStrLn p1
> putStrLn p2
> putStrLn p3



> (.) :: (b -> c) -> (a -> b) -> a -> c
> (.) f g = \x -> f (g x)
>
> infixr 0 $
> ($) :: (a -> b) -> a -> b
> f $ x = f x


A more advance example, including composition of a function returning a function:

> f = g . h . (*2) . succ
>     where
>         h x y = (x + 1) * (y + 2)
>         g x = x 42
>
> print $ f 41


 ### Using compostition to avoid repetition

> poem = unlines [
>       "There was a young lady named Bright"
>     , "Whose speed was far faster than light;"
>     , "She set out one day"
>     , "In a relative way"
>     , "And returned on the previous night."
>     ]
> putStrLn poem


 #### Composition

> import Data.List (sort)
>
> process  t = unlines (sort (lines t))
> process' t = (unlines . sort . lines) t
> process''  = unlines . sort . lines
>
> putStrLn . process'' $ poem


 #### Avoiding repetition

> sortLines     = unlines . sort . lines
> reverseLines  = unlines . reverse . lines
> firstTwoLines = unlines . take 2 . lines
>
> byLines f = unlines . f . lines
>
> sortLines'     = byLines sort
> reverseLines'  = byLines reverse
> firstTwoLines' = byLines (take 2)
>
> putStrLn $ sortLines' poem
> putStrLn $ reverseLines' poem
> putStrLn $ firstTwoLines' poem






