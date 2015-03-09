 # Lesson 3

 ## Pattern matching

* In pattern matching we attempt to match values against patterns
* If desired, we can bind variables to successful matches
* Pattern matching is a very important and common feature in Haskell
* We will see a lot of uses of pattern matching during the course:
    * Values
    * Lists
    * Tuples
    * Records
* Pattern matching can be used wherever you bind variables:
    * Function arguments
    * ``let`` statements
    * ``where`` statements
    * List comprehensions
    * ``do`` blocks

> :option no-lint


 ### Matching function argument values

> f x = case x of
>     0 -> False
>     1 -> True
>     _ -> False
> 
> 
> f' 0 = False
> f' 1 = True
> f' _ = False


 ### Matching tuples and binding variables

> g (_, 1) = 42
> g (a, b) = a + b
> g x = fst x + snd x
> 
> g (111, 1)
> g (1,2)
> 
> g' x = let (_,_,c,_)  = x 
>     in c
>     
> g' (11,22,33,44)


 ### Matching lists

> g :: [Int] -> Int
> g [] = 0
> g [1] = 42
> g [1,2] = 43
> g (x:y:z:[]) = x + y + z
> g (x:y:xs) = sum xs
> g x = sum x
> 
> g []
> g [1]
> g [1,2]
> g [1,2,3]
> g [1,2,3,4]


