# Lesson 6
## Looping: maps, filters, folds and recursion
* Since Haskell is a pure functional language, we don't have variables to use as loop counters.
* Instead we can use recursion to create loops.
* Fortunately, we seldom have to recurse explicitly:
* ``map``, ``filter`` and ``fold`` are easier than recursion and clearer than loops
### map
Apply a function to *all* elements of a list, and return a new list:

> map (*2) [1..10]
> map (\x -> if odd x then 2 * x else 2 * (x + 1))


### filter
Apply a predicate to a list and return a list with the elements for wich the predicate holds *true*:

> filter odd [1..10]



> filter (\x -> x `mod` 3 == 1) [1..10]


### fold
Apply a function to the elements of a list and accumulate.

* folds are more general than filter and map, and hence more complicated
* folds come in two variants: left folds ``foldl`` and right folds ``foldr``
    * ``foldl``: ((((1 `f` 2) `f` 3) `f` 4) `f` 5)
    * ``foldr``: 1 `f` (2 `f` (3 `f` (4 `f` 5))))
    * foldl is more common than foldr
    * Use foldr to build lists
    * foldr is primitive recursive (foldl can be written in terms of foldr, but it hurts your head)
    * foldl suffers easily from *space leaks* and the strict ``foldl'`` is mostly used in practice
    * (understanding the space-time behavior of folds is a bit complicated, and depends on wheather ``f`` is strict or lazy)

> thesum = foldl (+) 0
> thesum [1..10]
> 
> oddsum = foldl (\acc x -> if odd x then acc + x else x) 0
> oddsum [1..10]
> 
> sumlist = foldr (\x acc -> (x + sum acc):acc) []
> sumlist [1..10]


## Recursion
* General loops can be emulated by recursion and *tail call* optimized recursion
* Tail recursion is equivalent of normal looping, i.e. it is efficient both performance wise and memory wise (it doesn't grow on the stack)
#### Factorial (tail call optimized)

> fac 1 = 1
> fac k = k * fac (k-1)
> 
> fac' 0 x = x 
> fac' n x = fac' (n - 1) (n * x)
> 
> fac 100
> fac' 100 0


#### Lenght of a list (tail call optimized)

> len x = len' x 0
> len' [] n = n
> len' (x:xs) n = len' xs (n + 1)
> 
> len [1..10]


