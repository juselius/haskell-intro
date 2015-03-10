 # Lesson 2

 ## Lists
* Lists are immensly imortant in Haskell
* Lists have dynamic length
* Lists are homogenous
* The default lists are simple, but not very efficient

 ### List construction
* The list constructor is ``(:)``
* The empty list is ``[]``

> :option no-lint



> numbers = [1,2,75,6,7]
> numbers' = 75:6:7:[]
> letters = "hello"
> ranges  = [1..10]
> ranges' = [1.0,1.5..10.0]
>
> print (numbers ++ [1])
> print $ numbers ++ numbers'
> putStrLn $ letters ++ " world"
> putStrLn $ show [numbers, numbers']
> print $ letters !! 1
> print ranges
> print ranges'


 ### List comparison

> [3,2,1] > [2,1,0]
> [3,2,1] > [2,10,100]
> [3,2,1] > [3,10,100]
> [3,2,1] > [2,10]
> [3,2,1] == [2,10,100,1000]


 ### List functions

> l = [1..10]
> head l
> tail l
> last l
> init l
> length l
> null l
> reverse l
> take 5 l
> drop 5 l
> elem 5 l
> 5 `elem` l



> take 10 [1..]


 ### List comprehensions

> l' = [x^2 | x <- [1..10]]
> l'' = [x^2 | x <- [1..10], x `mod` 2 == 0]
> l''' = [if x < 5 then x^2 else x| x <- [1..10], odd x]
> print l'
> print l''
> print l'''



> ll  = [x*y | x <- [1..10], y <- [1..10]]
> ll' = [x*y | x <- [1..10], y <- [1..10], x /= 3, x /= 7, y /= 2]
> print ll
> print ll'



> length' xs = sum [1 | _ <- xs]
> length' l


 ## Tuples

* Tuples have static length
* Tuples can be heterogenous
* The tuple constructor is ``(,)`` (and pals)

> tp = (1,2)
> tp' = (1,'a',"hello")
> fst tp
> snd tp



> zip [1,2,3] [11.1, 22.2]



> phyttipanna a = [(x,y) | x <- [1.0,2.0..(2*a)], y <- [1.0,2.0..(2*a)], sqrt (x^2 + y^2) <= a]
> phyttipanna 5.0


* Constructing tuples the "funny" way

> (,) 1 2
> (,,) 'a' "cat" "sat"
> (,,,,) 1 2 75 6 7


