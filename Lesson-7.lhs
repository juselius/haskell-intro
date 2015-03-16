 # Lesson 7
 ## IO

* All IO in Haskell happens in the *IO monad*
* Functions running in the IO monad are identified by returning the
  parametric type *IO a*
* The IO monad ensures that IO happens in *in sequence* in a single thread
* On can only call functions returing *IO a* in the IO monad
    * We can *lift* any function to the IO monad using the *return* function
    * Calling IO functions from pure functions is allowed, but has no
      *effect*
    * As a consequence, all IO must happen close to the "surface"
* The program entry point is ``main :: IO ()``
    * The ``()`` is the empty type: When the program is done, it is done, and
      and all IO with real data and values has been performed
    * Returning ``IO ()`` has nothing to do with return codes, is simply
      means we have depleted all IO we want/can perform and we are done.

> main :: IO ()
> main = print "Hello world!"

 ### ``do`` notation
* Haskell has a special syntax for doing IO (actually for monadic computations)
* ``do`` allows us to write imperative code while reatining functional safety
* Variable assignment is done using the special ``<-`` operator,
  which extracts the value out of the ``IO a`` type container.

 ### Reading from standard in
``getContents`` reads input lazily from stdin:

> main = do
>   s <- getContents
>   putStrLn s
>
> :t getContents
> :t putStrLn
> :t print

 ### Reading and writing files
``readFile`` and ``writeFile`` read and write files:

> main = do
>   putStrLn "Let's do some IO!"
>   revfile "Lesson-7.lhs"
>   putStrLn "done."
>
> revfile :: String -> IO ()
> revfile f = do
>   s <- readFile f
>   writeFile "foo.lhs" $ reverse s
>
> :t readFile
> :t writeFile

## Concurrency

Writing concurrent programs i never trival, but Haskell makes it quite easy.
This is a big topic, and we'll only scratch the surface. The basic ingredients
we'll use are:

* ``forkIO :: IO () -> IO ThreadId``: start a new thread in the IO monad
* STM (Software Transactional Memory):
    * ``TVar``: Transactional variable, used to contain *mutable* data
    * ``TMVar``: Transactional MVar, synchronisation variable

> import Control.Monad
> import Control.Concurrent
> import Control.Concurrent.STM
> import System.Exit
>
> main :: IO ()
> main = do
>     lock <- atomically newEmptyTMVar
>     shared <- atomically $ newTVar 0
>     void . forkIO $ countup lock shared
>     void . forkIO $ countdn lock shared
>     forever $ do
>         void . atomically $ takeTMVar lock
>         val <- atomically $ readTVar shared
>         when (val >= 5) exitSuccess
>         print val
>
> countup :: TMVar () -> TVar Int -> IO ()
> countup lock shared = forever $ do
>     void . atomically $ count lock shared succ
>     sleep 25
>
> countdn :: TMVar () -> TVar Int -> IO ()
> countdn lock shared = forever $ do
>     void . atomically $ count lock shared pred
>     sleep 35
>
> count :: TMVar () -> TVar Int -> (Int -> Int) -> STM Int
> count lock shared f = do
>     val <- readTVar shared
>     let v = f val
>     writeTVar shared v
>     putTMVar lock ()
>     return v
>
> sleep :: Int -> IO ()
> sleep = threadDelay . (*) 10000
>
> main


