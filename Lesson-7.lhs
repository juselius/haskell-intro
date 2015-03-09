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

### Reading and writing files
