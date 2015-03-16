# Lesson 8
> :option no-lint
## Functors, Monads and Applicative functors

## Functors

A *functor* is (from a practical perspective) a function which *lifts*
functions from one type to another:

    fmap :: (a -> b) -> (f a -> f b)

In other words, it *maps* a function over some container type. Somewhat
loosely we say that a functor is a type which can be mapped over:

> fmap (*2) [1, 2, 3]
> fmap show (Just 42)
> fmap show Nothing

In Haskell functors are defined throught the *Functor* typeclass:

    class Functor f where
        fmap :: (a -> b) -> (f a -> f b)

## Monads

A very common *pattern* in programming is the following:

    f :: a -> T b

For example, a function wich might fail:

    f :: Int -> Just Int

The pattern is: *A function which takes a value and returns a value inside a
container type.* Very often we would like to use the power of function
composition on such functions, but normal function composition doesn't work:

   { (b -> M c) . (a -> M b) -->  M b /= b }

We must thus invent a new type of function composition, which pulls out ``b``,
feeds it into the next function collects the result:

    { (b -> M c) <=< (a -> M b) --> (a -> M c) }

The ``<=<`` operator is called Kleisli right-to-left composition. We also
not that the "pulling out" part is in fact nothing more than a functor in
disquise:

    { fmap (b -> M c) (M b) --> M M c }

We now immediately spot a problem, we wanted a ``M c`` back, but we got a ``M
M c``. The "collect reusults" part of monadic composition is commonly called
``join``, and it's implementation is the heart of every monad. For example,
for lists it's just the function ``concatenate``, for Maybe it involves logic
and flow control, etc.

An equivalent, but in practice the more common operator is the
left-to-right assymetric *bind* operator ``>>=``:

    (>>=) :: M a -> (a -> M b) -> M b

The *bind* operator allows us to feed in a monadic value to a monadic function
from the left, and chain the operations:

> f :: Int -> Maybe Int
> f x = if x >= 0 then Just x else Nothing
>
> g :: Int -> Maybe Int
> g x = Just $ x - 1
>
> return 1 >>= f >>= g >>= f
> return 1 >>= f >>= g >>= f >>= g >>= f
>

Note that monads allow us use the result of previous calculations by binding
names to the results using lambda functions:

> return 1 >>= \x -> f x >>= g >>= \_ -> f x

### Monad typeclass

In Haskell monads are defined through the *Monad* typeclass:

    class  Monad m  where
        (>>=)       :: m a -> (a -> m b) -> m b
        (>>)        :: m a -> m b -> m b
        return      :: a -> m a
        fail        :: String -> m a
        m >> k      = m >>= \_ -> k

### Monad Laws
Instantiating the Monad typeclass for a container type ``M a`` does not make
``M a`` a monadi, just an instance of ``Monad``. For ``M a`` to be a true,
mathematically well defined monad, it must satisfy the follwing laws:

> return a >>= k  ==  k a
> m >>= return  ==  m
> m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

These laws are quite strict, and severely restrict what *bind* can do to
``M a``. Bind cannot change the data in ``a``, not the container type. It can,
however, invoke functions, do logic and flow control, handle non-determinism,
etc.

## Applicative functors

Applicative functors have more power than functors, and less power than a
monad. Applicatives are almost monads, except that you cannot use the result
on previous computations in the later computations.

Applicative functors capture a common pattern, when using monads: You perform
a number of computations, binding the result to names, only to feed the
results into a final calculation:

> import Control.Applicative
> h :: Int -> Maybe Int
> h x = do
>   a <- f x
>   b <- g x
>   return $ a + b
>
> h' :: Int -> Maybe Int
> h' x = (+) <$> f x <*> g x
>
> h  2
> h' 2

Applicative functors are defined through the *Applicative* typeclass:

    class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
        (*>) :: f a -> f b -> f b
        (<*) :: f a -> f b -> f a
        (*>) = liftA2 (const id)
        (<*) = liftA2 const


For an Applicative to be a true applicative functor, the following laws must
hold:

* identity: ``pure id <*> v = v``
* composition: ``pure (.) <*> u <*> v <*> w = u <*> (v <*> w)``
* homomorphism: ``pure f <*> pure x = pure (f x)``
* interchange: ``u <*> pure y = pure ($ y) <*> u``
