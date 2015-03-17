import Control.Applicative -- definition of typeclass Applicative

data Count a = Count Int a deriving (Show, Eq)

instance Functor Count where
    fmap g (Count n x) = Count n (g x)

-- every monad is an applicative functor
instance Applicative Count where
    pure = Count 0
    (Count n g) <*> x = let (Count n' y) = fmap g x in Count (n + n') y

instance Monad Count where
    return = Count 0
    x >>= g = let (Count n (Count n' y)) = fmap g x in Count (n + n') y
    (>>) x g = x >>= \_ -> g

main :: IO ()
main = do
    verifyApplicativeLaws
    putStrLn "---"
    verifyMonadLaws
    putStrLn "---"
    printit "start value"              x
    printit "applicative start value"  (pure 10   :: Count Int)
    printit "monadic start value"      (return 10 :: Count Int)
    putStrLn "---"
    printit "functor application 1"    (fmap fun x)
    printit "functor application 2"    (fmap (*10) x)
    putStrLn "---"
    printit "applicative functor 0"    (pure $ fun 10 :: Count Int)
    printit "applicative functor 1"    (pure fun <*> x)
    printit "applicative functor 2"    (return fun <*> x)
    printit "applicative functor 3"    (pure fun' <*> x <*> x)
    putStrLn "---"
    printit "monad 0"                  (funM 10)
    printit "monad 1"                  (return 10 >>= funM >>= funM)
    printit "monad 2"                  (pure 10 >>= funM >> return 42)
    printit "monad 3"                  (x >>= funM >>= funM >>= funM)
    where
        x = Count 1 (42 :: Int)
        printit s v = putStrLn $ s ++ ": " ++ show v

verifyApplicativeLaws :: IO ()
verifyApplicativeLaws = do
    putStrLn "Applicative laws for Counter"
    putStrLn $ "identity    : " ++ show identity
    putStrLn $ "compsition  : " ++ show comp
    putStrLn $ "homomorphism: " ++ show homo
    putStrLn $ "interchange : " ++ show inter
    where
        identity = (pure id <*> w) == w
        comp = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
        homo = (pure f <*> pure 1 :: Count Int) == (pure (f 1))
        inter = (u <*> pure 1) == (pure ($ 1) <*> u)
        f = (+1)
        u = Count 1 (*2)
        v = Count 1 (+3)
        w = Count 1 (1 :: Int)

verifyMonadLaws :: IO ()
verifyMonadLaws = do
    putStrLn "Monad laws for Counter"
    putStrLn $ "left identity : " ++ show leftId
    putStrLn $ "right identity: " ++ show rightId
    putStrLn $ "associativity : " ++ show assoc
    where
        leftId = (return 1 >>= u) == (u 1)
        rightId = (w >>= return) == w
        assoc = (w >>= (\x -> u x >>= v)) == ((w >>= u) >>= v)
        u x = Count 0 (x * 2)
        v x = Count 0 (x + 3)
        w = Count 0 (1 :: Int)

fun :: Int -> Int
fun = (*2)

fun' :: Int -> Int -> Int
fun' = (*)

funM :: Int -> Count Int
funM x = Count 1 (x * 2)

-- List monad example

flizt :: Int -> [(Int, Int)]
flizt n = do
    a <- [1..n]
    b <- [2..n]
    return $ (,) a b

flizt' :: Int -> [(Int, Int)]
flizt' n = [1..n] >>= \a -> [2..n] >>= \b -> return $ (,) a b

flizt'' :: Int -> [(Int, Int)]
flizt'' n = pure (,) <*> [1..n] <*> [2..n]

-- Maybe monad example

succTenOrLess :: Int -> Maybe Int
succTenOrLess x = if x > 10 then Nothing else Just $ succ x

perhaps :: Int -> Maybe Int
perhaps n =
    succTenOrLess n >>= succTenOrLess >>= succTenOrLess >>= succTenOrLess

perhaps' :: Int -> Maybe Int
perhaps' n = do
    t0 <- succTenOrLess n
    t1 <- succTenOrLess t0
    t2 <- succTenOrLess t1
    succTenOrLess t2
