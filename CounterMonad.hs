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


newtype Counter a = Counter { runCounter :: Int -> Count a }

instance Functor Counter where
    fmap g x = Counter $ \n -> let y = runCounter x n in fmap g y

-- every monad is an applicative functor
instance Applicative Counter where
    pure x = Counter (\n -> Count n x)
    g <*> x = Counter $ \n ->
        let (Count _ g') = runCounter g n
            (Count _ y) = runCounter (fmap g' x) n
        in Count n y
    -- g <*> x = let (Count _ g') = runCounter g 0 in fmap g' x

instance Monad Counter where
    return x = Counter (\n -> Count n x)
    x >>= g =
        let (Count n x') = runCounter (fmap g x) 0
            (Count m x'') = runCounter x' n
        in Counter $ \_ -> Count m x''
    (>>) x g = x >>= \_ -> g


main :: IO ()
main = do
    countOnMe
    putStrLn . take 75 $ cycle "+-"
    counterStrike

countOnMe :: IO ()
countOnMe = do
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
        x = Count 42 (42 :: Int)
        printit s v = putStrLn $ s ++ ": " ++ show v

counterStrike :: IO ()
counterStrike = do
    verifyApplicativeLaws'
    putStrLn "---"
    verifyMonadLaws'
    putStrLn "---"
    printit "start value"              x
    printit "applicative start value"  (pure 10   :: Counter Int)
    printit "monadic start value"      (return 10 :: Counter Int)
    putStrLn "---"
    printit "functor application 1"    (fmap fun x)
    printit "functor application 2"    (fmap (*10) x)
    putStrLn "---"
    printit "applicative functor 0"    (pure $ fun 10 :: Counter Int)
    printit "applicative functor 1"    (pure fun <*> step')
    printit "applicative functor 2"    (return fun <*> x)
    printit "applicative functor 3"    (pure fun' <*> step' <*> step')
    putStrLn "---"
    printit "monad 0"                  (funM' 10)
    printit "monad 1"                  (return 10 >>= step >>= funM')
    printit "monad 2"                  (pure 10 >>= funM' >> return 42)
    printit "monad 3"                  (x >>= funM' >>= funM' >>= funM')
    where
        x = Counter $ \n -> Count n (2 :: Int)
        step y = Counter $ \n -> Count (n+1) y
        step' = Counter $ \n -> Count (n+1) (0 :: Int)
        printit s v = putStrLn $ s ++ ": " ++ show (runCounter v 0)

verifyApplicativeLaws :: IO ()
verifyApplicativeLaws = do
    putStrLn "Applicative laws for Counter"
    putStrLn $ "identity    : " ++ show identity
    putStrLn $ "compsition  : " ++ show comp
    putStrLn $ "homomorphism: " ++ show homo
    putStrLn $ "interchange : " ++ show inter
    where
        identity = check (pure id <*> w) w
        comp = check (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))
        homo = check (pure f <*> pure 1 :: Counter Int) (pure (f 1))
        inter = check (u <*> pure 1) (pure ($ 1) <*> u)
        f = (+1)
        u = Counter $ \n -> Count n (*2)
        v = Counter $ \n -> Count n (+3)
        w = Counter $ \n -> Count n (1 :: Int)
        check a b = runCounter a 0 == runCounter b 0

verifyMonadLaws :: IO ()
verifyMonadLaws = do
    putStrLn "Monad laws for Counter"
    putStrLn $ "left identity : " ++ show leftId
    putStrLn $ "right identity: " ++ show rightId
    putStrLn $ "associativity : " ++ show assoc
    where
        leftId = check (return 1 >>= u) (u 1)
        rightId = check (w >>= return) w
        assoc = check (w >>= (\x -> u x >>= v)) ((w >>= u) >>= v)
        u :: Int -> Counter Int
        u x = Counter $ \n -> Count n (2 * x)
        v :: Int -> Counter Int
        v x = Counter $ \n -> Count n (1 + x)
        w = Counter $ \n -> Count n (1 :: Int)
        check a b = runCounter a 0 == runCounter b 0

fun :: Int -> Int
fun = (*2)

fun' :: Int -> Int -> Int
fun' = (*)

funM :: Int -> Count Int
funM x = Count 42 (x * 2)

funM' :: Int -> Counter Int
funM' x = Counter $ \_ -> Count 42 (x * 2)

verifyApplicativeLaws' :: IO ()
verifyApplicativeLaws' = do
    putStrLn "Applicative laws for Counter"
    putStrLn $ "identity    : " ++ show identity
    putStrLn $ "compsition  : " ++ show comp
    putStrLn $ "homomorphism: " ++ show homo
    putStrLn $ "interchange : " ++ show inter
    where
        identity = check (pure id <*> w) w
        comp = check (pure (.) <*> u <*> v <*> w) (u <*> (v <*> w))
        homo = check (pure f <*> pure 1 :: Counter Int) (pure (f 1))
        inter = check (u <*> pure 1) (pure ($ 1) <*> u)
        f = (+1)
        u = Counter $ \n -> Count n (*2)
        v = Counter $ \n -> Count n (+3)
        w = Counter $ \n -> Count n (1 :: Int)
        check a b = runCounter a 0 == runCounter b 0

verifyMonadLaws' :: IO ()
verifyMonadLaws' = do
    putStrLn "Monad laws for Counter"
    putStrLn $ "left identity : " ++ show leftId
    putStrLn $ "right identity: " ++ show rightId
    putStrLn $ "associativity : " ++ show assoc
    where
        leftId = check (return 1 >>= u) (u 1)
        rightId = check (w >>= return) w
        assoc = check (w >>= (\x -> u x >>= v)) ((w >>= u) >>= v)
        u :: Int -> Counter Int
        u x = Counter $ \n -> Count n (2 * x)
        v :: Int -> Counter Int
        v x = Counter $ \n -> Count n (1 + x)
        w = Counter $ \n -> Count n (1 :: Int)
        check a b = runCounter a 0 == runCounter b 0

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
