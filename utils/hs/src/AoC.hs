module AoC where


data TestResult a = Ok | Fail a a

instance (Show a) => Show (TestResult a) where
    show Ok = "Test passed."
    show (Fail a b) = "Test failed! Expected " ++ show a ++ " but got " ++ show b

instance Semigroup (TestResult a) where
    (Fail a b) <> _ = Fail a b
    _ <> (Fail a b) = Fail a b
    _ <> _ = Ok

-- to test multiple tests at once, do
-- mconcat $ zipWith (test f) expectedlist argslist
instance Monoid (TestResult a) where
    mempty = Ok


test :: (Eq b) => (a -> b) -> b -> a -> TestResult b
test f expected x
    | out == expected = Ok
    | otherwise = Fail expected out
    where
        out = f x
