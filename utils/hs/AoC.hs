module AoC where

import Options.Applicative


data TestResult a = Ok | Fail a a

instance (Show a) => Show (TestResult a) where
    show Ok = "Test passed."
    show (Fail a b) = "Test failed! Expected " ++ show a ++ " but got " ++ show b

test :: (Eq b) => (a -> b) -> b -> a -> TestResult b
test f expected x
    | out == expected = Ok
    | otherwise = Fail expected out
    where
        out = f x

-- ============================================== CLI option parser

data CLIOptions = CLIOptions
    { clipart :: Int
    , clitest :: Bool
    }
    deriving (Show)

cliparser :: Parser CLIOptions
cliparser =
    CLIOptions
        <$> option auto
            ( long "part"
            <> short 'p'
            <> metavar "PART"
            <> help "specify a part"
            <> value 0
            )
        <*> switch
            ( long "test"
            <> short 't'
            <> help "run tests"
            )


clioptions :: IO CLIOptions
clioptions = execParser options
    where
        options = info (cliparser <**> helper) fullDesc