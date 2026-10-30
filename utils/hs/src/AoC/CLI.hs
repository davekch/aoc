module AoC.CLI where

import AoC (getInput, test)
import Options.Applicative


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


printPart :: (Show a) => Int -> a -> IO ()
printPart part solution = do
    putStr $ "Part " ++ (show part) ++ ": "
    print solution

main'' :: (Show b, Show c, Eq b, Eq c) =>
    (String -> a) -> (a -> b) -> (a -> c) -> String -> b -> c -> CLIOptions -> IO ()
main'' parse solve1 solve2 _ _ _ (CLIOptions p False) = do
    parsed <- parse <$> getInput
    case p of
        1 -> printPart 1 (solve1 parsed)
        2 -> printPart 2 (solve2 parsed)
        _ -> (do
            printPart 1 (solve1 parsed)
            printPart 2 (solve2 parsed))
main'' parse solve1 solve2 testdata testresult1 testresult2 (CLIOptions p True) =
    case p of
        1 -> printPart 1 test1
        2 -> printPart 2 test2
        _ -> (do
            printPart 1 test1
            printPart 2 test2)
    where
        test1 = test (solve1 . parse) testresult1 testdata
        test2 = test (solve2 . parse) testresult2 testdata


main' :: (Show b, Show c, Eq b, Eq c) =>
    (String -> a) -> (a -> b) -> (a -> c) -> String -> b -> c -> IO ()
main' parse solve1 solve2 testdata testresult1 testresult2 = (main'' parse solve1 solve2 testdata testresult1 testresult2) =<< clioptions
