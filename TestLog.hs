module TestLog (
    UnitTestLog,
    sUnitTestLog,
    fUnitTestLog
) where

import Data.List

type Failure = (Int, String)
type Failures = [Failure]
data TestLog a = TestLog a Int Failures
type UnitTestLog = TestLog ()

updateFailures :: Int -> Failures -> Failures
updateFailures n = fmap (\(m, f) -> (n + m, f))

instance Functor TestLog where
    fmap g (TestLog x n fs) = TestLog (g x) n fs

instance Applicative TestLog where
    pure x = TestLog x 0 []
    (TestLog g m es) <*> (TestLog x n fs) = TestLog (g x) (n + m) (updateFailures n es ++ fs)

instance Monad TestLog where
    (TestLog x n fs) >>= g = TestLog y (n + m) (updateFailures n es ++ fs)
        where (TestLog y m es) = g x

showFailures :: Failures -> String
showFailures = foldl (\e (n,f) -> "\nFailure in test " ++ show n ++ ": " ++ f ++ e) ""

instance Show (TestLog a) where
    show (TestLog _ n fs) =
        "Ran " ++ show n ++ " test cases.\nSuccesses: " ++ show m ++
        ". Failures: " ++ show (n - m) ++ ".\n" ++ showFailures fs
        where m = n - length fs

-- Simple constructors for convenience --

-- Standard single success test log
sUnitTestLog :: UnitTestLog
sUnitTestLog = TestLog () 1 []

-- Standard single failure test log from failure description
fUnitTestLog :: String -> UnitTestLog
fUnitTestLog f = TestLog () 1 [(0, f)]
