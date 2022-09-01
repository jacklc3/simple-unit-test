module TestLog (
    TestLog,
    sTestLog,
    fTestLog
) where

import Data.List

type Failure = (Int, String)
type Failures = [Failure]
data TestLog a = TestLog a Int Failures

updateFailures :: Int -> Failures -> Failures
updateFailures n = fmap (\(m, f) -> (n + m, f))

instance Functor TestLog where
    fmap g (TestLog x n fs) = TestLog (g x) n fs

instance Applicative TestLog where
    pure x = TestLog x 0 []
    (TestLog g m es) <*> (TestLog x n fs) = TestLog (g x) (n + m) (es ++ updateFailures n fs)

instance Monad TestLog where
    (TestLog x n es) >>= g = TestLog y (n + m) (es ++ updateFailures n fs)
        where (TestLog y m fs) = g x

showFailures :: Failures -> String
showFailures = intercalate "\n" . fmap (\(n,f) -> "Failure in test " ++ show n ++ ": " ++ f)

instance Show (TestLog a) where
    show (TestLog _ n fs) =
        "Ran " ++ show n ++ " test cases.\nSuccesses: " ++ show m ++
        ". Failures: " ++ show (n - m) ++ ".\n\n" ++ showFailures fs
        where m = n - length fs

-- Simple constructors for convenience --

-- Standard single success test log
sTestLog :: TestLog ()
sTestLog = TestLog () 1 []

-- Standard single failure test log from failure description
fTestLog :: String -> TestLog ()
fTestLog f = TestLog () 1 [(0, f)]
