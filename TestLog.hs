module TestLog (TestLog, makeFailedLog) where

import Data.List

type Failure = (Int, String)
type Failures = [Failure]
data TestLog a = TestLog a Int Failures

makeFailedLog :: String -> TestLog ()
makeFailedLog f = TestLog () 1 [(0, f)]

updateFails :: Int -> Failures -> Failures
updateFails n = fmap (\(m, f) -> (n + m, f))

instance Functor TestLog where
    fmap g (TestLog x n fs) = TestLog (g x) n fs

instance Applicative TestLog where
    pure x = TestLog x 1 []
    (TestLog g m es) <*> (TestLog x n fs) = TestLog (g x) (n + m) (es ++ updateFails n fs)

instance Monad TestLog where
    (TestLog x n es) >>= g = TestLog y (n + m) (es ++ updateFails n fs)
        where (TestLog y m fs) = g x

showFails :: Failures -> String
showFails = intercalate "\n" . fmap (\(n,f) -> "Failure in test " ++ show n ++ ": " ++ f)

instance Show (TestLog a) where
    show (TestLog _ n fs) =
        "Ran " ++ show n ++ " test cases.\nSuccesses: " ++ show m ++
        ". Failures: " ++ show (n - m) ++ ".\n\n" ++ showFails fs
        where m = n - length fs
