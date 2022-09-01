import UnitTest

f :: (Num a) => a -> a
f x = x * x

tests = do
    check (not True)
    check (1 == 1)
    checkEqual (2 * 2) 3
    checkEqual (f 5) 24
    checkEqual (f 5) 25
    checkEqual (f 5) 24
    checkCloseAbs 2 3 7
    checkCloseRel 0.01 3.005 3
    checkCloseRel 0.01 3.005 4

main :: IO ()
main = putTestLog tests
