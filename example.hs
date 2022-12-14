import UnitTest

f :: (Num a) => a -> a
f x = x * x

tests :: UnitTestLog
tests = do
    check (not True)
    check (1 == 1)
    checkEqual (2 * 2) 3
    checkEqual (f 5) 24
    checkEqual (f 5) 25
    checkEqual (f 5) 24
    checkEquivalent (Just 3) "Just 3"
    checkCloseAbs 2 3 7
    checkCloseRel 0.01 3.005 3
    checkCloseRel 0.01 3.005 4

main :: IO ()
main = putUnitTestLog tests

{-
Output from running this compiled executable:

$ ./example
Ran 10 test cases.
Successes: 3. Failures: 7.

Failure in test 0: Check is False
Failure in test 2: 4 does not equal 3
Failure in test 3: 25 does not equal 24
Failure in test 5: 25 does not equal 24
Failure in test 6: Just 3 is different to "Just 3"
Failure in test 7: Difference between 3 and 7 is greater than 2
Failure in test 9: 4.0 is greater than 3.005e-2 from 3.005
-}
