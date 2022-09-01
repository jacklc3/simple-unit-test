module Checks (
    check,
    checkEqual,
    checkSame,
    checkCloseAbs,
    checkCloseRel
) where

import TestLog

check :: Bool -> TestLog ()
check True  = pure ()
check False = makeFailedLog "Check is False"

checkEqual :: (Show a, Eq a) => a -> a -> TestLog ()
checkEqual x y
    | x == y    = pure ()
    | otherwise = makeFailedLog (show x ++ " does not equal " ++ show y)

checkSame :: (Show a, Show b) => a -> b -> TestLog ()
checkSame x y
    | show x == show y = pure ()
    | otherwise        = makeFailedLog (show x ++ " is different to " ++ show y)

checkCloseAbs :: (Show a, Num a, Ord a) => a -> a -> a -> TestLog ()
checkCloseAbs t x y
    | y < x + t && y > x - t = pure ()
    | otherwise              = makeFailedLog ("Difference between " ++ show x ++ " and " ++ show y ++ " is greater than " ++ show t)

checkCloseRel :: (Show a, Floating a, Ord a) => a -> a -> a -> TestLog ()
checkCloseRel p x y
    | y < x + t && y > x - t = pure ()
    | otherwise              = makeFailedLog (show y ++ " is greater than " ++ show t ++ " from " ++ show x)
    where t = p * x
