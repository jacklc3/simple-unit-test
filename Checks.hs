module Checks (
    check,
    checkEqual,
    checkEquivalent,
    checkCloseAbs,
    checkCloseRel
) where

import TestLog

check :: Bool -> TestLog ()
check True  = sTestLog
check False = fTestLog "Check is False"

checkEqual :: (Show a, Eq a) => a -> a -> TestLog ()
checkEqual x y
    | x == y    = sTestLog
    | otherwise = fTestLog (show x ++ " does not equal " ++ show y)

checkEquivalent :: (Show a, Show b) => a -> b -> TestLog ()
checkEquivalent x y
    | show x == show y = sTestLog
    | otherwise        = fTestLog (show x ++ " is different to " ++ show y)

checkCloseAbs :: (Show a, Num a, Ord a) => a -> a -> a -> TestLog ()
checkCloseAbs t x y
    | y < x + t && y > x - t = sTestLog
    | otherwise              = fTestLog ("Difference between " ++ show x ++ " and " ++ show y ++ " is greater than " ++ show t)

checkCloseRel :: (Show a, Num a, Ord a) => a -> a -> a -> TestLog ()
checkCloseRel p x y
    | y < x + t && y > x - t = sTestLog
    | otherwise              = fTestLog (show y ++ " is greater than " ++ show t ++ " from " ++ show x)
    where t = p * x
