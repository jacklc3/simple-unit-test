module Checks (
    check,
    checkEqual,
    checkEquivalent,
    checkCloseAbs,
    checkCloseRel
) where

import TestLog

check :: Bool -> UnitTestLog
check True  = sUnitTestLog
check False = fUnitTestLog "Check is False"

checkEqual :: (Show a, Eq a) => a -> a -> UnitTestLog
checkEqual x y
    | x == y    = sUnitTestLog
    | otherwise = fUnitTestLog (show x ++ " does not equal " ++ show y)

checkEquivalent :: (Show a, Show b) => a -> b -> UnitTestLog
checkEquivalent x y
    | show x == show y = sUnitTestLog
    | otherwise        = fUnitTestLog (show x ++ " is different to " ++ show y)

checkCloseAbs :: (Show a, Num a, Ord a) => a -> a -> a -> UnitTestLog
checkCloseAbs t x y
    | y < x + t && y > x - t = sUnitTestLog
    | otherwise              = fUnitTestLog ("Difference between " ++ show x ++ " and " ++ show y ++ " is greater than " ++ show t)

checkCloseRel :: (Show a, Num a, Ord a) => a -> a -> a -> UnitTestLog
checkCloseRel p x y
    | y < x + t && y > x - t = sUnitTestLog
    | otherwise              = fUnitTestLog (show y ++ " is greater than " ++ show t ++ " from " ++ show x)
    where t = p * x
