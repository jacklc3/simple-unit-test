module UnitTest (
    module Checks,
    putTestLog
) where

import TestLog
import Checks

putTestLog :: TestLog a -> IO ()
putTestLog = putStrLn . show
