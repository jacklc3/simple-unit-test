module UnitTest (
    module Checks,
    UnitTestLog,
    putUnitTestLog
) where

import TestLog
import Checks

putUnitTestLog :: UnitTestLog -> IO ()
putUnitTestLog = putStrLn . show
