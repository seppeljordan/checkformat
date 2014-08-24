#!/usr/bin/runhaskell

module RunTests

where

import System.Process
import System.Exit

-- |Run the specified command and return True if the test succeeded
-- and False if the test failed.
runTestCmd :: [String] -> IO Bool
runTestCmd [] = error "Specify a command to run"
runTestCmd (cmd:args) =
    let evalReturn code =     
            if (code == ExitSuccess)
            then True
            else False
        execCmd = rawSystem cmd args
    in fmap evalReturn execCmd
