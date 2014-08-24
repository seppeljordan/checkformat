#!/usr/bin/runhaskell

-- This program checks the length of lines and trailing whitespaces in
-- a file.

module CheckFormat

where

import Data.Char
import System.IO
import System.Environment
import System.Exit

lengthOk :: String -> Bool
lengthOk s =
    length s < 80


whitespaceOk :: String -> Bool
whitespaceOk s =
    if length s > 0
    then (not . isSpace . last) s
    else True


putResult :: (Int, Bool, Bool) -> IO ()
putResult (lineNo, len, whitesp) =
    let putIf cond string =
            if cond
            then putStrLn $ "\tLine "++(show lineNo)++": "++string
            else return ()
    in putIf (not len) "Line too long" >>
       putIf (not whitesp) "Trailing whitespace"

checkText :: [String] -> IO Bool
checkText strings =
    let wsLines = fmap whitespaceOk strings
        lLines = fmap lengthOk strings
        allOk = and $ wsLines ++ lLines
    in (sequence_ . (fmap putResult)) (zip3 [1..] lLines wsLines) >>
       return allOk


checkFile :: FilePath -> IO Bool
checkFile fn =
    fmap lines (readFile fn) >>=
    checkText

exit :: String -> Bool -> IO a
exit fn b =
    if b
    then putStrLn ("...Ok ("++fn++")") >> exitSuccess
    else putStrLn ("...false formatting ("++fn++")") >> exitFailure

main =
    fmap head getArgs >>= \filename ->
    putStrLn ("Checking file \'"++filename++"\'...") >>
    checkFile filename >>=
    exit filename
