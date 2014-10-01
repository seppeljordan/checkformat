#!/usr/bin/runhaskell

-- |This program checks the length of lines and trailing whitespaces
-- in a file.

module CheckFormat

where

import Data.Char
import System.Environment
import System.Exit
import Data.Maybe


type LineNumber = Integer
data CheckError = LineTooLong | TrailingWhitespaces

lengthOk :: String -> Bool
lengthOk s =
    length s < 80


whitespaceOk :: String -> Bool
whitespaceOk s =
    if length s > 0
    then (not . isSpace . last) s
    else True


putIndentLn :: String -> IO ()
putIndentLn s = putStrLn ("\t"++s)


renderCheckError :: (Show a) => a -> CheckError -> IO ()
renderCheckError lineNo LineTooLong =
    putIndentLn $ show lineNo ++": Line too long"
renderCheckError lineNo TrailingWhitespaces =
    putIndentLn $ show lineNo ++": Trailing whitespaces"


putResult :: LineNumber -> [CheckError] -> IO ()
putResult lineNo errs =
    (mapM_ (renderCheckError lineNo)) errs

checkText :: [String] -> IO Bool
checkText strings =
    let results = map checkLine strings
        resultsOk = all null results
    in sequence_ ( zipWith putResult [1..] results) >> return resultsOk

checkLine :: String -> [CheckError]
checkLine str =
    foldl prependJust [] [ checkLength
                         , checkWhitespaces
                         ]
    where prependJust list mfun | isJust mval = (fromJust mval):list
                                | otherwise = list
                                where mval = mfun str


checkLength :: [a] -> Maybe CheckError
checkLength str | length str < 80 = Nothing
                | otherwise = Just LineTooLong

checkWhitespaces :: String -> Maybe CheckError
checkWhitespaces str | whitespaceOk str = Nothing
                     | otherwise = Just TrailingWhitespaces


checkFiles :: [FilePath] -> IO Bool
checkFiles filenames =
    (fmap (all id).mapM checkFile) filenames


checkFile :: FilePath -> IO Bool
checkFile filename =
    putStrLn ("Checking file \'"++filename++"\'...") >>
    fmap lines (readFile filename) >>=
    checkText >>= \result ->
    renderResult filename result >>
    return result


renderResult :: FilePath -> Bool -> IO ()
renderResult filename False = putStrLn ("...false formatting ("++filename++")")
renderResult filename True = putStrLn ("...Ok ("++filename++")")


exit :: Bool -> IO a
exit b =
    if b
    then exitSuccess
    else exitFailure


main :: IO a
main =
    getArgs >>= \filenames ->
    checkFiles filenames >>=
    exit
