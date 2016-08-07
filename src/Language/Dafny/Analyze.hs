module Language.Dafny.Analyze where

import Data.String.Utils

analyze :: String -> IO Report
analyze s = do
    putStrLn $ "Analyzing: " ++ s
    if startswith "Dafny program verifier finished with" (strip s)
        then return Verified
        else do
            putStrLn $ "Dafny returned failed: " ++ s
            return Failed

data Report = Verified
            | Failed
            deriving Show
