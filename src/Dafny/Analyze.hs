module Dafny.Analyze where

import Data.String.Utils

analyze :: String -> IO Report
analyze s =
    if startswith "\r\nDafny program verifier finished with" s
        then return Verified
        else do
            putStrLn $ "Dafny returned failed: " ++ s
            return Failed

data Report = Verified
            | Failed
            deriving Show
