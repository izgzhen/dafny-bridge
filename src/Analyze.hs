module Analyze where

import Data.String.Utils

analyze :: String -> Report
analyze s =
    if startswith "\r\nDafny program verifier finished with" s
        then Verified
        else Error

data Report = Verified
            | Error
            deriving Show
