module Dafny.Request (askDafny, Backend(..)) where

import Network.HTTP
import Dafny.Analyze

data Backend = REST

askDafnyREST :: String -> IO (Either String Report)
askDafnyREST src = do
    let req = postRequestWithBody
                "http://rise4fun.com/rest/ask/dafny/" -- URL
                "application/octet-stream" -- content-type
                src
    eRes <- simpleHTTP req
    case eRes of
        Right res -> Right <$> analyze (rspBody res)
        Left err  -> return (Left $ show err)

askDafny :: Backend -> String -> IO (Either String Report)
askDafny REST = askDafnyREST
