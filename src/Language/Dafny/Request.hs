module Language.Dafny.Request (askDafny, Backend(..)) where

import Network.HTTP
import Language.Dafny.Analyze
import System.Process
import System.IO.Temp
import System.IO

data Backend = REST
             | Local FilePath

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

askDafnyLocal :: FilePath -> String -> IO (Either String Report)
askDafnyLocal binPath src =
    withSystemTempDirectory "xwidl" $ \fp -> do
        writeFile fp src
        (_, Just hout, _, _) <- createProcess (proc "./dafny" [fp])
                                              { cwd = Just binPath,
                                                std_out = CreatePipe }
        s <- hGetContents hout
        Right <$> (analyze s)


