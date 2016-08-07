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
askDafny (Local fp) = askDafnyLocal fp

askDafnyLocal :: FilePath -> String -> IO (Either String Report)
askDafnyLocal binPath src =
    withSystemTempFile "xwidl.dfy" $ \fp hd -> do
        hPutStrLn hd src
        (_, Just hout, _, _) <- createProcess (proc "./dafny" ["/nologo", "/compile:0", fp])
                                              { cwd = Just binPath,
                                                std_out = CreatePipe }
        s <- hGetContents hout
        Right <$> (analyze s)


