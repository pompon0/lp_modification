module IO where

import Control.Monad.IO.Class(MonadIO,liftIO)
import System.IO(hFlush,hPutStrLn,stdout,stderr)

putStrLnE :: MonadIO m => String -> m ()
putStrLnE s = liftIO (hPutStrLn stderr s >> hFlush stderr)
printE :: (MonadIO m, Show a) => a -> m ()
printE x = putStrLnE (show x)
