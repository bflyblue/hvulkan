module Log where

import           Control.Monad.IO.Class

logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . putStrLn
