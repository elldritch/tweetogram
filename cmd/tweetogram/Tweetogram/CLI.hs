module Tweetogram.CLI () where

import Data.Conduit (ConduitT)
import Data.Conduit.Combinators (mapAccumWhileM)
import Relude

type PageCount = Int

onProgress :: (MonadIO m) => (PageCount -> IO ()) -> ConduitT i i m Int
onProgress f = (`mapAccumWhileM` 1) $ \x i -> do
  liftIO $ f i
  pure (Right (i + 1, x))

{-

Delegated to application:

- Option parsing
- Printing progress
- Coordinating concurrency
- Printing user-friendly error messages
- Rendering output

-}
