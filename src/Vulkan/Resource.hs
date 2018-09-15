module Vulkan.Resource where

import           Control.Monad.Trans.Resource

vulkanResource :: MonadResource m => IO a -> (a -> IO ()) -> m a
vulkanResource alloc free_ = do
  (_releaseKey, a) <- allocate alloc free_
  return a
