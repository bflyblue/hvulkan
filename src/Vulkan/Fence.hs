{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Fence
  ( fence
  , createFence
  , destroyFence
  , waitForFences
  , resetFences
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Foreign.Extra
import           Foreign.Marshal.Array
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Log

import           Vulkan.Exception
import           Vulkan.Resource

fence :: VkDevice -> VkFenceCreateFlags -> ResIO VkFence
fence device flags =
  vulkanResource
    ( createFence device flags )
    ( waitDestroyFence device )

createFence :: MonadIO m => VkDevice -> VkFenceCreateFlags -> m VkFence
createFence device flags = liftIO $
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateFence device ciPtr VK_NULL
        >=> throwVkResult "vkCreateSemaphore: Failed to create fence."
    )
    <* logMsg "Created fence"
  where
    createInfo = createVk @VkFenceCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" flags

destroyFence :: MonadIO m => VkDevice -> VkFence -> m ()
destroyFence device fence_ = liftIO $
  vkDestroyFence device fence_ VK_NULL
    <* logMsg "Destroyed fence"

waitDestroyFence :: MonadIO m => VkDevice -> VkFence -> m ()
waitDestroyFence device fence_ = liftIO $ do
  waitForFences device [fence_] maxBound
  destroyFence device fence_

waitForFences :: MonadIO m => VkDevice -> [VkFence] -> Word64 -> m ()
waitForFences device fences timeout = liftIO $
  withArrayLen fences $ \count pFences ->
    vkWaitForFences device (fromIntegral count) pFences VK_TRUE timeout
      >>= throwVkResult "vkWaitForFences: Failed to wait for fences."

resetFences :: MonadIO m => VkDevice -> [VkFence] -> m ()
resetFences device fences = liftIO $
  withArrayLen fences $ \count pFences ->
    vkResetFences device (fromIntegral count) pFences
      >>= throwVkResult "vkResetFences: Failed to reset fences."
