{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.CommandPool
  ( commandPool
  , createCommandPool
  , destroyCommandPool
  , resetCommandPool
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Foreign.Extra
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Log

import           Vulkan.Exception
import           Vulkan.Resource

commandPool :: VkDevice -> Word32 -> ResIO VkCommandPool
commandPool device familyIndex =
  vulkanResource
    ( createCommandPool device familyIndex )
    ( destroyCommandPool device )

createCommandPool :: MonadIO m => VkDevice -> Word32 -> m VkCommandPool
createCommandPool device familyIndex = liftIO $
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateCommandPool device ciPtr VK_NULL
        >=> throwVkResult "vkCreateCommandPool: Failed to create command pool."
    )
    <* logMsg "Created command pool"
  where
    createInfo = createVk @VkCommandPoolCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" 0
      &* set           @"queueFamilyIndex" familyIndex

destroyCommandPool :: MonadIO m => VkDevice -> VkCommandPool -> m ()
destroyCommandPool device pool = liftIO $
  vkDestroyCommandPool device pool VK_NULL
    <* logMsg "Destroyed command pool"

resetCommandPool :: MonadIO m => VkDevice -> VkCommandPool -> VkCommandPoolResetFlags -> m ()
resetCommandPool device pool flags =
  liftIO $ vkResetCommandPool device pool flags
    >>= throwVkResult "vkResetCommandPool: Failed to reset command pool."
