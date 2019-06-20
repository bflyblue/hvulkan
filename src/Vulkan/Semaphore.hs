{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Semaphore
  ( semaphore
  , createSemaphore
  , destroySemaphore
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Foreign
import           Foreign.Extra
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Log

import           Vulkan.Exception
import           Vulkan.Resource

semaphore :: VkDevice -> ResIO VkSemaphore
semaphore device =
  vulkanResource
    ( createSemaphore device )
    ( destroySemaphore device )

createSemaphore :: MonadIO m => VkDevice -> m VkSemaphore
createSemaphore device = liftIO $
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateSemaphore device ciPtr VK_NULL
        >=> throwVkResult "vkCreateSemaphore: Failed to create semaphore."
    )
    <* logMsg "Created semaphore"
  where
    createInfo = createVk @VkSemaphoreCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" zeroBits

destroySemaphore :: MonadIO m => VkDevice -> VkSemaphore -> m ()
destroySemaphore device sem = liftIO $
  vkDestroySemaphore device sem VK_NULL
    <* logMsg "Destroyed semaphore"
