{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Buffer
  ( buffer
  , createBuffer
  , destroyBuffer
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

buffer
  :: VkDevice
  -> VkBufferUsageFlags
  -> VkDeviceSize
  -> VkSharingMode
  -> [Word32]
  -> ResIO VkBuffer
buffer device usage size sharingMode queueFamilyIndices =
  vulkanResource
    ( createBuffer device usage size sharingMode queueFamilyIndices )
    ( destroyBuffer device )

createBuffer
  :: MonadIO m
  => VkDevice
  -> VkBufferUsageFlags
  -> VkDeviceSize
  -> VkSharingMode
  -> [Word32]
  -> m VkBuffer
createBuffer device usage size sharingMode queueFamilyIndices = liftIO $
  withPtr bufferInfo $ \biPtr ->
    allocaPeek
    ( vkCreateBuffer device biPtr VK_NULL
        >=> throwVkResult "vkCreateBuffer: Failed to create buffer."
    )
    <* logMsg "Created buffer"
  where
    bufferInfo = createVk @VkBufferCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" 0
      &* set           @"usage" usage
      &* set           @"size" size
      &* set           @"sharingMode" sharingMode
      &* set           @"queueFamilyIndexCount" (fromIntegral $ length queueFamilyIndices)
      &* setListRef    @"pQueueFamilyIndices" queueFamilyIndices

destroyBuffer
  :: MonadIO m
  => VkDevice
  -> VkBuffer
  -> m ()
destroyBuffer device buffer_ = liftIO $
  vkDestroyBuffer device buffer_ VK_NULL
    <* logMsg "Destroyed buffer"
