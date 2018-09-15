{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.CommandBuffer
  ( commandBuffers
  , allocateCommandBuffers
  , freeCommandBuffers
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

commandBuffers
  :: VkDevice
  -> VkCommandPool
  -> Int
  -> ResIO [VkCommandBuffer]
commandBuffers device pool bufferCount =
  vulkanResource
    ( allocateCommandBuffers device pool bufferCount )
    ( freeCommandBuffers device pool )

allocateCommandBuffers
  :: MonadIO m
  => VkDevice
  -> VkCommandPool
  -> Int
  -> m [VkCommandBuffer]
allocateCommandBuffers device pool bufferCount = liftIO $
  withPtr allocInfo $ \aiPtr ->
    allocaArrayPeek bufferCount
    ( vkAllocateCommandBuffers device aiPtr
        >=> throwVkResult "vkAllocateCommandBuffers: Failed to allocate command buffers."
    )
    <* logMsg "Allocated command buffers"
  where
    allocInfo = createVk @VkCommandBufferAllocateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"commandPool" pool
      &* set           @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
      &* set           @"commandBufferCount" (fromIntegral bufferCount)

freeCommandBuffers
  :: MonadIO m
  => VkDevice
  -> VkCommandPool
  -> [VkCommandBuffer]
  -> m ()
freeCommandBuffers device pool buffers = liftIO $
  withArrayLen buffers $ \count pBuffers ->
    vkFreeCommandBuffers device pool (fromIntegral count) pBuffers
      <* logMsg "Freed command buffers"
