{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.CommandBuffer
  ( commandBuffers
  , allocateCommandBuffers
  , freeCommandBuffers

  , beginCommandBuffer
  , endCommandBuffer

  , queueSubmit
  , queueWaitIdle
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

beginCommandBuffer
  :: MonadIO m
  => VkCommandBuffer
  -> VkCommandBufferUsageFlags
  -> m ()
beginCommandBuffer buffer_ flags = liftIO $
  withPtr beginInfo
    ( vkBeginCommandBuffer buffer_
        >=> throwVkResult "vkBeginCommandBuffer: Failed to begin command buffer."
    )
  where
    beginInfo = createVk @VkCommandBufferBeginInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" flags
      &* set           @"pInheritanceInfo" VK_NULL

endCommandBuffer
  :: MonadIO m
  => VkCommandBuffer
  -> m ()
endCommandBuffer buffer_ = liftIO $
  vkEndCommandBuffer buffer_
    >>= throwVkResult "vkEndCommandBuffer: Failed to record command buffer."

queueSubmit
 :: MonadIO m
 => VkQueue
 -> [(VkSemaphore, VkPipelineStageFlags)]
 -> [VkSemaphore]
 -> VkFence
 -> [VkCommandBuffer]
 -> m ()
queueSubmit queue waits signal fence_ buffers = liftIO $
  withArrayLen [submitInfo] $ \count siPtr ->
    vkQueueSubmit queue (fromIntegral count) siPtr fence_
      >>= throwVkResult "vkQueueSubmit: Failed to submit queue."
  where
    (wait, waitStages) = unzip waits
    submitInfo = createVk @VkSubmitInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"waitSemaphoreCount" (fromIntegral $ length waits)
      &* setListRef    @"pWaitSemaphores" wait
      &* setListRef    @"pWaitDstStageMask" waitStages
      &* set           @"signalSemaphoreCount" (fromIntegral $ length signal)
      &* setListRef    @"pSignalSemaphores" signal
      &* set           @"commandBufferCount" (fromIntegral $ length buffers)
      &* setListRef    @"pCommandBuffers" buffers

queueWaitIdle :: MonadIO m => VkQueue -> m ()
queueWaitIdle queue = liftIO $
  vkQueueWaitIdle queue
   >>= throwVkResult "vkQueueWaitIdle: Failed to wait for idle."
