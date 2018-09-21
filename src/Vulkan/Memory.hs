{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Memory
  ( memoryFor
  , allocateMemoryFor
  , freeMemory

  , getBufferMemoryRequirements
  , bindBufferMemory

  , mapMemory
  , unmapMemory
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
import           Vulkan.PhysicalDevice
import           Vulkan.Resource

memoryFor
  :: VkPhysicalDevice
  -> VkDevice
  -> VkMemoryRequirements
  -> VkMemoryPropertyFlags
  -> ResIO VkDeviceMemory
memoryFor physicalDevice device requirements requiredFlags =
  vulkanResource
    ( allocateMemoryFor physicalDevice device requirements requiredFlags )
    ( freeMemory device )

allocateMemoryFor
  :: MonadIO m
  => VkPhysicalDevice
  -> VkDevice
  -> VkMemoryRequirements
  -> VkMemoryPropertyFlags
  -> m VkDeviceMemory
allocateMemoryFor physicalDevice device requirements requiredFlags = liftIO $ do
  memoryTypeIndex <- findSuitableMemory physicalDevice requirements requiredFlags
  withPtr (allocInfo memoryTypeIndex) $ \aiPtr ->
    allocaPeek
    ( vkAllocateMemory device aiPtr VK_NULL
        >=> throwVkResult "vkCreateMemory: Failed to allocate memory."
    )
    <* logMsg "Allocated memory"
  where
    allocInfo memoryTypeIndex = createVk @VkMemoryAllocateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"allocationSize" allocSize
      &* set           @"memoryTypeIndex" memoryTypeIndex
    allocSize = getField @"size" requirements

freeMemory
  :: MonadIO m
  => VkDevice
  -> VkDeviceMemory
  -> m ()
freeMemory device mem = liftIO $
  vkFreeMemory device mem VK_NULL
    <* logMsg "Freed memory"

findSuitableMemory
  :: MonadIO m
  => VkPhysicalDevice
  -> VkMemoryRequirements
  -> VkMemoryPropertyFlags
  -> m Word32
findSuitableMemory physicalDevice requirements requiredFlags = liftIO $ do
  (memoryTypes, _) <- physicalDeviceMemory physicalDevice
  let suitable = filter suitableMemoryType (zip [0..] memoryTypes)
  case suitable of
    [] -> undefined
    ((i, _) : _) -> return $ fromIntegral i
  where
    memoryTypeBits = getField @"memoryTypeBits" requirements
    suitableMemoryType (memoryTypeIndex, memoryType) =
      testBit memoryTypeBits memoryTypeIndex &&
        (getField @"propertyFlags" memoryType .&. requiredFlags == requiredFlags)

physicalDeviceMemory :: MonadIO m => VkPhysicalDevice -> m ([VkMemoryType], [VkMemoryHeap])
physicalDeviceMemory physicalDevice = liftIO $ do
  memoryProperties <- physicalDeviceMemoryProperties physicalDevice
  let memoryTypeCount = getField @"memoryTypeCount" memoryProperties
  memoryTypes <- readArray @"memoryTypes" memoryProperties (fromIntegral memoryTypeCount)
  let memoryHeapCount = getField @"memoryHeapCount" memoryProperties
  memoryHeaps <- readArray @"memoryHeaps" memoryProperties (fromIntegral memoryHeapCount)
  return (memoryTypes, memoryHeaps)

readArray
  :: forall fname a. ( VulkanMarshal a, Storable (FieldType fname a), CanReadFieldArray fname 0 a )
  => a
  -> Int
  -> IO [FieldType fname a]
readArray x n =
  peekArray n
  (unsafePtr x `plusPtr` fieldOffset @fname @a)

getBufferMemoryRequirements
  :: MonadIO m
  => VkDevice
  -> VkBuffer
  -> m VkMemoryRequirements
getBufferMemoryRequirements device buffer = liftIO $
  allocaPeek
  ( vkGetBufferMemoryRequirements device buffer
  )

bindBufferMemory
  :: MonadIO m
  => VkDevice
  -> VkBuffer
  -> VkDeviceMemory
  -> VkDeviceSize
  -> m ()
bindBufferMemory device buffer memory offset = liftIO $
  vkBindBufferMemory device buffer memory offset
    >>= throwVkResult "vkBindBufferMemory: Failed to bind buffer memory."

mapMemory
  :: MonadIO m
  => VkDevice
  -> VkDeviceMemory
  -> VkDeviceSize
  -> VkDeviceSize
  -> VkMemoryMapFlags
  -> m (Ptr a)
mapMemory device memory offset size flags = liftIO $
  fmap castPtr $
    allocaPeek $ vkMapMemory device memory offset size flags
      >=> throwVkResult "vkMapMempry: Failed to map memory."

unmapMemory
  :: MonadIO m
  => VkDevice
  -> VkDeviceMemory
  -> m ()
unmapMemory device memory = liftIO $
  vkUnmapMemory device memory
