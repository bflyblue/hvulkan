{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.DescriptorPool
  ( descriptorPool
  , createDescriptorPool
  , destroyDescriptorPool
  , resetDescriptorPool
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

descriptorPool
  :: VkDevice
  -> VkDescriptorPoolCreateFlags
  -> [(VkDescriptorType, Word32)]
  -> Word32
  -> ResIO VkDescriptorPool
descriptorPool device flags pools maxSets =
  vulkanResource
    ( createDescriptorPool device flags pools maxSets )
    ( destroyDescriptorPool device )

createDescriptorPool
  :: MonadIO m
  => VkDevice
  -> VkDescriptorPoolCreateFlags
  -> [(VkDescriptorType, Word32)]
  -> Word32
  -> m VkDescriptorPool
createDescriptorPool device flags pools maxSets = liftIO $ do
  let poolInfos = map poolInfo pools
  withPtr (createInfo poolInfos) $ \ciPtr ->
    allocaPeek
    ( vkCreateDescriptorPool device ciPtr VK_NULL
        >=> throwVkResult "vkCreateDescriptorPool: Failed to create descriptor pool."
    )
    <* logMsg "Created descriptor pool"
  where
    createInfo poolSizes = createVk @VkDescriptorPoolCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" flags
      &* set           @"poolSizeCount" (fromIntegral $ length poolSizes)
      &* setListRef    @"pPoolSizes" poolSizes
      &* set           @"maxSets" maxSets

    poolInfo (type_, descriptorCount) = createVk @VkDescriptorPoolSize
      $  set           @"type" type_
      &* set           @"descriptorCount" descriptorCount

destroyDescriptorPool :: MonadIO m => VkDevice -> VkDescriptorPool -> m ()
destroyDescriptorPool device pool = liftIO $
  vkDestroyDescriptorPool device pool VK_NULL
    <* logMsg "Destroyed descriptor pool"

resetDescriptorPool :: MonadIO m => VkDevice -> VkDescriptorPool -> VkDescriptorPoolResetFlags -> m ()
resetDescriptorPool device pool flags =
  liftIO $ vkResetDescriptorPool device pool flags
    >>= throwVkResult "vkResetDescriptorPool: Failed to reset descriptor pool."
