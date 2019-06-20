{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.DescriptorSet
  ( descriptorSetLayout
  , createDescriptorSetLayout
  , destroyDescriptorSetLayout

  , descriptorSets
  , allocateDescriptorSets
  , freeDescriptorSets

  , updateDescriptorSets
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

descriptorSetLayout
  :: VkDevice
  -> [(Word32, VkDescriptorType, Word32, VkShaderStageFlags)]
  -> ResIO VkDescriptorSetLayout
descriptorSetLayout device bindings =
  vulkanResource
    ( createDescriptorSetLayout device bindings )
    ( destroyDescriptorSetLayout device )

createDescriptorSetLayout
  :: MonadIO m
  => VkDevice
  -> [(Word32, VkDescriptorType, Word32, VkShaderStageFlags)]
  -> m VkDescriptorSetLayout
createDescriptorSetLayout device bindings = liftIO $ do
  let bindings' = map layoutBinding bindings
  withPtr (layoutInfo bindings') $ \lbPtr ->
    allocaPeek
    ( vkCreateDescriptorSetLayout device lbPtr VK_NULL
        >=> throwVkResult "vkCreateDescriptorSetLayout: Failed to create descriptor set layout."
    )
    <* logMsg "Created desciptor set layout"
  where
    layoutInfo bindings_ = createVk @VkDescriptorSetLayoutCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" zeroBits
      &* set           @"bindingCount" (fromIntegral $ length bindings_)
      &* setListRef    @"pBindings" bindings_

    layoutBinding (binding, descriptorType, descriptorCount, flags) = createVk @VkDescriptorSetLayoutBinding
      $  set           @"binding" binding
      &* set           @"descriptorType" descriptorType
      &* set           @"descriptorCount" descriptorCount
      &* set           @"stageFlags" flags
      &* set           @"pImmutableSamplers" VK_NULL

destroyDescriptorSetLayout
  :: MonadIO m
  => VkDevice
  -> VkDescriptorSetLayout
  -> m ()
destroyDescriptorSetLayout device layout = liftIO $
  vkDestroyDescriptorSetLayout device layout VK_NULL
    <* logMsg "Destroyed descriptor set layout"

descriptorSets
  :: VkDevice
  -> VkDescriptorPool
  -> [VkDescriptorSetLayout]
  -> ResIO [VkDescriptorSet]
descriptorSets device pool layouts =
  vulkanResource
    ( allocateDescriptorSets device pool layouts )
    ( freeDescriptorSets device pool )

allocateDescriptorSets
  :: MonadIO m
  => VkDevice
  -> VkDescriptorPool
  -> [VkDescriptorSetLayout]
  -> m [VkDescriptorSet]
allocateDescriptorSets device pool layouts = liftIO $
  withPtr allocInfo $ \aiPtr ->
    allocaArrayPeek count
      ( vkAllocateDescriptorSets device aiPtr
          >=> throwVkResult "vkCreateDescriptorSetLayout: Failed to allocate descriptor sets."
      )
    <* logMsg "Allocated descriptor sets"
  where
    count = length layouts
    allocInfo = createVk @VkDescriptorSetAllocateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"descriptorPool" pool
      &* set           @"descriptorSetCount" (fromIntegral count)
      &* setListRef    @"pSetLayouts" layouts

freeDescriptorSets
  :: MonadIO m
  => VkDevice
  -> VkDescriptorPool
  -> [VkDescriptorSet]
  -> m ()
freeDescriptorSets device pool sets = liftIO $
  withArrayLen sets $ \count pSets ->
    ( vkFreeDescriptorSets device pool (fromIntegral count) pSets
      >>= throwVkResult "vkCreateDescriptorSetLayout: Failed to allocate descriptor sets."
    )
  <* logMsg "Freed descriptor sets"

updateDescriptorSets :: MonadIO m => VkDevice -> [VkWriteDescriptorSet] -> [VkCopyDescriptorSet] -> m ()
updateDescriptorSets device writes copies = liftIO $
  withArrayLen writes $ \writeCount pWrites ->
    withArrayLen copies $ \copyCount pCopies ->
      vkUpdateDescriptorSets device (fromIntegral writeCount) pWrites (fromIntegral copyCount) pCopies
