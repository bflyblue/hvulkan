{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Pipeline
  ( graphicsPipeline
  , createGraphicsPipelines
  , destroyPipeline

  , pipelineLayout
  , createPipelineLayout
  , destroyPipelineLayout
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

graphicsPipeline :: VkDevice -> VkGraphicsPipelineCreateInfo -> ResIO VkPipeline
graphicsPipeline device pipelineInfo =
  vulkanResource
    ( createGraphicsPipelines device [pipelineInfo] )
    ( destroyPipeline device )

createGraphicsPipelines :: MonadIO m => VkDevice -> [VkGraphicsPipelineCreateInfo] -> m VkPipeline
createGraphicsPipelines device pipelines = liftIO $
  withArrayLen pipelines $ \count ciPtr ->
    allocaPeek
    ( vkCreateGraphicsPipelines device VK_NULL (fromIntegral count) ciPtr VK_NULL
        >=> throwVkResult "vkCreateGraphicsPipelines: Failed to create graphics pipelines."
    )
    <* logMsg "Created graphics pipeline"

destroyPipeline :: MonadIO m => VkDevice -> VkPipeline -> m ()
destroyPipeline device pipeline = liftIO $
  vkDestroyPipeline device pipeline VK_NULL
    <* logMsg "Destroyed graphics pipeline"

pipelineLayout :: VkDevice -> [VkDescriptorSetLayout] -> ResIO VkPipelineLayout
pipelineLayout device setLayouts =
  vulkanResource
    ( createPipelineLayout device setLayouts )
    ( destroyPipelineLayout device )

createPipelineLayout :: MonadIO m => VkDevice -> [VkDescriptorSetLayout] -> m VkPipelineLayout
createPipelineLayout device setLayouts = liftIO $
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreatePipelineLayout device ciPtr VK_NULL
        >=> throwVkResult "vkCreatePipelineLayout: Failed to create pipeline layout."
    )
    <* logMsg "Created pipeline layout"
  where
    createInfo =
      createVk @VkPipelineLayoutCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"setLayoutCount" (fromIntegral $ length setLayouts)
        &* setListRef @"pSetLayouts" setLayouts
        &* set        @"pushConstantRangeCount" 0
        &* set        @"pPushConstantRanges" VK_NULL

destroyPipelineLayout :: MonadIO m => VkDevice -> VkPipelineLayout -> m ()
destroyPipelineLayout device layout = liftIO $
  vkDestroyPipelineLayout device layout VK_NULL
    <* logMsg "Destroyed pipeline layout"
