{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.RenderPass
  ( renderPass
  , createRenderPass
  , destroyRenderPass
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Foreign.Extra
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Log

import           Vulkan.Exception
import           Vulkan.Resource

renderPass :: VkDevice -> VkFormat -> ResIO VkRenderPass
renderPass device format =
  vulkanResource
    ( createRenderPass device format )
    ( destroyRenderPass device )

createRenderPass :: MonadIO m => VkDevice -> VkFormat -> m VkRenderPass
createRenderPass device format = liftIO $
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateRenderPass device ciPtr VK_NULL
        >=> throwVkResult "vkCreateRenderPass: Failed to create render pass."
    )
    <* logMsg "Created render pass"
  where
    colorAttachment =
      createVk @VkAttachmentDescription
        $  set        @"format" format
        &* set        @"samples" VK_SAMPLE_COUNT_1_BIT
        &* set        @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
        &* set        @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
        &* set        @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* set        @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* set        @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
        &* set        @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

    colorAttachmentRef =
      createVk @VkAttachmentReference
        $  set        @"attachment" 0
        &* set        @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

    subpass =
      createVk @VkSubpassDescription
        $  set        @"flags" zeroBits
        &* set        @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
        &* set        @"inputAttachmentCount" 0
        &* set        @"pInputAttachments" VK_NULL
        &* set        @"colorAttachmentCount" 1
        &* setListRef @"pColorAttachments" [colorAttachmentRef]
        &* set        @"pResolveAttachments" VK_NULL
        &* set        @"pDepthStencilAttachment" VK_NULL
        &* set        @"preserveAttachmentCount" 0
        &* set        @"pPreserveAttachments" VK_NULL

    createInfo =
      createVk @VkRenderPassCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" zeroBits
        &* set        @"attachmentCount" 1
        &* setListRef @"pAttachments" [colorAttachment]
        &* set        @"subpassCount" 1
        &* setListRef @"pSubpasses" [subpass]
        &* set        @"dependencyCount" 1
        &* setListRef @"pDependencies" [dependency]

    dependency =
      createVk @VkSubpassDependency
        $  set        @"srcSubpass" VK_SUBPASS_EXTERNAL
        &* set        @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* set        @"srcAccessMask" zeroBits
        &* set        @"dstSubpass" 0
        &* set        @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* set        @"dstAccessMask" (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)

destroyRenderPass :: MonadIO m => VkDevice -> VkRenderPass -> m ()
destroyRenderPass device renderpass = liftIO $
  vkDestroyRenderPass device renderpass VK_NULL
    <* logMsg "Destroyed render pass"
