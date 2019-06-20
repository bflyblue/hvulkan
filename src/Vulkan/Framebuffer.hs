{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Framebuffer
  ( framebuffer
  , createFramebuffer
  , destroyFramebuffer
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

framebuffer
  :: VkDevice
  -> VkRenderPass
  -> VkExtent2D
  -> [VkImageView]
  -> ResIO VkFramebuffer
framebuffer device renderpass extent_ attachments =
  vulkanResource
    ( createFramebuffer
        device
        renderpass
        extent_
        attachments
    )
    ( destroyFramebuffer device )

createFramebuffer
  :: MonadIO m
  => VkDevice
  -> VkRenderPass
  -> VkExtent2D
  -> [VkImageView]
  -> m VkFramebuffer
createFramebuffer device renderpass extent_ attachments = liftIO $
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateFramebuffer device ciPtr VK_NULL
        >=> throwVkResult "vkCreateFramebuffer: Failed to create framebuffer."
    )
    <* logMsg "Created framebuffer"
  where
    createInfo = createVk @VkFramebufferCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" zeroBits
      &* set           @"renderPass" renderpass
      &* set           @"attachmentCount" (fromIntegral $ length attachments)
      &* setListRef    @"pAttachments" attachments
      &* set           @"width" (getField @"width" extent_)
      &* set           @"height" (getField @"height" extent_)
      &* set           @"layers" 1

destroyFramebuffer :: MonadIO m => VkDevice -> VkFramebuffer -> m ()
destroyFramebuffer device framebuffer_ = liftIO $
  vkDestroyFramebuffer device framebuffer_ VK_NULL
    <* logMsg "Destroyed framebuffer"
