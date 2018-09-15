{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.ImageView
  ( imageView
  , createImageView
  , destroyImageView
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

imageView
  :: VkDevice
  -> VkFormat
  -> VkImage
  -> ResIO VkImageView
imageView device imageFormat image =
  vulkanResource
    ( createImageView
        device
        image
        imageFormat
     )
     ( destroyImageView device )

createImageView
  :: MonadIO m
  => VkDevice
  -> VkImage
  -> VkFormat
  -> m VkImageView
createImageView device image imageFormat = liftIO $
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateImageView device ciPtr VK_NULL
        >=> throwVkResult "vkCreateCreateImageView: Failed to create image view."
    )
    <* logMsg "Created image view"
  where
    components = createVk @VkComponentMapping
      $  set        @"r" VK_COMPONENT_SWIZZLE_IDENTITY
      &* set        @"g" VK_COMPONENT_SWIZZLE_IDENTITY
      &* set        @"b" VK_COMPONENT_SWIZZLE_IDENTITY
      &* set        @"a" VK_COMPONENT_SWIZZLE_IDENTITY

    subresourceRange = createVk @VkImageSubresourceRange
      $  set        @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
      &*  set       @"baseMipLevel" 0
      &*  set       @"levelCount" 1
      &*  set       @"baseArrayLayer" 0
      &*  set       @"layerCount" 1

    createInfo = createVk @VkImageViewCreateInfo
      $  set        @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
      &* set        @"pNext" VK_NULL
      &* set        @"image" image
      &* set        @"viewType" VK_IMAGE_VIEW_TYPE_2D
      &* set        @"format" imageFormat
      &* set        @"components" components
      &* set        @"subresourceRange" subresourceRange

destroyImageView :: MonadIO m => VkDevice -> VkImageView -> m ()
destroyImageView device view = liftIO $
  vkDestroyImageView device view VK_NULL
    <* logMsg "Destroyed image view"
