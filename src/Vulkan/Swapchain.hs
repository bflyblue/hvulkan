{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Swapchain
  ( swapchain
  , createSwapchain
  , destroySwapchain

  , getSwapchainImages
  , acquireNextImage
  , queuePresent
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Foreign.Extra
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Log

import           Vulkan.Exception
import           Vulkan.Foreign
import           Vulkan.Resource

swapchain
  :: VkDevice
  -> Word32
  -> Word32
  -> VkSurfaceKHR
  -> VkFormat
  -> VkColorSpaceKHR
  -> VkPresentModeKHR
  -> Word32
  -> VkExtent2D
  -> VkSurfaceTransformFlagBitsKHR
  -> ResIO VkSwapchainKHR
swapchain
    device
    graphicsFamily
    presentFamily
    surface
    surfaceFormat
    colorSpace
    presentMode
    imageCount
    swapExtent
    transform =
  vulkanResource
    ( createSwapchain
        device
        graphicsFamily
        presentFamily
        surface
        surfaceFormat
        colorSpace
        presentMode
        imageCount
        swapExtent
        transform
     )
     ( destroySwapchain device )

createSwapchain
  :: MonadIO m
  => VkDevice
  -> Word32
  -> Word32
  -> VkSurfaceKHR
  -> VkFormat
  -> VkColorSpaceKHR
  -> VkPresentModeKHR
  -> Word32
  -> VkExtent2D
  -> VkSurfaceTransformFlagBitsKHR
  -> m VkSwapchainKHR
createSwapchain
    device
    graphicsFamily
    presentFamily
    surface
    surfaceFormat
    colorSpace
    presentMode
    imageCount
    swapExtent
    transform = liftIO $
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateSwapchainKHR device ciPtr VK_NULL
        >=> throwVkResult "vkCreateSwapchainKHR: Failed to create swapchain."
    )
    <* logMsg "Created swapchain"
  where
    createInfo = createVk @VkSwapchainCreateInfoKHR
      $  set        @"sType" VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
      &* set        @"pNext" VK_NULL
      &* set        @"surface" surface
      &* set        @"minImageCount" imageCount
      &* set        @"imageFormat" surfaceFormat
      &* set        @"imageColorSpace" colorSpace
      &* set        @"imageExtent" swapExtent
      &* set        @"imageArrayLayers" 1
      &* set        @"imageUsage" VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      &* set        @"preTransform" transform
      &* set        @"compositeAlpha" VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
      &* set        @"presentMode" presentMode
      &* set        @"clipped" VK_TRUE
      &* set        @"oldSwapchain" VK_NULL
      &* if graphicsFamily == presentFamily
         then
              set        @"imageSharingMode" VK_SHARING_MODE_EXCLUSIVE
           &* set        @"queueFamilyIndexCount" 0
           &* set        @"pQueueFamilyIndices" VK_NULL
         else
              set        @"imageSharingMode" VK_SHARING_MODE_CONCURRENT
           &* set        @"queueFamilyIndexCount" 2
           &* setListRef @"pQueueFamilyIndices" [graphicsFamily, presentFamily]

destroySwapchain
  :: MonadIO m
  => VkDevice
  -> VkSwapchainKHR
  -> m ()
destroySwapchain device chain = liftIO $
  vkDestroySwapchainKHR device chain VK_NULL
    <* logMsg "Destroyed swapchain"

getSwapchainImages
  :: MonadIO m
  => VkDevice
  -> VkSwapchainKHR
  -> m [VkImage]
getSwapchainImages device chain = liftIO $
  fetchAllMsg
    "getSwapchainImages: Failed to get swapchain images."
    (vkGetSwapchainImagesKHR device chain)

acquireNextImage
  :: MonadIO m
  => VkDevice
  -> VkSwapchainKHR
  -> Word64
  -> VkSemaphore
  -> VkFence
  -> m Word32
acquireNextImage device swapChain timeout semaphore_ fence_ = liftIO $
  allocaPeek $
    vkAcquireNextImageKHR
      device
      swapChain
      timeout
      semaphore_
      fence_
    >=> throwVkResult "vkAcquireNextImageKHR: Failed to acquire image."

queuePresent
 :: MonadIO m
 => VkQueue
 -> [VkSwapchainKHR]
 -> [VkSemaphore]
 -> [Word32]
 -> m ()
queuePresent queue swapChains wait imageIndices = liftIO $
  withPtr presentInfo $
    vkQueuePresentKHR queue
      >=> throwVkResult "vkQueuePresentKHR: Failed to submit present request."
  where
    presentInfo = createVk @VkPresentInfoKHR
      $  set           @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
      &* set           @"pNext" VK_NULL
      &* set           @"waitSemaphoreCount" (fromIntegral $ length wait)
      &* setListRef    @"pWaitSemaphores" wait
      &* set           @"swapchainCount" (fromIntegral $ length swapChains)
      &* setListRef    @"pSwapchains" swapChains
      &* setListRef    @"pImageIndices" imageIndices
      &* set           @"pResults" VK_NULL
