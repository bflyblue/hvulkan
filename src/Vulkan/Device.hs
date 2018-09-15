{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Device
  ( logicalDevice
  , createLogicalDevice
  , destroyDevice

  , deviceWaitIdle
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Foreign.C.String
import           Foreign.Extra
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Log

import           Vulkan.Exception
import           Vulkan.Resource

logicalDevice
  :: VkPhysicalDevice
  -> [CString]
  -> [String]
  -> Word32
  -> ResIO VkDevice
logicalDevice physicalDevice extensions layers queueFamilyIndex =
  vulkanResource
    ( createLogicalDevice
        physicalDevice
        extensions
        layers
        queueFamilyIndex
    )
    destroyDevice

createLogicalDevice
  :: MonadIO m
  => VkPhysicalDevice
  -> [CString]
  -> [String]
  -> Word32
  -> m VkDevice
createLogicalDevice physicalDevice extensions layers queueFamilyIndex = liftIO $
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateDevice physicalDevice ciPtr VK_NULL
        >=> throwVkResult "vkCreateDevice: Failed to create device."
    )
    <* logMsg "Created logical device"
  where
    queueInfo = createVk @VkDeviceQueueCreateInfo
      $  set        @"sType" VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
      &* set        @"pNext" VK_NULL
      &* set        @"flags" 0
      &* set        @"queueFamilyIndex" queueFamilyIndex
      &* set        @"queueCount" 1
      &* setListRef @"pQueuePriorities" [1.0]

    createInfo = createVk @VkDeviceCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" 0
      &* setListRef    @"pQueueCreateInfos" [ queueInfo ]
      &* set           @"queueCreateInfoCount" 1
      &* set           @"enabledLayerCount" (fromIntegral $ length layers)
      &* setStrListRef @"ppEnabledLayerNames" layers
      &* set           @"enabledExtensionCount" (fromIntegral $ length extensions)
      &* setListRef    @"ppEnabledExtensionNames" extensions
      &* set           @"pEnabledFeatures" VK_NULL

destroyDevice :: MonadIO m => VkDevice -> m ()
destroyDevice device = liftIO $
  vkDestroyDevice device VK_NULL
    <* logMsg "Destroyed logical device"

deviceWaitIdle :: MonadIO m => VkDevice -> m ()
deviceWaitIdle device = liftIO $
  vkDeviceWaitIdle device
   >>= throwVkResult "vkDeviceWaitIdle: Failed to wait for idle."
