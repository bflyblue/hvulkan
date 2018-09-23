{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}

module Vulkan.Instance
  ( vulkanInstance
  , createVulkanInstance
  , destroyVulkanInstance
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource
import           Foreign.C
import           Foreign.Extra
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Log
import           Vulkan.Exception
import           Vulkan.Resource

vulkanInstance
  :: Word32
  -> String
  -> Word32
  -> String
  -> Word32
  -> [CString]
  -> [String]
  -> ResIO VkInstance
vulkanInstance apiVersion appName appVersion engineName engineVersion extensions layers =
  vulkanResource
    ( createVulkanInstance
        apiVersion
        appName
        appVersion
        engineName
        engineVersion
        extensions
        layers
    )
    destroyVulkanInstance

createVulkanInstance
  :: Word32
  -> String
  -> Word32
  -> String
  -> Word32
  -> [CString]
  -> [String]
  -> IO VkInstance
createVulkanInstance apiVersion appName appVersion engineName engineVersion extensions layers =
  withPtr createInfo $ \iciPtr ->
    allocaPeek
        (   vkCreateInstance iciPtr VK_NULL
        >=> throwVkResult "vkCreateInstance: Failed to create vkInstance."
        )
      <* logMsg "Created Vulkan instance"
 where
  appInfo =
    createVk @VkApplicationInfo
      $  set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
      &* set @"pNext" VK_NULL
      &* setStrRef @"pApplicationName" appName
      &* set @"applicationVersion" appVersion
      &* setStrRef @"pEngineName" engineName
      &* set @"engineVersion" engineVersion
      &* set @"apiVersion" apiVersion

  createInfo =
    createVk @VkInstanceCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* setVkRef @"pApplicationInfo" appInfo
      &* set @"enabledLayerCount" (fromIntegral $ length layers)
      &* setStrListRef @"ppEnabledLayerNames" layers
      &* set @"enabledExtensionCount" (fromIntegral $ length extensions)
      &* setListRef @"ppEnabledExtensionNames" extensions

destroyVulkanInstance :: VkInstance -> IO ()
destroyVulkanInstance vkInstance =
  vkDestroyInstance vkInstance VK_NULL
    <* logMsg "Destroyed Vulkan instance"
