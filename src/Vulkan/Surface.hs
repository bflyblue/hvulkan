{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.Surface
  ( getPhysicalDeviceSurfaceCapabilities
  , getPhysicalDeviceSurfaceFormats
  , getPhysicalDeviceSurfacePresentModes

  , supportsSwapchain
  , supportsSurface
  ) where

import           Control.Monad
import qualified Data.Set                 as Set
import           Foreign.Extra
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface

import           Vulkan.Exception
import           Vulkan.Foreign

getPhysicalDeviceSurfaceCapabilities
  :: VkPhysicalDevice
  -> VkSurfaceKHR
  -> IO VkSurfaceCapabilitiesKHR
getPhysicalDeviceSurfaceCapabilities device surface =
  allocaPeek
    ( vkGetPhysicalDeviceSurfaceCapabilitiesKHR device surface
        >=> throwVkResult "vkGetPhysicalDeviceSurfaceCapabilitiesKHR: Failed to get surface capabilities"
    )

getPhysicalDeviceSurfaceFormats
  :: VkPhysicalDevice
  -> VkSurfaceKHR
  -> IO [VkSurfaceFormatKHR]
getPhysicalDeviceSurfaceFormats device surface =
  fetchAllMsg "vkGetPhysicalDeviceSurfaceFormatsKHR: Failed to get surface formats"
    (vkGetPhysicalDeviceSurfaceFormatsKHR device surface)

getPhysicalDeviceSurfacePresentModes
  :: VkPhysicalDevice
  -> VkSurfaceKHR
  -> IO [VkPresentModeKHR]
getPhysicalDeviceSurfacePresentModes physicalDevice surface =
  fetchAllMsg "vkGetPhysicalDeviceSurfacePresentModesKHR: Failed to get surface present modes"
    (vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice surface)

supportsSwapchain
  :: Word32                     -- Minimum acceptable value for minImageCount
  -> Maybe [VkSurfaceFormatKHR] -- Must support one of these surface formats
  -> Maybe [VkPresentModeKHR]   -- Must support one of these present modes
  -> VkPhysicalDevice
  -> VkSurfaceKHR
  -> IO Bool
supportsSwapchain minImageCount acceptableFormats acceptablePresentModes device surface = do
  supportedCapabilities <- getPhysicalDeviceSurfaceCapabilities device surface
  supportedFormats      <- getPhysicalDeviceSurfaceFormats device surface
  supportedPresentModes <- getPhysicalDeviceSurfacePresentModes device surface

  return $ hasRequiredCapabilities supportedCapabilities
        && supportsAny acceptableFormats supportedFormats
        && supportsAny acceptablePresentModes supportedPresentModes
  where
    hasRequiredCapabilities caps =
      getField @"minImageCount" caps >= minImageCount

    supportsAny Nothing _ = True
    supportsAny (Just acceptable) supported =
      not $ Set.disjoint (Set.fromList acceptable) (Set.fromList supported)

supportsSurface :: VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> IO Bool
supportsSurface physicalDevice familyIndex surface =
  (== VK_TRUE) <$> allocaPeek
    ( vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice familyIndex surface
        >=> throwVkResult "vkGetPhysicalDeviceSurfaceSupportKHR: Failed to get surface support."
    )
