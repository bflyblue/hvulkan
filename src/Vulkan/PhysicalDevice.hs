{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.PhysicalDevice
  ( enumeratePhysicalDevices
  , enumerateDeviceExtensionProperties
  , physicalDeviceProperties
  , physicalDeviceFeatures
  , physicalDeviceQueueFamiliesProperties
  , physicalDeviceMemoryProperties 

  , isGpu
  , supportsExtensions
  , hasFeatures
  , geometryShaderFeature
  , tessellationShaderFeature
  ) where

import           Control.Monad.IO.Class
import           Data.Bits
import qualified Data.Set                 as Set
import           Foreign.C.String
import           Foreign.Extra
import           Graphics.Vulkan.Core_1_0

import           Vulkan.Foreign

enumeratePhysicalDevices :: MonadIO m => VkInstance -> m [VkPhysicalDevice]
enumeratePhysicalDevices vkInstance = liftIO $
  fetchAllMsg
    "enumeratePhysicalDevice: Failed to enumerate physical devices."
    (vkEnumeratePhysicalDevices vkInstance)

enumerateDeviceExtensionProperties :: MonadIO m => VkPhysicalDevice -> m [VkExtensionProperties]
enumerateDeviceExtensionProperties physicalDevice = liftIO $
  fetchAllMsg
    "enumerateDeviceExtensionProperties: Failed to enumerate extensions."
    (vkEnumerateDeviceExtensionProperties physicalDevice VK_NULL)

physicalDeviceProperties :: MonadIO m => VkPhysicalDevice -> m VkPhysicalDeviceProperties
physicalDeviceProperties = liftIO . allocaPeek . vkGetPhysicalDeviceProperties

physicalDeviceFeatures :: MonadIO m => VkPhysicalDevice -> m VkPhysicalDeviceFeatures
physicalDeviceFeatures = liftIO . allocaPeek . vkGetPhysicalDeviceFeatures

physicalDeviceQueueFamiliesProperties :: MonadIO m => VkPhysicalDevice -> m [VkQueueFamilyProperties]
physicalDeviceQueueFamiliesProperties = liftIO . fetchAll . vkGetPhysicalDeviceQueueFamilyProperties

physicalDeviceMemoryProperties :: MonadIO m => VkPhysicalDevice -> m VkPhysicalDeviceMemoryProperties
physicalDeviceMemoryProperties = liftIO . allocaPeek . vkGetPhysicalDeviceMemoryProperties

isGpu :: MonadIO m => VkPhysicalDevice -> m Bool
isGpu device = do
  props   <- physicalDeviceProperties device
  qfProps <- physicalDeviceQueueFamiliesProperties device
  return $ isGpuType props
        && hasGraphicsQueueFamily qfProps
  where
    isGpuType props =
      getField @"deviceType" props `elem`
        [ VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
        , VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
        , VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
        ]

    hasGraphicsQueueFamily = any isGraphicsQueueFamily

    isGraphicsQueueFamily props =
      getField @"queueFlags" props .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits

supportsExtensions :: MonadIO m => [CString] -> VkPhysicalDevice -> m Bool
supportsExtensions requiredExtensions device = do
  requiredSet <- Set.fromList <$> mapM (liftIO . peekCString) requiredExtensions
  supportedSet <- Set.fromList . map extensionName <$> enumerateDeviceExtensionProperties device
  return $ requiredSet `Set.isSubsetOf` supportedSet
  where
    extensionName = getStringField @"extensionName"

hasFeatures :: MonadIO m => [VkPhysicalDeviceFeatures -> Integer] -> VkPhysicalDevice -> m Bool
hasFeatures featureTests device = do
  feats   <- physicalDeviceFeatures device
  return $ all (supportsFeature feats) featureTests
  where
    supportsFeature feats predicate =
      case predicate feats of
        VK_TRUE -> True
        _       -> False

geometryShaderFeature :: VkPhysicalDeviceFeatures -> VkBool32
geometryShaderFeature = getField @"geometryShader"

tessellationShaderFeature :: VkPhysicalDeviceFeatures -> VkBool32
tessellationShaderFeature = getField @"tessellationShader"
