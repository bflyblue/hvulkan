{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Vulkan.ShaderModule
  ( loadShader
  , createShaderModule
  , destroyShaderModule
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString
import           Foreign.Extra
import           Foreign.Ptr
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Marshal.Create
import           Log

import           Vulkan.Exception
import           Vulkan.Resource

loadShader
  :: VkDevice
  -> FilePath
  -> ResIO VkShaderModule
loadShader device srcFile =
  vulkanResource
    ( createShaderModule device srcFile )
    ( destroyShaderModule device)

createShaderModule
  :: MonadIO m
  => VkDevice
  -> FilePath
  -> m VkShaderModule
createShaderModule device srcFile = liftIO $ do
  bytes <-
    liftIO ( Data.ByteString.readFile srcFile )
  Data.ByteString.useAsCStringLen bytes $ \( bytesPtr, len ) ->
    withPtr (createInfo bytesPtr len) $ \ciPtr ->
      allocaPeek
      ( vkCreateShaderModule device ciPtr VK_NULL
          >=> throwVkResult "vkCreateShaderModule: Failed to create image view."
      )
        <* logMsg ("Created shader module from " ++ srcFile)
  where
    createInfo bytesPtr len = createVk @VkShaderModuleCreateInfo
      $  set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
      &* set @"pNext" VK_NULL
      &* set @"flags" 0
      &* set @"pCode" ( castPtr bytesPtr )
      &* set @"codeSize" ( fromIntegral len )

destroyShaderModule
  :: VkDevice
  -> VkShaderModule
  -> IO ()
destroyShaderModule device shaderModule =
  vkDestroyShaderModule device shaderModule VK_NULL
    <* logMsg "Destroyed shader module"
