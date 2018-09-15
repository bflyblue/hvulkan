module Vulkan.Foreign where

import           Control.Monad
import           Foreign.Extra
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

import           Graphics.Vulkan.Core_1_0
import           Vulkan.Exception

fetchAll
  :: (Storable a, Storable b, Integral a)
  => (Ptr a -> Ptr b -> IO ())
  -> IO [b]
fetchAll f =
  alloca $ \countPtr -> do
    f countPtr VK_NULL_HANDLE
    devCount <- fromIntegral <$> peek countPtr

    allocaArrayPeek devCount $
      f countPtr

fetchAllMsg
  :: (Storable a, Storable b, Integral a)
  => String
  -> (Ptr a -> Ptr b -> IO VkResult)
  -> IO [b]
fetchAllMsg msg f =
  alloca $ \countPtr -> do
    f countPtr VK_NULL_HANDLE
      >>= throwVkResult msg
    devCount <- fromIntegral <$> peek countPtr

    allocaArrayPeek devCount $
      f countPtr >=> throwVkResult msg
