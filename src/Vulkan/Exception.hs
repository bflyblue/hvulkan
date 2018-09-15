module Vulkan.Exception where

import           Control.Exception
import           Control.Monad.IO.Class
import           Graphics.Vulkan.Core_1_0

data VulkanException
  = VulkanException
  { vkeCode    :: Maybe VkResult
  , vkeMessage :: String
  } deriving (Eq, Show, Read)

instance Exception VulkanException where
  displayException (VulkanException Nothing msg)
    = unlines
    [ ""
    , "Vulkan exception:"
    , "*** " ++ msg
    ]
  displayException (VulkanException (Just c) msg)
    = unlines
    [ ""
    , "Vulkan error: " ++ show c
    , "*** " ++ msg
    ]

throwVkResult
  :: MonadIO m
  => String
  -> VkResult
  -> m ()
throwVkResult _ VK_SUCCESS = return ()
throwVkResult msg result = liftIO $ throwIO $ VulkanException (Just result) msg

throwVkMsg :: String -> IO a
throwVkMsg msg = throwIO $ VulkanException Nothing msg
