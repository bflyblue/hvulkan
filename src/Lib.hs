{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Lib
    ( test
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Managed
-- import           Data.Bits
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
-- import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.UI.GLFW                       as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
-- import           Graphics.Vulkan.Ext.VK_KHR_surface
-- import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Safe

logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . putStrLn

test :: IO ()
test = runManaged $ do
  glfw
  glfwReqExts <- requiredGLFWExtensions
  vkInstance <- using $ vulkan "vulkan-test"
                               "vulkan-test"
                               glfwReqExts
                               ["VK_LAYER_LUNARG_standard_validation"]
  logMsg "Initialized"
  device <- liftIO $ pickPhysicalDevice vkInstance
  return ()

glfw :: Managed ()
glfw =
  managed_ $
    bracket_
      initializeGLFW
      terminateGLFW

initializeGLFW :: IO ()
initializeGLFW = do
  GLFW.init
    >>= throwGLFW "Unable to initialize GLFW"
  GLFW.vulkanSupported
    >>= throwGLFW "Vulkan not supported"
  logMsg "Initialized GLFW"

terminateGLFW :: IO ()
terminateGLFW = do
  GLFW.terminate
  logMsg "Terminated GLFW"

requiredGLFWExtensions :: MonadIO m => m [CString]
requiredGLFWExtensions = liftIO $ do
  extensions <- GLFW.getRequiredInstanceExtensions
  extNames <- mapM peekCString extensions
  logMsg $ "GLFW requires extensions: " ++ unwords extNames
  return extensions

vulkan
  :: String
  -> String
  -> [CString]
  -> [String]
  -> Managed VkInstance
vulkan progName engineName extensions layers =
  managed $
    withVulkanInstance
      progName
      engineName
      extensions
      layers

withVulkanInstance
  :: String
  -> String
  -> [CString]
  -> [String]
  -> (VkInstance -> IO a)
  -> IO a
withVulkanInstance progName engineName extensions layers =
  bracket
    ( createVulkanInstance
        progName
        engineName
        extensions
        layers
    )
    destroyVulkanInstance

allocaPeek
  :: Storable a
  => (Ptr a -> IO ()) -> IO a
allocaPeek action =
  alloca $ \ptr -> do
    action ptr
    peek ptr

createVulkanInstance
  :: String
  -> String
  -> [CString]
  -> [String]
  -> IO VkInstance
createVulkanInstance progName engineName extensions layers =
  withPtr iCreateInfo $ \iciPtr ->
    allocaPeek
    ( vkCreateInstance iciPtr VK_NULL
        >=> throwVkResult "vkCreateInstance: Failed to create vkInstance."
    )
    <* logMsg "Created Vulkan instance"
  where
    appInfo = createVk @VkApplicationInfo
      $  set       @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
      &* set       @"pNext" VK_NULL
      &* setStrRef @"pApplicationName" progName
      &* set       @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
      &* setStrRef @"pEngineName" engineName
      &* set       @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
      &* set       @"apiVersion" (_VK_MAKE_VERSION 1 0 0)

    iCreateInfo = createVk @VkInstanceCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* setVkRef      @"pApplicationInfo" appInfo
      &* set           @"enabledLayerCount" (fromIntegral $ length layers)
      &* setStrListRef @"ppEnabledLayerNames" layers
      &* set           @"enabledExtensionCount" (fromIntegral $ length extensions)
      &* setListRef    @"ppEnabledExtensionNames" extensions

destroyVulkanInstance :: VkInstance -> IO ()
destroyVulkanInstance vkInstance =
  vkDestroyInstance vkInstance VK_NULL
    <* logMsg "Destroyed Vulkan instance"

{-
fetchAll
  :: (Storable a, Storable b, Integral a)
  => (Ptr a -> Ptr b -> IO ())
  -> IO [b]
fetchAll f =
  alloca $ \countPtr -> do
    f countPtr VK_NULL_HANDLE
    devCount <- fromIntegral <$> peek countPtr

    allocaArray devCount $ \arrayPtr -> do
      f countPtr arrayPtr
      peekArray devCount arrayPtr
-}

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

    allocaArray devCount $ \arrayPtr -> do
      f countPtr arrayPtr
        >>= throwVkResult msg
      peekArray devCount arrayPtr

pickPhysicalDevice :: VkInstance -> IO VkPhysicalDevice
pickPhysicalDevice vkInstance = do
  allDevices <-
    fetchAllMsg
      "pickPhysicalDevice: Failed to enumerate physical devices."
      (vkEnumeratePhysicalDevices vkInstance )

  devices <- forM (zip [1..] allDevices) $ \(n::Int, device) -> do
    logMsg $ "---- Device #" ++ show n ++ " ----"
    props <- physicalDeviceProperties device
    logDeviceProperties props
    feats <- physicalDeviceFeatures device
    logDeviceFeatures feats
    logMsg "-------------------"
    return (device, props, feats)

  logMsg $ "Number of devices found " ++ show (length devices)

  gpus <- filterM isDeviceSuitable devices

  logMsg $ "Number of GPU devices found " ++ show (length gpus)

  case headMay gpus of
    Just (dev, _, _) -> return dev
    Nothing -> throwVkMsg "No suitable GPU devices!"

isDeviceSuitable :: (VkPhysicalDevice, VkPhysicalDeviceProperties, VkPhysicalDeviceFeatures) -> IO Bool
isDeviceSuitable (_, props, feats) =
  return $ isGpu && hasGeomShader
  where
    isGpu =
      case getField @"deviceType" props of
        VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU   -> True
        VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> True
        VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU    -> True
        _                                      -> False

    hasGeomShader =
      case getField @"geometryShader" feats of
        VK_TRUE -> True
        _       -> False

physicalDeviceProperties :: VkPhysicalDevice -> IO VkPhysicalDeviceProperties
physicalDeviceProperties = allocaPeek . vkGetPhysicalDeviceProperties

logDeviceProperties :: VkPhysicalDeviceProperties -> IO ()
logDeviceProperties props = do
  logMsg $ unwords [ "Device:", getStringField @"deviceName" props]
  logMsg $ unwords [ "Type:", show $ getField @"deviceType" props]
  logMsg $ unwords [ "Api:", toVersionString $ getField @"apiVersion" props]
  where
    toVersionString v = concat
      [ show (_VK_VERSION_MAJOR v)
      , "."
      , show (_VK_VERSION_MINOR v)
      , "."
      , show (_VK_VERSION_PATCH v)
      ]

physicalDeviceFeatures :: VkPhysicalDevice -> IO VkPhysicalDeviceFeatures
physicalDeviceFeatures = allocaPeek . vkGetPhysicalDeviceFeatures

logDeviceFeatures :: VkPhysicalDeviceFeatures -> IO ()
logDeviceFeatures feats = do
  logMsg $ unwords [ "Geometry Shader:", hasFeature @"geometryShader"]
  logMsg $ unwords [ "Tessellation Shader:", hasFeature @"tessellationShader"]
  where
    hasFeature
      :: forall fname
      . ( CanReadField fname VkPhysicalDeviceFeatures
        , FieldType fname VkPhysicalDeviceFeatures ~ VkBool32
        )
      => String
    hasFeature =
      case getField @fname feats of
        VK_TRUE -> "Yes"
        _ -> "No"

newtype GlfwException
  = GlfwException
  { glfweMessage :: String
  } deriving (Eq, Show, Read)

instance Exception GlfwException where
  displayException (GlfwException msg)
    = unlines
    [ ""
    , "Vulkan exception:"
    , "*** " ++ msg
    ]

throwGLFW :: MonadIO m => String -> Bool -> m ()
throwGLFW msg bool = liftIO $
  unless bool $
    throwIO $ GlfwException msg

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
