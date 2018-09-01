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
import           Data.Bits
import qualified Data.Set                               as Set
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.UI.GLFW                       as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Safe

logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . putStrLn

test :: IO ()
test = runManaged $ do
  glfw
  glfwReqExts <- requiredGLFWExtensions
  vkInstance  <- vulkanInstance
                   "vulkan-test"
                   "vulkan-test"
                   glfwReqExts
                   layers
  window      <- windowTitled "vulkan-test"
  surface     <- windowSurface vkInstance window
  (pDevice, queues)
              <- pickPhysicalDevice vkInstance extensions
  graphicsFamily
              <- findGraphicsQueueFamilyIndex queues
  presentFamily
              <- findPresentQueueFamilyIndex vkInstance pDevice queues
  device      <- logicalDevice
                   pDevice
                   extensions
                   layers
                   graphicsFamily

  return ()
  where
    extensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]
    layers = ["VK_LAYER_LUNARG_standard_validation"]

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

vulkanInstance
  :: String
  -> String
  -> [CString]
  -> [String]
  -> Managed VkInstance
vulkanInstance progName engineName extensions layers =
  managed $
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

pickPhysicalDevice
  :: MonadIO m
  => VkInstance
  -> [CString]
  -> m (VkPhysicalDevice, [VkQueueFamilyProperties])
pickPhysicalDevice vkInstance requiredExtensions = liftIO $ do
  logMsg "Enumerating Vulkan devices"
  allDevices <-
    fetchAllMsg
      "pickPhysicalDevice: Failed to enumerate physical devices."
      (vkEnumeratePhysicalDevices vkInstance)

  devices <- forM (zip [1..] allDevices) $ \(devIndex, device) -> do
    logMsg $ "---- Device #" ++ show devIndex ++ " ----"
    props <- physicalDeviceProperties device
    logDeviceProperties props
    feats <- physicalDeviceFeatures device
    logDeviceFeatures feats
    queues <- physicalDeviceQueueFamiliesProps device
    extensions <- enumerateDeviceExtensionProperties device
    logDeviceExtensionProperties extensions
    logMsg "-------------------"
    return (devIndex, device, props, feats, queues, extensions)

  logMsg $ "Number of devices found: " ++ show (length devices)

  gpus <- filterM (isDeviceSuitable requiredExtensions) devices

  logMsg $ "Number of GPU devices found: " ++ show (length gpus)

  case headMay gpus of
    Just (devIndex, dev, _, _, queues, _) -> do
      logMsg ("Picked device #" ++ show devIndex)
      return (dev, queues)
    Nothing ->
      throwVkMsg "No suitable GPU devices!"

isDeviceSuitable
  :: [CString]
  -> ( Word32
     , VkPhysicalDevice
     , VkPhysicalDeviceProperties
     , VkPhysicalDeviceFeatures
     , [VkQueueFamilyProperties]
     , [VkExtensionProperties]
     )
  -> IO Bool
isDeviceSuitable requiredExtensions (_, _, props, feats, queues, extensions) = do
  hasExtensions <- supportsRequiredExtensions
  return $ isGpu && hasGeomShader && hasGraphicsQueueFamily && hasExtensions
  where
    isGpu =
      getField @"deviceType" props `elem`
        [ VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
        , VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
        , VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
        ]

    hasGeomShader =
      case getField @"geometryShader" feats of
        VK_TRUE -> True
        _       -> False

    hasGraphicsQueueFamily =
      any isGraphicsQueueFamily queues

    supportsRequiredExtensions = do
      req <- Set.fromList <$> mapM peekCString requiredExtensions
      let exts = Set.fromList $ map extensionName extensions
      return $ req `Set.isSubsetOf` exts

isGraphicsQueueFamily :: VkQueueFamilyProperties -> Bool
isGraphicsQueueFamily queue =
  getField @"queueFlags" queue .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits

enumerateDeviceExtensionProperties
  :: VkPhysicalDevice
  -> IO [VkExtensionProperties]
enumerateDeviceExtensionProperties physicalDevice =
  fetchAllMsg
    "enumerateDeviceExtensionProperties: Failed to enumerate extensions."
    (vkEnumerateDeviceExtensionProperties physicalDevice VK_NULL)

logDeviceExtensionProperties :: [VkExtensionProperties] -> IO ()
logDeviceExtensionProperties extensions =
  logMsg $ unwords [ "Device Extensions:", unwords (map extensionName extensions)]

extensionName :: VkExtensionProperties -> String
extensionName = getStringField @"extensionName"

findGraphicsQueueFamilyIndex
  :: MonadIO m
  => [VkQueueFamilyProperties]
  -> m Word32
findGraphicsQueueFamilyIndex queues = liftIO $ do
  let families = filter (isGraphicsQueueFamily . snd) (zip [0..] queues)
  case headMay families of
    Just (queueFamilyIndex, _) -> do
      logMsg ("Picked graphics queue family #" ++ show queueFamilyIndex)
      return queueFamilyIndex
    Nothing ->
      throwVkMsg "No suitable graphics queue family!"

findPresentQueueFamilyIndex
  :: MonadIO m
  => VkInstance
  -> VkPhysicalDevice
  -> [VkQueueFamilyProperties]
  -> m Word32
findPresentQueueFamilyIndex vkInstance physicalDevice queues = liftIO $ do
  families <- filterM (isPresentQueueFamily . fst) (zip [0..] queues)
  case headMay families of
    Just (queueFamilyIndex, _) -> do
      logMsg ("Picked present queue family #" ++ show queueFamilyIndex)
      return queueFamilyIndex
    Nothing ->
      throwVkMsg "No suitable present queue family!"
  where
    isPresentQueueFamily =
      GLFW.getPhysicalDevicePresentationSupport
        vkInstance
        physicalDevice

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

physicalDeviceQueueFamiliesProps :: VkPhysicalDevice -> IO [VkQueueFamilyProperties]
physicalDeviceQueueFamiliesProps = fetchAll . vkGetPhysicalDeviceQueueFamilyProperties

logicalDevice
  :: VkPhysicalDevice
  -> [CString]
  -> [String]
  -> Word32
  -> Managed VkDevice
logicalDevice physicalDevice extensions layers queueFamilyIndex =
  managed $
    bracket
      ( createLogicalDevice
          physicalDevice
          extensions
          layers
          queueFamilyIndex
      )
      destroyDevice

createLogicalDevice
  :: VkPhysicalDevice
  -> [CString]
  -> [String]
  -> Word32
  -> IO VkDevice
createLogicalDevice physicalDevice extensions layers queueFamilyIndex =
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateDevice physicalDevice ciPtr VK_NULL
        >=> throwVkResult "vkCreateDevice: Failed to create device."
    )
    <* logMsg "Created logical device"
  where
    qCreateInfo = createVk @VkDeviceQueueCreateInfo
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
      &* setListRef    @"pQueueCreateInfos" [ qCreateInfo ]
      &* set           @"queueCreateInfoCount" 1
      &* set           @"enabledLayerCount" (fromIntegral $ length layers)
      &* setStrListRef @"ppEnabledLayerNames" layers
      &* set           @"enabledExtensionCount" (fromIntegral $ length extensions)
      &* setListRef    @"ppEnabledExtensionNames" extensions
      &* set           @"pEnabledFeatures" VK_NULL

destroyDevice :: VkDevice -> IO ()
destroyDevice device =
  vkDestroyDevice device VK_NULL
    <* logMsg "Destroyed logical device"

windowSurface
  :: VkInstance
  -> GLFW.Window
  -> Managed VkSurfaceKHR
windowSurface vkInstance window =
  managed $
    bracket
      ( createSurface
          vkInstance
          window
      )
      ( destroySurface
          vkInstance
      )

createSurface :: VkInstance -> GLFW.Window -> IO VkSurfaceKHR
createSurface vkInstance window =
  allocaPeek
    ( GLFW.createWindowSurface
        vkInstance
        window
        nullPtr
          >=> throwVkResult "createSurfaceWindow: Failed to create surface."
    )
    <* logMsg "Created surface"


destroySurface :: VkInstance -> VkSurfaceKHR -> IO ()
destroySurface vkInstance surface = do
  vkDestroySurfaceKHR vkInstance surface VK_NULL
  logMsg "Destroyed surface"

windowTitled :: String -> Managed GLFW.Window
windowTitled title =
  managed $
    bracket (createWindow title) destroyWindow

createWindow :: String -> IO GLFW.Window
createWindow title = do
   -- We don't require OpenGL context
  GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI

  maybeWindow <- GLFW.createWindow 640 360 title Nothing Nothing
  case maybeWindow of
    Nothing ->
      throwIO $ GlfwException "createWindow: Failed to create window."
    Just window -> do
      logMsg "Created window"
      return window

destroyWindow :: GLFW.Window -> IO ()
destroyWindow window = do
  GLFW.destroyWindow window
  logMsg "Destroyed window"

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
