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
import qualified Data.ByteString
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
  window      <- windowTitled winWidth winHeight "vulkan-test"
  surface     <- windowSurface vkInstance window
  (pDevice, queues, surfaceCaps, surfaceFormats, presentModes)
              <- pickPhysicalDevice vkInstance surface extensions
  graphicsFamily
              <- findGraphicsQueueFamilyIndex queues
  presentFamily
              <- findPresentQueueFamilyIndex
                   vkInstance
                   pDevice
                   surface
                   queues
  device      <- logicalDevice
                   pDevice
                   extensions
                   layers
                   graphicsFamily
  (chain, chainFormat)
              <- pickSwapchain
                   device
                   graphicsFamily
                   presentFamily
                   surface
                   surfaceCaps
                   surfaceFormats
                   presentModes
                   winWidth
                   winHeight
  swapImages  <- getSwapchainImages
                   device
                   chain
  swapViews   <- mapM (imageView device chainFormat) swapImages

  return ()
  where
    extensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]
    layers = ["VK_LAYER_LUNARG_standard_validation"]
    winWidth = 640
    winHeight = 360

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
  -> VkSurfaceKHR
  -> [CString]
  -> m (VkPhysicalDevice, [VkQueueFamilyProperties], VkSurfaceCapabilitiesKHR, [VkSurfaceFormatKHR], [VkPresentModeKHR])
pickPhysicalDevice vkInstance surface requiredExtensions = liftIO $ do
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
    surfaceCaps <- getPhysicalDeviceSurfaceCapabilities device surface
    logSurfaceCapabilities surfaceCaps
    surfaceFormats <- getPhysicalDeviceSurfaceFormats device surface
    logSurfaceFormats surfaceFormats
    presentModes <- getPhysicalDeviceSurfacePresentModes device surface
    logPresentModes presentModes
    logMsg "-------------------"
    return
      ( devIndex
      , device
      , props
      , feats
      , queues
      , extensions
      , surfaceCaps
      , surfaceFormats
      , presentModes
      )

  logMsg $ "Number of devices found: " ++ show (length devices)

  gpus <- filterM (isDeviceSuitable requiredExtensions) devices

  logMsg $ "Number of GPU devices found: " ++ show (length gpus)

  case headMay gpus of
    Just (devIndex, dev, _, _, queues, _, surfaceCaps, surfaceFormats, presentModes) -> do
      logMsg ("Picked device #" ++ show devIndex)
      return (dev, queues, surfaceCaps, surfaceFormats, presentModes)
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
     , VkSurfaceCapabilitiesKHR
     , [VkSurfaceFormatKHR]
     , [VkPresentModeKHR]
     )
  -> IO Bool
isDeviceSuitable requiredExtensions
    ( _
    , _
    , props
    , feats
    , queues
    , extensions
    , surfaceCaps
    , surfaceFormats
    , presentModes
    ) = do
  hasExtensions <- supportsRequiredExtensions
  return $ isGpu
        && hasGeomShader
        && hasGraphicsQueueFamily
        && hasExtensions
        && hasSuitableSwapChain
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

    hasSuitableSwapChain =
      getField @"minImageCount" surfaceCaps >= 2 &&
      (not . null) surfaceFormats &&
      (not . null) presentModes

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

getPhysicalDeviceSurfaceCapabilities
  :: VkPhysicalDevice
  -> VkSurfaceKHR
  -> IO VkSurfaceCapabilitiesKHR
getPhysicalDeviceSurfaceCapabilities physicalDevice surface =
  allocaPeek
    ( vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
        >=> throwVkResult "vkGetPhysicalDeviceSurfaceCapabilitiesKHR: Failed to get surface capabilities"
    )

logSurfaceCapabilities :: VkSurfaceCapabilitiesKHR -> IO ()
logSurfaceCapabilities caps =
  logMsg $ unwords [ "Surface Capabilities:", unwords [minImageCount, maxImageCount] ]
  where
    minImageCount = "minImageCount=" ++ show (getField @"minImageCount" caps)
    maxImageCount = "maxImageCount=" ++ show (getField @"maxImageCount" caps)

getPhysicalDeviceSurfaceFormats
  :: VkPhysicalDevice
  -> VkSurfaceKHR
  -> IO [VkSurfaceFormatKHR]
getPhysicalDeviceSurfaceFormats physicalDevice surface =
  fetchAllMsg "vkGetPhysicalDeviceSurfaceFormatsKHR: Failed to get surface formats"
    (vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice surface)

logSurfaceFormats :: [VkSurfaceFormatKHR] -> IO ()
logSurfaceFormats formats =
  logMsg $ unwords [ "Surface Formats:", unwords (map showFormat formats)]
  where
    showFormat f =
      show (getField @"format" f)
      ++ "/"
      ++ show (getField @"colorSpace" f)

getPhysicalDeviceSurfacePresentModes
  :: VkPhysicalDevice
  -> VkSurfaceKHR
  -> IO [VkPresentModeKHR]
getPhysicalDeviceSurfacePresentModes physicalDevice surface =
  fetchAllMsg "vkGetPhysicalDeviceSurfacePresentModesKHR: Failed to get surface present modes"
    (vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice surface)

logPresentModes :: [VkPresentModeKHR] -> IO ()
logPresentModes presentModes =
  logMsg $ unwords [ "Surface Present Modes:", unwords (map show presentModes)]

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
  -> VkSurfaceKHR
  -> [VkQueueFamilyProperties]
  -> m Word32
findPresentQueueFamilyIndex vkInstance physicalDevice surface queues = liftIO $ do
  families <- filterM (isSuitableFamily . fst) (zip [0..] queues)
  case headMay families of
    Just (queueFamilyIndex, _) -> do
      logMsg ("Picked present queue family #" ++ show queueFamilyIndex)
      return queueFamilyIndex
    Nothing ->
      throwVkMsg "No suitable present queue family!"
  where
    isSuitableFamily familyIndex =
      (&&) <$> isPresentQueueFamily familyIndex
           <*> canPresentSurface physicalDevice familyIndex surface
    isPresentQueueFamily =
      GLFW.getPhysicalDevicePresentationSupport
        vkInstance
        physicalDevice

canPresentSurface :: VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> IO Bool
canPresentSurface physicalDevice familyIndex surface =
  (== VK_TRUE) <$> allocaPeek
  ( vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice familyIndex surface
      >=> throwVkResult "vkGetPhysicalDeviceSurfaceSupportKHR: Failed to get surface support."
  )

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

windowTitled :: Word32 -> Word32 -> String -> Managed GLFW.Window
windowTitled width height title =
  managed $
    bracket (createWindow width height title) destroyWindow

createWindow :: Word32 -> Word32 -> String -> IO GLFW.Window
createWindow width height title = do
   -- We don't require OpenGL context
  GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI

  maybeWindow <-
    GLFW.createWindow
      (fromIntegral width)
      (fromIntegral height)
      title
      Nothing
      Nothing

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

pickSwapchain
  :: VkDevice
  -> Word32
  -> Word32
  -> VkSurfaceKHR
  -> VkSurfaceCapabilitiesKHR
  -> [VkSurfaceFormatKHR]
  -> [VkPresentModeKHR]
  -> Word32
  -> Word32
  -> Managed (VkSwapchainKHR, VkFormat)
pickSwapchain
    device
    graphicsFamily
    presentFamily
    surface
    surfaceCaps
    surfaceFormats
    presentModes
    width
    height = do
  (surfaceFormat, colorSpace) <- liftIO $ pickSurfaceFormat surfaceFormats
  presentMode <- liftIO $ pickPresentMode presentModes
  swapExtent <- liftIO $ pickSwapExtent surfaceCaps width height
  imageCount <- liftIO $ pickImageCount surfaceCaps
  logMsg
    ( "Picked swapchain: " ++ unwords
      [ "format=" ++ show surfaceFormat
      , "colorspace=" ++ show colorSpace
      , "mode=" ++ show presentMode
      , "extent=" ++ show swapExtent
      , "imagecount=" ++ show imageCount
      ]
    )
  chain <-
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
      (getField @"currentTransform" surfaceCaps)
  return (chain, surfaceFormat)

pickSurfaceFormat :: [VkSurfaceFormatKHR] -> IO (VkFormat, VkColorSpaceKHR)
pickSurfaceFormat surfaceFormats =
  if anyFormat || hasFormat VK_FORMAT_B8G8R8A8_UNORM VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  then
    return (VK_FORMAT_B8G8R8A8_UNORM, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)
  else
    return $ formatPair (head surfaceFormats)
  where
    formatPair sf = (getField @"format" sf, getField @"colorSpace" sf)
    anyFormat = length surfaceFormats == 1 && getField @"format" (head surfaceFormats) == VK_FORMAT_UNDEFINED
    hasFormat f cs = any (\sf -> getField @"format" sf == f && getField @"colorSpace" sf == cs) surfaceFormats

pickPresentMode :: [VkPresentModeKHR] -> IO VkPresentModeKHR
pickPresentMode presentModes = go preference
  where
    go [] =
      case headMay presentModes of
        Just mode -> return mode
        Nothing -> throwVkMsg "No suitable present modes!"

    go (pref : rest)
      | pref `elem` presentModes = return pref
      | otherwise = go rest

    preference =
      [ VK_PRESENT_MODE_MAILBOX_KHR
      , VK_PRESENT_MODE_FIFO_KHR
      , VK_PRESENT_MODE_FIFO_RELAXED_KHR
      , VK_PRESENT_MODE_IMMEDIATE_KHR
      ]

pickSwapExtent
  :: VkSurfaceCapabilitiesKHR
  -> Word32
  -> Word32
  -> IO VkExtent2D
pickSwapExtent surfaceCaps width height = do
  let currentExtent = getField @"currentExtent" surfaceCaps
      w = getField @"width" currentExtent
      h = getField @"height" currentExtent
  if (w, h) == (maxBound, maxBound)
  then do
    let minExtent = getField @"minImageExtent" surfaceCaps
        maxExtent = getField @"maxImageExtent" surfaceCaps
        wMin = getField @"width" minExtent
        wMax = getField @"width" maxExtent
        hMin = getField @"height" minExtent
        hMax = getField @"height" maxExtent
    return $
      createVk @VkExtent2D
        $  set @"width" (clamp wMin wMax width)
        &* set @"height" (clamp hMin hMax height)
  else
    return currentExtent
  where
    clamp mn mx = min mx . max mn

pickImageCount
  :: VkSurfaceCapabilitiesKHR
  -> IO Word32
pickImageCount surfaceCaps = do
  let cMin = getField @"minImageCount" surfaceCaps
      cMax = getField @"maxImageCount" surfaceCaps    -- 0 means no max limit
  if cMin >= 3 || cMax == cMin
  then
    return cMin
  else
    return (cMin + 1)

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
  -> Managed VkSwapchainKHR
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
  managed $
    bracket
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
  -> IO VkSwapchainKHR
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
    transform =
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
  :: VkDevice
  -> VkSwapchainKHR
  -> IO ()
destroySwapchain device chain =
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

imageView
  :: VkDevice
  -> VkFormat
  -> VkImage
  -> Managed VkImageView
imageView device imageFormat image =
  managed $
    bracket
      ( createImageView
          device
          image
          imageFormat
       )
       ( destroyImageView device )

createImageView
  :: VkDevice
  -> VkImage
  -> VkFormat
  -> IO VkImageView
createImageView device image imageFormat =
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateImageView device ciPtr VK_NULL
        >=> throwVkResult "vkCreateCreateImageView: Failed to create image view."
    )
    <* logMsg "Created image view"
  where
    components = createVk @VkComponentMapping
      $  set        @"r" VK_COMPONENT_SWIZZLE_IDENTITY
      &* set        @"g" VK_COMPONENT_SWIZZLE_IDENTITY
      &* set        @"b" VK_COMPONENT_SWIZZLE_IDENTITY
      &* set        @"a" VK_COMPONENT_SWIZZLE_IDENTITY

    subresourceRange = createVk @VkImageSubresourceRange
      $  set        @"aspectMask" VK_IMAGE_ASPECT_COLOR_BIT
      &*  set       @"baseMipLevel" 0
      &*  set       @"levelCount" 1
      &*  set       @"baseArrayLayer" 0
      &*  set       @"layerCount" 1

    createInfo = createVk @VkImageViewCreateInfo
      $  set        @"sType" VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
      &* set        @"pNext" VK_NULL
      &* set        @"image" image
      &* set        @"viewType" VK_IMAGE_VIEW_TYPE_2D
      &* set        @"format" imageFormat
      &* set        @"components" components
      &* set        @"subresourceRange" subresourceRange

destroyImageView
  :: VkDevice
  -> VkImageView
  -> IO ()
destroyImageView device view =
  vkDestroyImageView device view VK_NULL
    <* logMsg "Destroyed image view"

loadShader
  :: VkDevice
  -> FilePath
  -> Managed VkShaderModule
loadShader device srcFile =
  managed $
    bracket
      ( createShaderModule device srcFile )
      ( destroyShaderModule device)

createShaderModule
  :: VkDevice
  -> FilePath
  -> IO VkShaderModule
createShaderModule device srcFile = do
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

createGraphicsPipeline
  :: VkDevice
  -> VkExtent2D
  -> Managed ()
createGraphicsPipeline device chainExtent = do
  vertexShader   <- loadShader device "shaders/vert.spv"
  fragmentShader <- loadShader device "shaders/frag.spv"
  return ()
  -- vertexShaderStageInfo vertexShader
  -- fragmentShaderStageInfo fragmentShader
  -- viewport 0 0 (getField @"width" extent) (getField @"height" extent) 0 1

  where
    vertexShaderStageInfo shaderModule =
      createVk @VkPipelineShaderStageCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"stage" VK_SHADER_STAGE_VERTEX_BIT
        &* set        @"module" shaderModule
        &* setStrRef  @"pName" "main"
        &* set        @"pSpecializationInfo" VK_NULL

    fragmentShaderStageInfo shaderModule =
      createVk @VkPipelineShaderStageCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"stage" VK_SHADER_STAGE_FRAGMENT_BIT
        &* set        @"module" shaderModule
        &* setStrRef  @"pName" "main"
        &* set        @"pSpecializationInfo" VK_NULL

    vertexInputInfo =
      createVk @VkPipelineVertexInputStateCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"vertexBindingDescriptionCount" 0
        &* set        @"pVertexBindingDescriptions" VK_NULL
        &* set        @"vertexAttributeDescriptionCount" 0
        &* set        @"pVertexAttributeDescriptions" VK_NULL

    inputAssembly =
      createVk @VkPipelineInputAssemblyStateCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"topology" VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        &* set        @"primitiveRestartEnable" VK_FALSE

    viewport x y width height minDepth maxDepth =
      createVk @VkViewport
        $  set        @"x"  x
        &* set        @"y"  y
        &* set        @"width" width
        &* set        @"height" height
        &* set        @"minDepth" minDepth
        &* set        @"maxDepth" maxDepth

    offset x y =
      createVk @VkOffset2D
        $  set @"x" x
        &* set @"y" y

    extent width height =
      createVk @VkExtent2D
        $  set @"width" width
        &* set @"height" height

    scissor offset_ extent_ =
      createVk @VkRect2D
        $  set        @"offset" offset_
        &* set        @"extent" extent_

    viewportState viewports scissors =
      createVk @VkPipelineViewportStateCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"viewportCount" (fromIntegral $ length viewports)
        &* setListRef @"pViewports" viewports
        &* set        @"scissorCount" (fromIntegral $ length scissors)
        &* setListRef @"pScissors" scissors

    rasterizer =
      createVk @VkPipelineRasterizationStateCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"depthClampEnable" VK_FALSE
        &* set        @"rasterizerDiscardEnable" VK_FALSE
        &* set        @"polygonMode" VK_POLYGON_MODE_FILL
        &* set        @"lineWidth" 1
        &* set        @"cullMode" VK_CULL_MODE_BACK_BIT
        &* set        @"frontFace" VK_FRONT_FACE_CLOCKWISE
        &* set        @"depthBiasEnable" VK_FALSE
        &* set        @"depthBiasConstantFactor" 0
        &* set        @"depthBiasClamp" 0
        &* set        @"depthBiasSlopeFactor" 0

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
