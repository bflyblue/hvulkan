{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Lib
    ( test
    ) where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
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
test = runResourceT $ do
  glfw
  glfwReqExts     <- requiredGLFWExtensions
  vkInstance      <- vulkanInstance
                       "vulkan-test"
                       "vulkan-test"
                       glfwReqExts
                       layers
  window          <- windowTitled winWidth winHeight "vulkan-test"
  wasResized      <- liftIO $ newMVar False
  liftIO $ GLFW.setWindowSizeCallback window (Just $ windowResized wasResized)
  surface         <- windowSurface vkInstance window
  (pDevice, queues)
                  <- pickPhysicalDevice vkInstance surface extensions
  graphicsFamily  <- findGraphicsQueueFamilyIndex queues
  presentFamily   <- findPresentQueueFamilyIndex
                       vkInstance
                       pDevice
                       surface
                       queues
  device          <- logicalDevice
                       pDevice
                       extensions
                       layers
                       graphicsFamily
  graphicsQueue   <- liftIO $ getDeviceQueue device graphicsFamily 0
  presentQueue    <- liftIO $ getDeviceQueue device presentFamily 0

  liftIO $
    loop pDevice device window surface presentFamily graphicsFamily presentQueue graphicsQueue wasResized

  where
    extensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]
    layers = ["VK_LAYER_LUNARG_standard_validation"]
    winWidth = 640
    winHeight = 360

    windowResized :: MVar Bool -> GLFW.WindowSizeCallback
    windowResized flag _window _width _height = do
      _ <- takeMVar flag
      putMVar flag True

    loop pDevice device window surface presentFamily graphicsFamily presentQueue graphicsQueue wasResized = do
      outOfDate <- runResourceT $
        go pDevice device window surface presentFamily graphicsFamily presentQueue graphicsQueue wasResized
      when outOfDate $
        loop pDevice device window surface presentFamily graphicsFamily presentQueue graphicsQueue wasResized

    go pDevice device window surface presentFamily graphicsFamily presentQueue graphicsQueue wasResized = do
      liftIO $ do
        _ <- takeMVar wasResized
        putMVar wasResized False
      (surfaceCaps, surfaceFormats, presentModes)
                      <- querySwapChainSupport pDevice surface
      (width, height) <- liftIO $ GLFW.getFramebufferSize window
      logMsg $ "Framebuffer size is " ++ show width ++ "x" ++ show height
      (swapChain, chainFormat, swapExtent)
                      <- pickSwapchain
                           device
                           graphicsFamily
                           presentFamily
                           surface
                           surfaceCaps
                           surfaceFormats
                           presentModes
                           (fromIntegral width)
                           (fromIntegral height)
      views           <- imageViews
                           device
                           swapChain
                           chainFormat
      (pipeline, rpass)
                      <- createGraphicsPipeline
                           device
                           chainFormat
                           swapExtent
      framebuffers    <- createFramebuffers
                           device
                           rpass
                           swapExtent
                           views
      graphicsCommandPool
                      <- commandPool
                           device
                           graphicsFamily
      cmdBuffers      <- createCommandBuffers
                           device
                           graphicsCommandPool
                           framebuffers
                           rpass
                           swapExtent
                           pipeline

      syncs           <- replicateM 3 $ do
        imageAvailableSemaphore <- semaphore device
        renderFinishedSemaphore <- semaphore device
        inFlightFence           <- fence device VK_FENCE_CREATE_SIGNALED_BIT
        return (imageAvailableSemaphore, renderFinishedSemaphore, inFlightFence)

      mainloop
        device
        swapChain
        graphicsQueue
        presentQueue
        cmdBuffers
        syncs
        window
        wasResized

mainloop
  :: VkDevice
  -> VkSwapchainKHR
  -> VkQueue
  -> VkQueue
  -> [VkCommandBuffer]
  -> [(VkSemaphore, VkSemaphore, VkFence)]
  -> GLFW.Window
  -> MVar Bool
  -> ResIO Bool
mainloop
  device
  swapChain
  cmdBuffers
  graphicsQueue
  presentQueue
  syncs
  window
  wasResized =
    go (cycle syncs)
  where
    go [] = logMsg "No syncs!" >> return False
    go (sync : rest) = do
      liftIO GLFW.pollEvents
      shouldClose <- liftIO $ GLFW.windowShouldClose window
      if shouldClose
      then
        return False
      else do
        continue <-
          drawFrame
            device
            swapChain
            cmdBuffers
            graphicsQueue
            presentQueue
            sync
            wasResized
        if continue
        then go rest
        else return True

deviceWaitIdle :: MonadIO m => VkDevice -> m ()
deviceWaitIdle device = liftIO $
  vkDeviceWaitIdle device
   >>= throwVkResult "vkDeviceWaitIdel: Failed to wait for idle."

drawFrame
  :: VkDevice
  -> VkSwapchainKHR
  -> VkQueue
  -> VkQueue
  -> [VkCommandBuffer]
  -> (VkSemaphore, VkSemaphore, VkFence)
  -> MVar Bool
  -> ResIO Bool
drawFrame
  device
  swapChain
  graphicsQueue
  presentQueue
  cmdBuffers
  (imageAvailableSemaphore, renderFinishedSemaphore, inFlightFence)
  wasResized = do
    waitForFences device [inFlightFence] maxBound
    resized <- liftIO $ readMVar wasResized
    next <-
      if resized
      then do
        logMsg "GLFW reported resize"
        return Nothing
      else
        acquireNextImage device swapChain maxBound imageAvailableSemaphore VK_NULL
    case next of
      Just imageIndex -> do
        resetFences device [inFlightFence]
        submitQueue
          graphicsQueue
          [(imageAvailableSemaphore, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)]
          [renderFinishedSemaphore]
          inFlightFence
          [cmdBuffers !! fromIntegral imageIndex]
        present presentQueue [swapChain] [renderFinishedSemaphore] [imageIndex]
      Nothing -> do
        deviceWaitIdle device
        return False

waitForFences :: MonadIO m => VkDevice -> [VkFence] -> Word64 -> m ()
waitForFences device fences timeout = liftIO $
  withArrayLen fences $ \count pFences ->
    vkWaitForFences device (fromIntegral count) pFences VK_TRUE timeout
      >>= throwVkResult "vkWaitForFences: Failed to wait for fences."

resetFences :: MonadIO m => VkDevice -> [VkFence] -> m ()
resetFences device fences = liftIO $
  withArrayLen fences $ \count pFences ->
    vkResetFences device (fromIntegral count) pFences
      >>= throwVkResult "vkResetFences: Failed to reset fences."

present
 :: VkQueue
 -> [VkSwapchainKHR]
 -> [VkSemaphore]
 -> [Word32]
 -> ResIO Bool
present queue swapChains wait imageIndices = liftIO $
  withPtr presentInfo $ \ptr -> do
    result <- vkQueuePresentKHR queue ptr
    case result of
      VK_SUCCESS -> return True
      VK_SUBOPTIMAL_KHR -> logMsg "vkQueuePresentKHR: Suboptimal." >> return False
      VK_ERROR_OUT_OF_DATE_KHR -> logMsg "vkQueuePresentKHR: Out of date." >> return False
      err        -> throwVkResult "vkQueuePresentKHR: Failed to submit present request." err >> return False
  where
    presentInfo = createVk @VkPresentInfoKHR
      $  set           @"sType" VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
      &* set           @"pNext" VK_NULL
      &* set           @"waitSemaphoreCount" (fromIntegral $ length wait)
      &* setListRef    @"pWaitSemaphores" wait
      &* set           @"swapchainCount" (fromIntegral $ length swapChains)
      &* setListRef    @"pSwapchains" swapChains
      &* setListRef    @"pImageIndices" imageIndices
      &* set           @"pResults" VK_NULL

submitQueue
 :: VkQueue
 -> [(VkSemaphore, VkPipelineStageFlags)]
 -> [VkSemaphore]
 -> VkFence
 -> [VkCommandBuffer]
 -> ResIO ()
submitQueue queue waits signal fence_ buffers = liftIO $
  withArrayLen [submitInfo] $ \count siPtr ->
    vkQueueSubmit queue (fromIntegral count) siPtr fence_
      >>= throwVkResult "vkQueueSubmit: Failed to submit queue."
  where
    (wait, waitStages) = unzip waits
    submitInfo = createVk @VkSubmitInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"waitSemaphoreCount" (fromIntegral $ length waits)
      &* setListRef    @"pWaitSemaphores" wait
      &* setListRef    @"pWaitDstStageMask" waitStages
      &* set           @"signalSemaphoreCount" (fromIntegral $ length signal)
      &* setListRef    @"pSignalSemaphores" signal
      &* set           @"commandBufferCount" (fromIntegral $ length buffers)
      &* setListRef    @"pCommandBuffers" buffers

acquireNextImage
  :: VkDevice
  -> VkSwapchainKHR
  -> Word64
  -> VkSemaphore
  -> VkFence
  -> ResIO (Maybe Word32)
acquireNextImage device swapChain timeout semaphore_ fence_ = liftIO $
  alloca $ \ptr -> do
    result <- vkAcquireNextImageKHR
      device
      swapChain
      timeout
      semaphore_
      fence_
      ptr
    case result of
      VK_SUCCESS -> Just <$> peek ptr
      VK_SUBOPTIMAL_KHR -> logMsg "vkAcquireNextImageKHR: Out of date." >> return Nothing
      VK_ERROR_OUT_OF_DATE_KHR -> logMsg "vkAcquireNextImageKHR: Out of date." >> return Nothing
      err -> throwVkResult "vkAcquireNextImageKHR: Failed to acquire image." err >> return Nothing

glfw :: ResIO ()
glfw =
  managed
    initializeGLFW
    (const terminateGLFW)

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

managed :: MonadResource m => IO a -> (a -> IO ()) -> m a
managed alloc free_ = do
  (_releaseKey, a) <- allocate alloc free_
  return a

vulkanInstance
  :: String
  -> String
  -> [CString]
  -> [String]
  -> ResIO VkInstance
vulkanInstance progName engineName extensions layers =
  managed
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

allocaArrayPeek
  :: Storable a
  => Int
  -> (Ptr a -> IO ()) -> IO [a]
allocaArrayPeek n action =
  allocaArray n $ \ptr -> do
    action ptr
    peekArray n ptr

createVulkanInstance
  :: String
  -> String
  -> [CString]
  -> [String]
  -> IO VkInstance
createVulkanInstance progName engineName extensions layers =
  withPtr createInfo $ \iciPtr ->
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

    createInfo = createVk @VkInstanceCreateInfo
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

pickPhysicalDevice
  :: MonadIO m
  => VkInstance
  -> VkSurfaceKHR
  -> [CString]
  -> m (VkPhysicalDevice, [VkQueueFamilyProperties])
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
    (surfaceCaps, surfaceFormats, presentModes) <- querySwapChainSupport device surface
    logSurfaceCapabilities surfaceCaps
    logSurfaceFormats surfaceFormats
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
    Just (devIndex, dev, _, _, queues, _, _, _, _) -> do
      logMsg ("Picked device #" ++ show devIndex)
      return (dev, queues)
    Nothing ->
      throwVkMsg "No suitable GPU devices!"

querySwapChainSupport
  :: MonadIO m
  => VkPhysicalDevice
  -> VkSurfaceKHR
  -> m (VkSurfaceCapabilitiesKHR, [VkSurfaceFormatKHR], [VkPresentModeKHR])
querySwapChainSupport device surface = liftIO $ do
  surfaceCaps <- getPhysicalDeviceSurfaceCapabilities device surface
  surfaceFormats <- getPhysicalDeviceSurfaceFormats device surface
  presentModes <- getPhysicalDeviceSurfacePresentModes device surface
  return (surfaceCaps, surfaceFormats, presentModes)

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

getDeviceQueue :: VkDevice -> Word32 -> Word32 -> IO VkQueue
getDeviceQueue device familyIndex index = allocaPeek $ vkGetDeviceQueue device familyIndex index

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
  -> ResIO VkDevice
logicalDevice physicalDevice extensions layers queueFamilyIndex =
  managed
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

destroyDevice :: VkDevice -> IO ()
destroyDevice device =
  vkDestroyDevice device VK_NULL
    <* logMsg "Destroyed logical device"

windowSurface
  :: VkInstance
  -> GLFW.Window
  -> ResIO VkSurfaceKHR
windowSurface vkInstance window =
  managed
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

windowTitled :: Word32 -> Word32 -> String -> ResIO GLFW.Window
windowTitled width height title =
  managed
    (createWindow width height title)
    destroyWindow

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
  -> ResIO (VkSwapchainKHR, VkFormat, VkExtent2D)
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
  return (chain, surfaceFormat, swapExtent)

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
      extent2D
        (clamp wMin wMax width)
        (clamp hMin hMax height)
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
  -> ResIO VkSwapchainKHR
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
  managed
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

imageViews :: VkDevice -> VkSwapchainKHR -> VkFormat -> ResIO [VkImageView]
imageViews device chain chainFormat = do
  swapImages  <- getSwapchainImages
                   device
                   chain
  mapM (imageView device chainFormat) swapImages

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
  -> ResIO VkImageView
imageView device imageFormat image =
  managed
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
  -> ResIO VkShaderModule
loadShader device srcFile =
  managed
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
  -> VkFormat
  -> VkExtent2D
  -> ResIO (VkPipeline, VkRenderPass)
createGraphicsPipeline device format swapExtent = do
  vertexShader   <- loadShader device "shaders/vert.spv"
  fragmentShader <- loadShader device "shaders/frag.spv"
  rpass          <- renderPass device format
  layout         <- pipelineLayout device
  let vertexStage = vertexShaderStageInfo vertexShader
      fragmentStage = fragmentShaderStageInfo fragmentShader
      vp = viewport 0 0 (fromIntegral $ getField @"width" swapExtent) (fromIntegral $ getField @"height" swapExtent) 0 1
      ss = rect2D (offset2D 0 0) swapExtent
  pipeline <- graphicsPipeline device (pipelineInfo [vertexStage, fragmentStage] (viewportState [vp] [ss]) layout rpass)
  return (pipeline, rpass)

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

    multisampling =
      createVk @VkPipelineMultisampleStateCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"sampleShadingEnable" VK_FALSE
        &* set        @"rasterizationSamples" VK_SAMPLE_COUNT_1_BIT
        &* set        @"minSampleShading" 1.0
        &* set        @"pSampleMask" VK_NULL
        &* set        @"alphaToCoverageEnable" VK_FALSE
        &* set        @"alphaToOneEnable" VK_FALSE

    colorBlendAttachment =
      createVk @VkPipelineColorBlendAttachmentState
        $  set        @"colorWriteMask" (   VK_COLOR_COMPONENT_R_BIT
                                        .|. VK_COLOR_COMPONENT_G_BIT
                                        .|. VK_COLOR_COMPONENT_B_BIT
                                        .|. VK_COLOR_COMPONENT_A_BIT
                                        )
        &* set        @"blendEnable" VK_FALSE
        &* set        @"srcColorBlendFactor" VK_BLEND_FACTOR_ONE
        &* set        @"dstColorBlendFactor" VK_BLEND_FACTOR_ZERO
        &* set        @"colorBlendOp" VK_BLEND_OP_ADD
        &* set        @"srcAlphaBlendFactor" VK_BLEND_FACTOR_ONE
        &* set        @"dstAlphaBlendFactor" VK_BLEND_FACTOR_ZERO
        &* set        @"alphaBlendOp" VK_BLEND_OP_ADD

    colorBlending =
      createVk @VkPipelineColorBlendStateCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"logicOpEnable" VK_FALSE
        &* set        @"logicOp" VK_LOGIC_OP_COPY
        &* set        @"attachmentCount" 1
        &* setListRef @"pAttachments" [colorBlendAttachment]
        &* setAt      @"blendConstants" @0 0
        &* setAt      @"blendConstants" @1 0
        &* setAt      @"blendConstants" @2 0
        &* setAt      @"blendConstants" @3 0

    pipelineInfo stages viewportState_ layout rpass =
      createVk @VkGraphicsPipelineCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"stageCount" (fromIntegral $ length stages)
        &* setListRef @"pStages" stages
        &* setVkRef   @"pVertexInputState" vertexInputInfo
        &* setVkRef   @"pInputAssemblyState" inputAssembly
        &* setVkRef   @"pViewportState" viewportState_
        &* setVkRef   @"pRasterizationState" rasterizer
        &* setVkRef   @"pMultisampleState" multisampling
        &* set        @"pDepthStencilState" VK_NULL
        &* setVkRef   @"pColorBlendState" colorBlending
        &* set        @"pDynamicState" VK_NULL
        &* set        @"layout" layout
        &* set        @"renderPass" rpass
        &* set        @"subpass" 0
        &* set        @"basePipelineHandle" VK_NULL_HANDLE
        &* set        @"basePipelineIndex" -1

rect2D :: VkOffset2D -> VkExtent2D -> VkRect2D
rect2D offset extent =
  createVk @VkRect2D
    $  set        @"offset" offset
    &* set        @"extent" extent

offset2D :: Int32 -> Int32 -> VkOffset2D
offset2D x y =
  createVk @VkOffset2D
    $  set @"x" x
    &* set @"y" y

extent2D :: Word32 -> Word32 -> VkExtent2D
extent2D width height =
  createVk @VkExtent2D
    $  set @"width" width
    &* set @"height" height

graphicsPipeline :: VkDevice -> VkGraphicsPipelineCreateInfo -> ResIO VkPipeline
graphicsPipeline device pipelineInfo =
  managed
    ( createGraphicsPipelines device [pipelineInfo] )
    ( destroyPipeline device )

createGraphicsPipelines :: VkDevice -> [VkGraphicsPipelineCreateInfo] -> IO VkPipeline
createGraphicsPipelines device pipelines =
  withArrayLen pipelines $ \count ciPtr ->
    allocaPeek
    ( vkCreateGraphicsPipelines device VK_NULL (fromIntegral count) ciPtr VK_NULL
        >=> throwVkResult "vkCreateGraphicsPipelines: Failed to create graphics pipelines."
    )
    <* logMsg "Created graphics pipeline"

destroyPipeline :: VkDevice -> VkPipeline -> IO ()
destroyPipeline device pipeline =
  vkDestroyPipeline device pipeline VK_NULL
    <* logMsg "Destroyed graphics pipeline"

pipelineLayout :: VkDevice -> ResIO VkPipelineLayout
pipelineLayout device =
  managed
    ( createPipelineLayout device )
    ( destroyPipelineLayout device )

createPipelineLayout :: VkDevice -> IO VkPipelineLayout
createPipelineLayout device =
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreatePipelineLayout device ciPtr VK_NULL
        >=> throwVkResult "vkCreatePipelineLayout: Failed to create pipeline layout."
    )
    <* logMsg "Created pipeline layout"
  where
    createInfo =
      createVk @VkPipelineLayoutCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"setLayoutCount" 0
        &* set        @"pSetLayouts" VK_NULL
        &* set        @"pushConstantRangeCount" 0
        &* set        @"pPushConstantRanges" VK_NULL

destroyPipelineLayout :: VkDevice -> VkPipelineLayout -> IO ()
destroyPipelineLayout device layout =
  vkDestroyPipelineLayout device layout VK_NULL
    <* logMsg "Destroyed pipeline layout"

renderPass :: VkDevice -> VkFormat -> ResIO VkRenderPass
renderPass device format =
  managed
    ( createRenderPass device format )
    ( destroyRenderPass device )

createRenderPass :: VkDevice -> VkFormat -> IO VkRenderPass
createRenderPass device format =
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateRenderPass device ciPtr VK_NULL
        >=> throwVkResult "vkCreateRenderPass: Failed to create render pass."
    )
    <* logMsg "Created render pass"
  where
    colorAttachment =
      createVk @VkAttachmentDescription
        $  set        @"format" format
        &* set        @"samples" VK_SAMPLE_COUNT_1_BIT
        &* set        @"loadOp" VK_ATTACHMENT_LOAD_OP_CLEAR
        &* set        @"storeOp" VK_ATTACHMENT_STORE_OP_STORE
        &* set        @"stencilLoadOp" VK_ATTACHMENT_LOAD_OP_DONT_CARE
        &* set        @"stencilStoreOp" VK_ATTACHMENT_STORE_OP_DONT_CARE
        &* set        @"initialLayout" VK_IMAGE_LAYOUT_UNDEFINED
        &* set        @"finalLayout" VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

    colorAttachmentRef =
      createVk @VkAttachmentReference
        $  set        @"attachment" 0
        &* set        @"layout" VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

    subpass =
      createVk @VkSubpassDescription
        $  set        @"flags" 0
        &* set        @"pipelineBindPoint" VK_PIPELINE_BIND_POINT_GRAPHICS
        &* set        @"inputAttachmentCount" 0
        &* set        @"pInputAttachments" VK_NULL
        &* set        @"colorAttachmentCount" 1
        &* setListRef @"pColorAttachments" [colorAttachmentRef]
        &* set        @"pResolveAttachments" VK_NULL
        &* set        @"pDepthStencilAttachment" VK_NULL
        &* set        @"preserveAttachmentCount" 0
        &* set        @"pPreserveAttachments" VK_NULL

    createInfo =
      createVk @VkRenderPassCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"attachmentCount" 1
        &* setListRef @"pAttachments" [colorAttachment]
        &* set        @"subpassCount" 1
        &* setListRef @"pSubpasses" [subpass]
        &* set        @"dependencyCount" 1
        &* setListRef @"pDependencies" [dependency]

    dependency =
      createVk @VkSubpassDependency
        $  set        @"srcSubpass" VK_SUBPASS_EXTERNAL
        &* set        @"srcStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* set        @"srcAccessMask" 0
        &* set        @"dstSubpass" 0
        &* set        @"dstStageMask" VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        &* set        @"dstAccessMask" (VK_ACCESS_COLOR_ATTACHMENT_READ_BIT .|. VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)

destroyRenderPass :: VkDevice -> VkRenderPass -> IO ()
destroyRenderPass device rpass =
  vkDestroyRenderPass device rpass VK_NULL
    <* logMsg "Destroyed render pass"

framebuffer
  :: VkDevice
  -> VkRenderPass
  -> VkExtent2D
  -> [VkImageView]
  -> ResIO VkFramebuffer
framebuffer device rpass extent_ attachments =
  managed
    ( createFramebuffer
        device
        rpass
        extent_
        attachments
    )
    ( destroyFramebuffer device )

createFramebuffer
  :: VkDevice
  -> VkRenderPass
  -> VkExtent2D
  -> [VkImageView]
  -> IO VkFramebuffer
createFramebuffer device rpass extent_ attachments =
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateFramebuffer device ciPtr VK_NULL
        >=> throwVkResult "vkCreateFramebuffer: Failed to create framebuffer."
    )
    <* logMsg "Created framebuffer"
  where
    createInfo = createVk @VkFramebufferCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" 0
      &* set           @"renderPass" rpass
      &* set           @"attachmentCount" (fromIntegral $ length attachments)
      &* setListRef    @"pAttachments" attachments
      &* set           @"width" (getField @"width" extent_)
      &* set           @"height" (getField @"height" extent_)
      &* set           @"layers" 1

destroyFramebuffer :: VkDevice -> VkFramebuffer -> IO ()
destroyFramebuffer device framebuffer_ =
  vkDestroyFramebuffer device framebuffer_ VK_NULL
    <* logMsg "Destroyed framebuffer"

createFramebuffers
  :: VkDevice
  -> VkRenderPass
  -> VkExtent2D
  -> [VkImageView]
  -> ResIO [VkFramebuffer]
createFramebuffers device rpass extent_ =
  mapM (\attachment -> framebuffer device rpass extent_ [attachment])

commandPool :: VkDevice -> Word32 -> ResIO VkCommandPool
commandPool device familyIndex =
  managed
    ( createCommandPool device familyIndex )
    ( destroyCommandPool device )

createCommandPool :: VkDevice -> Word32 -> IO VkCommandPool
createCommandPool device familyIndex =
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateCommandPool device ciPtr VK_NULL
        >=> throwVkResult "vkCreateCommandPool: Failed to create command pool."
    )
    <* logMsg "Created command pool"
  where
    createInfo = createVk @VkCommandPoolCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" 0
      &* set           @"queueFamilyIndex" familyIndex

destroyCommandPool :: VkDevice -> VkCommandPool -> IO ()
destroyCommandPool device pool =
  vkDestroyCommandPool device pool VK_NULL
    <* logMsg "Destroyed command pool"

createCommandBuffers
  :: VkDevice
  -> VkCommandPool
  -> [VkFramebuffer]
  -> VkRenderPass
  -> VkExtent2D
  -> VkPipeline
  -> ResIO [VkCommandBuffer]
createCommandBuffers device pool frameBuffers rpass extent_ pipeline = do
  cmdBuffers <- commandBuffers device pool (length frameBuffers)

  liftIO $
    forM_ (zip frameBuffers cmdBuffers) $ \(frameBuffer_, cmd) -> do
      beginCommandBuffer cmd VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
      beginRenderPass cmd frameBuffer_
      vkCmdBindPipeline cmd VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
      vkCmdDraw cmd 3 1 0 0
      vkCmdEndRenderPass cmd
      endCommandBuffer cmd

  return cmdBuffers

  where
    beginCommandBuffer buffer flags =
      let
        beginInfo = createVk @VkCommandBufferBeginInfo
          $  set           @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
          &* set           @"pNext" VK_NULL
          &* set           @"flags" flags
          &* set           @"pInheritanceInfo" VK_NULL
      in
        withPtr beginInfo
          ( vkBeginCommandBuffer buffer
              >=> throwVkResult "vkBeginCommandBuffer: Failed to begin command buffer."
          )

    endCommandBuffer buffer =
      vkEndCommandBuffer buffer
        >>= throwVkResult "vkEndCommandBuffer: Failed to record command buffer."

    beginRenderPass buffer framebuffer_ =
      let
        area = rect2D (offset2D 0 0) extent_
        clearValue = createVk @VkClearValue
          $ set            @"color" clearColor

        clearColor = createVk @VkClearColorValue
          $  setAt         @"float32" @0 0
          &* setAt         @"float32" @1 0
          &* setAt         @"float32" @2 1
          &* setAt         @"float32" @3 1

        renderPassInfo = createVk @VkRenderPassBeginInfo
          $  set           @"sType" VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
          &* set           @"pNext" VK_NULL
          &* set           @"renderPass" rpass
          &* set           @"framebuffer" framebuffer_
          &* set           @"renderArea" area
          &* set           @"clearValueCount" 1
          &* setListRef    @"pClearValues" [clearValue]
      in
        withPtr renderPassInfo $ \rpiPtr ->
          vkCmdBeginRenderPass buffer rpiPtr VK_SUBPASS_CONTENTS_INLINE

commandBuffers
  :: VkDevice
  -> VkCommandPool
  -> Int
  -> ResIO [VkCommandBuffer]
commandBuffers device pool bufferCount =
  managed
    ( allocateCommandBuffers device pool bufferCount )
    ( freeCommandBuffers device pool )

allocateCommandBuffers
  :: MonadIO m
  => VkDevice
  -> VkCommandPool
  -> Int
  -> m [VkCommandBuffer]
allocateCommandBuffers device pool bufferCount = liftIO $
  withPtr allocInfo $ \aiPtr ->
    allocaArrayPeek bufferCount
    ( vkAllocateCommandBuffers device aiPtr
        >=> throwVkResult "vkAllocateCommandBuffers: Failed to allocate command buffers."
    )
    <* logMsg "Allocated command buffers"
  where
    allocInfo = createVk @VkCommandBufferAllocateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"commandPool" pool
      &* set           @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
      &* set           @"commandBufferCount" (fromIntegral bufferCount)

freeCommandBuffers
  :: MonadIO m
  => VkDevice
  -> VkCommandPool
  -> [VkCommandBuffer]
  -> m ()
freeCommandBuffers device pool buffers = liftIO $
  withArrayLen buffers $ \count pBuffers ->
    vkFreeCommandBuffers device pool (fromIntegral count) pBuffers
      <* logMsg "Freed command buffers"

semaphore :: VkDevice -> ResIO VkSemaphore
semaphore device =
  managed
    ( createSemaphore device )
    ( destroySemaphore device )

createSemaphore :: VkDevice -> IO VkSemaphore
createSemaphore device =
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateSemaphore device ciPtr VK_NULL
        >=> throwVkResult "vkCreateSemaphore: Failed to create semaphore."
    )
    <* logMsg "Created semaphore"
  where
    createInfo = createVk @VkSemaphoreCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" 0

destroySemaphore :: VkDevice -> VkSemaphore -> IO ()
destroySemaphore device sem =
  vkDestroySemaphore device sem VK_NULL
    <* logMsg "Destroyed semaphore"

fence :: VkDevice -> VkFenceCreateFlags -> ResIO VkFence
fence device flags =
  managed
    ( createFence device flags )
    ( waitDestroyFence device )

createFence :: VkDevice -> VkFenceCreateFlags -> IO VkFence
createFence device flags =
  withPtr createInfo $ \ciPtr ->
    allocaPeek
    ( vkCreateFence device ciPtr VK_NULL
        >=> throwVkResult "vkCreateSemaphore: Failed to create fence."
    )
    <* logMsg "Created fence"
  where
    createInfo = createVk @VkFenceCreateInfo
      $  set           @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
      &* set           @"pNext" VK_NULL
      &* set           @"flags" flags

waitDestroyFence :: VkDevice -> VkFence -> IO ()
waitDestroyFence device fence_ = do
  waitForFences device [fence_] maxBound
  destroyFence device fence_

destroyFence :: VkDevice -> VkFence -> IO ()
destroyFence device fence_ =
  vkDestroyFence device fence_ VK_NULL
    <* logMsg "Destroyed fence"

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
