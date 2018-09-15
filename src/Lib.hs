{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Lib
    ( test
    ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Foreign.C.String
import           Foreign.Extra
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Storable
import qualified Graphics.UI.GLFW                       as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Log
import           Safe

import           Vulkan.CommandBuffer
import           Vulkan.CommandPool
import           Vulkan.Device
import           Vulkan.Exception
import           Vulkan.Fence
import           Vulkan.Framebuffer
import           Vulkan.ImageView
import           Vulkan.Instance
import           Vulkan.PhysicalDevice
import           Vulkan.Pipeline
import           Vulkan.RenderPass
import           Vulkan.Semaphore
import           Vulkan.ShaderModule
import           Vulkan.Surface
import           Vulkan.Swapchain
import           Vulkan.WSI

data Config = Config
  { layers        :: [String]
  , extensions    :: [CString]
  , winWidth      :: Word32
  , winHeight     :: Word32
  }

defaultConfig :: Config
defaultConfig = Config
  { extensions = [VK_KHR_SWAPCHAIN_EXTENSION_NAME]
  , layers = ["VK_LAYER_LUNARG_standard_validation"]
  , winWidth = 640
  , winHeight = 360
  }

data Context = Context
  { vkInstance                :: VkInstance
  , physicalDevice            :: VkPhysicalDevice
  , device                    :: VkDevice
  , surface                   :: VkSurfaceKHR
  , window                    :: GLFW.Window
  , resizedFlag               :: MVar Bool
  , graphicsQueueFamilyIndex  :: Word32
  , graphicsQueue             :: VkQueue
  , presentQueueFamilyIndex   :: Word32
  , presentQueue              :: VkQueue
  , framebufferWidth          :: Int
  , framebufferHeight         :: Int
  , swapChain                 :: VkSwapchainKHR
  , chainFormat               :: VkFormat
  , swapExtent                :: VkExtent2D
  , views                     :: [VkImageView]
  , pipeline                  :: VkPipeline
  , renderpass                :: VkRenderPass
  , syncs                     :: [(VkSemaphore, VkSemaphore, VkFence)]
  , framebuffers              :: [VkFramebuffer]
  , graphicsCommandPool       :: VkCommandPool
  , cmdBuffers                :: [VkCommandBuffer]
  }

data Status a
  = Done a
  | Resized
  | Suboptimal
  | OutOfDate
  deriving (Eq, Show, Functor)

withContext :: Config -> (Context -> IO (Status a)) -> IO a
withContext Config{..} action = runResourceT $ do
  glfw
  glfwReqExts               <- requiredGLFWExtensions
  vkInstance                <- vulkanInstance
                                 VK_API_VERSION_1_0
                                 "vulkan-test" (_VK_MAKE_VERSION 1 0 0)
                                 "vulkan-test" (_VK_MAKE_VERSION 1 0 0)
                                 glfwReqExts
                                 layers
  window                    <- windowTitled winWidth winHeight "vulkan-test"
  resizedFlag               <- monitorWindowResize window
  surface                   <- windowSurface vkInstance window
  physicalDevice            <- pickPhysicalDevice vkInstance surface extensions
  queues                    <- physicalDeviceQueueFamiliesProperties physicalDevice
  graphicsQueueFamilyIndex  <- findGraphicsQueueFamilyIndex queues
  presentQueueFamilyIndex   <- findPresentQueueFamilyIndex
                                 vkInstance
                                 physicalDevice
                                 surface
                                 queues
  device                    <- logicalDevice
                                 physicalDevice
                                 extensions
                                 layers
                                 graphicsQueueFamilyIndex
  graphicsQueue             <- getDeviceQueue device graphicsQueueFamilyIndex 0
  presentQueue              <- getDeviceQueue device presentQueueFamilyIndex 0

  let
    swapchainLoop = do
      status <- runResourceT $ do
        resetFlag resizedFlag
        (framebufferWidth, framebufferHeight)
                            <- liftIO $ GLFW.getFramebufferSize window
        logMsg $ "Framebuffer size is " ++ show framebufferWidth ++ "x" ++ show framebufferHeight
        (swapChain, chainFormat, swapExtent)
                            <- pickSwapchain
                                 physicalDevice
                                 device
                                 graphicsQueueFamilyIndex
                                 presentQueueFamilyIndex
                                 surface
                                 (fromIntegral framebufferWidth)
                                 (fromIntegral framebufferHeight)
        views               <- imageViews
                                 device
                                 swapChain
                                 chainFormat
        (pipeline, renderpass)
                            <- createGraphicsPipeline
                                 device
                                 chainFormat
                                 swapExtent
        framebuffers        <- createFramebuffers
                                 device
                                 renderpass
                                 swapExtent
                                 views
        graphicsCommandPool <- commandPool
                                 device
                                 graphicsQueueFamilyIndex
        cmdBuffers          <- createCommandBuffers
                                 device
                                 graphicsCommandPool
                                 framebuffers
                                 renderpass
                                 swapExtent
                                 pipeline

        syncs               <- replicateM 3 $ do
          imageAvailableSemaphore <- semaphore device
          renderFinishedSemaphore <- semaphore device
          inFlightFence           <- fence device VK_FENCE_CREATE_SIGNALED_BIT
          return (imageAvailableSemaphore, renderFinishedSemaphore, inFlightFence)

        liftIO $ action Context{..}

      -- Swap chain resources will be freed after runResourceT here

      case status of
        Done a -> return a
        Resized -> swapchainLoop
        Suboptimal -> swapchainLoop
        OutOfDate -> swapchainLoop

  liftIO swapchainLoop

test :: IO ()
test = withContext defaultConfig mainloop

mainloop :: Context -> IO (Status ())
mainloop ctx@Context{..} = go (cycle syncs)
  where
    go [] = logMsg "No syncs!" >> return (Done ())
    go (sync : rest) = do
      liftIO GLFW.pollEvents
      shouldClose <- liftIO $ GLFW.windowShouldClose window
      if shouldClose
      then
        return $ Done ()
      else do
        status <- drawFrame ctx sync
        case status of
          Done () -> go rest
          a       -> return a

drawFrame
  :: MonadIO m
  => Context
  -> (VkSemaphore, VkSemaphore, VkFence)
  -> m (Status ())
drawFrame Context{..} (imageAvailableSemaphore, renderFinishedSemaphore, inFlightFence)= do
    waitForFences device [inFlightFence] maxBound
    resized <- liftIO $ readMVar resizedFlag
    next <-
      if resized
      then do
        logMsg "GLFW reported resize"
        return Resized
      else
        acquireNextImage device swapChain maxBound imageAvailableSemaphore VK_NULL
    case next of
      Done imageIndex -> do
        resetFences device [inFlightFence]
        submitQueue
          graphicsQueue
          [(imageAvailableSemaphore, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)]
          [renderFinishedSemaphore]
          inFlightFence
          [cmdBuffers !! fromIntegral imageIndex]
        present presentQueue [swapChain] [renderFinishedSemaphore] [imageIndex]
      status -> do
        deviceWaitIdle device
        return $ void status

present
 :: MonadIO m
 => VkQueue
 -> [VkSwapchainKHR]
 -> [VkSemaphore]
 -> [Word32]
 -> m (Status ())
present queue swapChains wait imageIndices = liftIO $
  withPtr presentInfo $ \ptr -> do
    result <- vkQueuePresentKHR queue ptr
    case result of
      VK_SUCCESS -> return $ Done ()
      VK_SUBOPTIMAL_KHR -> logMsg "vkQueuePresentKHR: Suboptimal." >> return Suboptimal
      VK_ERROR_OUT_OF_DATE_KHR -> logMsg "vkQueuePresentKHR: Out of date." >> return OutOfDate
      err        -> throwVkResult "vkQueuePresentKHR: Failed to submit present request." err >> return (Done ())
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
 :: MonadIO m
 => VkQueue
 -> [(VkSemaphore, VkPipelineStageFlags)]
 -> [VkSemaphore]
 -> VkFence
 -> [VkCommandBuffer]
 -> m ()
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
  :: MonadIO m
  => VkDevice
  -> VkSwapchainKHR
  -> Word64
  -> VkSemaphore
  -> VkFence
  -> m (Status Word32)
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
      VK_SUCCESS -> Done <$> peek ptr
      VK_SUBOPTIMAL_KHR -> logMsg "vkAcquireNextImageKHR: Out of date." >> return OutOfDate
      VK_ERROR_OUT_OF_DATE_KHR -> logMsg "vkAcquireNextImageKHR: Out of date." >> return OutOfDate
      err -> throwVkResult "vkAcquireNextImageKHR: Failed to acquire image." err >> return OutOfDate

pickPhysicalDevice
  :: MonadIO m
  => VkInstance
  -> VkSurfaceKHR
  -> [CString]
  -> m VkPhysicalDevice
pickPhysicalDevice vkInstance surface requiredExtensions = liftIO $ do
  logMsg "Enumerating Vulkan devices"
  allDevices <- enumeratePhysicalDevices vkInstance

  devices <- forM (zip [1::Int ..] allDevices) $ \(devIndex, device) -> do
    logMsg $ "---- Device #" ++ show devIndex ++ " ----"
    physicalDeviceProperties device >>= logDeviceProperties
    physicalDeviceFeatures device >>= logDeviceFeatures
    enumerateDeviceExtensionProperties device >>= logDeviceExtensionProperties
    getPhysicalDeviceSurfaceCapabilities device surface >>= logSurfaceCapabilities
    getPhysicalDeviceSurfaceFormats device surface >>= logSurfaceFormats
    getPhysicalDeviceSurfacePresentModes device surface >>= logPresentModes
    logMsg "-------------------"
    return (devIndex , device)

  logMsg $ "Number of devices found: " ++ show (length devices)

  gpus <- filterM (isDeviceSuitable . snd) devices

  logMsg $ "Number of GPU devices found: " ++ show (length gpus)

  case headMay gpus of
    Just (devIndex, dev) -> do
      logMsg ("Picked device #" ++ show devIndex)
      return dev
    Nothing ->
      throwVkMsg "No suitable GPU devices!"
  where
    isDeviceSuitable device =
      and <$> sequence
        [ isGpu device
        , supportsExtensions requiredExtensions device
        , supportsSwapchain 2 Nothing Nothing device surface
        ]

    logDeviceProperties props = do
      logMsg $ unwords [ "Device:", getStringField @"deviceName" props]
      logMsg $ unwords [ "Type:", show $ getField @"deviceType" props]
      logMsg $ unwords [ "Api:", toVersionString $ getField @"apiVersion" props]

    toVersionString v = concat
      [ show (_VK_VERSION_MAJOR v)
      , "."
      , show (_VK_VERSION_MINOR v)
      , "."
      , show (_VK_VERSION_PATCH v)
      ]

    logDeviceFeatures :: VkPhysicalDeviceFeatures -> IO ()
    logDeviceFeatures feats = do
      logMsg $ unwords [ "Geometry Shader:", isSupported $ geometryShaderFeature feats]
      logMsg $ unwords [ "Tessellation Shader:", isSupported $ tessellationShaderFeature feats]

    isSupported VK_TRUE = "Yes"
    isSupported _       = "No"

    logDeviceExtensionProperties extensions =
      logMsg $ unwords [ "Device Extensions:", unwords (map extensionName extensions)]

    extensionName = getStringField @"extensionName"

    logSurfaceCapabilities caps =
      logMsg $ unwords [ "Surface Capabilities:", unwords [minImageCount, maxImageCount] ]
      where
        minImageCount = "minImageCount=" ++ show (getField @"minImageCount" caps)
        maxImageCount = "maxImageCount=" ++ show (getField @"maxImageCount" caps)

    logSurfaceFormats formats =
      logMsg $ unwords [ "Surface Formats:", unwords (map showFormat formats)]
      where
        showFormat f =
          show (getField @"format" f)
          ++ "/"
          ++ show (getField @"colorSpace" f)

    logPresentModes presentModes =
      logMsg $ unwords [ "Surface Present Modes:", unwords (map show presentModes)]

isGraphicsQueueFamily :: VkQueueFamilyProperties -> Bool
isGraphicsQueueFamily queue =
  getField @"queueFlags" queue .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits

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

getDeviceQueue :: MonadIO m => VkDevice -> Word32 -> Word32 -> m VkQueue
getDeviceQueue device familyIndex index = liftIO $
  allocaPeek $ vkGetDeviceQueue device familyIndex index

canPresentSurface :: MonadIO m => VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> m Bool
canPresentSurface physicalDevice familyIndex surface = liftIO $
  (== VK_TRUE) <$> allocaPeek
  ( vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice familyIndex surface
      >=> throwVkResult "vkGetPhysicalDeviceSurfaceSupportKHR: Failed to get surface support."
  )

pickSwapchain
  :: VkPhysicalDevice
  -> VkDevice
  -> Word32
  -> Word32
  -> VkSurfaceKHR
  -> Word32
  -> Word32
  -> ResIO (VkSwapchainKHR, VkFormat, VkExtent2D)
pickSwapchain
    physicalDevice
    device
    graphicsFamily
    presentFamily
    surface
    width
    height = do
  surfaceCaps <- liftIO $ getPhysicalDeviceSurfaceCapabilities physicalDevice surface
  surfaceFormats <- liftIO $ getPhysicalDeviceSurfaceFormats physicalDevice surface
  presentModes <- liftIO $ getPhysicalDeviceSurfacePresentModes physicalDevice surface
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

pickSurfaceFormat :: MonadIO m => [VkSurfaceFormatKHR] -> m (VkFormat, VkColorSpaceKHR)
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

pickPresentMode :: MonadIO m => [VkPresentModeKHR] -> m VkPresentModeKHR
pickPresentMode presentModes = liftIO $ go preference
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
  :: MonadIO m
  => VkSurfaceCapabilitiesKHR
  -> Word32
  -> Word32
  -> m VkExtent2D
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

pickImageCount :: MonadIO m => VkSurfaceCapabilitiesKHR -> m Word32
pickImageCount surfaceCaps = do
  let cMin = getField @"minImageCount" surfaceCaps
      cMax = getField @"maxImageCount" surfaceCaps    -- 0 means no max limit
  if cMin >= 3 || cMax == cMin
  then
    return cMin
  else
    return (cMin + 1)

imageViews :: VkDevice -> VkSwapchainKHR -> VkFormat -> ResIO [VkImageView]
imageViews device chain chainFormat = do
  swapImages  <- getSwapchainImages
                   device
                   chain
  mapM (imageView device chainFormat) swapImages

createGraphicsPipeline
  :: VkDevice
  -> VkFormat
  -> VkExtent2D
  -> ResIO (VkPipeline, VkRenderPass)
createGraphicsPipeline device format swapExtent = do
  vertexShader   <- loadShader device "shaders/vert.spv"
  fragmentShader <- loadShader device "shaders/frag.spv"
  renderpass     <- renderPass device format
  layout         <- pipelineLayout device
  let vertexStage = vertexShaderStageInfo vertexShader
      fragmentStage = fragmentShaderStageInfo fragmentShader
      vp = viewport 0 0 (fromIntegral $ getField @"width" swapExtent) (fromIntegral $ getField @"height" swapExtent) 0 1
      ss = rect2D (offset2D 0 0) swapExtent
  pipeline <- graphicsPipeline device (pipelineInfo [vertexStage, fragmentStage] (viewportState [vp] [ss]) layout renderpass)
  return (pipeline, renderpass)

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

    pipelineInfo stages viewportState_ layout renderpass =
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
        &* set        @"renderPass" renderpass
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

createFramebuffers
  :: VkDevice
  -> VkRenderPass
  -> VkExtent2D
  -> [VkImageView]
  -> ResIO [VkFramebuffer]
createFramebuffers device renderpass extent_ =
  mapM (\attachment -> framebuffer device renderpass extent_ [attachment])

createCommandBuffers
  :: VkDevice
  -> VkCommandPool
  -> [VkFramebuffer]
  -> VkRenderPass
  -> VkExtent2D
  -> VkPipeline
  -> ResIO [VkCommandBuffer]
createCommandBuffers device pool frameBuffers renderpass extent_ pipeline = do
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
          &* set           @"renderPass" renderpass
          &* set           @"framebuffer" framebuffer_
          &* set           @"renderArea" area
          &* set           @"clearValueCount" 1
          &* setListRef    @"pClearValues" [clearValue]
      in
        withPtr renderPassInfo $ \rpiPtr ->
          vkCmdBeginRenderPass buffer rpiPtr VK_SUBPASS_CONTENTS_INLINE
