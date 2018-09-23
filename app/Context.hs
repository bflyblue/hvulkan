{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Context
  ( Context (..)
  , Config (..)
  , SyncSet (..)
  , defaultConfig
  , withContext
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import qualified Data.Vector.Storable                   as Vector
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal
import qualified Graphics.UI.GLFW                       as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Graphics.Vulkan.Ext.VK_KHR_swapchain
import           Graphics.Vulkan.Marshal.Create
import           Linear
import           Log
import           Record
import           Safe

import           Vulkan

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

data SyncSet = SyncSet
  { imageAvailableSemaphore   :: VkSemaphore
  , renderFinishedSemaphore   :: VkSemaphore
  , inFlightFence             :: VkFence
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
  , syncs                     :: [SyncSet]
  , framebuffers              :: [VkFramebuffer]
  , graphicsCommandPool       :: VkCommandPool
  , cmdBuffers                :: [VkCommandBuffer]
  , vertexBuffer              :: VkBuffer
  , indexBuffer               :: VkBuffer
  }

data Vertex = Vertex
  { vPosition                 :: !(V2 CFloat)
  , vColor                    :: !(V3 CFloat)
  }

makeRecord ''Vertex

withContext :: Config -> (Context -> IO Bool) -> IO ()
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
  graphicsCommandPool       <- commandPool device graphicsQueueFamilyIndex
  vertexBuffer              <- createVertexBuffer
                                 physicalDevice
                                 device
                                 graphicsCommandPool
                                 graphicsQueue
  indexBuffer               <- createIndexBuffer
                                 physicalDevice
                                 device
                                 graphicsCommandPool
                                 graphicsQueue

  let
    swapchainLoop = do
      done <- runResourceT $ do
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
        resetCommandPool device graphicsCommandPool VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
        cmdBuffers          <- createCommandBuffers
                                 device
                                 graphicsCommandPool
                                 framebuffers
                                 renderpass
                                 swapExtent
                                 pipeline
                                 vertexBuffer
                                 indexBuffer
        syncs               <- replicateM 3 (syncSet device)

        liftIO $ action Context{..}

      unless done swapchainLoop

  liftIO swapchainLoop

  where
    syncSet device  = do
      imageAvailableSemaphore <- semaphore device
      renderFinishedSemaphore <- semaphore device
      inFlightFence           <- fence device VK_FENCE_CREATE_SIGNALED_BIT
      return SyncSet{..}

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
      where
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
  where
    isGraphicsQueueFamily queue =
      getField @"queueFlags" queue .&. VK_QUEUE_GRAPHICS_BIT /= zeroBits

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
           <*> supportsSurface physicalDevice familyIndex surface
    isPresentQueueFamily =
      GLFW.getPhysicalDevicePresentationSupport
        vkInstance
        physicalDevice

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
      [ VK_PRESENT_MODE_FIFO_KHR
      , VK_PRESENT_MODE_IMMEDIATE_KHR
      ]
      -- [ VK_PRESENT_MODE_MAILBOX_KHR
      -- , VK_PRESENT_MODE_FIFO_KHR
      -- , VK_PRESENT_MODE_FIFO_RELAXED_KHR
      -- , VK_PRESENT_MODE_IMMEDIATE_KHR
      -- ]

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

    bindingDescription =
      createVk @VkVertexInputBindingDescription
        $  set        @"binding" 0
        &* set        @"stride" (fromIntegral vertexSize)
        &* set        @"inputRate" VK_VERTEX_INPUT_RATE_VERTEX

    positionAttributeDescription =
      createVk @VkVertexInputAttributeDescription
        $  set        @"binding" 0
        &* set        @"location" 0
        &* set        @"format" VK_FORMAT_R32G32_SFLOAT
        &* set        @"offset" (fromIntegral vPositionOffset)

    colorAttributeDescription =
      createVk @VkVertexInputAttributeDescription
        $  set        @"binding" 0
        &* set        @"location" 1
        &* set        @"format" VK_FORMAT_R32G32B32_SFLOAT
        &* set        @"offset" (fromIntegral vColorOffset)

    bindings = [ bindingDescription ]

    attributes = [ positionAttributeDescription, colorAttributeDescription ]

    vertexInputInfo =
      createVk @VkPipelineVertexInputStateCreateInfo
        $  set        @"sType" VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        &* set        @"pNext" VK_NULL
        &* set        @"flags" 0
        &* set        @"vertexBindingDescriptionCount" (fromIntegral $ length bindings)
        &* setListRef @"pVertexBindingDescriptions" bindings
        &* set        @"vertexAttributeDescriptionCount" (fromIntegral $ length attributes)
        &* setListRef @"pVertexAttributeDescriptions" attributes

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

bufferCopy :: VkDeviceSize -> VkDeviceSize -> VkDeviceSize -> VkBufferCopy
bufferCopy srcOffset dstOffset size =
  createVk @VkBufferCopy
    $  set @"srcOffset" srcOffset
    &* set @"dstOffset" dstOffset
    &* set @"size" size

createFramebuffers
  :: VkDevice
  -> VkRenderPass
  -> VkExtent2D
  -> [VkImageView]
  -> ResIO [VkFramebuffer]
createFramebuffers device renderpass extent_ =
  mapM (\attachment -> framebuffer device renderpass extent_ [attachment])

createVertexBuffer
  :: VkPhysicalDevice
  -> VkDevice
  -> VkCommandPool
  -> VkQueue
  -> ResIO VkBuffer
createVertexBuffer physicalDevice device pool graphicsQueue = do
  let vertices = Vector.fromList
                  [ Vertex (V2 -0.5 -0.5) (V3 1.0 0.0 0.0)
                  , Vertex (V2  0.5 -0.5) (V3 0.0 1.0 0.0)
                  , Vertex (V2  0.5  0.5) (V3 0.0 0.0 1.0)
                  , Vertex (V2 -0.5  0.5) (V3 1.0 1.0 1.0)
                  ]

      numVertices  = Vector.length vertices
      verticesSize = fromIntegral $ vertexSize * numVertices

  (vertexBuffer, _vertexBufferMemory)
         <- createBuffer'
              physicalDevice
              device
              verticesSize
              (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_VERTEX_BUFFER_BIT)
              VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  liftIO $ runResourceT $ do
    (stagingBuffer, stagingBufferMemory)
           <- createBuffer'
                physicalDevice
                device
                verticesSize
                VK_BUFFER_USAGE_TRANSFER_SRC_BIT
                (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
    mapPtr <- mapMemory device stagingBufferMemory 0 verticesSize 0
    liftIO $ Vector.unsafeWith vertices $ \ptr ->
      copyBytes mapPtr ptr (fromIntegral verticesSize)
    unmapMemory device stagingBufferMemory

    copyBuffer device pool graphicsQueue stagingBuffer vertexBuffer verticesSize

  return vertexBuffer

createIndexBuffer
  :: VkPhysicalDevice
  -> VkDevice
  -> VkCommandPool
  -> VkQueue
  -> ResIO VkBuffer
createIndexBuffer physicalDevice device pool graphicsQueue = do
  let indices = Vector.fromList [ 0, 1, 2, 2, 3, 0 ] :: Vector.Vector CUShort
      numIndices  = Vector.length indices
      indicesSize = fromIntegral $ sizeOf @CUShort 0  * numIndices

  (indexBuffer, _indexBufferMemory)
         <- createBuffer'
              physicalDevice
              device
              indicesSize
              (VK_BUFFER_USAGE_TRANSFER_DST_BIT .|. VK_BUFFER_USAGE_INDEX_BUFFER_BIT)
              VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  liftIO $ runResourceT $ do
    (stagingBuffer, stagingBufferMemory)
           <- createBuffer'
                physicalDevice
                device
                indicesSize
                VK_BUFFER_USAGE_TRANSFER_SRC_BIT
                (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
    mapPtr <- mapMemory device stagingBufferMemory 0 indicesSize 0
    liftIO $ Vector.unsafeWith indices $ \ptr ->
      copyBytes mapPtr ptr (fromIntegral indicesSize)
    unmapMemory device stagingBufferMemory

    copyBuffer device pool graphicsQueue stagingBuffer indexBuffer indicesSize

  return indexBuffer

copyBuffer
  :: VkDevice
  -> VkCommandPool
  -> VkQueue
  -> VkBuffer
  -> VkBuffer
  -> VkDeviceSize
  -> ResIO ()
copyBuffer device pool graphicsQueue src dst size = do
  [cmd] <- commandBuffers device pool 1
  beginCommandBuffer cmd VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
  liftIO $ withPtr copyRegion $ vkCmdCopyBuffer cmd src dst 1
  endCommandBuffer cmd
  queueSubmit graphicsQueue [] [] VK_NULL [cmd]
  queueWaitIdle graphicsQueue
  where
    copyRegion = bufferCopy 0 0 size

createBuffer'
  :: VkPhysicalDevice
  -> VkDevice
  -> VkDeviceSize
  -> VkBufferUsageFlags
  -> VkMemoryPropertyFlags
  -> ResIO (VkBuffer, VkDeviceMemory)
createBuffer' physicalDevice device size usage flags = do
  buffer_       <- buffer
                     device
                     usage
                     size
                     VK_SHARING_MODE_EXCLUSIVE
                     []
  requirements  <- getBufferMemoryRequirements
                     device
                     buffer_
  bufferMemory  <- memoryFor
                     physicalDevice
                     device
                     requirements
                     flags

  bindBufferMemory device buffer_ bufferMemory 0
  return (buffer_, bufferMemory)

createCommandBuffers
  :: VkDevice
  -> VkCommandPool
  -> [VkFramebuffer]
  -> VkRenderPass
  -> VkExtent2D
  -> VkPipeline
  -> VkBuffer
  -> VkBuffer
  -> ResIO [VkCommandBuffer]
createCommandBuffers device pool frameBuffers renderpass extent_ pipeline vertexBuffer indexBuffer = do
  cmdBuffers <- commandBuffers device pool (length frameBuffers)

  liftIO $
    forM_ (zip frameBuffers cmdBuffers) $ \(frameBuffer_, cmd) -> do
      beginCommandBuffer cmd VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
      beginRenderPass cmd frameBuffer_
      vkCmdBindPipeline cmd VK_PIPELINE_BIND_POINT_GRAPHICS pipeline
      withArrayLen [vertexBuffer] $ \_ buffersPtr ->
        withArrayLen [0] $ \_ offsetsPtr ->
          vkCmdBindVertexBuffers cmd 0 1 buffersPtr offsetsPtr
      vkCmdBindIndexBuffer cmd indexBuffer 0 VK_INDEX_TYPE_UINT16
      vkCmdDrawIndexed cmd 6 1 0 0 0
      vkCmdEndRenderPass cmd
      endCommandBuffer cmd

  return cmdBuffers

  where
    beginRenderPass buffer_ framebuffer_ =
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
          vkCmdBeginRenderPass buffer_ rpiPtr VK_SUBPASS_CONTENTS_INLINE
