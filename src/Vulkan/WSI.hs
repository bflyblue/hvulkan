module Vulkan.WSI
  ( GlfwException (..)
  , throwGLFW

  , glfw
  , initializeGLFW
  , terminateGLFW
  , requiredGLFWExtensions

  , windowTitled
  , createWindow
  , destroyWindow

  , windowSurface
  , createSurface
  , destroySurface

  , monitorWindowResize
  , resetFlag
  ) where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Foreign.C.String
import           Foreign.Extra
import           Foreign.Ptr
import qualified Graphics.UI.GLFW                       as GLFW
import           Graphics.Vulkan
import           Graphics.Vulkan.Ext.VK_KHR_surface
import           Log

import           Vulkan.Exception
import           Vulkan.Resource

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

glfw :: ResIO ()
glfw =
  vulkanResource
    initializeGLFW
    (const terminateGLFW)

initializeGLFW :: MonadIO m => m ()
initializeGLFW = liftIO $ do
  GLFW.init
    >>= throwGLFW "Unable to initialize GLFW"
  GLFW.vulkanSupported
    >>= throwGLFW "Vulkan not supported"
  logMsg "Initialized GLFW"

terminateGLFW :: MonadIO m => m ()
terminateGLFW = liftIO $ do
  GLFW.terminate
  logMsg "Terminated GLFW"

requiredGLFWExtensions :: MonadIO m => m [CString]
requiredGLFWExtensions = liftIO $ do
  extensions <- GLFW.getRequiredInstanceExtensions
  extNames <- mapM peekCString extensions
  logMsg $ "GLFW requires extensions: " ++ unwords extNames
  return extensions

windowTitled :: Word32 -> Word32 -> String -> ResIO GLFW.Window
windowTitled width height title =
  vulkanResource
    (createWindow width height title)
    destroyWindow

createWindow :: MonadIO m => Word32 -> Word32 -> String -> m GLFW.Window
createWindow width height title = liftIO $ do
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

destroyWindow :: MonadIO m => GLFW.Window -> m ()
destroyWindow window = liftIO $ do
  GLFW.destroyWindow window
  logMsg "Destroyed window"

windowSurface
  :: VkInstance
  -> GLFW.Window
  -> ResIO VkSurfaceKHR
windowSurface vkInstance window =
  vulkanResource
    ( createSurface
        vkInstance
        window
    )
    ( destroySurface
        vkInstance
    )

createSurface :: MonadIO m => VkInstance -> GLFW.Window -> m VkSurfaceKHR
createSurface vkInstance window = liftIO $
  allocaPeek
    ( GLFW.createWindowSurface
        vkInstance
        window
        nullPtr
          >=> throwVkResult "createSurfaceWindow: Failed to create surface."
    )
    <* logMsg "Created surface"


destroySurface :: MonadIO m => VkInstance -> VkSurfaceKHR -> m ()
destroySurface vkInstance surface = liftIO $ do
  vkDestroySurfaceKHR vkInstance surface VK_NULL
  logMsg "Destroyed surface"

monitorWindowResize :: MonadIO m => GLFW.Window -> m (MVar Bool)
monitorWindowResize window = liftIO $ do
  resizedFlag <- newMVar False
  GLFW.setWindowSizeCallback window (Just $ windowResizedCallback resizedFlag)
  return resizedFlag

resetFlag :: MonadIO m => MVar Bool -> m ()
resetFlag flag = liftIO $ do
  _ <- takeMVar flag
  putMVar flag False

windowResizedCallback :: MVar Bool -> GLFW.WindowSizeCallback
windowResizedCallback flag _window _width _height = do
  _ <- takeMVar flag
  putMVar flag True
