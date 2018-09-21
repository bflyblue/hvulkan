{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Graphics.UI.GLFW                       as GLFW

import           Context
import           Log
import           Vulkan

main :: IO ()
main = withContext defaultConfig mainloop

mainloop :: Context -> IO Bool
mainloop ctx@Context{..} = go (cycle syncs)
  where
    go [] = logMsg "No syncs!" >> return True
    go (sync : rest) = do
      liftIO GLFW.pollEvents
      shouldClose <- liftIO $ GLFW.windowShouldClose window
      if shouldClose
      then
        done
      else
        catchJust outOfDateOrSuboptimal
          (drawFrame ctx sync >> go rest)
          (const wantNewSwapchain)

    outOfDateOrSuboptimal (VulkanException code _) =
      case code of
        Just VK_SUBOPTIMAL_KHR -> Just code
        Just VK_ERROR_OUT_OF_DATE_KHR -> Just code
        _ -> Nothing

    done = return True

    wantNewSwapchain = do
      deviceWaitIdle device
      return False

drawFrame
  :: MonadIO m
  => Context
  -> SyncSet
  -> m ()
drawFrame Context{..} SyncSet{..} = do
    waitForFences device [inFlightFence] maxBound
    resized <- liftIO $ readMVar resizedFlag
    when resized $ do
        logMsg "GLFW reported resize"
        throwVkResult "Window resized" VK_ERROR_OUT_OF_DATE_KHR
    imageIndex <- acquireNextImage device swapChain maxBound imageAvailableSemaphore VK_NULL
    resetFences device [inFlightFence]
    queueSubmit
      graphicsQueue
      [(imageAvailableSemaphore, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)]
      [renderFinishedSemaphore]
      inFlightFence
      [cmdBuffers !! fromIntegral imageIndex]
    queuePresent presentQueue [swapChain] [renderFinishedSemaphore] [imageIndex]
