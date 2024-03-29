module Foreign.Extra where

import           Foreign

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
