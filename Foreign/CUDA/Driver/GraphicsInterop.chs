{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------------------
-- |
-- Module    : Foreign.CUDA.Driver.Marshal
-- Copyright : [2009..2014] Trevor L. McDonell
-- License   : BSD
--
-- Memory management for low-level driver interface
--
--------------------------------------------------------------------------------

module Foreign.CUDA.Driver.GraphicsInterop (

  glRegisterBuffer,
  mapResources,
  unmapResources,
  getMappedPointer,
  unregisterResource

) where

#include "cbits/stubs.h"
{# context lib="cuda" #}

-- Friends
import Foreign.CUDA.Ptr
import Foreign.CUDA.Driver.Error
import Foreign.CUDA.Driver.Marshal                      ( peekDeviceHandle )
import Foreign.CUDA.Driver.Stream                       ( Stream(..), defaultStream )
import Foreign.CUDA.Internal.C2HS

-- System
import Data.Int
import Data.Maybe
import Prelude

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal


newtype GraphicsResource = GraphicsResource { useGraphicsResource :: {# type CUgraphicsResource #} }
  deriving (Eq, Show)

{# enum CUgraphicsRegisterFlags as RegisterFlag
    { underscoreToCase }
    with prefix="CU_GRAPHICS_REGISTER_FLAGS" deriving (Eq, Show, Bounded) #}

{-# INLINABLE glRegisterBuffer #-}
glRegisterBuffer :: Int -> [RegisterFlag] -> IO GraphicsResource
glRegisterBuffer buffer flags = resultIfOk =<< cuGraphicsGLRegisterBuffer buffer flags

{-# INLINE cuGraphicsGLRegisterBuffer #-}
{# fun unsafe cuGraphicsGLRegisterBuffer
  { alloca'-        `GraphicsResource' peekGR*
  ,                 `Int'
  , combineBitMasks `[RegisterFlag]'           } -> `Status' cToEnum #}
  where alloca' !f = alloca $ \ !p -> poke p nullPtr >> f (castPtr p)
        peekGR = fmap GraphicsResource . peek

{-# INLINABLE mapResources #-}
mapResources :: [GraphicsResource] -> Maybe Stream -> IO ()
mapResources resources mst = 
  withArrayLen (map useGraphicsResource resources) $ \i p_rscs ->
    nothingIfOk =<< cuGraphicsMapResources i p_rscs (maybe defaultStream id mst)

{-# INLINE cuGraphicsMapResources #-}
{# fun unsafe cuGraphicsMapResources
  {           `Int'
  , id        `Ptr (Ptr ())'
  , useStream `Stream'           } -> `Status' cToEnum #}

{-# INLINABLE getMappedPointer #-}
getMappedPointer :: GraphicsResource -> IO (DevicePtr a, Int)
getMappedPointer resource = do
    (!status,!dptr,!bytes) <- cuGraphicsResourceGetMappedPointer resource
    resultIfOk (status, (dptr, bytes))

{-# INLINE cuGraphicsResourceGetMappedPointer #-}
{# fun unsafe cuGraphicsResourceGetMappedPointer
  { alloca-             `DevicePtr a'      peekDeviceHandle*
  , alloca-             `Int'              peekIntConv*
  , useGraphicsResource `GraphicsResource'                   } -> `Status' cToEnum #}

{-# INLINABLE unmapResources #-}
unmapResources :: [GraphicsResource] -> Maybe Stream -> IO ()
unmapResources resources mst =
  withArrayLen (map useGraphicsResource resources) $ \i p_rscs ->
    nothingIfOk =<< cuGraphicsUnmapResources i p_rscs (maybe defaultStream id mst)

{-# INLINE cuGraphicsUnmapResources #-}
{# fun unsafe cuGraphicsUnmapResources
  {           `Int'
  , id        `Ptr (Ptr ())'
  , useStream `Stream'       } -> `Status' cToEnum #}

unregisterResource :: GraphicsResource -> IO ()
unregisterResource resource = nothingIfOk =<< cuGraphicsUnregisterResource resource

{-# INLINE cuGraphicsUnregisterResource #-}
{# fun unsafe cuGraphicsUnregisterResource
  { useGraphicsResource `GraphicsResource' } -> `Status' cToEnum #}
