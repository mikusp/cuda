{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------------------
-- |
-- Module : Foreign.CUDA.NVRTC.Error
-- Copyright : [2009..2015] Trevor L. McDonell
-- License : BSD
--
-- NVRTC errors
--
--------------------------------------------------------------------------------

module Foreign.CUDA.NVRTC.Error (
    Result(..),
    NVRTCException(..),
    resultIfOk,
    nothingIfOk
 ) where


-- Friends
import Foreign.CUDA.Internal.C2HS

-- System
import Foreign
import Foreign.C
import System.IO.Unsafe
import Control.Exception
import Data.Typeable



#include "cbits/stubs.h"
{# context lib="nvrtc" #}

{# enum nvrtcResult as Result
    { underscoreToCase }
    with prefix="NVRTC" deriving (Eq, Show) #}

data NVRTCException
  = NVRTCException Result
  deriving Typeable

instance Exception NVRTCException

instance Show NVRTCException where
  showsPrec _ (NVRTCException s) = showString ("NVRTC Exception: " ++ describe s)

{# fun pure unsafe nvrtcGetErrorString as describe
    { cFromEnum `Result' } -> `String' #}


-- |
-- Return the results of a function on successful execution, otherwise return
-- the error string associated with the return code
--
{-# INLINE resultIfOk #-}
resultIfOk :: (Result, a) -> IO a
resultIfOk (status, !result) =
    case status of
        Success -> return result
        _       -> throwIO (NVRTCException status)


-- |
-- Return the error string associated with an unsuccessful return code,
-- otherwise Nothing
--
{-# INLINE nothingIfOk #-}
nothingIfOk :: Result -> IO ()
nothingIfOk status =
    case status of
        Success -> return  ()
        _       -> throwIO (NVRTCException status)

