{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------------------
-- |
-- Module : Foreign.CUDA.NVRTC.Compile
-- Copyright : [2009..2015] Trevor L. McDonell
-- License : BSD
--
-- NVRTC compile
--
--------------------------------------------------------------------------------

module Foreign.CUDA.NVRTC.Compile where


-- Friends
import Foreign.CUDA.Internal.C2HS

-- System
import Foreign
import Foreign.C
import System.IO.Unsafe

import Foreign.CUDA.NVRTC.Error

#include "cbits/stubs.h"
{# context lib="nvrtc" #}

newtype Program = Program { useProg :: {# type nvrtcProgram #} }

createProgram :: String -> String -> [(String, String)] -> IO Program
createProgram source name headers = resultIfOk =<< nvrtcCreateProgram source name (length headers) contents names
    where
        (names, contents) = unzip headers

{# fun nvrtcCreateProgram
    { alloca- `Program' peekProg*
    ,         `String'
    ,         `String'
    ,         `Int'
    , withCStringArray* `[String]'
    , withCStringArray* `[String]'
    } -> `Result' cToEnum #}

destroyProgram :: Program -> IO ()
destroyProgram p = nothingIfOk =<< nvrtcDestroyProgram p

{# fun nvrtcDestroyProgram
    { foo* `Program' } -> `Result' cToEnum #}

foo :: Program -> (Ptr (Ptr ()) -> IO Result) -> IO Result
foo p cont = alloca $ \foo -> do
    poke foo (useProg p)
    cont foo

compileProgram :: Program -> [String] -> IO ()
compileProgram p opts = nothingIfOk =<< nvrtcCompileProgram p (length opts) opts

{# fun nvrtcCompileProgram
    { useProg `Program'
    ,         `Int'
    , withCStringArray* `[String]'
    } -> `Result' cToEnum #}

getPTX :: Program -> IO String
getPTX p = do
    size <- getPTXSize p
    allocaBytes size $ \s -> do
        nothingIfOk =<< nvrtcGetPTX p s
        peekCString (castPtr s)

getPTXSize :: Program -> IO Int
getPTXSize p = resultIfOk =<< nvrtcGetPTXSize p

{# fun nvrtcGetPTX
    { useProg `Program'
    , castPtr `Ptr Char'
    } -> `Result' cToEnum #}

{# fun nvrtcGetPTXSize
    { useProg `Program'
    , alloca- `Int' peekSizet*
    } -> `Result' cToEnum #}

getProgramLog :: Program -> IO String
getProgramLog p = do
    size <- getProgramLogSize p
    allocaBytes size $ \s -> do
        nothingIfOk =<< nvrtcGetProgramLog p s
        peekCString (castPtr s)

getProgramLogSize :: Program -> IO Int
getProgramLogSize p = resultIfOk =<< nvrtcGetProgramLogSize p


{# fun nvrtcGetProgramLog
    { useProg `Program'
    , castPtr `Ptr Char'
    } -> `Result' cToEnum #}

{# fun nvrtcGetProgramLogSize
    { useProg `Program'
    , alloca- `Int' peekSizet*
    } -> `Result' cToEnum #}

{# fun nvrtcVersion
    { alloca- `Int' peekInt*
    , alloca- `Int' peekInt*
    } -> `Result' cToEnum #}

peekInt = fmap fromIntegral . peek
peekSizet = fmap fromIntegral . peek
peekProg = fmap Program . peek

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings ss f = case ss of
  [] -> f []
  (s:ss') -> withCString s $ \cs -> do
    withCStrings ss' $ \css -> f (cs:css)

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray ss f = withCStrings ss $ \css -> withArray css f
