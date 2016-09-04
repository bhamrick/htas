{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module HTas.Direct where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data GB_Obj
type GB = Ptr GB_Obj

foreign import ccall "gambatte_create" gambatte_create :: IO GB

foreign import ccall "gambatte_load" gambatte_load :: GB -> CString -> CUInt -> CLLong -> CUInt -> IO CInt

foreign import ccall "gambatte_runfor" gambatte_runfor :: GB -> Ptr CShort -> Ptr CUInt -> IO CInt

foreign import ccall "gambatte_iscgb" gambatte_iscgb :: GB -> IO CInt

foreign import ccall "gambatte_isloaded" gambatte_isloaded :: GB -> IO CInt
