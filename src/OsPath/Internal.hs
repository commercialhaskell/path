{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
module OsPath.Internal(module OsPath.Internal.Windows) where
import OsPath.Internal.Windows
#else
module OsPath.Internal(module OsPath.Internal.Posix) where
import OsPath.Internal.Posix
#endif
