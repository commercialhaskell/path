{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

#if defined(mingw32_HOST_OS)
module OsPath.Aeson(module OsPath.Aeson.Windows) where
import OsPath.Aeson.Windows ()
#else
module OsPath.Aeson(module OsPath.Aeson.Posix) where
import OsPath.Aeson.Posix ()
#endif
