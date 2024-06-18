-- | This library provides a well-typed representation of paths in a filesystem
-- directory tree.
--
-- Both "Path.Posix" and "Path.Windows" provide the same interface. This
-- module will reexport the appropriate module for your platform.

{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
module OsPath(module OsPath.Windows) where
import OsPath.Windows
#else
module OsPath(module OsPath.Posix) where
import OsPath.Posix
#endif
