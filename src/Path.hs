-- | This library provides a well-typed representation of paths in a filesystem
-- directory tree.
--
-- Both "Path.Posix" and "Path.Windows" provide the same interface. This
-- module will reexport the appropriate module for your platform.

{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
module Path(module Path.Windows) where
import Path.Windows
#else
module Path(module Path.Posix) where
import Path.Posix
#endif
