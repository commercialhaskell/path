-- | This library provides a well-typed representation of paths in a filesystem
-- directory tree.
--
-- Both "Path.Posix" and "Path.Windows" provide the same interface. This
-- module will reexport the appropriate module for your platform.

{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
module Path.Except(module Path.Except.Windows) where
import Path.Except.Windows
#else
module Path.Except(module Path.Except.Posix) where
import Path.Except.Posix
#endif
