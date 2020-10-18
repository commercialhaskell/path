{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
module Path.Internal(module Path.Internal.Windows) where
import Path.Internal.Windows
#else
module Path.Internal(module Path.Internal.Posix) where
import Path.Internal.Posix
#endif
