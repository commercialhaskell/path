{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
module Path(module Path.Windows) where
import Path.Windows
#else
module Path(module Path.Posix) where
import Path.Posix
#endif
