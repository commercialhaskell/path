{-# LANGUAGE CPP #-}
#define PREFIX          Path.Except
#define PLATFORM_NAME   Windows
#define IS_WINDOWS      True
#define MONAD_PATH      MonadError PathException
#define THROW           throwError
#include "../Include.hs"
