{-# LANGUAGE CPP #-}
#define PREFIX          Path.Except
#define PLATFORM_NAME   Posix
#define IS_WINDOWS      False
#define MONAD_PATH      MonadError PathException
#define THROW           throwError
#include "../Include.hs"
