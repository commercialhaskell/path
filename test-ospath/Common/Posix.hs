{-# LANGUAGE CPP #-}

#define PLATFORM_NAME Posix
#define PLATFORM_PATH PosixPath
#define PLATFORM_STRING PosixString
#include "Include.hs"

drives_ :: NonEmpty PLATFORM_PATH
drives_ = NonEmpty.singleton [OsString.pstr|/|]
