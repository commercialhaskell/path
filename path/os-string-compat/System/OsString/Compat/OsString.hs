{-# LANGUAGE CPP #-}

#define PLATFORM_NAME        OsString
#define OSSTRING_MODULE      System.OsString
#define PLATFORM_STRING      OsString
#define PLATFORM_CHAR        OsChar
#define MODULE_NAME          System.OsString.Compat.OsString
#define EXPOSE_CONSTRUCTOR_MODULE System.OsString.Internal.Types
#define EXTRA_DATA_DERIVATION PlatformString

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#define IS_WINDOWS           1
#else
#define IS_WINDOWS           0
#endif

#define ALTERNATE_OSSTRING_PSTR       OsString.osstr

#include "Include.hs"
