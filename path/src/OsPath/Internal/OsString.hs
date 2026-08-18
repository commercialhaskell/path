{-# LANGUAGE CPP #-}

#define MODULE_NAME          OsPath.Internal.OsString
#define PLATFORM_NAME        OsString
#define OSPATH_MODULE        OsPath.Internal.OsPathWrapper
#define OSSTRING_MODULE      System.OsString
#define PLATFORM_PATH        OsPath
#define PLATFORM_PATH_SINGLE 'OsPath'
#define PLATFORM_STRING      OsString

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#define IS_WINDOWS           1
#else
#define IS_WINDOWS           0
#endif

#include "Include.hs"
