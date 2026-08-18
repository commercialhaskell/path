{-# LANGUAGE CPP #-}

#define PLATFORM_NAME        OsString
#define PLATFORM_PATH        OsPath
#define PLATFORM_PATH_SINGLE 'OsPath'
#define PLATFORM_STRING      OsString
#define PLATFORM_STRING_SINGLE 'OsString'
#define OSPATH_MODULE        System.OsPath

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#define IS_WINDOWS           1
#define PLATFORM_UTF_CODEC   UTF16-LE
#else
#define IS_WINDOWS           0
#define PLATFORM_UTF_CODEC   UTF8
#endif

#include "Include.hs"
