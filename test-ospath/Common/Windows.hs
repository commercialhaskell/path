{-# LANGUAGE CPP #-}

#define PLATFORM_NAME Windows
#define PLATFORM_PATH WindowsPath
#define PLATFORM_STRING WindowsString
#include "Include.hs"

-- See https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats
drives_ :: NonEmpty PLATFORM_STRING
drives_ = NonEmpty.fromList
  [ [OsString.pstr|C:\|] -- Common
  , [OsString.pstr|C:/|] -- Common
  , [OsString.pstr|\\host|] -- UNC
  --, [OsString.pstr|\\.\C:\|] -- DOS Device Path
  , [OsString.pstr|\\?\C:\|] -- DOS Device Path
  --, [OsString.pstr|\\?\UNC\|] -- DOS Device Path
  --, [OsString.pstr|\\.\UNC\|] -- DOS Device Path
  ]
