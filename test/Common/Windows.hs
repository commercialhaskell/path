{-# LANGUAGE CPP #-}

#define PLATFORM_NAME Windows
#include "Include.hs"

-- See https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats
drives :: NonEmpty FilePath
drives = NonEmpty.fromList
  [ "C:\\" -- Common
  , "C:/" -- Common
  , "\\\\host" -- UNC
  --, "\\\\.\\C:\\" -- DOS Device Path
  , "\\\\?\\C:\\" -- DOS Device Path
  --, "\\\\?\\UNC\\" -- DOS Device Path
  --, "\\\\.\\UNC\\" -- DOS Device Path
  ]
