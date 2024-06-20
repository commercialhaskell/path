{-# LANGUAGE CPP #-}

#define PLATFORM_NAME Posix
#include "Include.hs"

drives :: NonEmpty FilePath
drives = NonEmpty.singleton "/"
