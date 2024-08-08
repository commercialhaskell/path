{-# LANGUAGE CPP #-}

#define PLATFORM_NAME Posix
#include "Include.hs"

drives_ :: NonEmpty FilePath
drives_ = NonEmpty.singleton "/"
