{-# LANGUAGE TemplateHaskell #-}

-- | Test functions that are common to Posix and Windows

module Common (extensionOperations) where

import Control.Monad
import Path
import System.FilePath (pathSeparator)
import Test.Hspec

validExtensionsSpec :: String -> Path b File -> Path b File -> Spec
validExtensionsSpec ext file fext = do
    let f = show $ toFilePath file
    let fx = show $ toFilePath fext

    it ("addExtension " ++ show ext ++ " " ++ f ++ " == " ++ fx) $
        addExtension ext file `shouldReturn` fext

    it ("fileExtension " ++ fx ++ " == " ++ ext) $
        fileExtension fext `shouldBe` ext

    it ("replaceExtension " ++ show ext ++ " " ++ fx ++ " == " ++ fx) $
        replaceExtension ext fext `shouldReturn` fext

extensionOperations :: String -> Spec
extensionOperations rootDrive = do
    let ext = ".foo"
    let extensions = ext : [".foo.", ".foo.."]

    -- Only filenames and extensions
    forM_ extensions (\x ->
        forM_ filenames $ \f -> do
            let Just file = parseRelFile f
            let Just fext = parseRelFile (f ++ x)
            (validExtensionsSpec x file fext))

    -- Relative dir paths
    forM_ dirnames (\d -> do
        forM_ filenames (\f -> do
            let f1 = d ++ [pathSeparator] ++ f
            let Just file = parseRelFile f1
            let Just fext = parseRelFile (f1 ++ ext)
            validExtensionsSpec ext file fext))

    -- Absolute dir paths
    forM_ dirnames (\d -> do
        forM_ filenames (\f -> do
            let f1 = rootDrive ++ d ++ [pathSeparator] ++ f
            let Just file = parseAbsFile f1
            let Just fext = parseAbsFile (f1 ++ ext)
            validExtensionsSpec ext file fext))

    -- Invalid extensions
    forM_ invalidExtensions $ \x -> do
        it ("throws InvalidExtension when extension is [" ++ x ++ "]")  $
            addExtension x $(mkRelFile "name")
            `shouldThrow` (== InvalidExtension x)

    where

    filenames =
        [ "name"
        , "name."
        , "name.."
        , ".name"
        , "..name"
        , "name.name"
        , "name..name"
        ]
    dirnames = filenames ++ ["."]
    invalidExtensions =
        [ ""
        , "."
        , "x"
        , ".."
        , "..."
        , "xy"
        , "foo"
        , "foo."
        , "foo.."
        , "..foo"
        , "...foo"
        , ".foo.bar"
        , ".evil" ++ [pathSeparator] ++ "foo"
        ]
