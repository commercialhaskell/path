{-# LANGUAGE TemplateHaskell #-}

-- | Test functions that are common to Posix and Windows

module Common (extensionOperations) where

import Data.Maybe (isJust, fromJust)
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
        fileExtension fext `shouldReturn` ext

    it ("replaceExtension " ++ show ext ++ " " ++ fx ++ " == " ++ fx) $
        replaceExtension ext fext `shouldReturn` fext

extensionOperations :: String -> Spec
extensionOperations rootDrive = do
    let extension = ".foo"
    let extensions = extension : [".foo.", ".foo.."]

    -- Only filenames and extensions
    forM_ extensions (\ext ->
        forM_ filenames $ \f -> do
            runTests parseRelFile f ext)

    -- Relative dir paths
    forM_ dirnames (\d -> do
        forM_ filenames (\f -> do
            let f1 = d ++ [pathSeparator] ++ f
            runTests parseRelFile f1 extension))

    -- Absolute dir paths
    forM_ dirnames (\d -> do
        forM_ filenames (\f -> do
            let f1 = rootDrive ++ d ++ [pathSeparator] ++ f
            runTests parseAbsFile f1 extension))

    -- Invalid extensions
    forM_ invalidExtensions $ \ext -> do
        it ("throws InvalidExtension when extension is [" ++ ext ++ "]")  $
            addExtension ext $(mkRelFile "name")
            `shouldThrow` (== InvalidExtension ext)

    where

    runTests parse file ext = do
        let maybeFile = parse file
        let maybeFileWithExt = parse (file ++ ext)
        it ("Files " ++ file ++ " and " ++ (file ++ ext) ++ " are parsed successfully.") $
            (isJust maybeFile, isJust maybeFileWithExt) `shouldBe` (True, True)
        when (isJust maybeFile && isJust maybeFileWithExt) $
            validExtensionsSpec ext (fromJust maybeFile) (fromJust maybeFileWithExt)

    filenames =
        [ "name"
        , "name."
        , "name.."
        , ".name"
        , "..name"
        , "name.name"
        , "name..name"
        , "..."
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
        , ".foo" ++ [pathSeparator] ++ "bar"
        ]
