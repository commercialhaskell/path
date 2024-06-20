-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows

{-# LANGUAGE TemplateHaskell #-}

-- | Test functions that are common to Posix and Windows
module Common.PLATFORM_NAME
  (operationDirname
  ,operationFilename
  ,operationParent
  ,operationSplitDrive
  ,operationIsDrive
  ,operationIsProperPrefixOf
  ,extensionOperations
  ) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust, isNothing)
import qualified System.FilePath.PLATFORM_NAME as FilePath
import Test.Hspec

import Path.Internal.PLATFORM_NAME
import Path.PLATFORM_NAME

currentDir :: Path Rel Dir
currentDir = (fromJust . parseRelDir) "."

drives :: NonEmpty (Path Abs Dir)
drives = (fromJust . traverse parseAbsDir) drives_

relDir :: Path Rel Dir
relDir = (fromJust . parseRelDir) "directory"

relFile :: Path Rel File
relFile = (fromJust . parseRelFile) "file"

-- | The 'dirname' operation.
operationDirname :: Spec
operationDirname = do
  it
    "dirname (relDir </> relDir) == dirname relDir"
    (dirname (relDir </> relDir) == dirname relDir)
  it
    "dirname \".\" == dirname \".\""
    (dirname currentDir == currentDir)

  forDrives $ \drive -> do
    let absDir = drive </> relDir
    it
      "dirname (absDir </> relDir) == dirname relDir"
      (dirname (absDir </> relDir) == dirname relDir)
    it
      "dirname of a drive must be a Rel path"
      (isNothing (parseAbsDir . toFilePath . dirname $ drive))

-- | The 'filename' operation.
operationFilename :: Spec
operationFilename = do
  it
    "filename (relDir </> relFile) == filename relFile"
    (filename (relDir </> relFile) == filename relFile)

  forDrives $ \drive -> do
    let absDir = drive </> relDir
    it
      "filename (absDir </> relFile) == filename relFile"
      (filename (absDir </> relFile) == filename relFile)

-- | The 'parent' operation.
operationParent :: Spec
operationParent = do
  it
    "parent \"name\" == \".\""
    (parent relDir == currentDir)
  it
    "parent \".\" == \".\""
    (parent currentDir == currentDir)

  forDrives $ \drive -> do
    let absDir = drive </> relDir
    it
      "parent (absDir </> \"name\") == absDir"
      (parent (absDir </> relDir) == absDir)
    it
      "parent \"/name\" == drive"
      (parent absDir == drive)
    it
      "parent drive == drive"
      (parent drive == drive)

-- | The 'splitDrive' operation.
operationSplitDrive :: Spec
operationSplitDrive = forDrives $ \drive -> do
  let absDir = drive </> relDir
      absFile = drive </> relFile
  it
    "splitDrive \"/dir\" == (drive, Just \"dir\")"
    (splitDrive absDir == (drive, Just relDir))
  it
    "splitDrive \"/file\" == (drive, Just \"file\")"
    (splitDrive absFile == (drive, Just relFile))
  it
    "splitDrive drive == (drive, Nothing)"
    (splitDrive drive == (drive, Nothing))

-- | The 'isDrive' operation.
operationIsDrive :: Spec
operationIsDrive = forDrives $ \drive -> do
  let absDir = drive </> relDir
  it
    "isDrive drive"
    (isDrive drive)
  it
    "not (isDrive absDir)"
    (not (isDrive absDir))

-- | The 'isProperPrefixOf' operation.
operationIsProperPrefixOf :: Spec
operationIsProperPrefixOf = do
  it
    "isProperPrefixOf relDir (relDir </> relDir)"
    (isProperPrefixOf relDir (relDir </> relDir))

  it
    "not (relDir `isProperPrefixOf` relDir)"
    (not (isProperPrefixOf relDir relDir))

  forDrives $ \drive -> do
    let absDir = drive </> relDir
    it
      "isProperPrefixOf absDir (absDir </> relDir)"
      (isProperPrefixOf absDir (absDir </> relDir))

    it
      "not (drive `isProperPrefixOf` drive)"
      (not (isProperPrefixOf drive drive))

extensionOperations :: Spec
extensionOperations = do
    let extension = ".foo"
    let extensions = extension : [".foo.", ".foo.."]

    describe "Only filenames and extensions" $
      forM_ extensions $ \ext ->
          forM_ filenames $ \f -> do
              runTests parseRelFile f ext

    describe "Relative dir paths" $
      forM_ dirnames $ \d -> do
          forM_ filenames $ \f -> do
              let f1 = d ++ [FilePath.pathSeparator] ++ f
              runTests parseRelFile f1 extension

    describe "Absolute dir paths" $
      forM_ drives_ $ \drive -> do
        forM_ dirnames $ \dir -> do
          forM_ filenames $ \file -> do
            let filepath = drive ++ dir ++ [FilePath.pathSeparator] ++ file
            runTests parseAbsFile filepath extension

    -- Invalid extensions
    forM_ invalidExtensions $ \ext -> do
        it ("throws InvalidExtension when extension is [" ++ ext ++ "]")  $
            addExtension ext $(mkRelFile "name")
            `shouldThrow` (== InvalidExtension ext)

    where

    runTests parse file ext = do
        let maybePathFile = parse file
        let maybePathFileWithExt = parse (file ++ ext)
        case (maybePathFile, maybePathFileWithExt) of
            (Just pathFile, Just pathFileWithExt) -> validExtensionsSpec ext pathFile pathFileWithExt
            _ -> it ("Files " ++ show file ++ " and/or " ++ show (file ++ ext) ++ " should parse successfully.") $
                     expectationFailure $
                         show file ++ " parsed to " ++ show maybePathFile ++ ", "
                         ++ show (file ++ ext) ++ " parsed to " ++ show maybePathFileWithExt

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
        , ".foo" ++ [FilePath.pathSeparator] ++ "bar"
        ]

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

forDrives :: (Path Abs Dir -> Spec) -> Spec
forDrives f = case drives of
  (drive :| []) -> f drive
  _ -> forM_ drives $ \drive ->
    describe ("Drive " ++ show drive) (f drive)
