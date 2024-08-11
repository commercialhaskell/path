-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_PATH = PosixPath | WindowsPath
--     PLATFORM_STRING = PosixString | WindowsString

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

-- | Test functions that are common to Posix and Windows
module Common.PLATFORM_NAME
  (spec
  ,parseFails
  ,parseSucceeds
  ,parserTest
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, void)
import Control.Monad.Catch (MonadThrow)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust, isNothing)
import System.OsPath.PLATFORM_NAME (PLATFORM_PATH)
import qualified System.OsPath.PLATFORM_NAME as OsPath
import Test.Hspec

import OsPath.PLATFORM_NAME
import OsPath.Internal.PLATFORM_NAME
import System.OsString.Compat.PLATFORM_NAME (PLATFORM_STRING)
import qualified System.OsString.Compat.PLATFORM_NAME as OsString

currentDir :: Path Rel Dir
currentDir = (fromJust . parseRelDir) [OsString.pstr|.|]

drives :: NonEmpty (Path Abs Dir)
drives = (fromJust . traverse parseAbsDir) drives_

relDir :: Path Rel Dir
relDir = (fromJust . parseRelDir) [OsString.pstr|directory|]

relFile :: Path Rel File
relFile = (fromJust . parseRelFile) [OsString.pstr|file|]

spec :: Spec
spec = do
  describe "Operations: (</>)" operationAppend
  describe "Operations: dirname" operationDirname
  describe "Operations: filename" operationFilename
  describe "Operations: parent" operationParent
  describe "Operations: toOsPath" operationToOsPath
  describe "Operations: isProperPrefixOf" operationIsProperPrefixOf
  describe "Operations: stripProperPrefix" operationStripProperPrefix
  describe "Operations: isDrive" operationIsDrive
  describe "Operations: splitDrive" operationSplitDrive
  describe "Operations: extensions" extensionOperations

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
      (isNothing (parseAbsDir . toOsPath . dirname $ drive))

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
    "parent relDir == \".\""
    (parent relDir == currentDir)
  it
    "parent \".\" == \".\""
    (parent currentDir == currentDir)

  forDrives $ \drive -> do
    let absDir = drive </> relDir
    it
      "parent (absDir </> relDir) == absDir"
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
    "splitDrive absDir == (drive, Just relDir)"
    (splitDrive absDir == (drive, Just relDir))
  it
    "splitDrive absFile == (drive, Just relFile)"
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

-- | The 'stripProperPrefix' operation.
operationStripProperPrefix :: Spec
operationStripProperPrefix = do
  it
    "stripProperPrefix relDir (relDir </> relDir) == Just relDir"
    (stripProperPrefix relDir (relDir </> relDir) == Just relDir)

  forDrives $ \drive -> do
    let absDir = drive </> relDir
    it
      "stripProperPrefix absDir (absDir </> relDir) == Just relDir"
      (stripProperPrefix absDir (absDir </> relDir) == Just relDir)
    it
      "stripProperPrefix absDir absDir == Nothing"
      (isNothing (stripProperPrefix absDir absDir))

-- | The '</>' operation.
operationAppend :: Spec
operationAppend = do
  let Path relDir' = relDir
      Path relFile' = relFile
  it
    "RelDir + RelDir == RelDir"
     (relDir </> relDir == Path (relDir' OsPath.</> relDir'))
  it
    "\".\" + \".\" == \".\""
    (currentDir </> currentDir == currentDir)
  it
    "\".\" + relDir == relDir"
     (currentDir </> relDir == relDir)
  it
    "relDir + \".\" == x"
    (relDir </> currentDir == relDir)
  it
    "RelDir + RelFile == RelFile"
    (relDir </> relFile == Path (relDir' OsPath.</> relFile'))

  forDrives $ \drive -> do
    let absDir@(Path absDir') = drive </> relDir
    it
      "AbsDir + RelDir == AbsDir"
      (absDir </> relDir == Path (absDir' OsPath.</> relDir'))
    it
      "AbsDir + RelFile == AbsFile"
      (absDir </> relFile == Path (absDir' OsPath.</> relFile'))

-- | The 'toOsPath' operation.
operationToOsPath :: Spec
operationToOsPath = do
  let expected = relRoot
  it
    ("toOsPath \".\" == " ++ show expected)
    (toOsPath currentDir == expected)
  it
    ("show \".\" == " ++ (show . show) expected)
    (show currentDir == show expected)

-- | Testing operations related to extensions.
extensionOperations :: Spec
extensionOperations = do
    describe "Only filenames and extensions" $
      forM_ filenames $ \file -> do
        forM_ validExtensions $ \ext -> do
          runTests parseRelFile file ext

    describe "Relative dir paths" $
      forM_ dirnames $ \dir -> do
        forM_ filenames $ \file -> do
          forM_ validExtensions $ \ext -> do
              let ospath =
                    dir <> OsString.singleton OsPath.pathSeparator <> file
              runTests parseRelFile ospath ext

    describe "Absolute dir paths" $
      forM_ drives_ $ \drive -> do
        forM_ dirnames $ \dir -> do
          forM_ filenames $ \file -> do
            forM_ validExtensions $ \ext -> do
              let ospath = drive <> dir <> pathSep <> file
              runTests parseAbsFile ospath ext

    -- Invalid extensions
    forM_ invalidExtensions $ \ext -> do
      it ("throws InvalidExtension when extension is " ++ show ext)  $
         addExtension ext (Path [OsString.pstr|name|])
         `shouldThrow` (== InvalidExtension ext)

    where

    runTests :: (forall m . MonadThrow m => PLATFORM_PATH -> m (Path b File))
             -> PLATFORM_PATH
             -> PLATFORM_STRING
             -> Spec
    runTests parse file ext = do
        let maybePathFile = parse file
        let maybePathFileWithExt = parse (file <> ext)
        case (maybePathFile, maybePathFileWithExt) of
            (Just pathFile, Just pathFileWithExt) -> validExtensionsSpec ext pathFile pathFileWithExt
            _ -> it ("Files " ++ show file ++ " and/or " ++ show (file <> ext) ++ " should parse successfully.") $
                     expectationFailure $
                         show file ++ " parsed to " ++ show maybePathFile ++ ", "
                         ++ show (file <> ext) ++ " parsed to " ++ show maybePathFileWithExt

    filenames :: [PLATFORM_PATH]
    filenames =
        [ [OsString.pstr|name|]
        , [OsString.pstr|name.|]
        , [OsString.pstr|name..|]
        , [OsString.pstr|.name|]
        , [OsString.pstr|..name|]
        , [OsString.pstr|name.name|]
        , [OsString.pstr|name..name|]
        , [OsString.pstr|...|]
        ]

    dirnames :: [PLATFORM_PATH]
    dirnames = filenames ++ [ [OsString.pstr|.|] ]

    invalidExtensions :: [PLATFORM_STRING]
    invalidExtensions =
        [ [OsString.pstr||]
        , [OsString.pstr|.|]
        , [OsString.pstr|x|]
        , [OsString.pstr|..|]
        , [OsString.pstr|...|]
        , [OsString.pstr|xy|]
        , [OsString.pstr|foo|]
        , [OsString.pstr|foo.|]
        , [OsString.pstr|foo..|]
        , [OsString.pstr|..foo|]
        , [OsString.pstr|...foo|]
        , [OsString.pstr|.foo.bar|]
        , [OsString.pstr|.foo|] <> pathSep <> [OsString.pstr|bar|]
        ]

    validExtensions :: [PLATFORM_STRING]
    validExtensions =
        [ [OsString.pstr|.foo|]
        , [OsString.pstr|.foo.|]
        , [OsString.pstr|.foo..|]
        ]

validExtensionsSpec :: PLATFORM_STRING -> Path b File -> Path b File -> Spec
validExtensionsSpec ext file fext = do
    let f = show $ toOsPath file
    let fx = show $ toOsPath fext

    it ("addExtension " ++ show ext ++ " " ++ f ++ " == " ++ fx) $
        addExtension ext file `shouldReturn` fext

    it ("fileExtension " ++ fx ++ " == " ++ show ext) $
        fileExtension fext `shouldReturn` ext

    it ("replaceExtension " ++ show ext ++ " " ++ fx ++ " == " ++ fx) $
        replaceExtension ext fext `shouldReturn` fext

forDrives :: (Path Abs Dir -> Spec) -> Spec
forDrives f = case drives of
  (drive :| []) -> f drive
  _ -> forM_ drives $ \drive ->
    describe ("Drive " ++ show drive) (f drive)

parseFails :: PLATFORM_PATH -> Spec
parseFails x = it (show x ++ " should be rejected")
  (isNothing (void (parseAbsDir x) <|>
              void (parseRelDir x) <|>
              void (parseAbsFile x) <|>
              void (parseRelFile x)))

parseSucceeds :: PLATFORM_PATH -> Path Rel Dir -> Spec
parseSucceeds x with = parserTest parseRelDir x (Just with)

-- | Parser test.
parserTest :: (Show a, Show b, Eq b)
           => (a -> Maybe b) -> a -> Maybe b -> Spec
parserTest parser input expected =
  it (message1 ++ "Parsing " ++ show input ++ " " ++ message2)
     (parser input `shouldBe` expected)
  where message1
          | isNothing expected =  "Failing: "
          | otherwise = "Succeeding: "

        message2 = case expected of
          Nothing -> "should fail."
          Just x -> "should succeed with: " ++ show x
