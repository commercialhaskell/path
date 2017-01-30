{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test suite.

module Main where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Path
import Path.Internal
import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Path.Gen ()

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = do
     describe "Parsing: Path Abs Dir" parseAbsDirSpec
     describe "Parsing: Path Rel Dir" parseRelDirSpec
     describe "Parsing: Path Abs File" parseAbsFileSpec
     describe "Parsing: Path Rel File" parseRelFileSpec
     describe "Operations: (</>)" operationAppend
     describe "Operations: stripDir" operationStripDir
     describe "Operations: isParentOf" operationIsParentOf
     describe "Operations: parent" operationParent
     describe "Operations: filename" operationFilename

-- | The 'filename' operation.
operationFilename :: Spec
operationFilename = do
     it "filename ($(mkAbsDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename)" $
         forAll genValid $ \(parent :: Path Abs Dir) ->
             forAll genValid $ \file ->
                 filename (parent </> file) `shouldBe` filename file

     it "filename ($(mkRelDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename)" $
         forAll genValid $ \(parent :: Path Rel Dir) ->
             forAll genValid $ \file ->
                 filename (parent </> file) `shouldBe` filename file

     it "produces a valid path on when passed a valid absolute path" $ do
        producesValidsOnValids (filename :: Path Abs File -> Path Rel File)

     it "produces a valid path on when passed a valid relative path" $ do
        producesValidsOnValids (filename :: Path Rel File -> Path Rel File)

-- | The 'parent' operation.
operationParent :: Spec
operationParent = do
     it "produces a valid path on when passed a valid file path" $ do
        producesValidsOnValids (parent :: Path Abs File -> Path Abs Dir)

     it "produces a valid path on when passed a valid directory path" $ do
        producesValidsOnValids (parent :: Path Abs Dir -> Path Abs Dir)

-- | The 'isParentOf' operation.
operationIsParentOf :: Spec
operationIsParentOf = do
     it "isParentOf parent (parent </> child)" $
        forAll genValid $ \(parent :: Path Abs Dir) ->
            forAll genValid $ \(child :: Path Rel File) ->
                isParentOf parent (parent </> child)

     it "isParentOf parent (parent </> child)" $
        forAll genValid $ \(parent :: Path Abs Dir) ->
            forAll genValid $ \(child :: Path Rel Dir) ->
                isParentOf parent (parent </> child)

     it "isParentOf parent (parent </> child)" $
        forAll genValid $ \(parent :: Path Rel Dir) ->
            forAll genValid $ \(child :: Path Rel File) ->
                isParentOf parent (parent </> child)

     it "isParentOf parent (parent </> child)" $
        forAll genValid $ \(parent :: Path Rel Dir) ->
            forAll genValid $ \(child :: Path Rel Dir) ->
                isParentOf parent (parent </> child)

-- | The 'stripDir' operation.
operationStripDir :: Spec
operationStripDir = do
     it "stripDir parent (parent </> child) = child" $
        forAll genValid $ \(parent :: Path Abs Dir) ->
            forAll genValid $ \(child :: Path Rel File) ->
                stripDir parent (parent </> child) == Just child

     it "stripDir parent (parent </> child) = child" $
        forAll genValid $ \(parent :: Path Rel Dir) ->
            forAll genValid $ \(child :: Path Rel File) ->
                stripDir parent (parent </> child) == Just child

     it "stripDir parent (parent </> child) = child" $
        forAll genValid $ \(parent :: Path Abs Dir) ->
            forAll genValid $ \(child :: Path Rel Dir) ->
                stripDir parent (parent </> child) == Just child

     it "stripDir parent (parent </> child) = child" $
        forAll genValid $ \(parent :: Path Rel Dir) ->
            forAll genValid $ \(child :: Path Rel Dir) ->
                stripDir parent (parent </> child) == Just child
        
     it "produces a valid path on when passed a valid absolute file paths" $ do
        producesValidsOnValids2 (stripDir :: Path Abs Dir -> Path Abs File -> Maybe (Path Rel File))

     it "produces a valid path on when passed a valid absolute directory paths" $ do
        producesValidsOnValids2 (stripDir :: Path Abs Dir -> Path Abs Dir -> Maybe (Path Rel Dir))

     it "produces a valid path on when passed a valid relative file paths" $ do
        producesValidsOnValids2 (stripDir :: Path Rel Dir -> Path Rel File -> Maybe (Path Rel File))

     it "produces a valid path on when passed a valid relative directory paths" $ do
        producesValidsOnValids2 (stripDir :: Path Rel Dir -> Path Rel Dir -> Maybe (Path Rel Dir))

-- | The '</>' operation.
operationAppend :: Spec
operationAppend = do
     it "produces a valid path on when creating valid absolute file paths" $ do
        producesValidsOnValids2 ((</>) :: Path Abs Dir -> Path Rel File -> Path Abs File)

     it "produces a valid path on when creating valid absolute directory paths" $ do
        producesValidsOnValids2 ((</>) :: Path Abs Dir -> Path Rel Dir -> Path Abs Dir)

     it "produces a valid path on when creating valid relative file paths" $ do
        producesValidsOnValids2 ((</>) :: Path Rel Dir -> Path Rel File -> Path Rel File)

     it "produces a valid path on when creating valid relative directory paths" $ do
        producesValidsOnValids2 ((</>) :: Path Rel Dir -> Path Rel Dir -> Path Rel Dir)


parseAbsDirSpec :: Spec
parseAbsDirSpec = do
     it "Produces valid paths when it succeeds" $
       validIfSucceedsOnArbitrary
         (parseAbsDir :: FilePath -> Maybe (Path Abs Dir))

parseRelDirSpec :: Spec
parseRelDirSpec = do
     it "Produces valid paths when it succeeds" $
       validIfSucceedsOnArbitrary
         (parseRelDir :: FilePath -> Maybe (Path Rel Dir))

parseAbsFileSpec :: Spec
parseAbsFileSpec = do
     it "Produces valid paths when it succeeds" $
       validIfSucceedsOnArbitrary
         (parseAbsFile :: FilePath -> Maybe (Path Abs File))

parseRelFileSpec :: Spec
parseRelFileSpec = do
     it "Produces valid paths when it succeeds" $
       validIfSucceedsOnArbitrary
         (parseRelFile :: FilePath -> Maybe (Path Rel File))