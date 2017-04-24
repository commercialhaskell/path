{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

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
import Test.Validity.Property

import Path.Gen

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = parallel $ do
     describe "Parsing: Path Abs Dir" (parserSpec parseAbsDir)
     describe "Parsing: Path Rel Dir" (parserSpec parseRelDir)
     describe "Parsing: Path Abs File" (parserSpec parseAbsFile)
     describe "Parsing: Path Rel File" (parserSpec parseRelFile)
     describe "Operations: (</>)" operationAppend
     describe "Operations: stripDir" operationStripDir
     describe "Operations: isParentOf" operationIsParentOf
     describe "Operations: parent" operationParent
     describe "Operations: filename" operationFilename

-- | The 'filename' operation.
operationFilename :: Spec
operationFilename = do
     it "filename ($(mkAbsDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename)" $
         forAllShrink genValid shrinkValidAbsDir $ \parent ->
             forAllShrink genValid shrinkValidRelFile $ \file ->
                 filename (parent </> file) `shouldBe` filename file

     it "filename ($(mkRelDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename)" $
         forAllShrink genValid shrinkValidRelDir $ \parent ->
             forAllShrink genValid shrinkValidRelFile $ \file ->
                 filename (parent </> file) `shouldBe` filename file

     it "produces a valid path on when passed a valid absolute path" $ do
        producesValidsOnValids (filename :: Path 'Abs 'File -> Path 'Rel 'File)

     it "produces a valid path on when passed a valid relative path" $ do
        producesValidsOnValids (filename :: Path 'Rel 'File -> Path 'Rel 'File)

-- | The 'parent' operation.
operationParent :: Spec
operationParent = do
     it "produces a valid path on when passed a valid file path" $ do
        producesValidsOnValids (parent :: Path 'Abs 'File -> Path 'Abs 'Dir)

     it "produces a valid path on when passed a valid directory path" $ do
        producesValidsOnValids (parent :: Path 'Abs 'Dir -> Path 'Abs 'Dir)

-- | The 'isParentOf' operation.
operationIsParentOf :: Spec
operationIsParentOf = do
     it "isParentOf parent (parent </> child)" $
        forAllShrink genValid shrinkValidAbsDir $ \parent ->
            forAllShrink genValid shrinkValidRelFile $ \child ->
                isParentOf parent (parent </> child)

     it "isParentOf parent (parent </> child)" $
        forAllShrink genValid shrinkValidAbsDir $ \parent ->
            forAllShrink genValid shrinkValidRelDir $ \child ->
                isParentOf parent (parent </> child)

     it "isParentOf parent (parent </> child)" $
        forAllShrink genValid shrinkValidRelDir $ \parent ->
            forAllShrink genValid shrinkValidRelFile $ \child ->
                isParentOf parent (parent </> child)

     it "isParentOf parent (parent </> child)" $
        forAllShrink genValid shrinkValidRelDir $ \parent ->
            forAllShrink genValid shrinkValidRelDir $ \child ->
                isParentOf parent (parent </> child)

-- | The 'stripDir' operation.
operationStripDir :: Spec
operationStripDir = do
     it "stripDir parent (parent </> child) = child" $
        forAllShrink genValid shrinkValidAbsDir $ \parent ->
            forAllShrink genValid shrinkValidRelFile $ \child ->
                stripDir parent (parent </> child) == Just child

     it "stripDir parent (parent </> child) = child" $
        forAllShrink genValid shrinkValidRelDir $ \parent ->
            forAllShrink genValid shrinkValidRelFile $ \child ->
                stripDir parent (parent </> child) == Just child

     it "stripDir parent (parent </> child) = child" $
        forAllShrink genValid shrinkValidAbsDir $ \parent ->
            forAllShrink genValid shrinkValidRelDir $ \child ->
                stripDir parent (parent </> child) == Just child

     it "stripDir parent (parent </> child) = child" $
        forAllShrink genValid shrinkValidRelDir $ \parent ->
            forAllShrink genValid shrinkValidRelDir $ \child ->
                stripDir parent (parent </> child) == Just child

     it "produces a valid path on when passed a valid absolute file paths" $ do
        producesValidsOnValids2 (stripDir :: Path 'Abs 'Dir -> Path 'Abs 'File -> Maybe (Path 'Rel 'File))

     it "produces a valid path on when passed a valid absolute directory paths" $ do
        producesValidsOnValids2 (stripDir :: Path 'Abs 'Dir -> Path 'Abs 'Dir -> Maybe (Path 'Rel 'Dir))

     it "produces a valid path on when passed a valid relative file paths" $ do
        producesValidsOnValids2 (stripDir :: Path 'Rel 'Dir -> Path 'Rel 'File-> Maybe (Path 'Rel 'File))

     it "produces a valid path on when passed a valid relative directory paths" $ do
        producesValidsOnValids2 (stripDir :: Path 'Rel 'Dir -> Path 'Rel 'Dir -> Maybe (Path 'Rel 'Dir))

-- | The '</>' operation.
operationAppend :: Spec
operationAppend = do
     it "produces a valid path on when creating valid absolute file paths" $ do
        producesValidsOnValids2 ((</>) :: Path 'Abs 'Dir -> Path 'Rel 'File -> Path 'Abs 'File)

     it "produces a valid path on when creating valid absolute directory paths" $ do
        producesValidsOnValids2 ((</>) :: Path 'Abs 'Dir -> Path 'Rel 'Dir -> Path 'Abs 'Dir)

     it "produces a valid path on when creating valid relative file paths" $ do
        producesValidsOnValids2 ((</>) :: Path 'Rel 'Dir -> Path 'Rel 'File -> Path 'Rel 'File)

     it "produces a valid path on when creating valid relative directory paths" $ do
        producesValidsOnValids2 ((</>) :: Path 'Rel 'Dir -> Path 'Rel 'Dir -> Path 'Rel 'Dir)

parserSpec :: (Show p, Validity p) => (FilePath -> Maybe p) -> Spec
parserSpec parser =
     it "Produces valid paths when it succeeds" $
       validIfSucceedsOnGen parser genFilePath
