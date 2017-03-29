{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test suite.

module Main where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Path
import Path.Internal
import qualified System.FilePath as FP
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity.Property

import Path.Gen

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = parallel $ do
     describe "Parsing:" parserSpec
     describe "Operations: (</>)" operationAppend
     describe "Operations: stripDir" operationStripDir
     describe "Operations: isParentOf" operationIsParentOf
     describe "Operations: parent" operationParent
     describe "Operations: filename" operationFilename

-- | The 'filename' operation.
operationFilename :: Spec
operationFilename = do
    prop "filename (abdir </> relfile) == filename relfile" $
        \(parent :: Path Abs Dir) file ->
            filename (parent </> file) `shouldBe` filename file

    prop "filename (reldir </> relfile) == filename relfile" $
        \(parent :: Path Rel Dir) file ->
            filename (parent </> file) `shouldBe` filename file

    prop "isValid (filename absfile)" $
        \(p :: Path Abs File) -> isValid (filename p)

    prop "isValid (filename relfile)" $
        \(p :: Path Rel File) -> isValid (filename p)

-- | The 'parent' operation.
operationParent :: Spec
operationParent = do
    prop "isValid (parent absfile)" $
        \(p :: Path Abs File) -> isValid (parent p)

    prop "isValid (parent absdir)" $
        \(p :: Path Abs Dir) -> isValid (parent p)

-- | The 'isParentOf' operation.
operationIsParentOf :: Spec
operationIsParentOf = do
    prop_isParentOf "abs" (Proxy :: Proxy Abs) "file" (Proxy :: Proxy File)
    prop_isParentOf "abs" (Proxy :: Proxy Abs) "dir"  (Proxy :: Proxy Dir)
    prop_isParentOf "rel" (Proxy :: Proxy Rel) "file" (Proxy :: Proxy File)
    prop_isParentOf "rel" (Proxy :: Proxy Rel) "dir"  (Proxy :: Proxy Dir)
  where
    prop_isParentOf
        :: forall b t. (Arbitrary (Path b Dir), Arbitrary (Path Rel t))
        => String -> Proxy b -> String -> Proxy t -> Spec
    prop_isParentOf parentBase _ childType _ =
        prop ("isParentOf " <> parentBase <> "dir (" <> parentBase <> "dir </> rel" <> childType <> ")") $
            \(parent :: Path b Dir) (child :: Path Rel t) ->
                isParentOf parent (parent </> child)

-- | The 'stripDir' operation.
operationStripDir :: Spec
operationStripDir = do
    prop_stripDir_append "abs" (Proxy :: Proxy Abs) "file" (Proxy :: Proxy File)
    prop_stripDir_append "abs" (Proxy :: Proxy Abs) "dir"  (Proxy :: Proxy Dir)
    prop_stripDir_append "rel" (Proxy :: Proxy Rel) "file" (Proxy :: Proxy File)
    prop_stripDir_append "rel" (Proxy :: Proxy Rel) "dir"  (Proxy :: Proxy Dir)

    -- FIXME: The properties below will still pass if all they ever produce are 'Nothing's.
    it "produces a valid path on when passed a valid absolute file paths" $ do
        producesValidsOnValids2 (stripDir :: Path Abs Dir -> Path Abs File -> Maybe (Path Rel File))

    it "produces a valid path on when passed a valid absolute directory paths" $ do
        producesValidsOnValids2 (stripDir :: Path Abs Dir -> Path Abs Dir -> Maybe (Path Rel Dir))

    it "produces a valid path on when passed a valid relative file paths" $ do
        producesValidsOnValids2 (stripDir :: Path Rel Dir -> Path Rel File-> Maybe (Path Rel File))

    it "produces a valid path on when passed a valid relative directory paths" $ do
        producesValidsOnValids2 (stripDir :: Path Rel Dir -> Path Rel Dir -> Maybe (Path Rel Dir))
  where
    prop_stripDir_append
        :: forall b t. (Arbitrary (Path b Dir), Arbitrary (Path Rel t))
        => String -> Proxy b -> String -> Proxy t -> Spec
    prop_stripDir_append parentBase _ childType _ = do
        let p = parentBase <> "dir"
            c = "rel" <> childType
        prop ("stripDir " <> p <> " (" <> p <> " </> " <> c <> ") == Just " <> c) $
            \(parent :: Path b Dir) (child :: Path Rel t) ->
                stripDir parent (parent </> child) == Just child

-- | The '</>' operation.
operationAppend :: Spec
operationAppend = do
    prop_append "abs" (Proxy :: Proxy Abs) "file" (Proxy :: Proxy File)
    prop_append "abs" (Proxy :: Proxy Abs) "dir"  (Proxy :: Proxy Dir)
    prop_append "rel" (Proxy :: Proxy Rel) "file" (Proxy :: Proxy File)
    prop_append "rel" (Proxy :: Proxy Rel) "dir"  (Proxy :: Proxy Dir)
  where
    prop_append
        :: forall b t. (Arbitrary (Path b Dir), Arbitrary (Path Rel t), Validity (Path b t))
        => String -> Proxy b -> String -> Proxy t -> Spec
    prop_append parentBase _ childType _ = do
        let p = parentBase <> "dir"
            c = "rel" <> childType
        prop ("isValid (" <> p <> " </> " <> c <> ")") $
            \(parent :: Path b Dir) (child :: Path Rel t) ->
                isValid (parent </> child)

parserSpec :: Spec
parserSpec = do
    parsedPathIsValid "parseAbsDir"  parseAbsDir
    parsedPathIsValid "parseAbsFile" parseAbsFile
    parsedPathIsValid "parseRelDir"  parseRelDir
    parsedPathIsValid "parseRelFile" parseRelFile
  where
    parsedPathIsValid
        :: (Show p, Validity p)
        => String -> (FilePath -> Maybe p) -> Spec
    parsedPathIsValid pname parser =
        it ("(isValid <$> " <> pname <> " fp) `elem` [Nothing, Just True]") $
            forAllShrink
                (genFilePath `suchThat` (isJust . parser)) -- Make sure we can parse the FilePath so QC doesn't give up
                shrink $ -- TODO: Use a better shrinking function if available
                \fp -> (isValid <$> parser fp) == Just True
