-- | Support for well-typed paths.

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Path
  (-- * Types
   Path
  ,PathRelativity(..)
  ,FileType(..)
  -- * QuasiQuoters
  -- | Using the following requires the QuasiQuotes language extension.
  --
  -- __For Windows users__, the QuasiQuoters are especially beneficial because they
  -- prevent Haskell from treating @\\@ as an escape character.
  -- This makes Windows paths easier to write.
  --
  -- @
  -- [absfile|C:\\chris\\foo.txt|]
  -- @
  ,absdir
  ,reldir
  ,absfile
  ,relfile
  -- * Operations
  ,(</>)
  ,stripDir
  ,isParentOf
  ,parent
  ,filename
  ,dirname
  ,fileExtension
  ,setFileExtension
   -- * Parsing
  ,parseAbsDir
  ,parseRelDir
  ,parseAbsFile
  ,parseRelFile
  ,PathParseException
  -- * Conversion
  ,toFilePath
  ,fromAbsDir
  ,fromRelDir
  ,fromAbsFile
  ,fromRelFile
  -- * TemplateHaskell constructors
  -- | These require the TemplateHaskell language extension.
  ,mkAbsDir
  ,mkRelDir
  ,mkAbsFile
  ,mkRelFile
  )
  where

import           Control.Monad.Catch (MonadThrow(..))
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Path.Internal
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------
-- QuasiQuoters

qq :: (String -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
  { quoteExp  = quoteExp'
  , quotePat  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
  }

-- | Construct a 'Path' 'Abs' 'Dir' using QuasiQuotes.
--
-- @
-- [absdir|/|]
--
-- [absdir|\/home\/chris|]
-- @
--
-- Remember: due to the nature of absolute paths a path like @[absdir|\/home\/chris|]@
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
--
-- @since 0.5.13
absdir :: QuasiQuoter
absdir = qq mkAbsDir

-- | Construct a 'Path' 'Rel' 'Dir' using QuasiQuotes.
--
-- @
-- [absdir|\/home|]\<\/>[reldir|chris|]
-- @
--
-- @since 0.5.13
reldir :: QuasiQuoter
reldir = qq mkRelDir

-- | Construct a 'Path' 'Abs' 'File' using QuasiQuotes.
--
-- @
-- [absfile|\/home\/chris\/foo.txt|]
-- @
--
-- Remember: due to the nature of absolute paths a path like @[absdir|\/home\/chris\/foo.txt|]@
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
--
-- @since 0.5.13
absfile :: QuasiQuoter
absfile = qq mkAbsFile

-- | Construct a 'Path' 'Rel' 'File' using QuasiQuotes.
--
-- @
-- [absdir|\/home\/chris|]\<\/>[relfile|foo.txt|]
-- @
--
-- @since 0.5.13
relfile :: QuasiQuoter
relfile = qq mkRelFile

--------------------------------------------------------------------------------
-- Operations

-- | Append two paths.
--
-- The following cases are valid and the equalities hold:
--
-- @$(mkAbsDir x) \<\/> $(mkRelDir y) = $(mkAbsDir (x ++ \"/\" ++ y))@
--
-- @$(mkAbsDir x) \<\/> $(mkRelFile y) = $(mkAbsFile (x ++ \"/\" ++ y))@
--
-- @$(mkRelDir x) \<\/> $(mkRelDir y) = $(mkRelDir (x ++ \"/\" ++ y))@
--
-- @$(mkRelDir x) \<\/> $(mkRelFile y) = $(mkRelFile (x ++ \"/\" ++ y))@
--
-- The following are proven not possible to express:
--
-- @$(mkAbsFile …) \<\/> x@
--
-- @$(mkRelFile …) \<\/> x@
--
-- @x \<\/> $(mkAbsFile …)@
--
-- @x \<\/> $(mkAbsDir …)@
--
(</>) :: Path b 'Dir -> Path 'Rel t -> Path b t
(</>) (Path a) (Path b) = Path (a ++ b)

-- | Strip directory from path, making it relative to that directory.
-- Throws 'Couldn'tStripPrefixDir' if directory is not a parent of the path.
--
-- The following properties hold:
--
-- @stripDir x (x \<\/> y) = y@
--
-- Cases which are proven not possible:
--
-- @stripDir (a :: Path Abs …) (b :: Path Rel …)@
--
-- @stripDir (a :: Path Rel …) (b :: Path Abs …)@
--
-- In other words the bases must match.
--
stripDir :: MonadThrow m
         => Path b 'Dir -> Path b t -> m (Path 'Rel t)
stripDir (Path p) (Path l) =
  case stripPrefix p l of
    Nothing -> throwM (Couldn'tStripPrefixDir p l)
    Just "" -> throwM (Couldn'tStripPrefixDir p l)
    Just ok -> return (Path ok)

-- | Is p a parent of the given location? Implemented in terms of
-- 'stripDir'. The bases must match.
--
-- The following properties hold:
--
-- @not (x \`isParentOf\` x)@
--
-- @x \`isParentOf\` (x \<\/\> y)@
--
isParentOf :: Path b 'Dir -> Path b t -> Bool
isParentOf p l =
  isJust (stripDir p l)

-- | Take the absolute parent directory from the absolute path.
--
-- The following properties hold:
--
-- @parent (x \<\/> y) == x@
--
-- On the root, getting the parent is idempotent:
--
-- @parent (parent \"\/\") = \"\/\"@
--
parent :: Path 'Abs t -> Path 'Abs 'Dir
parent (Path fp) =
  Path (normalizeDir (FilePath.takeDirectory (FilePath.dropTrailingPathSeparator fp)))

-- | Extract the file part of a path.
--
-- The following properties hold:
--
-- @filename (p \<\/> a) == filename a@
--
filename :: Path b 'File -> Path 'Rel 'File
filename (Path l) =
  Path (FilePath.takeFileName l)

-- | Extract the last directory name of a path.
--
-- The following properties hold:
--
-- @dirname (p \<\/> a) == dirname a@
--
dirname :: Path b 'Dir -> Path 'Rel 'Dir
dirname (Path l) =
  Path (last (FilePath.splitPath l))

-- | Get extension from given file path.
--
-- @since 0.5.11
fileExtension :: Path b 'File -> String
fileExtension = FilePath.takeExtension . toFilePath

-- | Replace\/add extension to given file path. Throws if the
-- resulting filename does not parse.
--
-- @since 0.5.11
setFileExtension :: MonadThrow m
  => String            -- ^ Extension to set
  -> Path b 'File       -- ^ Old file name
  -> m (Path b 'File)   -- ^ New file name with the desired extension
setFileExtension ext (Path path) =
  if FilePath.isAbsolute path
    then fmap coercePath (parseAbsFile (FilePath.replaceExtension path ext))
    else fmap coercePath (parseRelFile (FilePath.replaceExtension path ext))
  where coercePath :: Path a b -> Path a' b'
        coercePath (Path a) = Path a


--------------------------------------------------------------------------------
-- Conversion

-- | Convert to a 'FilePath' type.
--
-- All directories have a trailing slash, so if you want no trailing
-- slash, you can use 'System.FilePath.dropTrailingPathSeparator' from
-- the filepath package.
toFilePath :: Path b t -> FilePath
toFilePath (Path l) = l

-- | Convert absolute path to directory to 'FilePath' type.
fromAbsDir :: Path 'Abs 'Dir -> FilePath
fromAbsDir = toFilePath

-- | Convert relative path to directory to 'FilePath' type.
fromRelDir :: Path 'Rel 'Dir -> FilePath
fromRelDir = toFilePath

-- | Convert absolute path to file to 'FilePath' type.
fromAbsFile :: Path 'Abs 'File -> FilePath
fromAbsFile = toFilePath

-- | Convert relative path to file to 'FilePath' type.
fromRelFile :: Path 'Rel 'File -> FilePath
fromRelFile = toFilePath

--------------------------------------------------------------------------------
-- Constructors

-- | Make a 'Path' 'Abs' Dir'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsDir :: FilePath -> Q Exp
mkAbsDir s =
  case parseAbsDir s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path 'Abs 'Dir|]

-- | Make a 'Path' 'Rel' 'Dir'.
mkRelDir :: FilePath -> Q Exp
mkRelDir s =
  case parseRelDir s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path 'Rel 'Dir|]

-- | Make a 'Path' 'Abs' 'File'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsFile :: FilePath -> Q Exp
mkAbsFile s =
  case parseAbsFile s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path 'Abs 'File|]

-- | Make a 'Path' 'Rel' 'File'.
mkRelFile :: FilePath -> Q Exp
mkRelFile s =
  case parseRelFile s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path 'Rel 'File|]
