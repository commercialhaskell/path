-- | Support for well-typed paths.

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}

module Path
  (-- * Types
   Path
  ,Abs
  ,Rel
  ,File
  ,Dir
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

import           Control.Exception (Exception)
import           Control.Monad
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Aeson (FromJSON (..))
import qualified Data.Aeson.Types as Aeson
import           Data.Data
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Path.Internal
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------
-- Types

-- | An absolute path.
data Abs deriving (Typeable)

-- | A relative path; one without a root. Note that a @..@ path component to
-- represent the parent directory is not allowed by this library.
data Rel deriving (Typeable)

-- | A file path.
data File deriving (Typeable)

-- | A directory path.
data Dir deriving (Typeable)

instance FromJSON (Path Abs File) where
  parseJSON = parseJSONWith parseAbsFile
  {-# INLINE parseJSON #-}

instance FromJSON (Path Rel File) where
  parseJSON = parseJSONWith parseRelFile
  {-# INLINE parseJSON #-}

instance FromJSON (Path Abs Dir) where
  parseJSON = parseJSONWith parseAbsDir
  {-# INLINE parseJSON #-}

instance FromJSON (Path Rel Dir) where
  parseJSON = parseJSONWith parseRelDir
  {-# INLINE parseJSON #-}

parseJSONWith :: (Show e, FromJSON a)
              => (a -> Either e b) -> Aeson.Value -> Aeson.Parser b
parseJSONWith f x =
  do fp <- parseJSON x
     case f fp of
       Right p -> return p
       Left e -> fail (show e)
{-# INLINE parseJSONWith #-}

-- | Exception when parsing a location.
data PathParseException
  = InvalidAbsDir FilePath
  | InvalidRelDir FilePath
  | InvalidAbsFile FilePath
  | InvalidRelFile FilePath
  | Couldn'tStripPrefixDir FilePath FilePath
  deriving (Show,Typeable)
instance Exception PathParseException


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
(</>) :: Path b Dir -> Path Rel t -> Path b t
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
         => Path b Dir -> Path b t -> m (Path Rel t)
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
isParentOf :: Path b Dir -> Path b t -> Bool
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
parent :: Path Abs t -> Path Abs Dir
parent (Path fp) =
  Path (normalizeDir (FilePath.takeDirectory (FilePath.dropTrailingPathSeparator fp)))

-- | Extract the file part of a path.
--
-- The following properties hold:
--
-- @filename (p \<\/> a) == filename a@
--
filename :: Path b File -> Path Rel File
filename (Path l) =
  Path (FilePath.takeFileName l)

-- | Extract the last directory name of a path.
--
-- The following properties hold:
--
-- @dirname $(mkRelDir ".") == $(mkRelDir ".")@
--
-- @dirname (p \<\/> a) == dirname a@
--
dirname :: Path b Dir -> Path Rel Dir
dirname (Path "") = Path ""
dirname (Path l) | FilePath.isDrive l = Path ""
dirname (Path l) = Path (last (FilePath.splitPath l))

-- | Get extension from given file path.
--
-- @since 0.5.11
fileExtension :: Path b File -> String
fileExtension = FilePath.takeExtension . toFilePath

-- | Replace\/add extension to given file path. Throws if the
-- resulting filename does not parse.
--
-- @since 0.5.11
setFileExtension :: MonadThrow m
  => String            -- ^ Extension to set
  -> Path b File       -- ^ Old file name
  -> m (Path b File)   -- ^ New file name with the desired extension
setFileExtension ext (Path path) =
  if FilePath.isAbsolute path
    then liftM coercePath (parseAbsFile (FilePath.replaceExtension path ext))
    else liftM coercePath (parseRelFile (FilePath.replaceExtension path ext))
  where coercePath :: Path a b -> Path a' b'
        coercePath (Path a) = Path a


--------------------------------------------------------------------------------
-- Parsers

-- | Convert an absolute 'FilePath' to a normalized absolute dir 'Path'.
--
-- Throws: 'PathParseException' when the supplied path:
--
-- * is not an absolute path
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseAbsDir :: MonadThrow m
            => FilePath -> m (Path Abs Dir)
parseAbsDir filepath =
  if FilePath.isAbsolute filepath &&
     not (hasParentDir filepath) &&
     FilePath.isValid filepath
     then return (Path (normalizeDir filepath))
     else throwM (InvalidAbsDir filepath)

-- | Convert a relative 'FilePath' to a normalized relative dir 'Path'.
--
-- Throws: 'PathParseException' when the supplied path:
--
-- * is not a relative path
-- * is @""@
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseRelDir :: MonadThrow m
            => FilePath -> m (Path Rel Dir)
parseRelDir filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (hasParentDir filepath) &&
     not (null filepath) &&
     FilePath.isValid filepath
     then return (Path (normalizeRelDir filepath))
     else throwM (InvalidRelDir filepath)
  where
      normalizeRelDir = toRelDirPath . normalizeDir
      -- Represent a "." in relative dir path as "" internally so that it
      -- composes without having to renormalize the path.
      toRelDirPath p | p == curDirNormalizedFP = ""
      toRelDirPath p = p

-- | Convert an absolute 'FilePath' to a normalized absolute file 'Path'.
--
-- Throws: 'PathParseException' when the supplied path:
--
-- * is not an absolute path
-- * is a directory path i.e.
--
--     * has a trailing path separator
--     * is @.@ or ends in @/.@
--
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseAbsFile :: MonadThrow m
             => FilePath -> m (Path Abs File)
parseAbsFile filepath =
  case validAbsFile filepath of
    True
      | normalized <- normalizeFilePath filepath
      , validAbsFile normalized ->
        return (Path normalized)
    _ -> throwM (InvalidAbsFile filepath)

-- | Is the string a valid absolute file?
validAbsFile :: FilePath -> Bool
validAbsFile filepath =
  FilePath.isAbsolute filepath &&
  not (FilePath.hasTrailingPathSeparator filepath) &&
  not (hasParentDir filepath) &&
  FilePath.isValid filepath

-- | Convert a relative 'FilePath' to a normalized relative file 'Path'.
--
-- Throws: 'PathParseException' when the supplied path:
--
-- * is not a relative path
-- * is @""@
-- * is a directory path i.e.
--
--     * has a trailing path separator
--     * is @.@ or ends in @/.@
--
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseRelFile :: MonadThrow m
             => FilePath -> m (Path Rel File)
parseRelFile filepath =
  case validRelFile filepath of
    True
      | normalized <- normalizeFilePath filepath
      , validRelFile normalized -> return (Path normalized)
    _ -> throwM (InvalidRelFile filepath)

-- | Is the string a valid relative file?
validRelFile :: FilePath -> Bool
validRelFile filepath =
  not
    (FilePath.isAbsolute filepath || FilePath.hasTrailingPathSeparator filepath) &&
  not (null filepath) &&
  not (hasParentDir filepath) &&
  filepath /= "." && FilePath.isValid filepath

--------------------------------------------------------------------------------
-- Conversion

-- | Convert absolute path to directory to 'FilePath' type.
fromAbsDir :: Path Abs Dir -> FilePath
fromAbsDir = toFilePath

-- | Convert relative path to directory to 'FilePath' type.
fromRelDir :: Path Rel Dir -> FilePath
fromRelDir = toFilePath

-- | Convert absolute path to file to 'FilePath' type.
fromAbsFile :: Path Abs File -> FilePath
fromAbsFile = toFilePath

-- | Convert relative path to file to 'FilePath' type.
fromRelFile :: Path Rel File -> FilePath
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
      [|Path $(return (LitE (StringL str))) :: Path Abs Dir|]

-- | Make a 'Path' 'Rel' 'Dir'.
mkRelDir :: FilePath -> Q Exp
mkRelDir s =
  case parseRelDir s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path Rel Dir|]

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
      [|Path $(return (LitE (StringL str))) :: Path Abs File|]

-- | Make a 'Path' 'Rel' 'File'.
mkRelFile :: FilePath -> Q Exp
mkRelFile s =
  case parseRelFile s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path Rel File|]


--------------------------------------------------------------------------------
-- Internal functions

-- | Internal use for normalizing a directory.
normalizeDir :: FilePath -> FilePath
normalizeDir = FilePath.addTrailingPathSeparator . normalizeFilePath

normalizeFilePath :: FilePath -> FilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__) || MIN_VERSION_filepath(1,4,0)
normalizeFilePath = FilePath.normalise
#else
normalizeFilePath = normalizeLeadingSeparators . FilePath.normalise
    where
        sep = FilePath.pathSeparator
        normalizeLeadingSeparators (x1:x2:xs) | x1 == sep && x2 == sep
            = normalizeLeadingSeparators (sep:xs)
        normalizeLeadingSeparators x = x
#endif

