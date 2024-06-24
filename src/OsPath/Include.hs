-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     IS_WINDOWS    = 0 | 1

-- | This library provides a well-typed representation of paths in a filesystem
-- directory tree.
--
-- __Note__: This module is for working with PLATFORM_NAME style paths. Importing
-- "Path" is usually better.
--
-- A path is represented by a number of path components separated by a path
-- separator which is a @/@ on POSIX systems and can be a @/@ or @\\@ on Windows.
-- The root of the tree is represented by a @/@ on POSIX and a drive letter
-- followed by a @/@ or @\\@ on Windows (e.g. @C:\\@).  Paths can be absolute
-- or relative. An absolute path always starts from the root of the tree (e.g.
-- @\/x/y@) whereas a relative path never starts with the root (e.g. @x/y@).
-- Just like we represent the notion of an absolute root by "@/@", the same way
-- we represent the notion of a relative root by "@.@". The relative root denotes
-- the directory which contains the first component of a relative path.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module OsPath.PLATFORM_NAME
  (-- * Types
   Path
  ,Abs
  ,Rel
  ,File
  ,Dir
  ,SomeBase(..)
   -- * Exceptions
  ,PathException(..)
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
  ,stripProperPrefix
  ,isProperPrefixOf
  ,replaceProperPrefix
  ,parent
  ,filename
  ,dirname
  ,addExtension
  ,splitExtension
  ,fileExtension
  ,replaceExtension
  ,splitDrive
  ,takeDrive
  ,dropDrive
  ,isDrive
  ,mapSomeBase
  ,prjSomeBase
   -- * Parsing
  ,parseAbsDir
  ,parseRelDir
  ,parseAbsFile
  ,parseRelFile
  ,parseSomeDir
  ,parseSomeFile
  -- * Conversion
  ,toOsPath
  ,fromAbsDir
  ,fromRelDir
  ,fromAbsFile
  ,fromRelFile
  ,fromSomeDir
  ,fromSomeFile
  -- * TemplateHaskell constructors
  -- | These require the TemplateHaskell language extension.
  ,mkAbsDir
  ,mkRelDir
  ,mkAbsFile
  ,mkRelFile
  -- * Deprecated
  ,PathParseException
  ,stripDir
  ,isParentOf
  ,addFileExtension
  ,(<.>)
  ,setFileExtension
  ,(-<.>)
  )
  where

import           Control.Applicative (Alternative(..))
import           Control.DeepSeq (NFData (..))
import           Control.Exception (Exception(..))
import           Control.Monad (liftM, when, (<=<))
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Aeson (FromJSON (..), FromJSONKey(..), ToJSON(..))
import qualified Data.Aeson.Types as Aeson
import           Data.Data (Data, Typeable)
import qualified Data.Text as Text
import           Data.Hashable (Hashable (..))
import           Data.Maybe (isJust, isNothing)
import           GHC.Generics (Generic)
import           Language.Haskell.TH (Exp, Q)
import           Language.Haskell.TH.Syntax (lift)
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           System.OsPath.PLATFORM_NAME (PLATFORM_PATH)
import qualified System.OsPath.PLATFORM_NAME as OsPath
import           System.OsString.PLATFORM_NAME (PLATFORM_STRING)
import qualified System.OsString.PLATFORM_NAME as OsString

import           OsPath.Internal.PLATFORM_NAME

--------------------------------------------------------------------------------
-- Types

-- | An absolute path.
data Abs deriving (Typeable, Data)

-- | A relative path; one without a root. Note that a @..@ path component to
-- represent the parent directory is not allowed by this library.
data Rel deriving (Typeable, Data)

-- | A file path.
data File deriving (Typeable, Data)

-- | A directory path.
data Dir deriving (Typeable, Data)

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

parseJSONWith :: (forall m . MonadThrow m => PLATFORM_PATH -> m a)
              -> Aeson.Value
              -> Aeson.Parser a
parseJSONWith f x =
  do fp <- parseJSON x
     either (fail . displayException) return $ do
       ospath <- OsString.encodeUtf fp
       f ospath
{-# INLINE parseJSONWith #-}

instance FromJSONKey (Path Abs File) where
  fromJSONKey = fromJSONKeyWith parseAbsFile
  {-# INLINE fromJSONKey #-}

instance FromJSONKey (Path Rel File) where
  fromJSONKey = fromJSONKeyWith parseRelFile
  {-# INLINE fromJSONKey #-}

instance FromJSONKey (Path Abs Dir) where
  fromJSONKey = fromJSONKeyWith parseAbsDir
  {-# INLINE fromJSONKey #-}

instance FromJSONKey (Path Rel Dir) where
  fromJSONKey = fromJSONKeyWith parseRelDir
  {-# INLINE fromJSONKey #-}

fromJSONKeyWith :: (forall m . MonadThrow m => PLATFORM_PATH -> m b)
                -> Aeson.FromJSONKeyFunction b
fromJSONKeyWith f =
  Aeson.FromJSONKeyTextParser $ \text ->
    either (fail . displayException) return $ do
      ospath <- (OsPath.encodeUtf . Text.unpack) text
      f ospath
{-# INLINE fromJSONKeyWith #-}

-- | Exceptions that can occur during path operations.
--
-- @since 0.6.0
data PathException
  = InvalidAbsDir PLATFORM_PATH
  | InvalidRelDir PLATFORM_PATH
  | InvalidAbsFile PLATFORM_PATH
  | InvalidRelFile PLATFORM_PATH
  | InvalidFile PLATFORM_PATH
  | InvalidDir PLATFORM_PATH
  | NotAProperPrefix PLATFORM_PATH PLATFORM_PATH
  | HasNoExtension PLATFORM_PATH
  | InvalidExtension PLATFORM_STRING
  deriving (Show,Eq,Typeable)

instance Exception PathException where
  displayException (InvalidExtension ext) = concat
    [ "Invalid extension "
    , show ext
    , ". A valid extension starts with a '.' followed by one or more "
    , "characters other than '.', and it must be a valid filename, "
    , "notably it cannot include a path separator."
    ]
  displayException x = show x

--------------------------------------------------------------------------------
-- QuasiQuoters

qq :: (PLATFORM_PATH -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
  { quoteExp  = quoteExp' <=< OsPath.encodeUtf
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
-- @$(mkAbsDir x) \<\/> $(mkRelDir y) = $(mkAbsDir (x <> [pstr|/|] <> y))@
--
-- @$(mkAbsDir x) \<\/> $(mkRelFile y) = $(mkAbsFile (x <> [pstr|/|] <> y))@
--
-- @$(mkRelDir x) \<\/> $(mkRelDir y) = $(mkRelDir (x <> [pstr|/|] <> y))@
--
-- @$(mkRelDir x) \<\/> $(mkRelFile y) = $(mkRelFile (x <> [pstr|/|] <> y))@
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
infixr 5 </>
(</>) :: Path b Dir -> Path Rel t -> Path b t
(</>) (Path a) (Path b) = Path (a <> b)

-- | If the directory in the first argument is a proper prefix of the path in
-- the second argument strip it from the second argument, generating a path
-- relative to the directory.
-- Throws 'NotAProperPrefix' if the directory is not a proper prefix of the
-- path.
--
-- The following properties hold:
--
-- @stripProperPrefix x (x \<\/> y) = y@
--
-- Cases which are proven not possible:
--
-- @stripProperPrefix (a :: Path Abs …) (b :: Path Rel …)@
--
-- @stripProperPrefix (a :: Path Rel …) (b :: Path Abs …)@
--
-- In other words the bases must match.
--
-- @since 0.6.0
stripProperPrefix :: MonadThrow m
         => Path b Dir -> Path b t -> m (Path Rel t)
stripProperPrefix (Path p) (Path l) =
  case OsString.stripPrefix p l of
    Nothing -> throwM (NotAProperPrefix p l)
    Just result
      | OsString.null result -> throwM (NotAProperPrefix p l)
      | otherwise -> return (Path result)

-- | Determines if the path in the first parameter is a proper prefix of the
-- path in the second parameter.
--
-- The following properties hold:
--
-- @not (x \`isProperPrefixOf\` x)@
--
-- @x \`isProperPrefixOf\` (x \<\/\> y)@
--
-- @since 0.6.0
isProperPrefixOf :: Path b Dir -> Path b t -> Bool
isProperPrefixOf p l = isJust (stripProperPrefix p l)

-- | Change from one directory prefix to another.
--
-- Throw 'NotAProperPrefix' if the first argument is not a proper prefix of the
-- path.
--
-- >>> replaceProperPrefix $(mkRelDir "foo") $(mkRelDir "bar") $(mkRelFile "foo/file.txt") == $(mkRelFile "bar/file.txt")
replaceProperPrefix :: MonadThrow m => Path b Dir -> Path b' Dir -> Path b t -> m (Path b' t)
replaceProperPrefix src dst fp = (dst </>) <$> stripProperPrefix src fp

-- | Take the parent path component from a path.
--
-- The following properties hold:
--
-- @
-- parent (x \<\/> y) == x
-- parent [pstr|\/x|] == [pstr|\/|]
-- parent [pstr|x|] == [pstr|.|]
-- @
--
-- On the root (absolute or relative), getting the parent is idempotent:
--
-- @
-- parent [pstr|\/|] = [pstr|\/|]
-- parent [pstr|\.|] = [pstr|\.|]
-- @
--
parent :: Path b t -> Path b Dir
parent (Path fp)
  | OsString.null fp = Path OsString.empty
  | OsPath.isDrive fp = Path fp
  | otherwise =
      Path
      $ normalizeDir
      $ OsPath.takeDirectory
      $ OsPath.dropTrailingPathSeparator fp

-- | Split an absolute path into a drive and, perhaps, a path. On POSIX, @/@ is
-- a drive.
splitDrive :: Path Abs t -> (Path Abs Dir, Maybe (Path Rel t))
splitDrive (Path fp) =
    let (d, rest) = OsPath.splitDrive fp
        mRest = if OsString.null rest then Nothing else Just (Path rest)
    in  (Path d, mRest)

-- | Get the drive from an absolute path. On POSIX, @/@ is a drive.
--
-- > takeDrive x = fst (splitDrive x)
takeDrive :: Path Abs t -> Path Abs Dir
takeDrive = fst . splitDrive

-- | Drop the drive from an absolute path. May result in 'Nothing' if the path
-- is just a drive.
--
-- > dropDrive x = snd (splitDrive x)
dropDrive :: Path Abs t -> Maybe (Path Rel t)
dropDrive = snd . splitDrive

-- | Is an absolute directory path a drive?
isDrive :: Path Abs Dir -> Bool
isDrive = isNothing . dropDrive

-- | Extract the file part of a path.
--
-- The following properties hold:
--
-- @filename (p \<\/> a) == filename a@
--
filename :: Path b File -> Path Rel File
filename (Path l) =
  Path (OsPath.takeFileName l)

-- | Extract the last directory name of a path.
--
-- The following properties hold:
--
-- @dirname $(mkRelDir ".") == $(mkRelDir ".")@
--
-- @dirname (p \<\/> a) == dirname a@
--
dirname :: Path b Dir -> Path Rel Dir
dirname (Path l)
  | OsString.null l = Path OsString.empty
  | OsPath.isDrive l = Path OsString.empty
  | otherwise = Path (last (OsPath.splitPath l))

-- | 'splitExtension' is the inverse of 'addExtension'. It splits the given
-- file path into a valid filename and a valid extension.
--
-- >>> splitExtension $(mkRelFile "name.foo"     ) == Just ($(mkRelFile "name"    ), [pstr|.foo|]  )
-- >>> splitExtension $(mkRelFile "name.foo."    ) == Just ($(mkRelFile "name"    ), [pstr|.foo.|] )
-- >>> splitExtension $(mkRelFile "name.foo.."   ) == Just ($(mkRelFile "name"    ), [pstr|.foo..|])
-- >>> splitExtension $(mkRelFile "name.bar.foo" ) == Just ($(mkRelFile "name.bar"), [pstr|.foo|]  )
-- >>> splitExtension $(mkRelFile ".name.foo"    ) == Just ($(mkRelFile ".name"   ), [pstr|.foo|]  )
-- >>> splitExtension $(mkRelFile "name..foo"    ) == Just ($(mkRelFile "name."   ), [pstr|.foo|]  )
-- >>> splitExtension $(mkRelFile "....foo"      ) == Just ($(mkRelFile "..."     ), [pstr|.foo|]  )
--
-- Throws 'HasNoExtension' exception if the filename does not have an extension
-- or in other words it cannot be split into a valid filename and a valid
-- extension. The following cases throw an exception, please note that "." and
-- ".." are not valid filenames:
--
-- >>> splitExtension $(mkRelFile "name"   )
-- >>> splitExtension $(mkRelFile "name."  )
-- >>> splitExtension $(mkRelFile "name.." )
-- >>> splitExtension $(mkRelFile ".name"  )
-- >>> splitExtension $(mkRelFile "..name" )
-- >>> splitExtension $(mkRelFile "...name")
--
-- 'splitExtension' and 'addExtension' are inverses of each other, the
-- following laws hold:
--
-- @
-- uncurry addExtension . swap >=> splitExtension == return
-- splitExtension >=> uncurry addExtension . swap == return
-- @
--
-- @since 0.7.0
splitExtension :: MonadThrow m => Path b File -> m (Path b File, PLATFORM_STRING)
splitExtension (Path ospath) =
    if OsString.null nameDot
       || OsString.null name
       || OsString.null ext
       || name == [OsString.pstr|.|]
       || name == [OsString.pstr|..|]
       then throwM $ HasNoExtension ospath
       else return ( Path (normalizeDrive drv <> dir <> name)
                   , OsString.singleton OsPath.extSeparator <> ext
                   )
    where

    -- trailing separators are ignored for the split and considered part of the
    -- second component in the split.
    splitLast isSep str =
        let (withoutTrailingSeps, trailingSeps) = OsString.spanEnd isSep str
            (oneSep, rest) = OsString.breakEnd isSep withoutTrailingSeps
        in (oneSep, rest <> trailingSeps)

    (drv, ospathRel) = OsPath.splitDrive ospath
    (dir, file)      = splitLast OsPath.isPathSeparator ospathRel
    (nameDot, ext)   = splitLast OsPath.isExtSeparator file
    name             = OsString.init nameDot

-- | Get extension from given file path. Throws 'HasNoExtension' exception if
-- the file does not have an extension.  The following laws hold:
--
-- @
-- flip addExtension file >=> fileExtension == return
-- fileExtension == (fmap snd) . splitExtension
-- @
--
-- @since 0.5.11
fileExtension :: MonadThrow m => Path b File -> m PLATFORM_STRING
fileExtension = (liftM snd) . splitExtension

-- | Add extension to given file path.
--
-- >>> addExtension [pstr|.foo|]   $(mkRelFile "name"     ) == Just $(mkRelFile "name.foo"    )
-- >>> addExtension [pstr|.foo.|]  $(mkRelFile "name"     ) == Just $(mkRelFile "name.foo."   )
-- >>> addExtension [pstr|.foo..|] $(mkRelFile "name"     ) == Just $(mkRelFile "name.foo.."  )
-- >>> addExtension [pstr|.foo|]   $(mkRelFile "name.bar" ) == Just $(mkRelFile "name.bar.foo")
-- >>> addExtension [pstr|.foo|]   $(mkRelFile ".name"    ) == Just $(mkRelFile ".name.foo"   )
-- >>> addExtension [pstr|.foo|]   $(mkRelFile "name."    ) == Just $(mkRelFile "name..foo"   )
-- >>> addExtension [pstr|.foo|]   $(mkRelFile "..."      ) == Just $(mkRelFile "....foo"     )
--
-- Throws an 'InvalidExtension' exception if the extension is not valid. A
-- valid extension starts with a @.@ followed by one or more characters not
-- including @.@ followed by zero or more @.@ in trailing position. Moreover,
-- an extension must be a valid filename, notably it cannot include path
-- separators. Particularly, @.foo.bar@ is an invalid extension, instead you
-- have to first set @.foo@ and then @.bar@ individually. Some examples of
-- invalid extensions are:
--
-- >>> addExtension [pstr|foo|]      $(mkRelFile "name")
-- >>> addExtension [pstr|..foo|]    $(mkRelFile "name")
-- >>> addExtension [pstr|.foo.bar|] $(mkRelFile "name")
-- >>> addExtension [pstr|.foo/bar|] $(mkRelFile "name")
--
-- @since 0.7.0
addExtension :: MonadThrow m
  => PLATFORM_STRING   -- ^ Extension to add
  -> Path b File       -- ^ Old file name
  -> m (Path b File)   -- ^ New file name with the desired extension added at the end
addExtension ext (Path path) = do
    (sep, xtn) <- case OsString.uncons ext of
        Nothing -> throwM $ InvalidExtension ext
        Just result -> pure result

    let withoutTrailingSeps = OsString.dropWhileEnd OsPath.isExtSeparator xtn

    -- Has to start with a "."
    when (not $ OsPath.isExtSeparator sep) $
        throwM $ InvalidExtension ext

    -- Cannot have path separators
    when (OsString.any OsPath.isPathSeparator xtn) $
        throwM $ InvalidExtension ext

    -- All "."s is not a valid extension
    when (OsString.null withoutTrailingSeps) $
        throwM $ InvalidExtension ext

    -- Cannot have "."s except in trailing position
    when (OsString.any OsPath.isExtSeparator withoutTrailingSeps) $
        throwM $ InvalidExtension ext

    -- Must be valid as a filename
    _ <- parseRelFile ext

    return $ Path (path <> ext)

-- | If the file has an extension replace it with the given extension otherwise
-- add the new extension to it. Throws an 'InvalidExtension' exception if the
-- new extension is not a valid extension (see 'fileExtension' for validity
-- rules).
--
-- The following law holds:
--
-- @(fileExtension >=> flip replaceExtension file) file == return file@
--
-- @since 0.7.0
replaceExtension :: MonadThrow m
  => PLATFORM_STRING   -- ^ Extension to set
  -> Path b File       -- ^ Old file name
  -> m (Path b File)   -- ^ New file name with the desired extension
replaceExtension ext path =
    addExtension ext (maybe path fst $ splitExtension path)

--------------------------------------------------------------------------------
-- Parsers

-- | Convert an absolute PLATFORM_PATH_SINGLE to a normalized absolute dir
--   'Path'.
--
-- Throws: 'InvalidAbsDir' when the supplied path:
--
-- * is not an absolute path
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'OsPath.isValid')
--
parseAbsDir :: MonadThrow m
            => PLATFORM_PATH -> m (Path Abs Dir)
parseAbsDir ospath
  | isValidAbsDir ospath = return (Path (normalizeDir ospath))
  | otherwise = throwM (InvalidAbsDir ospath)

-- | Convert a relative PLATFORM_PATH_SINGLE to a normalized relative dir
--   'Path'.
--
-- Throws: 'InvalidRelDir' when the supplied path:
--
-- * is not a relative path
-- * is @""@
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'OsPath.isValid')
-- * is all path separators
--
parseRelDir :: MonadThrow m
            => PLATFORM_PATH -> m (Path Rel Dir)
parseRelDir ospath
  | isValidRelDir ospath = return (Path (normalizeDir ospath))
  | otherwise = throwM (InvalidRelDir ospath)

-- | Convert an absolute PLATFORM_PATH_SINGLE to a normalized absolute file
--   'Path'.
--
-- Throws: 'InvalidAbsFile' when the supplied path:
--
-- * is not an absolute path
-- * is a directory path i.e.
--
--     * has a trailing path separator
--     * is @.@ or ends in @/.@
--
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'OsPath.isValid')
--
parseAbsFile :: MonadThrow m
             => PLATFORM_PATH -> m (Path Abs File)
parseAbsFile ospath
  | isValidAbsFile ospath
  , let normalized = normalizeFile ospath
  , isValidAbsFile normalized = return (Path normalized)
  | otherwise = throwM (InvalidAbsFile ospath)

-- | Convert a relative PLATFORM_PATH_SINGLE to a normalized relative file
--   'Path'.
--
-- Throws: 'InvalidRelFile' when the supplied path:
--
-- * is not a relative path
-- * is @""@
-- * is a directory path i.e.
--
--     * has a trailing path separator
--     * is @.@ or ends in @/.@
--
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'OsPath.isValid')
--
parseRelFile :: MonadThrow m
             => PLATFORM_PATH -> m (Path Rel File)
parseRelFile ospath
  | isValidRelFile ospath
  , let normalized = normalizeFile ospath
  , isValidRelFile normalized = return (Path normalized)
  | otherwise = throwM (InvalidRelFile ospath)

--------------------------------------------------------------------------------
-- Conversion

-- | Convert absolute path to directory to PLATFORM_PATH_SINGLE type.
fromAbsDir :: Path Abs Dir -> PLATFORM_PATH
fromAbsDir = toOsPath

-- | Convert relative path to directory to PLATFORM_PATH_SINGLE type.
fromRelDir :: Path Rel Dir -> PLATFORM_PATH
fromRelDir = toOsPath

-- | Convert absolute path to file to PLATFORM_PATH_SINGLE type.
fromAbsFile :: Path Abs File -> PLATFORM_PATH
fromAbsFile = toOsPath

-- | Convert relative path to file to PLATFORM_PATH_SINGLE type.
fromRelFile :: Path Rel File -> PLATFORM_PATH
fromRelFile = toOsPath

--------------------------------------------------------------------------------
-- Constructors

-- | Make a 'Path' 'Abs' 'Dir'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsDir :: PLATFORM_PATH -> Q Exp
mkAbsDir = either (fail . displayException) lift . parseAbsDir

-- | Make a 'Path' 'Rel' 'Dir'.
mkRelDir :: PLATFORM_PATH -> Q Exp
mkRelDir = either (fail . displayException) lift . parseRelDir

-- | Make a 'Path' 'Abs' 'File'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsFile :: PLATFORM_PATH -> Q Exp
mkAbsFile = either (fail . displayException) lift . parseAbsFile

-- | Make a 'Path' 'Rel' 'File'.
mkRelFile :: PLATFORM_PATH -> Q Exp
mkRelFile = either (fail . displayException) lift . parseRelFile

--------------------------------------------------------------------------------
-- Path of some type.

-- | Path of some type.  @t@ represents the type, whether file or
-- directory. Pattern match to find whether the path is absolute or relative.
data SomeBase t = Abs (Path Abs t)
                | Rel (Path Rel t)
  deriving (Typeable, Generic, Eq, Ord)

instance NFData (SomeBase t) where
  rnf (Abs p) = rnf p
  rnf (Rel p) = rnf p

instance Show (SomeBase t) where
  show = show . fromSomeBase

instance ToJSON (SomeBase t) where
  toJSON = prjSomeBase toJSON
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding = prjSomeBase toEncoding
  {-# INLINE toEncoding #-}
#endif

instance Hashable (SomeBase t) where
  -- See 'Hashable' 'Path' instance for details.
  hashWithSalt n path = hashWithSalt n (fromSomeBase path)

instance FromJSON (SomeBase Dir) where
  parseJSON = parseJSONWith parseSomeDir
  {-# INLINE parseJSON #-}

instance FromJSON (SomeBase File) where
  parseJSON = parseJSONWith parseSomeFile
  {-# INLINE parseJSON #-}

-- | Helper to project the contents out of a SomeBase object.
--
-- >>> prjSomeBase toOsPath (Abs [absfile|/foo/bar/cow.moo|]) == [pstr|/foo/bar/cow.moo|]
--
prjSomeBase :: (forall b . Path b t -> a) -> SomeBase t -> a
prjSomeBase f = \case
  Abs a -> f a
  Rel r -> f r

-- | Helper to apply a function to the SomeBase object
--
-- >>> mapSomeBase parent (Abs [absfile|/foo/bar/cow.moo|]) == Abs [absdir|/foo/bar|]
--
mapSomeBase :: (forall b . Path b t -> Path b t') -> SomeBase t -> SomeBase t'
mapSomeBase f = \case
  Abs a -> Abs $ f a
  Rel r -> Rel $ f r

-- | Convert a valid path to a PLATFORM_PATH_SINGLE.
fromSomeBase :: SomeBase t -> PLATFORM_PATH
fromSomeBase = prjSomeBase toOsPath

-- | Convert a valid directory to a PLATFORM_PATH_SINGLE.
fromSomeDir :: SomeBase Dir -> PLATFORM_PATH
fromSomeDir = fromSomeBase

-- | Convert a valid file to a PLATFORM_PATH_SINGLE.
fromSomeFile :: SomeBase File -> PLATFORM_PATH
fromSomeFile = fromSomeBase

-- | Convert an absolute or relative PLATFORM_PATH_SINGLE to a normalized 'SomeBase'
-- representing a directory.
--
-- Throws: 'InvalidDir' when the supplied path:
--
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'OsPath.isValid')
parseSomeDir :: MonadThrow m => PLATFORM_PATH -> m (SomeBase Dir)
parseSomeDir fp = maybe (throwM (InvalidDir fp)) pure
                $ (Abs <$> parseAbsDir fp)
              <|> (Rel <$> parseRelDir fp)

-- | Convert an absolute or relative PLATFORM_PATH_SINGLE to a normalized 'SomeBase'
-- representing a file.
--
-- Throws: 'InvalidFile' when the supplied path:
--
-- * is a directory path i.e.
--
--     * has a trailing path separator
--     * is @.@ or ends in @/.@
--
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'OsPath.isValid')
parseSomeFile :: MonadThrow m => PLATFORM_PATH -> m (SomeBase File)
parseSomeFile fp = maybe (throwM (InvalidFile fp)) pure
                 $ (Abs <$> parseAbsFile fp)
               <|> (Rel <$> parseRelFile fp)

--------------------------------------------------------------------------------
-- Deprecated

-- | Add extension to given file path. Throws if the
-- resulting filename does not parse.
--
-- >>> addFileExtension [pstr|txt|] $(mkRelFile "foo")
-- "foo.txt"
-- >>> addFileExtension [pstr|symbols|] $(mkRelFile "Data.List")
-- "Data.List.symbols"
-- >>> addFileExtension [pstr|.symbols|] $(mkRelFile "Data.List")
-- "Data.List.symbols"
-- >>> addFileExtension [pstr|symbols|] $(mkRelFile "Data.List.")
-- "Data.List..symbols"
-- >>> addFileExtension [pstr|.symbols|] $(mkRelFile "Data.List.")
-- "Data.List..symbols"
-- >>> addFileExtension [pstr|evil/|] $(mkRelFile "Data.List")
-- *** Exception: InvalidRelFile "Data.List.evil/"
--
-- @since 0.6.1
{-# DEPRECATED addFileExtension "Please use addExtension instead." #-}
addFileExtension :: MonadThrow m
  => PLATFORM_STRING   -- ^ Extension to add
  -> Path b File       -- ^ Old file name
  -> m (Path b File)   -- ^ New file name with the desired extension added at the end
addFileExtension ext (Path path) =
  if OsPath.isAbsolute path
    then liftM coercePath (parseAbsFile (OsPath.addExtension path ext))
    else liftM coercePath (parseRelFile (OsPath.addExtension path ext))
  where coercePath :: Path a b -> Path a' b'
        coercePath (Path a) = Path a

-- | A synonym for 'addFileExtension' in the form of an infix operator.
-- See more examples there.
--
-- >>> $(mkRelFile "Data.List") <.> [pstr|symbols|]
-- "Data.List.symbols"
-- >>> $(mkRelFile "Data.List") <.> [pstr|evil/|]
-- *** Exception: InvalidRelFile "Data.List.evil/"
--
-- @since 0.6.1
infixr 7 <.>
{-# DEPRECATED (<.>) "Please use addExtension instead." #-}
(<.>) :: MonadThrow m
  => Path b File       -- ^ Old file name
  -> PLATFORM_STRING   -- ^ Extension to add
  -> m (Path b File)   -- ^ New file name with the desired extension added at the end
(<.>) = flip addFileExtension

-- | Replace\/add extension to given file path. Throws if the
-- resulting filename does not parse.
--
-- @since 0.5.11
{-# DEPRECATED setFileExtension "Please use replaceExtension instead." #-}
setFileExtension :: MonadThrow m
  => PLATFORM_STRING   -- ^ Extension to set
  -> Path b File       -- ^ Old file name
  -> m (Path b File)   -- ^ New file name with the desired extension
setFileExtension ext (Path path) =
  if OsPath.isAbsolute path
    then liftM coercePath (parseAbsFile (OsPath.replaceExtension path ext))
    else liftM coercePath (parseRelFile (OsPath.replaceExtension path ext))
  where coercePath :: Path a b -> Path a' b'
        coercePath (Path a) = Path a

-- | A synonym for 'setFileExtension' in the form of an operator.
--
-- @since 0.6.0
infixr 7 -<.>
{-# DEPRECATED (-<.>) "Please use replaceExtension instead." #-}
(-<.>) :: MonadThrow m
  => Path b File       -- ^ Old file name
  -> PLATFORM_STRING   -- ^ Extension to set
  -> m (Path b File)   -- ^ New file name with the desired extension
(-<.>) = flip setFileExtension


{-# DEPRECATED PathParseException "Please use PathException instead." #-}
-- | Same as 'PathException'.
type PathParseException = PathException

{-# DEPRECATED stripDir "Please use stripProperPrefix instead." #-}
-- | Same as 'stripProperPrefix'.
stripDir :: MonadThrow m
         => Path b Dir -> Path b t -> m (Path Rel t)
stripDir = stripProperPrefix

{-# DEPRECATED isParentOf "Please use isProperPrefixOf instead." #-}
-- | Same as 'isProperPrefixOf'.
isParentOf :: Path b Dir -> Path b t -> Bool
isParentOf = isProperPrefixOf
