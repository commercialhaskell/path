-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     IS_WINDOWS    = False | True

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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}

module Path.PLATFORM_NAME
  (-- * Types
   Path
  ,Abs
  ,Rel
  ,File
  ,Dir
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
  ,parent
  ,filename
  ,dirname
  ,addExtension
  ,splitExtension
  ,fileExtension
  ,replaceExtension
   -- * Parsing
  ,parseAbsDir
  ,parseRelDir
  ,parseAbsFile
  ,parseRelFile
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

import           Control.Exception (Exception(..))
import           Control.Monad (liftM, when)
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Aeson (FromJSON (..))
import qualified Data.Aeson.Types as Aeson
import           Data.Data
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (lift)
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Path.Internal
import qualified System.FilePath.PLATFORM_NAME as FilePath

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

-- | Exceptions that can occur during path operations.
--
-- @since 0.6.0
data PathException
  = InvalidAbsDir FilePath
  | InvalidRelDir FilePath
  | InvalidAbsFile FilePath
  | InvalidRelFile FilePath
  | NotAProperPrefix FilePath FilePath
  | HasNoExtension FilePath
  | InvalidExtension String
  deriving (Show,Eq,Typeable)

instance Exception PathException where
  displayException (InvalidExtension ext) = concat
    [ "Invalid extension ["
    , ext
    , "]. A valid extension starts with a '.' followed by one or more "
    , "characters other than '.', and it must be a valid filename, "
    , "notably it cannot include a path separator."
    ]
  displayException x = show x

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
infixr 5 </>
(</>) :: Path b Dir -> Path Rel t -> Path b t
(</>) (Path a) (Path b) = Path (a ++ b)

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
  case stripPrefix p l of
    Nothing -> throwM (NotAProperPrefix p l)
    Just "" -> throwM (NotAProperPrefix p l)
    Just ok -> return (Path ok)

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

-- | Take the parent path component from a path.
--
-- The following properties hold:
--
-- @
-- parent (x \<\/> y) == x
-- parent \"\/x\" == \"\/\"
-- parent \"x\" == \".\"
-- @
--
-- On the root (absolute or relative), getting the parent is idempotent:
--
-- @
-- parent \"\/\" = \"\/\"
-- parent \"\.\" = \"\.\"
-- @
--
parent :: Path b t -> Path b Dir
parent (Path "") = Path ""
parent (Path fp) | FilePath.isDrive fp = Path fp
parent (Path fp) =
    Path
    $ normalizeDir
    $ FilePath.takeDirectory
    $ FilePath.dropTrailingPathSeparator fp

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

-- | 'splitExtension' is the inverse of 'addExtension'. It splits the given
-- file path into a valid filename and a valid extension.
--
-- >>> splitExtension $(mkRelFile "name.foo"     ) == Just ($(mkRelFile "name"    ), ".foo"  )
-- >>> splitExtension $(mkRelFile "name.foo."    ) == Just ($(mkRelFile "name"    ), ".foo." )
-- >>> splitExtension $(mkRelFile "name.foo.."   ) == Just ($(mkRelFile "name"    ), ".foo..")
-- >>> splitExtension $(mkRelFile "name.bar.foo" ) == Just ($(mkRelFile "name.bar"), ".foo"  )
-- >>> splitExtension $(mkRelFile ".name.foo"    ) == Just ($(mkRelFile ".name"   ), ".foo"  )
-- >>> splitExtension $(mkRelFile "name..foo"    ) == Just ($(mkRelFile "name."   ), ".foo"  )
-- >>> splitExtension $(mkRelFile "....foo"      ) == Just ($(mkRelFile "..."     ), ".foo"  )
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
splitExtension :: MonadThrow m => Path b File -> m (Path b File, String)
splitExtension (Path fpath) =
    if nameDot == [] || ext == []
    then throwM $ HasNoExtension fpath
    else let fname = init nameDot
         in if fname == [] || fname == "." || fname == ".."
            then throwM $ HasNoExtension fpath
            else return ( Path (normalizeDrive drv ++ dir ++ fname)
                        , FilePath.extSeparator : ext
                        )
    where

    -- trailing separators are ignored for the split and considered part of the
    -- second component in the split.
    splitLast isSep str =
        let rstr = reverse str
            notSep = not . isSep
            name = (dropWhile notSep . dropWhile isSep) rstr
            trailingSeps = takeWhile isSep rstr
            xtn  = (takeWhile notSep . dropWhile isSep) rstr
        in (reverse name, reverse xtn ++ trailingSeps)
    normalizeDrive
        | IS_WINDOWS = normalizeTrailingSeps
        | otherwise  = id

    (drv, pth)     = FilePath.splitDrive fpath
    (dir, file)    = splitLast FilePath.isPathSeparator pth
    (nameDot, ext) = splitLast FilePath.isExtSeparator file

-- | Get extension from given file path. Throws 'HasNoExtension' exception if
-- the file does not have an extension.  The following laws hold:
--
-- @
-- flip addExtension file >=> fileExtension == return
-- fileExtension == (fmap snd) . splitExtension
-- @
--
-- @since 0.5.11
fileExtension :: MonadThrow m => Path b File -> m String
fileExtension = (liftM snd) . splitExtension

-- | Add extension to given file path.
--
-- >>> addExtension ".foo"   $(mkRelFile "name"     ) == Just $(mkRelFile "name.foo"    )
-- >>> addExtension ".foo."  $(mkRelFile "name"     ) == Just $(mkRelFile "name.foo."   )
-- >>> addExtension ".foo.." $(mkRelFile "name"     ) == Just $(mkRelFile "name.foo.."  )
-- >>> addExtension ".foo"   $(mkRelFile "name.bar" ) == Just $(mkRelFile "name.bar.foo")
-- >>> addExtension ".foo"   $(mkRelFile ".name"    ) == Just $(mkRelFile ".name.foo"   )
-- >>> addExtension ".foo"   $(mkRelFile "name."    ) == Just $(mkRelFile "name..foo"   )
-- >>> addExtension ".foo"   $(mkRelFile "..."      ) == Just $(mkRelFile "....foo"     )
--
-- Throws an 'InvalidExtension' exception if the extension is not valid. A
-- valid extension starts with a @.@ followed by one or more characters not
-- including @.@ followed by zero or more @.@ in trailing position. Moreover,
-- an extension must be a valid filename, notably it cannot include path
-- separators. Particularly, @.foo.bar@ is an invalid extension, instead you
-- have to first set @.foo@ and then @.bar@ individually. Some examples of
-- invalid extensions are:
--
-- >>> addExtension "foo"      $(mkRelFile "name")
-- >>> addExtension "..foo"    $(mkRelFile "name")
-- >>> addExtension ".foo.bar" $(mkRelFile "name")
-- >>> addExtension ".foo/bar" $(mkRelFile "name")
--
-- @since 0.7.0
addExtension :: MonadThrow m
  => String            -- ^ Extension to add
  -> Path b File       -- ^ Old file name
  -> m (Path b File)   -- ^ New file name with the desired extension added at the end
addExtension ext (Path path) = do
    validateExtension ext
    return $ Path (path ++ ext)

    where

    validateExtension ex@(sep:xs) = do
        -- has to start with a "."
        when (not $ FilePath.isExtSeparator sep) $
            throwM $ InvalidExtension ex

        -- just a "." is not a valid extension
        when (xs == []) $
            throwM $ InvalidExtension ex

        -- cannot have path separators
        when (any FilePath.isPathSeparator xs) $
            throwM $ InvalidExtension ex

        -- All "."s is not a valid extension
        let ys = dropWhile FilePath.isExtSeparator (reverse xs)
        when (ys == []) $
            throwM $ InvalidExtension ex

        -- Cannot have "."s except in trailing position
        when (any FilePath.isExtSeparator ys) $
            throwM $ InvalidExtension ex

        -- must be valid as a filename
        _ <- parseRelFile ex
        return ()
    validateExtension ex = throwM $ InvalidExtension ex

-- | Add extension to given file path. Throws if the
-- resulting filename does not parse.
--
-- >>> addFileExtension "txt $(mkRelFile "foo")
-- "foo.txt"
-- >>> addFileExtension "symbols" $(mkRelFile "Data.List")
-- "Data.List.symbols"
-- >>> addFileExtension ".symbols" $(mkRelFile "Data.List")
-- "Data.List.symbols"
-- >>> addFileExtension "symbols" $(mkRelFile "Data.List.")
-- "Data.List..symbols"
-- >>> addFileExtension ".symbols" $(mkRelFile "Data.List.")
-- "Data.List..symbols"
-- >>> addFileExtension "evil/" $(mkRelFile "Data.List")
-- *** Exception: InvalidRelFile "Data.List.evil/"
--
-- @since 0.6.1
{-# DEPRECATED addFileExtension "Please use addExtension instead." #-}
addFileExtension :: MonadThrow m
  => String            -- ^ Extension to add
  -> Path b File       -- ^ Old file name
  -> m (Path b File)   -- ^ New file name with the desired extension added at the end
addFileExtension ext (Path path) =
  if FilePath.isAbsolute path
    then liftM coercePath (parseAbsFile (FilePath.addExtension path ext))
    else liftM coercePath (parseRelFile (FilePath.addExtension path ext))
  where coercePath :: Path a b -> Path a' b'
        coercePath (Path a) = Path a

-- | A synonym for 'addFileExtension' in the form of an infix operator.
-- See more examples there.
--
-- >>> $(mkRelFile "Data.List") <.> "symbols"
-- "Data.List.symbols"
-- >>> $(mkRelFile "Data.List") <.> "evil/"
-- *** Exception: InvalidRelFile "Data.List.evil/"
--
-- @since 0.6.1
infixr 7 <.>
{-# DEPRECATED (<.>) "Please use addExtension instead." #-}
(<.>) :: MonadThrow m
  => Path b File       -- ^ Old file name
  -> String            -- ^ Extension to add
  -> m (Path b File)   -- ^ New file name with the desired extension added at the end
(<.>) = flip addFileExtension

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
  => String            -- ^ Extension to set
  -> Path b File       -- ^ Old file name
  -> m (Path b File)   -- ^ New file name with the desired extension
replaceExtension ext path =
    addExtension ext (maybe path fst $ splitExtension path)

-- | Replace\/add extension to given file path. Throws if the
-- resulting filename does not parse.
--
-- @since 0.5.11
{-# DEPRECATED setFileExtension "Please use replaceExtension instead." #-}
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

-- | A synonym for 'setFileExtension' in the form of an operator.
--
-- @since 0.6.0
infixr 7 -<.>
{-# DEPRECATED (-<.>) "Please use replaceExtension instead." #-}
(-<.>) :: MonadThrow m
  => Path b File       -- ^ Old file name
  -> String            -- ^ Extension to set
  -> m (Path b File)   -- ^ New file name with the desired extension
(-<.>) = flip setFileExtension

--------------------------------------------------------------------------------
-- Parsers

-- | Convert an absolute 'FilePath' to a normalized absolute dir 'Path'.
--
-- Throws: 'InvalidAbsDir' when the supplied path:
--
-- * is not an absolute path
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'FilePath.isValid')
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
-- Throws: 'InvalidRelDir' when the supplied path:
--
-- * is not a relative path
-- * is @""@
-- * contains a @..@ path component representing the parent directory
-- * is not a valid path (See 'FilePath.isValid')
-- * is all path separators
--
parseRelDir :: MonadThrow m
            => FilePath -> m (Path Rel Dir)
parseRelDir filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (hasParentDir filepath) &&
     not (null filepath) &&
     not (all FilePath.isPathSeparator filepath) &&
     FilePath.isValid filepath
     then return (Path (normalizeDir filepath))
     else throwM (InvalidRelDir filepath)

-- | Convert an absolute 'FilePath' to a normalized absolute file 'Path'.
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
-- * is not a valid path (See 'FilePath.isValid')
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
-- * is not a valid path (See 'FilePath.isValid')
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
mkAbsDir = either (error . show) lift . parseAbsDir

-- | Make a 'Path' 'Rel' 'Dir'.
mkRelDir :: FilePath -> Q Exp
mkRelDir = either (error . show) lift . parseRelDir

-- | Make a 'Path' 'Abs' 'File'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsFile :: FilePath -> Q Exp
mkAbsFile = either (error . show) lift . parseAbsFile

-- | Make a 'Path' 'Rel' 'File'.
mkRelFile :: FilePath -> Q Exp
mkRelFile = either (error . show) lift . parseRelFile

--------------------------------------------------------------------------------
-- Internal functions

-- | Normalizes directory path with platform-specific rules.
normalizeDir :: FilePath -> FilePath
normalizeDir =
      normalizeRelDir
    . FilePath.addTrailingPathSeparator
    . normalizeFilePath
  where -- Represent a "." in relative dir path as "" internally so that it
        -- composes without having to renormalize the path.
        normalizeRelDir p
          | p == relRootFP = ""
          | otherwise = p

-- | Replaces consecutive path seps with single sep and replaces alt sep with standard sep.
normalizeAllSeps :: FilePath -> FilePath
normalizeAllSeps = foldr normSeps []
  where normSeps ch [] = [ch]
        normSeps ch path@(p0:_)
          | FilePath.isPathSeparator ch && FilePath.isPathSeparator p0 = path
          | FilePath.isPathSeparator ch = FilePath.pathSeparator:path
          | otherwise = ch:path

-- | Normalizes seps in whole path, but if there are 2+ seps at the beginning,
--   they are normalized to exactly 2 to preserve UNC and Unicode prefixed paths.
normalizeWindowsSeps :: FilePath -> FilePath
normalizeWindowsSeps path = normLeadingSeps ++ normalizeAllSeps rest
  where (leadingSeps, rest) = span FilePath.isPathSeparator path
        normLeadingSeps = replicate (min 2 (length leadingSeps)) FilePath.pathSeparator

-- | Normalizes seps only at the beginning of a path.
normalizeLeadingSeps :: FilePath -> FilePath
normalizeLeadingSeps path = normLeadingSep ++ rest
  where (leadingSeps, rest) = span FilePath.isPathSeparator path
        normLeadingSep = replicate (min 1 (length leadingSeps)) FilePath.pathSeparator

-- | Normalizes seps only at the end of a path.
normalizeTrailingSeps :: FilePath -> FilePath
normalizeTrailingSeps = reverse . normalizeLeadingSeps . reverse

-- | Applies platform-specific sep normalization following @FilePath.normalise@.
normalizeFilePath :: FilePath -> FilePath
normalizeFilePath
  | IS_WINDOWS = normalizeWindowsSeps . FilePath.normalise
  | otherwise  = normalizeLeadingSeps . FilePath.normalise

--------------------------------------------------------------------------------
-- Deprecated

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
