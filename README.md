# Path

![CI](https://github.com/commercialhaskell/path/workflows/CI/badge.svg?branch=master)
[![Hackage](https://img.shields.io/hackage/v/path.svg)](https://hackage.haskell.org/package/path)
[![Stackage LTS](http://stackage.org/package/path/badge/lts)](http://stackage.org/lts/package/path)
[![Stackage Nightly](http://stackage.org/package/path/badge/nightly)](http://stackage.org/nightly/package/path)

Support for well-typed paths in Haskell.

* [Motivation](#motivation)
* [Approach](#approach)
* [Solution](#solution)
* [Implementation](#implementation)
    * [The data types](#the-data-types)
    * [Parsers](#parsers)
    * [Smart constructors](#smart-constructors)
    * [Overloaded stings](#overloaded-strings)
    * [Operations](#operations)
* [Review](#review)
    * [Relative vs absolute confusion](#relative-vs-absolute-confusion)
    * [The equality problem](#the-equality-problem)
    * [Unpredictable concatenation issues](#unpredictable-concatenation-issues)
    * [Confusing files and directories](#confusing-files-and-directories)
    * [Self-documentation](#self-documentation)
* [In practice](#in-practice)
* [Doing I/O](#doing-io)
* [Doing textual manipulations](#doing-textual-manipulations)
* [Accepting user input](#accepting-user-input)
* [Comparing with existing path libraries](#comparing-with-existing-path-libraries)
    * [filepath and system-filepath](#filepath-and-system-filepath)
    * [system-canonicalpath, canonical-filepath, directory-tree](#system-canonicalpath-canonical-filepath-directory-tree)
    * [pathtype](#pathtype)
    * [data-filepath](#data-filepath)
* [Summary](#summary)

## Motivation

It was after working on a number of projects at FP Complete that use file
paths in various ways. We used the system-filepath package, which was
supposed to solve many path problems by being an opaque path type. It
occurred to me that the same kind of bugs kept cropping up:

* Expected a path to be absolute but it was relative, or vice-versa.

* Expected two equivalent paths to be equal or order the same, but they did
  not (`/home//foo` vs `/home/foo/` vs `/home/bar/../foo`, etc.).

* Unpredictable behaviour with regards to concatenating paths.

* Confusing files and directories.

* Not knowing whether a path was a file or directory or relative or absolute
  based on the type alone was a drag.

All of these bugs are preventable.

## Approach

My approach to problems like this is to make a type that encodes the
properties I want and then make it impossible to let those invariants be
broken, without compromise or backdoors to let the wrong value “slip
in”. Once I have a path, I want to be able to trust it fully. This theme
will be seen throughout the things I lay out below.

## Solution

After having to fix bugs due to these in our software, I put my foot down
and made:

* An opaque `Path` type (a newtype wrapper around `String`).

* Smart constructors which are very stringent in the parsing.

* Make the parsers highly normalizing.

* Leave equality and concatenation to basic string equality and
  concatenation.

* Include relativity (absolute/relative) and type (directory/file) in the
  type itself.

* Use the already cross-platform
  [filepath](http://hackage.haskell.org/package/filepath) package for
  implementation details.

## Implementation

### The data types

Here is the type:

```haskell
newtype Path b t = Path FilePath
  deriving (Data, Generic)
```

The type variables are:

* `b` — base, the base location of the path; absolute or relative.
* `t` — type, whether file or directory.

The base types can be filled with these:

```haskell
data Abs
data Rel
```

And the type can be filled with these:

```haskell
data File
data Dir
```

(Why not use data kinds like `data Type = File | Dir`? Because that imposes
an extension overhead of adding `{-# LANGUAGE DataKinds #-}` to every module
you might want to write out a path type in. Given that one cannot construct
paths of types other than these, via the operations in the module, it’s not
a concern for me.)

There is a conversion function to give you back the filepath:

```haskell
toFilePath :: Path b t -> FilePath
toFilePath (Path l) = l
```

Beginning from version 0.5.3, there are type-constrained versions of
`toFilePath` with the following signatures:

```haskell
fromAbsDir  :: Path Abs Dir -> FilePath
fromRelDir  :: Path Rel Dir -> FilePath
fromAbsFile :: Path Abs File -> FilePath
fromRelFile :: Path Rel File -> FilePath
```

### Parsers

To get a `Path` value, you need to use one of the four parsers:

```haskell
parseAbsDir  :: MonadThrow m => FilePath -> m (Path Abs Dir)
parseRelDir  :: MonadThrow m => FilePath -> m (Path Rel Dir)
parseAbsFile :: MonadThrow m => FilePath -> m (Path Abs File)
parseRelFile :: MonadThrow m => FilePath -> m (Path Rel File)
```

The following properties apply:

* Absolute parsers will reject non-absolute paths.

* The only delimiter syntax accepted is the path separator; `/` on POSIX and
  `\` on Windows.

* Any other delimiter is rejected; `..`, `~/`, `/./`, etc.

* All parsers normalize into single separators: `/home//foo` → `/home/foo`.

* Directory parsers always normalize with a final trailing `/`. So `/home/foo`
  parses into the string `/home/foo/`.

It was discussed briefly whether we should just have a class for parsing
rather than four separate parsing functions. In my experience so far, I have
had type errors where I wrote something `like x <- parseAbsDir
someAbsDirString` because `x` was then passed to a place that expected a
relative directory. In this way, overloading the return value would’ve just
been accepted. So I don’t think having a class is a good idea. Being
explicit here doesn’t exactly waste our time, either.

Why are these functions in `MonadThrow`? Because it means I can have it
return an `Either`, or a `Maybe`, if I’m in pure code, and if I’m in `IO`,
and I don’t expect parsing to ever fail, I can use it in IO like this:

```haskell
do x <- parseRelFile (fromCabalFileName x)
   foo x
   …
```

That’s really convenient and we take advantage of this at FP Complete a lot.

Equality, ordering and printing are simply re-using the `String` instances:

```haskell
instance Eq (Path b t) where
  (==) (Path x) (Path y) = x == y

instance Ord (Path b t) where
  compare (Path x) (Path y) = compare x y

instance Show (Path b t) where
  show (Path x) = show x
```

Which gives us for free the following equational properties:

```haskell
toFilePath x == toFilePath y        ≡ x == y           -- Eq instance
toFilePath x `compare` toFilePath y ≡ x `compare` y    -- Ord instance
toFilePath x == toFilePath y        ≡ show x == show y -- Show instance
```

In other words, the representation and the path you get out at the end are
the same. Two paths that are equal will always give you back the same thing.

### Smart constructors

For when you know what a path will be at compile-time, there are
constructors for that:

```haskell
$(mkAbsDir "/home/chris")
$(mkRelDir "chris")
$(mkAbsFile "/home/chris/x.txt")
$(mkRelFile "chris/x.txt")
```

With the [QuasiQuotes](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XQuasiQuotes)
language extension, paths can be written as follows:

```haskell
[absdir|/home/chris|]
[reldir|chris|]
[absfile|/home/chris/x.txt|]
[relfile|chris/x.txt|]
```

These will run at compile-time and underneath use the appropriate parser.

### Overloaded strings

No `IsString` instance is provided, because that has no way to statically
determine whether the path is correct, and would otherwise have to be a
partial function.

In practice I have written the wrong path format in a `$(mk… "")` and been
thankful it was caught early.

### Operations

There is path concatenation:

```haskell
(</>) :: Path b Dir -> Path Rel t -> Path b t
```

Get the parent directory of a path:

```haskell
parent :: Path Abs t -> Path Abs Dir
```

Get the filename of a file path:

```haskell
filename :: Path b File -> Path Rel File
```

Get the directory name of a directory path:

```haskell
dirname :: Path b Dir -> Path Rel Dir
```

Stripping the parent directory from a path:

```haskell
stripProperPrefix :: MonadThrow m => Path b Dir -> Path b t -> m (Path Rel t)
```

## Review

Let’s review my initial list of complaints and see if they’ve been
satisfied.

### Relative vs absolute confusion

Paths now distinguish in the type system whether they are relative or
absolute. You can’t append two absolute paths, for example:

```haskell
λ> [absdir|/home/chris|]</>[absdir|/home/chris|]
<interactive>:23:31-55:
    Couldn't match type ‘Abs’ with ‘Rel’
```

### The equality problem

Paths are now stringently normalized. They have to be a valid path, and they
only support single path separators, and all directories are suffixed with a
trailing path separator:

```haskell
λ> $(mkAbsDir "/home/chris//") == $(mkAbsDir "/./home//chris")
True
λ> toFilePath $(mkAbsDir "/home/chris//") ==
   toFilePath $(mkAbsDir "/./home//chris")
True
λ> ($(mkAbsDir "/home/chris//"),toFilePath $(mkAbsDir "/./home//chris"))
("/home/chris/","/home/chris/")
```

### Unpredictable concatenation issues

Because of the stringent normalization, path concatenation, as seen above,
is simply string concatenation. This is about as predictable as it can get:

```haskell
λ> toFilePath $(mkAbsDir "/home/chris//")
"/home/chris/"
λ> toFilePath $(mkRelDir "foo//bar")
"foo/bar/"
λ> [absdir|/home/chris//|]</>[reldir|foo//bar|]
"/home/chris/foo/bar/"
```

### Confusing files and directories

Now that the path type is encoded in the type system, our `</>` operator
prevents improper appending:

```haskell
λ> [absdir|/home/chris/|]</>[relfile|foo//bar|]
"/home/chris/foo/bar"
λ> [absfile|/home/chris|]</>[relfile|foo//bar|]
<interactive>:35:1-26:
    Couldn't match type ‘File’ with ‘Dir’
```

### Self-documentation

Now I can read the path like:

```haskell
{ fooPath :: Path Rel Dir, ... }
```

And know that this refers to the directory relative to some other path,
meaning I should be careful to consider the current directory when using
this in IO, or that I’ll probably need a parent to append to it at some
point.

## In practice

We’ve been using this at FP Complete in a number of packages for some months
now, it’s turned out surprisingly sufficient for most of our path work with
only one bug found. We weren’t sure initially whether it would just be too
much of a pain to use, but really it’s quite acceptable given the
advantages. You can see its use all over the
[`stack`](https://github.com/commercialhaskell/stack) codebase.

## Doing I/O

Currently any operations involving I/O can be done by using the existing I/O
library:

```haskell
doesFileExist (toFilePath fp)
readFile (toFilePath fp)
```

etc. This has problems with respect to accidentally running something like:

```haskell
doesFileExist $(mkRelDir "foo")
```

But I/O is currently outside the scope of what this package solves. Once you
leave the realm of the `Path` type invariants are back to your responsibility.

As with the original version of this library, we’re currently building up a
set of functions in a `Path.IO` module over time that fits our real-world
use-cases. It may or may not appear in the path package eventually. It’ll
need cleaning up and considering what should really be included.

**Edit:** There is now
[`path-io`](https://hackage.haskell.org/package/path-io) package that
complements the `path` library and includes complete well-typed interface to
[`directory`](https://hackage.haskell.org/package/directory) and
[`temporary`](https://hackage.haskell.org/package/temporary). There is work
to add more generally useful functions from Stack's `Path.IO` to it and make
Stack depend on the `path-io` package.

## Doing textual manipulations

One problem that crops up sometimes is wanting to manipulate
paths. Currently the way we do it is via the filepath library and re-parsing
the path:

```haskell
parseAbsFile . addExtension "/directory/path" "ext" . toFilePath
```

It doesn’t happen too often, in our experience, to the extent this needs to
be more convenient.

## Accepting user input

Sometimes you have user input that contains `../`. The solution we went with
is to have a function like `resolveDir` (found in [`path-io`](http://hackage.haskell.org/package/path-io) package):

```haskell
resolveDir :: (MonadIO m, MonadThrow m)
           => Path Abs Dir -> FilePath -> m (Path Abs Dir)
```

Which will call `canonicalizePath` which collapses and normalizes a path and
then we parse with regular old `parseAbsDir` and we’re cooking with
gas. This and others like it might get added to the `path` package.

## Comparing with existing path libraries

### filepath and system-filepath

The [filepath](http://hackage.haskell.org/package/filepath) package is
intended as the complimentary package to be used before parsing into a Path
value, and/or after printing from a Path value. The package itself contains
no type-safety, instead contains a range of cross-platform textual
operations. Definitely reach for this library when you want to do more
involved manipulations.

The `system-filepath` package is deprecated in favour of `filepath`.

### system-canonicalpath, canonical-filepath, directory-tree

The
[`system-canonicalpath`](http://hackage.haskell.org/package/system-canonicalpath)
and the
[`canonical-filepath`](http://hackage.haskell.org/package/canonical-filepath)
packages both are a kind of subset of `path`. They canonicalize a string
into an opaque path, but neither distinguish directories from files or
absolute/relative. Useful if you just want a canonical path but doesn’t do
anything else.

The [`directory-tree`](http://hackage.haskell.org/package/directory-tree)
package contains a sum type of dir/file/etc but doesn’t distinguish in its
operations relativity or path type.

### pathtype

Finally, we come to a path library that is similar to: the
[`pathtype`](http://hackage.haskell.org/package/pathtype) library. There are
the same types of `Path Abs File` / `Path Rel Dir`, etc.

The points where this library isn’t enough for me are:

* There is an `IsString` instance, which means people will use it, and will
  make mistakes.

* Paths are not normalized into a predictable format, leading to me being
  unsure when equality will succeed. This is the same problem I encountered
  in `system-filepath`. The equality function normalizes, but according to
  what properties I can reason about? I don’t know.

```haskell
System.Path.Posix> ("/tmp//" :: Path a Dir) == ("/tmp" :: Path a Dir)
True
System.Path.Posix> ("tmp" :: Path a Dir) == ("/tmp" :: Path a Dir)
True
System.Path.Posix> ("/etc/passwd/" :: Path a b) == ("/etc/passwd" :: Path a b)
True
System.Path.Posix> ("/tmp//" :: Path Abs Dir) == ("/tmp/./" :: Path Abs Dir)
False
System.Path.Posix> ("/tmp/../" :: Path Abs Dir) == ("/" :: Path Abs Dir)
False
```
* Empty string should not be allowed, and introduction of `.` due to that
  gets weird:

```haskell
System.Path.Posix> fmap getPathString (Right ("." :: Path Rel File))
Right "."
System.Path.Posix> fmap getPathString (mkPathAbsOrRel "")
Right "."
System.Path.Posix> (Right ("." :: Path Rel File)) == (mkPathAbsOrRel "")
False
System.Path.Posix> takeDirectory ("tmp" :: Path Rel Dir)
.
System.Path.Posix> (getPathString ("." :: Path Rel File) ==
                    getPathString ("" :: Path Rel File))
True
System.Path.Posix> (("." :: Path Rel File) == ("" :: Path Rel File))
False
```

* It has functions like `<.>/addExtension` which let you insert an
  arbitrary string into a path.

* Some functions let you produce nonsense (could be prevented by a stricter
  type), for example:

```haskell
System.Path.Posix> takeFileName ("/tmp/" :: Path Abs Dir)
tmp
```

I’m being a bit picky here, a bit unfair. But the point is really to show
the kind of things I tried to avoid in `path`. In summary, it’s just hard to
know where things can go wrong, similar to what was going on in
`system-filepath`.

### data-filepath

The [`data-filepath`](https://hackage.haskell.org/package/data-filepath) is
also very similar, I discovered it after writing my own at work and was
pleased to see it’s mostly the same. The main differences are:

* Uses `DataKinds` for the relative/absolute and file/dir distinction which
  as I said above is an overhead.

* Uses a GADT for the path type, which is fine. In my case I wanted to
  retain the original string which functions that work on the `FilePath`
  (`String`) type already deal with well. It does change the parsing step
  somewhat, because it parses into segments.

* It’s more lenient at parsing (allowing `..` and trailing `.`).

The API is a bit awkward to just parse a directory, requires a couple
functions to get it (going via `WeakFilePath`), returning only an `Either`,
and there are no functions like parent. But there’s not much to complain
about. It’s a fine library, but I didn’t feel the need to drop my own in
favor of it. Check it out and decide for yourself.

## Summary

There’s a growing interest in making practical use of well-typed file path
handling. I think everyone’s wanted it for a while, but few people have
really committed to it in practice. Now that I’ve been using `path` for a
while, I can’t really go back. It’ll be interesting to see what new packages
crop up in the coming year, I expect there’ll be more.
