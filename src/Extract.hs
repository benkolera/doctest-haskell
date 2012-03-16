{-# LANGUAGE DeriveDataTypeable #-}
module Extract (Module(..), extract) where

import           Prelude hiding (mod, catch)
import           Control.Exception

import           Control.DeepSeq (deepseq, NFData(rnf))
import           Data.Generics

import           GHC hiding (flags, Module)
import           NameSet (NameSet)
import           FastString (unpackFS)

import           GhcUtil (withGhc)

-- | A wrapper around `GhcException`, to allow a custom `Show` instance.
newtype WrappedGhcException = WrappedGhcException GhcException
  deriving Typeable

instance Show WrappedGhcException where
  show (WrappedGhcException e) = case e of
    Panic s -> unlines [
        ""
      , "GHC panic: " ++ s
      , ""
      , "This is most likely a bug in doctest."
      , ""
      , "Please report it here: https://github.com/sol/doctest-haskell/issues/new"
      ]
    _ -> show e

instance Exception WrappedGhcException

-- | Documentation for a module grouped together with the modules name.
data Module = Module {
  moduleName          :: String
, moduleDocumentation :: [String]
} deriving (Eq, Show)

instance NFData Module where
  rnf (Module name docs) = name `deepseq` docs `deepseq` ()

-- | Parse a list of modules.
parse :: [String] -- ^ flags
      -> [String] -- ^ files/modules
      -> IO [ParsedModule]
parse flags modules = withGhc flags $ do
  mapM (flip guessTarget Nothing) modules >>= setTargets
  depanal [] False >>= mapM parseModule

-- | Extract all docstrings from given list of files/modules.
--
-- This includes the docstrings of all local modules that are imported from
-- those modules (possibly indirect).
extract :: [String] -- ^ flags
        -> [String] -- ^ files/modules
        -> IO [Module]
extract flags modules = handle (throwIO . WrappedGhcException) $ do
  mods <- parse flags modules
  let docs = map extractFromModule mods
  docs `deepseq` return docs

-- | Extract all docstrings from given module and attach the modules name.
extractFromModule :: ParsedModule -> Module
extractFromModule m = Module name docs
  where
    docs = map unLoc (docStringsFromModule m)
    name = (moduleNameString . GHC.moduleName . ms_mod . pm_mod_summary) m

-- | Extract all docstrings from given module.
docStringsFromModule :: ParsedModule -> [Located String]
docStringsFromModule mod = map (fmap unpackDocString) docs
  where
    source   = (unLoc . pm_parsed_source) mod

    -- we use dlist-style concatenation here
    docs     = (maybe id (:) mHeader . maybe id (++) mExports) decls

    -- We process header, exports and declarations separately instead of
    -- traversing the whole source in a generic way, to ensure that we get
    -- everything in source order.
    mHeader  = hsmodHaddockModHeader source
    mExports = f `fmap` hsmodExports source
      where
        f xs = [L loc doc | L loc (IEDoc doc) <- xs]
    decls    = extractDocStrings (hsmodDecls source)


type Selector a = a -> ([LHsDocString], Bool)

-- | Ignore a subtree.
ignore :: Selector a
ignore = const ([], True)

-- | Collect given value and descend into subtree.
select :: a -> ([a], Bool)
select x = ([x], False)

-- | Extract all docstrings from given value.
extractDocStrings :: Data a => a -> [LHsDocString]
extractDocStrings = everythingBut (++) (([], False) `mkQ` fromLHsDecl
  `extQ` fromLDocDecl
  `extQ` fromLHsDocString
  `extQ` (ignore :: Selector NameSet)
  `extQ` (ignore :: Selector PostTcKind)
  `extQ` (ignore :: Selector GHC.Fixity)

  -- value bindings never contain any documentation, but they may contain error
  -- thunks (e.g. for parallel list comprehensions)
  `extQ` (ignore :: Selector (HsBind RdrName))

  )
  where
    fromLHsDecl :: Selector (LHsDecl RdrName)
    fromLHsDecl (L loc decl) = case decl of

      -- Top-level documentation has to be treated separately, because it has
      -- no location information attached.  The location information is
      -- attached to HsDecl instead.
      DocD x -> (select . L loc . docDeclDoc) x

      _ -> (extractDocStrings decl, True)

    fromLDocDecl :: Selector LDocDecl
    fromLDocDecl = select . fmap docDeclDoc

    fromLHsDocString :: Selector LHsDocString
    fromLHsDocString = select

-- | Convert a docstring to a plain string.
unpackDocString :: HsDocString -> String
unpackDocString (HsDocString s) = unpackFS s