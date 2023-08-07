{-# LANGUAGE DerivingVia #-}

module Graphics.Wayland.Scanner.Env (
  QualifiedName (..),
  lead,
  subName,
  Scan (..),
  ScanEnv (..),
  runScan,
  scanNewType,
  scannedType,
  NamingScheme (..),
  aQualified,
) where

import Control.Monad.Reader
import Data.IORef
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Monoid (Ap (..))
import Data.Text qualified as T
import Language.Haskell.TH qualified as TH

-- | Qualified name as list of texts.
newtype QualifiedName = QualifiedName (NE.NonEmpty T.Text)
  deriving (Eq, Ord)

instance Show QualifiedName where
  show :: QualifiedName -> String
  show (QualifiedName names) = "QName" <> show (NE.toList names)

lead :: T.Text -> QualifiedName
lead dom = QualifiedName (NE.singleton dom)

subName :: QualifiedName -> [T.Text] -> QualifiedName
subName (QualifiedName ls) sub = QualifiedName (NE.appendList ls sub)

-- | Environment for scanning operation.
--
-- Utilizes ReaderT-IO pattern, since TH.Q can perform IO.
data ScanEnv = ScanEnv
  { -- | Prefix of the protocol.
    prefix :: T.Text,
    -- | Scanned types indexed by the protocol-qualified name.
    scannedTypes :: IORef (M.Map QualifiedName TH.Name)
  }

-- | Base monad for scanning & codegen operations.
newtype Scan a = Scan (ReaderT ScanEnv TH.Q a)
  deriving newtype (Functor, Applicative, Monad)
  deriving (Semigroup, Monoid) via (Ap Scan a)

instance TH.Quote Scan where
  newName :: String -> Scan TH.Name
  newName name = Scan (lift $ TH.newName name)

runScan :: T.Text -> Scan a -> TH.Q a
runScan prefix (Scan runner) = do
  scannedTypes <- TH.runIO $ newIORef M.empty
  runReaderT runner ScanEnv{..}

withEnv :: (ScanEnv -> IO a) -> Scan a
withEnv act = Scan $ do
  env <- ask
  liftIO $ act env

-- | Create a new scanned type and advertises it to be available.
scanNewType :: QualifiedName -> Scan TH.Name
scanNewType qualName = do
  typeName <- aQualified HsConstructor qualName
  withEnv $ \env -> modifyIORef env.scannedTypes (M.insert qualName typeName)
  pure typeName

-- ? Local scopes?

-- | A scanned type.
scannedType :: QualifiedName -> Scan TH.Type
scannedType qualName = do
  scannedT <- withEnv $ \env -> (M.!? qualName) <$> readIORef env.scannedTypes
  maybe (error $ "Type for " <> show qualName <> " not found") TH.conT scannedT

data NamingScheme = HsConstructor | HsVariable

-- TODO Hide this as implementation detail

-- | Haskell does not support namespaces, so we approximate qualified names instead.
--
-- The input should be C-style names.
--
-- Note: substitution is performed for specific words.
--
-- >>> aQualified HsConstructor (subName (lead $ T.pack "foo_bar") [T.pack "baz_foo"])
-- FooBarBazFoo
-- >>> aQualified HsVariable (subName (lead $ T.pack "foo_bar") [T.pack "baz_foo"])
-- fooBarBazFoo
aQualified :: NamingScheme -> QualifiedName -> Scan TH.Name
aQualified scheme (QualifiedName names) = do
  Scan . asks $ \env ->
    TH.mkName . T.unpack . substitute . casing . dropPrefix env.prefix $ splitBar
 where
  splitBar = NE.toList names >>= T.splitOn (T.pack "_")

  substitute = \case
    name
      | name == T.pack "class" -> T.pack "klass"
      | name == T.pack "id" -> T.pack "ident"
      | otherwise -> name
  casing = case scheme of
    HsConstructor -> T.concat . map T.toTitle
    HsVariable -> \case
      [] -> T.empty
      begin : trailing -> T.concat (begin : map T.toTitle trailing)
  dropPrefix prefix = \case
    begin : trailing | begin == prefix -> trailing
    texts -> texts
