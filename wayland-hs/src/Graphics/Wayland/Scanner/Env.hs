{-# LANGUAGE DerivingVia #-}

module Graphics.Wayland.Scanner.Env (
  QualifiedName (..),
  lead,
  subName,
  fromDotted,
  Scan (..),
  ScanEnv (..),
  liftQ,
  runScan,
  scanNewType,
  scannedType,
  notifyEnum,
  scannedEnumType,
  scanNewField,
  NamingScheme (..),
  aQualified,
) where

import Control.Monad.Reader
import Data.IORef
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Monoid (Ap (..))
import Data.Set qualified as S
import Data.Text qualified as T
import Graphics.Wayland.Scanner.Types (EnumType)
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

-- | If dots are present, take it as a qualified name.
-- Otherwise, use the specified name instead.
fromDotted :: QualifiedName -> T.Text -> QualifiedName
fromDotted parent text = case T.splitOn (T.pack ".") text of
  [] -> error "impossible"
  [name] -> subName parent [name]
  begin : trailing -> QualifiedName (begin NE.:| trailing)

-- | Environment for scanning operation.
--
-- Utilizes ReaderT-IO pattern, since TH.Q can perform IO.
data ScanEnv = ScanEnv
  { -- | Prefix of the protocol.
    prefix :: T.Text,
    -- | Scanned types indexed by the protocol-qualified name.
    scannedTypes :: IORef (M.Map QualifiedName TH.Name),
    enumTypes :: IORef (M.Map QualifiedName EnumType),
    -- | Known fields, mainly to disambiguate on the rare cases.
    knownFields :: IORef (S.Set TH.Name)
  }

-- | Base monad for scanning & codegen operations.
newtype Scan a = Scan (ReaderT ScanEnv TH.Q a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)
  deriving (Semigroup, Monoid) via (Ap Scan a)

instance TH.Quote Scan where
  newName :: String -> Scan TH.Name
  newName name = Scan (lift $ TH.newName name)

liftQ :: TH.Q a -> Scan a
liftQ = Scan . lift

runScan :: T.Text -> Scan a -> TH.Q a
runScan prefix (Scan runner) = do
  scannedTypes <- TH.runIO $ newIORef M.empty
  enumTypes <- TH.runIO $ newIORef M.empty
  knownFields <- TH.runIO $ newIORef S.empty
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
  maybe (fail $ "Type for " <> show qualName <> " not found") TH.conT scannedT

-- ? Consider Enum type separately?
notifyEnum :: QualifiedName -> EnumType -> Scan ()
notifyEnum qualName enumType = do
  withEnv $ \env -> modifyIORef env.enumTypes (M.insert qualName enumType)

scannedEnumType :: QualifiedName -> Scan EnumType
scannedEnumType qualName = do
  scannedEnum <- withEnv $ \env -> (M.!? qualName) <$> readIORef env.enumTypes
  maybe (fail $ "Enum for " <> show qualName <> " not found") pure scannedEnum

-- | Create a new field.
scanNewField :: QualifiedName -> Scan TH.Name
scanNewField qualName = do
  candidate <- aQualified HsField qualName
  withEnv $ \env -> do
    knowns <- readIORef env.knownFields
    let fieldName = avoidingDupe knowns candidate
    modifyIORef env.knownFields (S.insert fieldName)
    pure fieldName
 where
  avoidingDupe knowns candidate =
    if candidate `S.notMember` knowns
      then candidate
      else avoidingDupe knowns (TH.mkName $ TH.nameBase candidate <> "_")

data NamingScheme = HsConstructor | HsVariable | HsField

-- TODO Hide this as implementation detail

-- | Haskell does not support namespaces, so we approximate qualified names instead.
--
-- The input should be C-style names.
--
-- Note: substitution is performed for specific words.
aQualified :: NamingScheme -> QualifiedName -> Scan TH.Name
aQualified scheme (QualifiedName names) = do
  Scan . asks $ \env ->
    TH.mkName . T.unpack . substitute . casing $ dropPrefix env.prefix <$> splitBar
 where
  splitBar = T.splitOn (T.pack "_") <$> names

  substitute = \case
    name
      | name == T.pack "class" -> T.pack "klass"
      | name == T.pack "id" -> T.pack "ident"
      | otherwise -> name
  casing cNames = case scheme of
    HsConstructor -> T.concat . map T.toTitle $ concat cNames
    HsVariable -> case concat cNames of
      [] -> T.empty
      begin : trailing -> T.concat $ begin : map T.toTitle trailing
    HsField -> case (NE.init cNames, NE.last cNames) of
      ([], begin : trailing) -> T.concat $ begin : map T.toTitle trailing
      (parents, name) -> T.concat $ map (T.take 1) (concat parents) <> map T.toTitle name

  dropPrefix prefix = \case
    begin : trailing | begin == prefix -> trailing
    texts -> texts
