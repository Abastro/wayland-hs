module Graphics.Wayland.Scanner.Env (
  Scan (..),
  ScanEnv (..),
  runScan,
  scanNewType,
  scannedType,
) where

import Control.Monad.Reader
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Graphics.Wayland.Scanner.Naming
import Language.Haskell.TH qualified as TH

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
  deriving (Functor, Applicative, Monad)

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
  withEnv $ \env -> modifyIORef env.scannedTypes (M.insert qualName typeName)
  pure typeName
 where
  typeName = aQualified HsConstructor qualName

-- ? Local scopes?

-- | A scanned type.
scannedType :: QualifiedName -> Scan TH.Name
scannedType qualName = do
  scannedT <- withEnv $ \env -> (M.!? qualName) <$> readIORef env.scannedTypes
  maybe (error $ "Type for " <> show qualName <> " not found") pure scannedT
