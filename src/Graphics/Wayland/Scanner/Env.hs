module Graphics.Wayland.Scanner.Env (
  Scan (..),
  ScanState (..),
  runScan,
  scanNewType,
  scannedType,
) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Graphics.Wayland.Scanner.Naming
import Language.Haskell.TH qualified as TH

-- | The scanning state.
newtype ScanState = ScanState
  { -- | Scanned types indexed by the protocol-qualified name.
    scannedTypes :: M.Map QualifiedName TH.Name
  }
  deriving (Semigroup, Monoid)

-- | Base monad for scanning & codegen operations.
newtype Scan a = Scan (StateT ScanState TH.Q a)
  deriving (Functor, Applicative, Monad, MonadState ScanState)

instance TH.Quote Scan where
  newName :: String -> Scan TH.Name
  newName name = Scan (lift $ TH.newName name)

runScan :: Scan a -> TH.Q a
runScan (Scan runner) = evalStateT runner mempty

-- | Create a new scanned type and advertises it to be available.
scanNewType :: QualifiedName -> Scan TH.Name
scanNewType qualName = do
  modify (<> ScanState (M.singleton qualName typeName))
  pure typeName
 where
  typeName = aQualified HsConstructor qualName

-- ? Local scopes?
-- | A scanned type.
scannedType :: QualifiedName -> Scan TH.Name
scannedType qualName = do
  ScanState scanned <- get
  maybe (error $ "Type for " <> show qualName <> " not found") pure (scanned M.!? qualName)
