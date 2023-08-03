module Graphics.Wayland.Scanner.Env (
  Scan (..),
  ScanState (..),
  scanNewType,
) where

import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Graphics.Wayland.Scanner.Naming
import Language.Haskell.TH qualified as TH

-- | The scanning state.
newtype ScanState = ScanState
  { -- | Scanned types indexed by the protocol-qualified name.
    scannedTypes :: M.Map (V.Vector T.Text) TH.Name
  }
  deriving (Semigroup, Monoid)

-- | Base monad for scanning & codegen operations.
newtype Scan a = Scan (StateT ScanState TH.Q a)
  deriving (Functor, Applicative, Monad, MonadState ScanState)

instance TH.Quote Scan where
  newName :: String -> Scan TH.Name
  newName name = Scan (lift $ TH.newName name)

scanNewType :: [T.Text] -> Scan TH.Name
scanNewType qual = do
  modify (<> ScanState (M.singleton (V.fromList qual) typeName))
  pure typeName
 where
  typeName = symbol . hsConName $ aQualified qual
