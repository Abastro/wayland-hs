module Graphics.Wayland.Scanner.Env (
  Scan (..),
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Writer.Class
import Data.Coerce
import Language.Haskell.TH qualified as TH



-- | Base monad for scanning & codegen operations.
newtype Scan a = Scan (CPS.WriterT [String] TH.Q a)
  deriving (Functor, Applicative, Monad)

instance MonadWriter [String] Scan where
  writer :: (a, [String]) -> Scan a
  writer = coerce (CPS.writer @_ @TH.Q)
  listen :: Scan a -> Scan (a, [String])
  listen = coerce (CPS.listen @_ @TH.Q)
  pass :: Scan (a, [String] -> [String]) -> Scan a
  pass = coerce (CPS.pass @_ @_ @TH.Q)

instance TH.Quote Scan where
  newName :: String -> Scan TH.Name
  newName name = Scan (lift $ TH.newName name)

