module Graphics.Wayland.Scanner.Generate.Documentation (
  addDescribe,
) where

import Data.Text qualified as T
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH

addDescribe :: Maybe Description -> TH.Dec -> Scan TH.Dec
addDescribe = \case
  Nothing -> pure
  Just desc -> liftQ . TH.withDecDoc (T.unpack $ desc.describe) . pure
