module Graphics.Wayland.Scanner.Generate.Documentation (
  addDescribe,
  addSummaryToLocation,
) where

import Data.Foldable
import Data.Text qualified as T
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

addDescribe :: Maybe Description -> TH.Dec -> Scan TH.Dec
addDescribe = \case
  Nothing -> pure
  Just desc -> liftQ . TH.withDecDoc (T.unpack $ desc.describe) . pure

addSummaryToLocation :: TH.DocLoc -> Maybe T.Text -> Scan ()
addSummaryToLocation loc = traverse_ $ \summary ->
  liftQ . TH.addModFinalizer $ TH.putDoc loc (T.unpack summary)
