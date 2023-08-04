module Graphics.Wayland.Scanner where

import Data.Foldable
-- import Data.Vector qualified as V
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Generate.GenTypes
import Graphics.Wayland.Scanner.Parse
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH

emitProtocolTypes :: FilePath -> TH.Q [TH.Dec]
emitProtocolTypes path = do
  Right protocol <- protocolFromXML path <$> TH.runIO (readFile path)
  runScan $ fold <$> traverse generateInterfaceTypes (protocol.interfaces)
