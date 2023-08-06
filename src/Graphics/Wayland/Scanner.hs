module Graphics.Wayland.Scanner (
  emitProtocolTypes,
  module Graphics.Wayland.Scanner.Marshall,
  module Graphics.Wayland.Util,
) where

import Data.Text qualified as T
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Generate.GenArguments (generateAllArguments)
import Graphics.Wayland.Scanner.Generate.GenTypes (generateAllTypes)
import Graphics.Wayland.Scanner.Marshall
import Graphics.Wayland.Scanner.Parse
import Graphics.Wayland.Util (WlArray (..))
import Language.Haskell.TH qualified as TH

-- | Emit the protocol types to a module.
emitProtocolTypes :: String -> FilePath -> TH.Q [TH.Dec]
emitProtocolTypes prefix path = do
  Right protocol <- protocolFromXML path <$> TH.runIO (readFile path)
  runScan (T.pack prefix) $ do
    typeDecs <- generateAllTypes protocol
    argDecs <- generateAllArguments protocol
    pure (typeDecs <> argDecs)
