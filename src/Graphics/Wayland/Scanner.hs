module Graphics.Wayland.Scanner (
  emitProtocolTypes,
) where

import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Generate.GenArguments (generateAllArguments)
import Graphics.Wayland.Scanner.Generate.GenTypes (generateAllTypes)
import Graphics.Wayland.Scanner.Parse
import Language.Haskell.TH qualified as TH

-- | Emit the protocol types to a module.
emitProtocolTypes :: FilePath -> TH.Q [TH.Dec]
emitProtocolTypes path = do
  Right protocol <- protocolFromXML path <$> TH.runIO (readFile path)
  runScan $ do
    typeDecs <- generateAllTypes protocol
    argDecs <- generateAllArguments protocol
    pure (typeDecs <> argDecs)
