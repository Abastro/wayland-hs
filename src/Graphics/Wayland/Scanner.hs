module Graphics.Wayland.Scanner (
  emitProtocolTypes,
  module Graphics.Wayland.Scanner.Marshall,
  module Graphics.Wayland.Scanner.Flag,
  Int32,
  Word32,
  WlArray (..),
  Text,
  Fd,
) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Flag
import Graphics.Wayland.Scanner.Generate.GenArguments (generateAllArguments)
import Graphics.Wayland.Scanner.Generate.GenTypes (generateAllTypes)
import Graphics.Wayland.Scanner.Marshall
import Graphics.Wayland.Scanner.Parse
import Graphics.Wayland.Util (WlArray (..))
import Language.Haskell.TH qualified as TH
import System.Posix.Types (Fd)

-- | Emit the protocol types to a module.
emitProtocolTypes :: String -> FilePath -> TH.Q [TH.Dec]
emitProtocolTypes prefix path = do
  Right protocol <- protocolFromXML path <$> TH.runIO (readFile path)
  runScan (T.pack prefix) $ do
    typeDecs <- generateAllTypes protocol
    argDecs <- generateAllArguments protocol
    pure (typeDecs <> argDecs)
