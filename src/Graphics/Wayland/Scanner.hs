module Graphics.Wayland.Scanner (
  emitProtocolTypes,
  emitProtocolEnd,
  module Graphics.Wayland.Remote,
  module Graphics.Flag,
  module Graphics.Wayland.Scanner.Marshal,
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
import Graphics.Flag
import Graphics.Wayland.Remote
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Generate.GenArguments (generateAllArguments)
import Graphics.Wayland.Scanner.Generate.GenMethods (generateAllMessages)
import Graphics.Wayland.Scanner.Generate.GenTypes (generateAllTypes)
import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Parse
import Graphics.Wayland.Server.Resource ()
import Graphics.Wayland.Util (WlArray (..))
import Language.Haskell.TH qualified as TH
import System.Posix.Types (Fd)

-- | Emit the protocol types to a module.
emitProtocolTypes :: String -> FilePath -> TH.Q [TH.Dec]
emitProtocolTypes prefix path = do
  parsed <- protocolFromXML path <$> TH.runIO (readFile path)
  case parsed of
    Left err -> fail err
    Right protocol -> runScan (T.pack prefix) $ do
      typeDecs <- generateAllTypes protocol
      argDecs <- generateAllArguments protocol
      pure (typeDecs <> argDecs)

-- | Emit client/protocol method calls.
emitProtocolEnd :: End -> String -> FilePath -> TH.Q [TH.Dec]
emitProtocolEnd end prefix path = do
  parsed <- protocolFromXML path <$> TH.runIO (readFile path)
  case parsed of
    Left err -> fail err
    Right protocol -> runScan (T.pack prefix) $ do
      generateAllMessages end protocol
