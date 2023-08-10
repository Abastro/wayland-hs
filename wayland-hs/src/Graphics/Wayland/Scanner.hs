module Graphics.Wayland.Scanner (
  emitProtocolTypes,
  emitProtocolEnd,
  module Graphics.Wayland.Remote,
  module Graphics.Flag,
  module Graphics.Wayland.Scanner.Marshal,
  Proxy (..),
  Ap (..),
  Int32,
  Word32,
  WlArray (..),
  Text,
  Fd,
) where

import Data.Monoid (Ap (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Flag
import Graphics.Wayland.Remote
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Generate.GenArguments (generateAllArguments)
import Graphics.Wayland.Scanner.Generate.GenMethods (generateAllMessages)
import Graphics.Wayland.Scanner.Generate.GenTypes (generateAllTypes)
import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Parse
import Graphics.Wayland.Util (Fd, Int32, WlArray (..), Word32)
import Language.Haskell.TH qualified as TH

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
