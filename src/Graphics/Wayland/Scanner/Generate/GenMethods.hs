{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenMethods (
  End (..),
  generateAllMessages,
  generateInterfaceMessages,
) where

import Control.Monad
import Data.Foldable
import Data.Text qualified as T
import Foreign
import Graphics.Flag (makeFlags)
import Graphics.Wayland.Client.Proxy (Proxy (..), proxyMarshalArrayFlags)
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Server.Resource (Resource (..), resourcePostEventArray)
import Language.Haskell.TH qualified as TH

data End = EndClient | EndServer

generateAllMessages :: End -> ProtocolSpec -> Scan [TH.Dec]
generateAllMessages end protocol = foldMap (generateInterfaceMessages end) protocol.interfaces

generateInterfaceMessages :: End -> InterfaceSpec -> Scan [TH.Dec]
generateInterfaceMessages end interface = do
  let receives = []
  sends <- concat <$> zipWithM (generateMessageSend end $ lead interface.ifName) [0 ..] (toList outbound)
  pure (receives <> sends)
 where
  (_inbound, outbound) = case end of
    EndClient -> (interface.events, interface.requests)
    EndServer -> (interface.requests, interface.events)

-- | Generate functions for sending messages.
generateMessageSend :: End -> QualifiedName -> Int -> MessageSpec -> Scan [TH.Dec]
generateMessageSend end parent idx message = do
  -- The environment is orthogonal, so we "scan new type" here.
  interfaceType <- scanNewType parent
  argsType <- TH.ConT <$> scanNewType (subName parent [message.msgName, T.pack "args"])
  fnName <- aQualified HsVariable $ subName parent [message.msgName]
  let patt = TH.conP interfaceType [TH.varP ptr]
  case end of
    EndServer -> do
      msgSig <- TH.sigD fnName [t|$(TH.conT interfaceType) -> $(pure argsType) -> IO ()|]
      msgDec <- TH.funD fnName [TH.clause [patt] (TH.normalB [e|sendEvent opcode $(TH.varE ptr)|]) []]
      pure [msgSig, msgDec]
    EndClient -> do
      msgSig <- TH.sigD fnName [t|$(TH.conT interfaceType) -> $(pure argsType) -> IO ()|]
      msgDec <- TH.funD fnName [TH.clause [patt] (TH.normalB [e|sendRequest opcode $(TH.varE ptr)|]) []]
      pure [msgSig, msgDec]
 where
  ptr = TH.mkName "ptr"

  opcode :: Word32
  opcode = fromIntegral idx

sendEvent :: (AsArguments arg) => Word32 -> Ptr a -> arg -> IO ()
sendEvent opcode resourcePtr args =
  withArgs args $ \argList -> withArray argList $ \argArray -> do
    resourcePostEventArray (Resource $ castPtr resourcePtr) opcode argArray

sendRequest :: (AsArguments arg) => Word32 -> Ptr a -> arg -> IO ()
sendRequest opcode proxyPtr args = do
  -- TODO Destructor & Returns
  _ <- withArgs args $ \argList -> withArray argList $ \argArray -> do
    proxyMarshalArrayFlags (Proxy $ castPtr proxyPtr) opcode Nothing 0 (makeFlags []) argArray
  pure ()
