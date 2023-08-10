{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenMethods (
  End (..),
  generateAllMessages,
  generateInterfaceMessages,
) where

import Control.Monad
import Data.Foldable
import Data.Monoid qualified as Monoid
import Data.Text qualified as T
import Foreign hiding (void)
import Graphics.Flag (makeFlags)
import Graphics.Wayland.Client.Proxy (proxyMarshalArrayFlags)
import Graphics.Wayland.Remote
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Server.Resource (resourcePostEventArray)
import Language.Haskell.TH qualified as TH

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
generateMessageSend :: End -> QualifiedName -> Word32 -> MessageSpec -> Scan [TH.Dec]
generateMessageSend end parent idx message = do
  -- The environment is orthogonal, so we "scan new type" here.
  ifName <- scanNewType parent
  argsName <- scanNewType (subName parent [message.msgName, T.pack "args"])
  fnName <- aQualified HsVariable $ subName parent [message.msgName]
  let interfaceType = TH.conT ifName
      argsType = TH.conT argsName
  case end of
    EndServer -> do
      msgSig <- TH.sigD fnName [t|Remote EndServer $interfaceType -> $argsType EndServer -> IO ()|]
      msgDec <- TH.funD fnName [TH.clause [] (TH.normalB [e|sendEvent idx|]) []]
      pure [msgSig, msgDec]
    EndClient | Just returnType <- mayReturn -> do
      msgSig <- TH.sigD fnName [t|Remote EndClient $interfaceType -> $argsType EndClient -> IO $returnType|]
      msgDec <- TH.funD fnName [TH.clause [] (TH.normalB [e|sendRequestRet idx|]) []]
      pure [msgSig, msgDec]
    EndClient -> do
      msgSig <- TH.sigD fnName [t|Remote EndClient $interfaceType -> $argsType EndClient -> IO ()|]
      msgDec <- TH.funD fnName [TH.clause [] (TH.normalB [e|sendRequest idx|]) []]
      pure [msgSig, msgDec]
 where
  Monoid.First mayReturn = foldMap (\arg -> Monoid.First $ getReturnType arg.argType) message.arguments

sendEvent :: (AsArguments arg) => Word32 -> Remote EndServer a -> arg -> IO ()
sendEvent opcode remote args =
  withArgs args $ \argList -> withArray argList $ \argArray ->
    resourcePostEventArray (untypeRemote remote) opcode argArray

sendRequest :: (AsArguments arg) => Word32 -> Remote EndClient a -> arg -> IO ()
sendRequest opcode remote args = void (sendRequestPrim opcode remote args)

sendRequestRet :: (AsArguments arg) => Word32 -> Remote EndClient a -> arg -> IO (Remote EndClient b)
sendRequestRet opcode remote args = typeRemote <$> sendRequestPrim opcode remote args

sendRequestPrim :: (AsArguments arg) => Word32 -> Remote EndClient a -> arg -> IO (RemoteAny EndClient)
sendRequestPrim opcode remote args = do
  -- TODO Destructor
  withArgs args $ \argList -> withArray argList $ \argArray ->
    proxyMarshalArrayFlags (untypeRemote remote) opcode Nothing 0 (makeFlags []) argArray

-- | Get return type if the "argument" should be a return.
getReturnType :: ArgumentType -> Maybe (Scan TH.Type)
getReturnType = \case
  RefType _ (ArgNewID name) -> Just $ do
    interfaceType <- scanNewType (lead name)
    [t|Remote EndClient $(TH.conT interfaceType)|]
  _ -> Nothing
