{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenMethods (
  generateAllMessages,
  generateInterfaceMessages,
) where

import Control.Monad
import Data.Foldable
import Data.Monoid qualified as Monoid
import Data.Text qualified as T
import Foreign hiding (void)
import Graphics.Flag (makeFlags, toFlags)
import Graphics.Wayland.Client.Proxy (MarshalFlag (..), proxyMarshalArrayFlags)
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
      msgSig <- TH.sigD fnName [t|Remote EServer $interfaceType -> $argsType EServer -> IO ()|]
      msgDec <- TH.funD fnName [TH.clause [] (TH.normalB [e|sendEvent idx|]) []]
      pure [msgSig, msgDec]
    -- new_id means the function returns something.
    EndClient | Just returnType <- mayReturn -> do
      msgSig <- TH.sigD fnName [t|Remote EClient $interfaceType -> $argsType EClient -> IO $returnType|]
      msgDec <- TH.funD fnName [TH.clause [] (TH.normalB [e|sendRequestRet idx|]) []]
      pure [msgSig, msgDec]
    -- Otherwise, simpler cases.
    EndClient -> do
      msgSig <- TH.sigD fnName [t|Remote EClient $interfaceType -> $argsType EClient -> IO ()|]
      msgDec <- TH.funD fnName [TH.clause [] (TH.normalB [e|sendRequest idx $flags|]) []]
      pure [msgSig, msgDec]
 where
  Monoid.First mayReturn = foldMap (\arg -> Monoid.First $ getReturnType arg.argType) message.arguments

  flags = case message.msgType of
    Normal -> [e|[]|]
    Destructor -> [e|[MarshalDestroy]|]

-- TODO Dispatcher and implementation

sendEvent :: (AsArguments arg) => Word32 -> Remote EServer a -> arg -> IO ()
sendEvent opcode remote args =
  withArgs args $ \argList -> withArray argList $ \argArray ->
    resourcePostEventArray (untypeRemote remote) opcode argArray

-- TODO Version
sendRequest :: (AsArguments arg) => Word32 -> [MarshalFlag] -> Remote EClient a -> arg -> IO ()
sendRequest opcode flags remote args = do
  _ <- withArgs args $ \argList -> withArray argList $ \argArray ->
    proxyMarshalArrayFlags (untypeRemote remote) opcode Nothing 0 (makeFlags flags) argArray
  pure ()

sendRequestRet :: (AsArguments arg) => Word32 -> Remote EClient a -> arg -> IO (Remote EClient b)
sendRequestRet opcode remote args = do
  -- TODO Pass interface C struct
  -- This means complication for wl_registry::bind.
  -- There should be a way to get around that..
  returned <- withArgs args $ \argList -> withArray argList $ \argArray ->
    proxyMarshalArrayFlags (untypeRemote remote) opcode Nothing 0 (toFlags 0) argArray
  pure (typeRemote returned)

-- | Get return type if the "argument" should be a return.
getReturnType :: ArgumentType -> Maybe (Scan TH.Type)
getReturnType = \case
  FlatType (ArgNewID name) -> Just $ do
    interfaceType <- scanNewType (lead name)
    [t|Remote EClient $(TH.conT interfaceType)|]
  _ -> Nothing
