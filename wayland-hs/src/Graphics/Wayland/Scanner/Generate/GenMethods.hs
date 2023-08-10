{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenMethods (
  generateAllMessages,
  generateInterfaceMessages,
) where

import Control.Monad
import Data.Foldable
import Data.Monoid qualified as Monoid
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Foreign hiding (void)
import Graphics.Flag (makeFlags, toFlags)
import Graphics.Wayland.Client.Proxy (MarshalFlag (..), proxyAddDispatcher, proxyGetVersion, proxyMarshalArrayFlags)
import Graphics.Wayland.Remote
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Generate.Documentation
import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Server.Resource (resourcePostEventArray, resourceSetDispatcher)
import Graphics.Wayland.Util (Argument, Dispatcher)
import Language.Haskell.TH qualified as TH

generateAllMessages :: End -> ProtocolSpec -> Scan [TH.Dec]
generateAllMessages end protocol = foldMap (generateInterfaceMessages end) protocol.interfaces

generateInterfaceMessages :: End -> InterfaceSpec -> Scan [TH.Dec]
generateInterfaceMessages end interface = do
  receives <- generateMessageHandler end (lead interface.ifName) (toList inbound)
  sends <- concat <$> zipWithM (generateMessageSend end $ lead interface.ifName) [0 ..] (toList outbound)
  pure (receives <> sends)
 where
  (inbound, outbound) = case end of
    EndClient -> (interface.events, interface.requests)
    EndServer -> (interface.requests, interface.events)

-- | Generate message handler type and a function to set the handler for resource.
generateMessageHandler :: End -> QualifiedName -> [MessageSpec] -> Scan [TH.Dec]
generateMessageHandler _ _ [] = pure [] -- Special empty case
generateMessageHandler end parent messages = do
  ifType <- TH.conT <$> scanNewType parent
  handlerName <- scanNewType (subName parent [T.pack "handler"])
  fields <- traverse (handlerField parent ifType theEnd) messages
  handlerDec <- TH.dataD (pure []) handlerName [] Nothing [TH.recC handlerName $ pure <$> fields] []

  let matches = zipWith (chooseHandle handler) [0 ..] [fieldName | (fieldName, _, _) <- fields]
  dispatchExp <-
    [e|
      \handlerPtr remote $(TH.varP opcode) _ argArray -> do
        $(TH.varP handler) <- deRefStablePtr handlerPtr
        let handle = $(TH.caseE (TH.varE opcode) matches)
        handle remote argArray
        pure 0
      |]

  setterName <- aQualified HsVariable (subName parent [T.pack "set", T.pack "handler"])
  case end of
    EndServer -> do
      setterSig <- TH.sigD setterName [t|Remote EServer $ifType -> $(TH.conT handlerName) -> IO ()|]
      setterDec <- TH.funD setterName [TH.clause [] (TH.normalB [e|setRequestHandler $ $(pure dispatchExp)|]) []]
      pure [handlerDec, setterSig, setterDec]
    EndClient -> do
      setterSig <- TH.sigD setterName [t|Remote EClient $ifType -> $(TH.conT handlerName) -> IO ()|]
      setterDec <- TH.funD setterName [TH.clause [] (TH.normalB [e|setEventHandler $ $(pure dispatchExp)|]) []]
      pure [handlerDec, setterSig, setterDec]
 where
  theEnd = case end of
    EndClient -> [t|EClient|]
    EndServer -> [t|EServer|]

  handler = TH.mkName "handler"
  opcode = TH.mkName "opcode"

handlerField :: QualifiedName -> Scan TH.Type -> Scan TH.Type -> MessageSpec -> Scan TH.VarBangType
handlerField parent ifType theEnd message = do
  field <- scanNewField $ subName parent [message.msgName <> T.pack "_handle"]
  argsType <- scanNewType $ subName parent [message.msgName, T.pack "args"]
  TH.varBangType field $ TH.bangType strict [t|Remote $theEnd $ifType -> $(TH.conT argsType) $theEnd -> IO ()|]
 where
  strict = TH.bang TH.noSourceUnpackedness TH.sourceStrict

chooseHandle :: TH.Name -> Word32 -> TH.Name -> Scan TH.Match
chooseHandle handler opcode field =
  TH.match (TH.litP $ TH.integerL $ fromIntegral opcode) (TH.normalB [e|handleMessage $selected|]) []
 where
  selected = TH.getFieldE (TH.varE handler) (TH.nameBase field)


-- | Generate functions for sending messages.
generateMessageSend :: End -> QualifiedName -> Word32 -> MessageSpec -> Scan [TH.Dec]
generateMessageSend end parent idx message = do
  -- The environment is orthogonal, so we "scan new type" here.
  ifName <- scanNewType parent
  argsName <- scanNewType (subName parent [message.msgName, T.pack "args"])
  fnName <- aQualified HsVariable $ subName parent [message.msgName]
  let interfaceType = TH.conT ifName
      argsType = TH.conT argsName
  addSummaryToLocation (TH.DeclDoc fnName) $ Just (T.pack $ "See '" <> TH.nameBase argsName <> "'.")
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

handleMessage :: forall arg end a. (AsArguments arg) => (Remote end a -> arg -> IO ()) -> RemoteAny end -> Ptr Argument -> IO ()
handleMessage handler remote argArray = do
  argList <- peekArray (argLength @arg Proxy) argArray
  args <- runPeelT peekArgs argList
  handler (typeRemote remote) args

setEventHandler :: Dispatcher EClient impl -> Remote EClient a -> impl -> IO ()
setEventHandler dispatcher source handler = do
  handlerPtr <- newStablePtr handler
  _ <- proxyAddDispatcher (untypeRemote source) dispatcher handlerPtr
  pure ()

-- ? Sophisticated destroy callback?
setRequestHandler :: Dispatcher EServer impl -> Remote EServer a -> impl -> IO ()
setRequestHandler dispatcher source handler = do
  handlerPtr <- newStablePtr handler
  _ <- resourceSetDispatcher (untypeRemote source) dispatcher handlerPtr $ \_ -> do
    freeStablePtr handlerPtr
  pure ()

sendEvent :: (AsArguments arg) => Word32 -> Remote EServer a -> arg -> IO ()
sendEvent opcode remote args = evalContT $ do
  argList <- withArgs args
  argArray <- ContT (withArray argList)
  lift $ resourcePostEventArray (untypeRemote remote) opcode argArray

sendRequest :: (AsArguments arg) => Word32 -> [MarshalFlag] -> Remote EClient a -> arg -> IO ()
sendRequest opcode flags remote args = evalContT $ do
  argList <- withArgs args
  argArray <- ContT (withArray argList)
  version <- lift $ proxyGetVersion (untypeRemote remote)
  _ <- lift $ proxyMarshalArrayFlags (untypeRemote remote) opcode Nothing version (makeFlags flags) argArray
  pure ()

sendRequestRet :: (AsArguments arg) => Word32 -> Remote EClient a -> arg -> IO (Remote EClient b)
sendRequestRet opcode remote args = evalContT $ do
  -- TODO Pass interface C struct
  argList <- withArgs args
  argArray <- ContT (withArray argList)
  version <- lift $ proxyGetVersion (untypeRemote remote)
  returned <- lift $ proxyMarshalArrayFlags (untypeRemote remote) opcode Nothing version (toFlags 0) argArray
  pure (typeRemote returned)

-- receiveEvent :: (AsArguments arg) =>

-- | Get return type if the "argument" should be a return.
getReturnType :: ArgumentType -> Maybe (Scan TH.Type)
getReturnType = \case
  FlatType (ArgNewID name) -> Just $ do
    interfaceType <- scanNewType (lead name)
    [t|Remote EClient $(TH.conT interfaceType)|]
  _ -> Nothing
