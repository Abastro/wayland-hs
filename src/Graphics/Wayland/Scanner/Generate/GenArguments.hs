{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenArguments (
  generateAllArguments,
  generateSignalArgument,
) where

import Data.Foldable
import Data.Int
import Data.Text qualified as T
import Data.Traversable
import Data.Word
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Marshall
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Util (WlArray)
import Language.Haskell.TH qualified as TH
import System.Posix.Types (Fd)

-- | Generate all arguments.
--
-- Note that this was written assuming that DuplicateRecordFields extension is enabled.
generateAllArguments :: ProtocolSpec -> Scan [TH.Dec]
generateAllArguments protocol = fmap fold . for protocol.interfaces $ \interface -> do
  fold <$> traverse (generateSignalArgument interface.ifName) (interface.requests <> interface.events)

-- | Generate argument type for specific signal.
--
-- The interface types must have been introduced first for this to work properly.
generateSignalArgument :: T.Text -> SignalSpec -> Scan [TH.Dec]
generateSignalArgument interfaceName signal = do
  argsType <- scanNewType $ subName qualSignal [T.pack "arg"]
  typeDec <- TH.dataD (pure []) argsType [] Nothing [TH.recC argsType fieldsQ] [derives]
  fields <- sequenceA fieldsQ
  instances <- argumentInstances argsType [fieldName | (fieldName, _, _) <- fields]
  pure (typeDec : instances)
 where
  qualSignal = subName (lead interfaceName) [signal.sigName]
  derives = TH.derivClause Nothing [[t|Show|]]
  fieldsQ = argumentField qualSignal <$> toList signal.arguments

argumentField :: QualifiedName -> ArgumentSpec -> Scan TH.VarBangType
argumentField _ arg = do
  field <- aQualified HsVariable $ lead arg.argName
  TH.varBangType field $ TH.bangType strict (argumentType arg.argType)
 where
  strict = TH.bang TH.noSourceUnpackedness TH.sourceStrict

-- |
--   Generates:
--
-- > instance AsArguments FooBarArg where
-- >   withArgs args emit =
-- >     withAtom args.foo $ \fooP ->
-- >       withAtom args.bar $ \barP ->
-- >         emit [fooP, barP]
-- >   peekArgs = \case
-- >     [fooP, barP] ->
-- >       peekAtom fooP >>= \foo ->
-- >         peekAtom barP >>= \bar ->
-- >           pure FooBarArg{foo, bar}
-- >     _ -> error "wrong number of arguments, expected 2."
argumentInstances :: TH.Name -> [TH.Name] -> Scan [TH.Dec]
argumentInstances argsType fieldNames =
  [d|
    instance AsArguments $(TH.conT argsType) where
      argLength _ = numArgs
      withArgs $(wildOnZero $ TH.varP args) $(TH.varP emit) =
        $(foldr withAtomOn withArgsRet fieldNames)
      peekArgs $(TH.listP $ TH.varP <$> argNameList) =
        $(foldr peekAtomOn peekArgsRet fieldNames)
      peekArgs _ = error $(TH.litE $ TH.StringL peekError)
    |]
 where
  wildOnZero pat = if numArgs == 0 then TH.wildP else pat
  argNameFor field = TH.mkName $ TH.nameBase field <> "P"
  argNameList = argNameFor <$> fieldNames

  withAtomOn field next =
    [e|
      withAtom $(TH.getFieldE (TH.varE args) (TH.nameBase field)) $ \ $(TH.varP $ argNameFor field) ->
        $next
      |]
  withArgsRet = [e|$(TH.varE emit) $(TH.listE $ TH.varE <$> argNameList)|]
  peekAtomOn field next =
    [e|
      peekAtom $(TH.varE $ argNameFor field) >>= \ $(TH.varP field) ->
        $next
      |]
  peekArgsRet = [e|pure $(TH.recConE argsType $ [TH.fieldExp aField (TH.varE aField) | aField <- fieldNames])|]
  peekError = "wrong number of arguments, expected " <> show numArgs <> "."

  numArgs = length fieldNames
  args = TH.mkName "args"
  emit = TH.mkName "emit"

argumentType :: ArgumentType -> Scan TH.Type
argumentType = \case
  ArgInt -> [t|Int32|]
  ArgUInt -> [t|Word32|]
  ArgObject canNull (Just name) -> addNullable canNull (interfaceTypeOf name)
  ArgObject _ Nothing -> [t|Word32|] -- Placeholder for typeless case
  ArgNewID canNull (Just name) -> addNullable canNull (interfaceTypeOf name)
  ArgNewID _ Nothing -> [t|Word32|] -- Placeholder as well
  ArgString canNull -> addNullable canNull [t|T.Text|]
  ArgArray canNull -> addNullable canNull [t|WlArray|]
  ArgFd -> [t|Fd|]
  ArgEnum _ _ -> error "TODO"
 where
  addNullable = \case
    NonNull -> id
    Nullable -> \typ -> [t|Maybe $typ|]
  interfaceTypeOf name = TH.ConT <$> scannedType (lead name)
