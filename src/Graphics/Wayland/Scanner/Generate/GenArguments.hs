{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenArguments (
  generateAllArguments,
  generateMessageArgument,
) where

import Data.Foldable
import Data.Int
import Data.Text qualified as T
import Data.Word
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Flag
import Graphics.Wayland.Scanner.Generate.Documentation
import Graphics.Wayland.Scanner.Marshall
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Util (WlArray)
import Language.Haskell.TH qualified as TH
import System.Posix.Types (Fd)

-- | Generate all arguments.
--
-- Note that this was written assuming that DuplicateRecordFields extension is enabled.
generateAllArguments :: ProtocolSpec -> Scan [TH.Dec]
generateAllArguments protocol = foldMap genForInterface protocol.interfaces
 where
  genForInterface interface =
    foldMap (generateMessageArgument $ lead interface.ifName) (interface.requests <> interface.events)

-- | Generate argument type for specific signal.
--
-- The interface types must have been introduced first for this to work properly.
generateMessageArgument :: QualifiedName -> MessageSpec -> Scan [TH.Dec]
generateMessageArgument parent message = do
  argsType <- scanNewType $ subName parent [message.msgName, T.pack "arg"]
  fields <- traverse (argumentField parent) (toList message.arguments)
  typeDec <- TH.dataD (pure []) argsType [] Nothing [TH.recC argsType $ pure <$> fields] [derives]
  docTypeDec <- addDescribe message.msgDescribe typeDec
  instances <- argumentInstances argsType [fieldName | (fieldName, _, _) <- fields]
  pure (docTypeDec : instances)
 where
  derives = TH.derivClause Nothing [[t|Show|]]

-- Note: parent here is the interface.
argumentField :: QualifiedName -> ArgumentSpec -> Scan TH.VarBangType
argumentField parent arg = do
  field <- aQualified HsVariable $ lead arg.argName
  -- TODO: duplicate record fields does not admit this - how to get around?
  -- addSummaryToName field arg.argSummary
  TH.varBangType field $ TH.bangType strict (argumentType parent arg.argType)
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

argumentType :: QualifiedName -> ArgumentType -> Scan TH.Type
argumentType parent = \case
  ArgInt -> [t|Int32|]
  ArgUInt -> [t|Word32|]
  ArgObject canNull (Just name) -> addNullable canNull (interfaceTypeOf name)
  ArgObject _ Nothing -> [t|Word32|] -- Placeholder for typeless case
  ArgNewID canNull (Just name) -> addNullable canNull (interfaceTypeOf name)
  ArgNewID _ Nothing -> [t|Word32|] -- Placeholder as well
  ArgString canNull -> addNullable canNull [t|T.Text|]
  ArgArray canNull -> addNullable canNull [t|WlArray|]
  ArgFd -> [t|Fd|]
  ArgEnum name -> enumTypeOf name
 where
  addNullable = \case
    NonNull -> id
    Nullable -> \typ -> [t|Maybe $typ|]
  interfaceTypeOf name = scannedType (lead name)
  enumTypeOf name = do
    let qualName = fromDotted parent name
        typ = scannedType qualName
    scannedEnumType qualName >>= \case
      SimpleEnum -> typ
      BitField -> [t|Flags $typ|]
