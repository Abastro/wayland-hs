{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenArguments (
  generateAllArguments,
  generateMessageArgument,
) where

import Data.Foldable
import Data.Int
import Data.Monoid (Ap (..))
import Data.Proxy
import Data.Text qualified as T
import Data.Word
import Graphics.Flag
import Graphics.Wayland.Remote
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Generate.Documentation
import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Util (WlArray, WlFixed, Fd)
import Language.Haskell.TH qualified as TH

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
  argsType <- scanNewType $ subName parent [message.msgName, T.pack "args"]
  fields <- traverse (argumentField parent (TH.varT typeVar) message.msgName) (toList message.arguments)
  let kinded = TH.KindedTV typeVar () (TH.ConT ''End)
  typeDec <- TH.dataD (pure []) argsType [kinded] Nothing [TH.recC argsType $ pure <$> fields] [derives]
  docTypeDec <- addDescribe message.msgDescribe typeDec

  instances <- argumentInstances argsType [(fieldName, fieldType) | (fieldName, _, fieldType) <- fields]
  pure (docTypeDec : instances)
 where
  typeVar = TH.mkName "e"
  derives = TH.derivClause Nothing [[t|Show|]]

-- Note: parent here is the interface.
argumentField :: QualifiedName -> Scan TH.Type -> T.Text -> ArgumentSpec -> Scan TH.VarBangType
argumentField parentIf theEnd msgName arg = do
  field <- scanNewField $ subName parentIf [msgName, arg.argName]
  -- TODO: NoRecordFieldSelectors does not admit documentation - how to get around?
  -- addSummaryToLocation (TH.DeclDoc field) arg.argSummary
  TH.varBangType field $ TH.bangType strict (argumentType parentIf theEnd arg.argType)
 where
  strict = TH.bang TH.noSourceUnpackedness TH.sourceStrict

-- |
--   Generates:
--
-- > instance AsArguments (FooBarArg e) where
-- >   argLength _ = sum [argLength @Foo Proxy, argLength @Bar Proxy]
-- >   withArgs (FooBarArg foo bar) = getAp $ foldMap Ap [withArgs foo, withArgs bar]
-- >   peekArgs = pure FooBarArg <*> peekArgs <*> peekArgs
argumentInstances :: TH.Name -> [(TH.Name, TH.Type)] -> Scan [TH.Dec]
argumentInstances argsType fields =
  [d|
    instance AsArguments ($(TH.conT argsType) e) where
      argLength _ = sum $(TH.listE [[e|argLength @($(pure typ)) Proxy|] | (_, typ) <- fields])

      withArgs $(TH.conP argsType [TH.varP field | (field, _) <- fields]) =
        getAp $ foldMap Ap $(TH.listE [[e|withArgs $(TH.varE field)|] | (field, _) <- fields])

      peekArgs =
        $(foldl' (\l r -> [e|$l <*> $r|]) [e|pure $(TH.conE argsType)|] [[e|peekArgs|] | _ <- fields])
    |]

-- TODO Client-allocated types should not be NewID.
argumentType :: QualifiedName -> Scan TH.Type -> ArgumentType -> Scan TH.Type
argumentType parent theEnd = \case
  FlatType typ -> case typ of
    ArgInt -> [t|Int32|]
    ArgUInt -> [t|Word32|]
    ArgFixed -> [t|WlFixed|]
    ArgFd -> [t|Fd|]
    ArgEnum name -> enumTypeOf name
    ArgNewID name -> [t|NewID $theEnd $(interfaceTypeOf name)|]
    ArgNewIDDyn -> [t|NewID $theEnd ()|]
  -- References can be nullable
  RefType canNull typ -> addNullable canNull $ case typ of
    ArgObject name -> [t|Remote $theEnd $(interfaceTypeOf name)|]
    ArgObjectAny -> [t|RemoteAny $theEnd|]
    ArgString -> [t|T.Text|]
    ArgArray -> [t|WlArray|]
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
