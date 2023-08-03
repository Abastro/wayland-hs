{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.Arguments (
  generateSignalArgument,
) where

import Data.Foldable
import Data.Int
import Data.Text qualified as T
import Data.Word
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Naming
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Util (WlArray)
import Language.Haskell.TH qualified as TH
import System.Posix.Types

-- | Generate argument type for specific signal.
--
-- The interface types must have been introduced first for this to work properly.
generateSignalArgument :: T.Text -> SignalSpec -> Scan [TH.Dec]
generateSignalArgument interfaceName signal = do
  argsType <- scanNewType [interfaceName, signal.sigName, T.pack "arg"]
  decl <-
    TH.dataD
      (pure [])
      argsType
      []
      Nothing
      [TH.recC argsType $ argumentField [interfaceName, signal.sigName] <$> toList signal.arguments]
      [TH.derivClause Nothing [[t|Show|], [t|Eq|]]]
  pure [decl]

argumentField :: [T.Text] -> ArgumentSpec -> Scan TH.VarBangType
argumentField qualSignal arg = TH.varBangType field $ TH.bangType strict (argumentType arg.argType)
 where
  strict = TH.bang TH.noSourceUnpackedness TH.sourceStrict
  field = symbol . hsVarName $ aQualified (qualSignal <> [arg.argName])

argumentType :: ArgumentType -> Scan TH.Type
argumentType = \case
  ArgInt -> [t|Int32|]
  ArgUInt -> [t|Word32|]
  ArgObject canNull name -> addNullable canNull (interfaceTypeOf name)
  ArgNewID canNull name -> addNullable canNull (interfaceTypeOf name)
  ArgString canNull -> addNullable canNull [t|T.Text|]
  ArgArray canNull -> addNullable canNull [t|WlArray|]
  ArgFd -> [t|Fd|]
  ArgEnum _ _ -> error "TODO"
 where
  addNullable = \case
    NonNull -> id
    Nullable -> \typ -> [t|Maybe $typ|]
  interfaceTypeOf name = TH.ConT <$> scannedType [name]
