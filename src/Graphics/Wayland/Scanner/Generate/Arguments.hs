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
import Language.Haskell.TH qualified as TH
import System.Posix.Types

-- | Generate argument type for specific signal.
generateSignalArgument :: T.Text -> SignalSpec -> Scan [TH.Dec]
generateSignalArgument interfaceName signal = do
  decl <-
    TH.dataD
      (pure [])
      argsType
      []
      Nothing
      [TH.recC argsType $ argumentField qualSignal <$> toList signal.arguments]
      [TH.derivClause Nothing [[t|Show|], [t|Eq|]]]
  pure [decl]
 where
  qualSignal = aQualified [hsConName interfaceName, hsConName signal.sigName]
  argsType = symbol $ aQualified [qualSignal, T.pack "Arg"]

argumentField :: T.Text -> ArgumentSpec -> Scan TH.VarBangType
argumentField qualSignal arg = TH.varBangType field $ TH.bangType strict (argumentType arg.argType)
 where
  strict = TH.bang TH.noSourceUnpackedness TH.sourceStrict
  field = symbol . hsVarName $ aQualified [qualSignal, arg.argName]

argumentType :: ArgumentType -> Scan TH.Type
argumentType = \case
  ArgInt -> [t|Int32|]
  ArgUInt -> [t|Word32|]
  ArgObject canNull _ -> addNullable canNull undefined
  ArgNewID canNull _ -> addNullable canNull undefined
  ArgString canNull -> addNullable canNull [t|T.Text|]
  ArgArray canNull -> addNullable canNull [t|T.Text|]
  ArgFd -> [t|Fd|]
  ArgEnum _ _ -> error "TODO"
  where
    addNullable = \case
      NonNull -> id
      Nullable -> \typ -> [t|Maybe $typ|]
