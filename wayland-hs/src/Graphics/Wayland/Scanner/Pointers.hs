module Graphics.Wayland.Scanner.Pointers (

) where

import Graphics.Wayland.Scanner.Types
import qualified Data.Text as T

typeSignature :: ArgumentType -> String
typeSignature = \case
  FlatType ArgInt -> "i"
  FlatType ArgUInt -> "u"
  FlatType ArgFixed -> "f"
  RefType canNull ArgString -> nullSig canNull <> "s"
  RefType canNull (ArgObject _) -> nullSig canNull <> "o"
  RefType canNull ArgObjectAny -> nullSig canNull <> "o"
  FlatType (ArgEnum _) -> error "TODO"
  FlatType (ArgNewID _) -> "n"
  FlatType ArgNewIDDyn -> "n"
  RefType canNull ArgArray -> nullSig canNull <> "a"
  FlatType ArgFd -> "h"
  where
    nullSig = \case
      NonNull -> ""
      Nullable -> "?"

interfaceName :: ArgumentType -> Maybe T.Text
interfaceName = \case
  FlatType (ArgNewID interface) -> Just interface
  RefType _ (ArgObject interface) -> Just interface
  _ -> Nothing

