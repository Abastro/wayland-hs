module Graphics.Wayland.Scanner.Naming (
  symbol,
  hsConName,
  hsVarName,
  aQualified,
) where

import Data.Text qualified as T
import Language.Haskell.TH qualified as TH

symbol :: T.Text -> TH.Name
symbol name = TH.mkName (T.unpack name)

-- | Turn snake_case into haskell constructor naming scheme.
hsConName :: T.Text -> T.Text
hsConName = error "TODO"

-- | Turn snake_case into haskell variable naming scheme.
hsVarName :: T.Text -> T.Text
hsVarName = error "TODO"

-- | Haskell does not support namespaces, so we approximate qualified names instead.
--
-- > aQualified [x] == x
-- > aQualified [aQualified xs, aQualified ys] == aQualified (xs <> ys)
aQualified :: [T.Text] -> T.Text
aQualified = error "TODO"
