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
hsConName = aQualified . T.splitOn (T.pack "_")

-- | Turn snake_case into haskell variable naming scheme.
hsVarName :: T.Text -> T.Text
hsVarName text = let (heading, remainder) = T.splitAt 1 $ hsConName text in T.toLower heading <> remainder

-- | Haskell does not support namespaces, so we approximate qualified names instead.
--
-- > aQualified [x] == x
-- > aQualified [aQualified xs, aQualified ys] == aQualified (xs <> ys)
aQualified :: [T.Text] -> T.Text
aQualified = T.concat . map T.toTitle
