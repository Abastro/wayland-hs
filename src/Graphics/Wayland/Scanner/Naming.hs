module Graphics.Wayland.Scanner.Naming (
  QualifiedName (..),
  lead,
  subName,
  NamingScheme (..),
  aQualified,
) where

import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Language.Haskell.TH qualified as TH

newtype QualifiedName = QualifiedName (NE.NonEmpty T.Text)
  deriving (Show, Eq, Ord)

lead :: T.Text -> QualifiedName
lead dom = QualifiedName (NE.singleton dom)

subName :: QualifiedName -> [T.Text] -> QualifiedName
subName (QualifiedName ls) sub = QualifiedName (NE.appendList ls sub)

data NamingScheme = HsConstructor | HsVariable

-- | Haskell does not support namespaces, so we approximate qualified names instead.
--
-- The input should be C-style names.
--
-- >>> aQualified HsConstructor (subName (lead $ T.pack "foo_bar") [T.pack "baz_foo"])
-- FooBarBazFoo
-- >>> aQualified HsVariable (subName (lead $ T.pack "foo_bar") [T.pack "baz_foo"])
-- fooBarBazFoo
--
aQualified :: NamingScheme -> QualifiedName -> TH.Name
aQualified scheme (QualifiedName names) = TH.mkName . T.unpack $ casing asSplit
 where
  asSplit = NE.toList names >>= T.splitOn (T.pack "_")
  casing = case scheme of
    HsConstructor -> T.concat . map T.toTitle
    HsVariable -> \case
      [] -> T.empty
      begin : trailing -> T.concat (begin : map T.toTitle trailing)
