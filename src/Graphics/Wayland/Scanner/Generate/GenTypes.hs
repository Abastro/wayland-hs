{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenTypes (
  generateAllTypes,
  generateInterfaceType,
) where

import Data.Foldable
import Data.Text qualified as T
import Data.Traversable
import Foreign
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Marshall
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH

-- ? Drop the prefix

generateAllTypes :: ProtocolSpec -> Scan [TH.Dec]
generateAllTypes protocol = do
  fold <$> traverse generateInterfaceType protocol.interfaces

generateInterfaceType :: InterfaceSpec -> Scan [TH.Dec]
generateInterfaceType interface = do
  interfaceType <- scanNewType (lead interface.ifName)
  let constr = TH.normalC interfaceType [TH.bangType noBang [t|Ptr $(TH.conT interfaceType)|]]
  typeDec <- TH.newtypeD (pure []) interfaceType [] Nothing constr []
  let typeName = TH.nameBase interfaceType
  instances <-
    [d|
      instance Show $(TH.conT interfaceType) where
        show _ = typeName

      instance ArgumentAtom $(TH.conT interfaceType) where
        withAtom = withAtomPtr $(TH.lam1E (TH.conP interfaceType [TH.varP ptr]) (TH.varE ptr))
        peekAtom = peekAtomPtr $(TH.conE interfaceType)
      |]
  pure (typeDec : instances)
 where
  ptr = TH.mkName "ptr"
  noBang = TH.bang TH.noSourceUnpackedness TH.noSourceStrictness

-- | Generate the enum attached to each interface.
generateEnums :: QualifiedName -> EnumSpec -> Scan [TH.Dec]
generateEnums parent enum = do
  enumType <- scanNewType (subName parent [enum.enumName])
  enumDec <- TH.dataD (pure []) enumType [] Nothing (entryConstructor <$> toList enum.enumEntries) [derives]
  instances <-
    [d|
      instance Enum $(TH.conT enumType)
      |]
  pure [enumDec]
 where
  entryPairs = for (toList enum.enumEntries) $ \entry ->
    (, entry.entryValue) <$> aQualified HsConstructor (subName parent [entry.entryName])

  entryName :: EnumEntry -> Scan TH.Name
  entryName entry = aQualified HsConstructor (subName parent [entry.entryName])
  entryConstructor :: EnumEntry -> Scan TH.Con
  entryConstructor entry = do
    name <- entryName entry
    TH.normalC name []
  derives = TH.derivClause Nothing [[t|Show|], [t|Eq|], [t|Ord|], [t|Bounded|]]
