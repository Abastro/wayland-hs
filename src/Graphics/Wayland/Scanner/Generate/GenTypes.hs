{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenTypes (
  generateAllTypes,
  generateInterfaceTypes,
) where

import Data.Foldable
import Foreign
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Flag
import Graphics.Wayland.Scanner.Marshall
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH

-- ? Drop the prefix

generateAllTypes :: ProtocolSpec -> Scan [TH.Dec]
generateAllTypes protocol = do
  fold <$> traverse generateInterfaceTypes protocol.interfaces

-- | Generate the interface type and relevant enum types.
generateInterfaceTypes :: InterfaceSpec -> Scan [TH.Dec]
generateInterfaceTypes interface = do
  interfaceType <- scanNewType (lead interface.ifName)
  let constr = TH.normalC interfaceType [TH.bangType noBang [t|Ptr $(TH.conT interfaceType)|]]
  typeDec <- TH.newtypeD (pure []) interfaceType [] Nothing constr [derives]
  let typeName = TH.nameBase interfaceType
  instances <-
    [d|
      instance Show $(TH.conT interfaceType) where
        show _ = typeName
      |]
  -- Then, enums
  enums <- fold <$> traverse (generateEnums (lead interface.ifName)) interface.enums
  pure (typeDec : instances <> enums)
 where
  derives = TH.derivClause Nothing [[t|ArgumentAtom|]]
  noBang = TH.bang TH.noSourceUnpackedness TH.noSourceStrictness

-- | Generate the enum attached to each interface.
generateEnums :: QualifiedName -> EnumSpec -> Scan [TH.Dec]
generateEnums parent enum = do
  enumType <- scanNewType (subName parent [enum.enumName])
  entries <- entryPairsOf (toList enum.enumEntries)
  enumDec <- TH.dataD (pure []) enumType [] Nothing (simpleC . fst <$> entries) [derives]
  instances <- enumInstanceDecl (TH.conT enumType) entries enum.enumType
  pure (enumDec : instances)
 where
  derives = TH.derivClause Nothing [[t|Show|], [t|Eq|], [t|Ord|]]
  entryPairsOf = traverse $ \entry ->
    (,entry.entryValue) <$> aQualified HsConstructor (subName parent [entry.entryName])
  simpleC name = TH.normalC name []

enumInstanceDecl :: Scan TH.Type -> [(TH.Name, Word)] -> EnumType -> Scan [TH.Dec]
enumInstanceDecl typ entryPairs = \case
  SimpleEnum ->
    [d|
      instance Enum $typ where
        fromEnum = $(TH.lamCaseE $ uncurry nameToVal <$> entryPairs)
        toEnum = $(TH.lamCaseE $ uncurry valToName <$> entryPairs)
      |]
  BitField ->
    [d|
      instance Flag $typ where
        flagBits = $(TH.lamCaseE $ uncurry nameToVal <$> entryPairs)
      |]
 where
  nameToVal name (val :: Word) = simpleMatch (TH.conP name []) [e|val|]
  valToName name (val :: Word) = simpleMatch (TH.litP . TH.IntegerL $ fromIntegral val) (TH.conE name)
  simpleMatch pat val = TH.match pat (TH.normalB val) []
