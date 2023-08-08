{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Scanner.Generate.GenTypes (
  generateAllTypes,
  generateInterfaceTypes,
) where

import Data.Foldable
import Foreign
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Flag
import Graphics.Wayland.Scanner.Generate.Documentation
import Graphics.Wayland.Scanner.Marshall
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH

-- ? Drop the prefix

generateAllTypes :: ProtocolSpec -> Scan [TH.Dec]
generateAllTypes protocol = foldMap generateInterfaceTypes protocol.interfaces

-- | Generate the interface type and relevant enum types.
generateInterfaceTypes :: InterfaceSpec -> Scan [TH.Dec]
generateInterfaceTypes interface = do
  interfaceType <- scanNewType domain
  let constr = TH.normalC interfaceType [TH.bangType noBang [t|Ptr $(TH.conT interfaceType)|]]
  typeDec <- TH.newtypeD (pure []) interfaceType [] Nothing constr [derives]
  docTypeDec <- addDescribe interface.ifDescribe typeDec
  let typeName = TH.nameBase interfaceType
  instances <-
    [d|
      instance Show $(TH.conT interfaceType) where
        show _ = typeName
      |]
  --
  enums <- foldMap (generateEnums domain) interface.enums
  pure (docTypeDec : instances <> enums)
 where
  domain = lead interface.ifName
  derives = TH.derivClause Nothing [[t|ArgumentAtom|]]
  noBang = TH.bang TH.noSourceUnpackedness TH.noSourceStrictness

-- | Generate the enum attached to each interface.
generateEnums :: QualifiedName -> EnumSpec -> Scan [TH.Dec]
generateEnums parent enum = do
  enumType <- scanNewType enumQualName
  notifyEnum enumQualName enum.enumType

  entries <- traverse (enumEntry parent) (toList enum.enumEntries)
  enumDec <- TH.dataD (pure []) enumType [] Nothing (simpleC . fst <$> entries) [derives]
  docEnumDec <- addDescribe enum.enumDescribe enumDec
  instances <- enumInstanceDec (TH.conT enumType) entries enum.enumType
  pure (docEnumDec : instances)
 where
  enumQualName = subName parent [enum.enumName]
  derives = TH.derivClause Nothing [[t|Show|], [t|Eq|], [t|Ord|]]
  simpleC name = TH.normalC name []

enumEntry :: QualifiedName -> EnumEntry -> Scan (TH.Name, Word)
enumEntry parent entry = do
  name <- aQualified HsConstructor (subName parent [entry.entryName])
  addSummaryToLocation (TH.DeclDoc name) entry.entrySummary
  pure (name, entry.entryValue)

enumInstanceDec :: Scan TH.Type -> [(TH.Name, Word)] -> EnumType -> Scan [TH.Dec]
enumInstanceDec typ entryPairs = \case
  SimpleEnum ->
    [d|
      instance Enum $typ where
        fromEnum = $(TH.lamCaseE $ uncurry nameToVal <$> entryPairs)
        toEnum = $(TH.lamCaseE $ uncurry valToName <$> entryPairs)

      instance ArgumentAtom $typ where
        withAtom enum = withAtom (EnumAtom enum)
        peekAtom arg = unEnumAtom <$> peekAtom arg
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
