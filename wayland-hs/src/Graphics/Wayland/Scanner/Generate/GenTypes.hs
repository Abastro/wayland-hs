{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenTypes (
  generateAllTypes,
  generateInterfaceTypes,
) where

import Data.Foldable
import Data.Text qualified as T
import Graphics.Flag
import Graphics.Wayland.Remote
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Generate.Documentation
import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH
import System.IO.Unsafe (unsafePerformIO)
import Foreign (newStablePtr, castStablePtrToPtr, castPtr)

generateAllTypes :: ProtocolSpec -> Scan [TH.Dec]
generateAllTypes protocol = foldMap generateInterfaceTypes protocol.interfaces

-- | Generate the interface type and relevant enum types.
generateInterfaceTypes :: InterfaceSpec -> Scan [TH.Dec]
generateInterfaceTypes interface = do
  interfaceType <- scanNewType domain
  typeDec <- TH.dataD (pure []) interfaceType [] Nothing [] []
  docTypeDec <- addDescribe interface.ifDescribe typeDec

  let typeName = TH.nameBase interfaceType
      version = interface.version
  instances <-
    [d|
      instance HasInterface $(TH.conT interfaceType) where
        interfaceName _ = typeName
        interfaceVersion _ = version
        asCInterface _ = undefined
      |]
  --
  enums <- foldMap (generateEnums domain) interface.enums
  pure (docTypeDec : instances <> enums)
 where
  domain = lead interface.ifName

-- | Generate the enum attached to each interface.
generateEnums :: QualifiedName -> EnumSpec -> Scan [TH.Dec]
generateEnums parent enum = do
  enumType <- scanNewType enumQualName
  notifyEnum enumQualName enum.enumType

  entries <- traverse (enumEntry parent) (toList enum.enumEntries)
  enumDec <- TH.dataD (pure []) enumType [] Nothing (simpleC . fst <$> entries) [derives]
  docEnumDec <- addDescribe enum.enumDescribe enumDec

  instances <- enumInstances (TH.conT enumType) entries enum.enumType
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

enumInstances :: Scan TH.Type -> [(TH.Name, Word)] -> EnumType -> Scan [TH.Dec]
enumInstances typ entryPairs = \case
  SimpleEnum ->
    [d|
      instance Enum $typ where
        fromEnum = $(TH.lamCaseE $ uncurry nameToVal <$> entryPairs)
        toEnum = $(TH.lamCaseE $ uncurry valToName <$> entryPairs)

      instance AsArguments $typ where
        withArgs enum = withArgs (EnumAtom enum)
        peekArgs = unEnumAtom <$> peekArgs
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

generateCInterface :: InterfaceSpec -> Scan [TH.Dec]
generateCInterface interface = do
  cifName <- aQualified HsVariable $ subName (lead interface.ifName) [T.pack "interface"]
  cifSig <- TH.sigD cifName [t|CInterface|]
  cifDec <- TH.valD (TH.varP cifName) (TH.normalB $ TH.unTypeCode cifExp) []
  undefined
 where
  cifExp :: TH.Code Scan CInterface
  cifExp =
    [||
    unsafePerformIO $ do
      -- stable <- castPtr . castStablePtrToPtr <$> newStablePtr ()
      undefined
    ||]
