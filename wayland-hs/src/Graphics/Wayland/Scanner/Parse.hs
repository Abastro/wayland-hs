module Graphics.Wayland.Scanner.Parse (
  protocolFromXML,
  parseProtocol,
) where

import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Graphics.Wayland.Scanner.Types
import Text.Read
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Posn (Posn, posInNewCxt)
import Text.XML.HaXml.Types
import Text.XML.HaXml.XmlContent

-- | Parse the protocol from XML.
-- First parameter is the file name, and the second is the contents.
protocolFromXML :: String -> String -> Either String ProtocolSpec
protocolFromXML file cont = do
  let Document _ _ rootElem _ = xmlParse file cont
  fst $ runParser parseProtocol [CElem rootElem (posInNewCxt file Nothing)]

-- String because laziness
attrsToMap :: [Attribute] -> M.Map String AttValue
attrsToMap attrList = M.fromList [(localName qName, value) | (qName, value) <- attrList]

attrText :: M.Map String AttValue -> String -> Maybe T.Text
attrText attrMap name = T.pack . attr2str <$> attrMap M.!? name

attrInt :: M.Map String AttValue -> String -> Maybe Int
attrInt attrMap name = readMaybe . attr2str =<< attrMap M.!? name

attrFlag :: M.Map String AttValue -> String -> Maybe Bool
attrFlag attrMap name = case attr2str <$> attrMap M.!? name of
  Just "true" -> Just True
  Just "false" -> Just False
  Nothing -> Just False
  _ -> Nothing

withElement :: [String] -> (M.Map String AttValue -> Element Posn -> XMLParser a) -> XMLParser a
withElement possibleTags elemParse = do
  (posn, theElement@(Elem qname attrs _)) <- posnElement possibleTags
  commit $
    elemParse (attrsToMap attrs) theElement
      `adjustErr` (\err -> err <> "\n" <> "in tag " <> printableName qname <> ", at " <> show posn)

simpleTag :: String -> (M.Map String AttValue -> XMLParser a) -> XMLParser a
simpleTag tag make = withElement [tag] $ \attrMap _ -> make attrMap

elementAttrIn :: [String] -> (String -> M.Map String AttValue -> XMLParser a) -> XMLParser a
elementAttrIn possibleTags inParser = withElement possibleTags $
  \attrMap theElement@(Elem qname _ _) -> do
    interior theElement $ inParser (localName qname) attrMap

-- See wayland.dtd
parseProtocol :: XMLParser ProtocolSpec
parseProtocol = elementAttrIn ["protocol"] $ \_ attrMap -> do
  Just prName <- pure (attrText attrMap "name")
  _ <- optional $ element ["copyright"]
  _ <- optional $ element ["description"]
  interfaces <- V.fromList <$> many1 parseInterface
  pure ProtocolSpec{prName, interfaces}

data InterfaceEntry = ERequest !MessageSpec | EEvent !MessageSpec | EEnum !EnumSpec
  deriving (Show)

parseInterface :: XMLParser InterfaceSpec
parseInterface = elementAttrIn ["interface"] $ \_ attrMap -> do
  ifName <- maybe (fail "interface is missing name") pure (attrText attrMap "name")
  Just version <- pure (attrInt attrMap "version")
  ifDescribe <- optional parseDescription
  interfaceEntries <- V.fromList <$> many1 (parseMessage <|> EEnum <$> parseEnum)
  let (requests, events, enums) = foldMap partitions interfaceEntries
  pure InterfaceSpec{ifName, version, ifDescribe, requests, events, enums}
 where
  partitions = \case
    ERequest message -> (V.singleton message, V.empty, V.empty)
    EEvent message -> (V.empty, V.singleton message, V.empty)
    EEnum enum -> (V.empty, V.empty, V.singleton enum)

parseMessage :: XMLParser InterfaceEntry
parseMessage = elementAttrIn ["request", "event"] $ \elemName attrMap -> do
  Just msgName <- pure (attrText attrMap "name")
  let msgType = asType $ attrText attrMap "type"
      msgSince = attrInt attrMap "since"
  msgDescribe <- optional parseDescription
  arguments <- V.fromList <$> many parseArgument
  pure $ asEntry elemName MessageSpec{msgName, msgType, msgSince, msgDescribe, arguments}
 where
  asType = \case
    Just typ | typ == T.pack "destructor" -> Destructor
    _ -> Normal
  asEntry = \case
    "request" -> ERequest
    "event" -> EEvent
    _ -> error "blame the cosmic rays flipping random bits"

parseEnum :: XMLParser EnumSpec
parseEnum = elementAttrIn ["enum"] $ \_ attrMap -> do
  Just enumName <- pure $ attrText attrMap "name"
  let enumSince = attrInt attrMap "since"
  Just enumType <- pure $ enumTypeOf <$> attrFlag attrMap "bitfield"
  enumDescribe <- optional parseDescription
  enumEntries <- V.fromList <$> many parseEnumEntry
  pure EnumSpec{enumName, enumSince, enumType, enumDescribe, enumEntries}
 where
  enumTypeOf flag = if flag then BitField else SimpleEnum

parseEnumEntry :: XMLParser EnumEntry
parseEnumEntry = simpleTag "entry" $ \attrMap -> do
  Just entryName <- pure $ attrText attrMap "name"
  Just entryValue <- pure $ fromIntegral <$> attrInt attrMap "value"
  let entrySummary = attrText attrMap "summary"
      entrySince = attrInt attrMap "since"
  pure EnumEntry{entryName, entryValue, entrySummary, entrySince}

parseArgument :: XMLParser ArgumentSpec
parseArgument = elementAttrIn ["arg"] $ \_ attrMap -> do
  _ <- optional $ element ["description"]
  Just argName <- pure $ attrText attrMap "name"
  Just argTypeName <- pure $ attr2str <$> attrMap M.!? "type"
  let argSummary = attrText attrMap "summary"
      mayInterface = attrText attrMap "interface"
  Just allowNull <- pure (canNull <$> attrFlag attrMap "allow-null")

  argType <-
    either FlatType (RefType allowNull) <$> case attrText attrMap "enum" of
      Just enum -> pure $ Left (ArgEnum enum) -- Special-case enum
      Nothing -> getArgumentType argName mayInterface argTypeName
  pure ArgumentSpec{argName, argType, argSummary}
 where
  canNull flag = if flag then Nullable else NonNull

getArgumentType :: T.Text -> Maybe T.Text -> String -> XMLParser (Either ArgFlat ArgReference)
getArgumentType argName mayInterface = \case
  "int" -> pure $ Left ArgInt
  "uint" -> pure $ Left ArgUInt
  "fixed" -> pure $ Left ArgFixed
  "object" -> pure $ Right (maybe ArgObjectAny ArgObject mayInterface)
  "new_id" -> pure $ Left (maybe ArgNewIDDyn ArgNewID mayInterface)
  "string" -> pure $ Right ArgString
  "array" -> pure $ Right ArgArray
  "fd" -> pure $ Left ArgFd
  _ -> fail $ "invalid type encountered for argument: " <> show argName

parseDescription :: XMLParser Description
parseDescription = elementAttrIn ["description"] $ \_ attrMap -> do
  Just summary <- pure $ attrText attrMap "summary"
  describe <- fmap fold . optional $ T.pack <$> text
  pure Description{summary, describe}
