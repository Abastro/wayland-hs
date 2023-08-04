module Graphics.Wayland.Scanner.Parse (
  protocolFromXML,
  parseProtocol,
) where

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

parseProtocol :: XMLParser ProtocolSpec
parseProtocol = elementAttrIn ["protocol"] $ \_ attrMap -> do
  Just prName <- pure (attrText attrMap "name")
  _ <- optional $ element ["copyright"]
  interfaces <- V.fromList <$> many parseInterface
  pure ProtocolSpec{prName, interfaces}

data InterfaceEntry = ESignal !SignalType !SignalSpec | EEnum EnumSpec
  deriving (Show)

parseInterface :: XMLParser InterfaceSpec
parseInterface = elementAttrIn ["interface"] $ \_ attrMap -> do
  ifName <- maybe (fail "interface is missing name") pure (attrText attrMap "name")
  Just version <- pure (attrInt attrMap "version")
  _ <- optional $ element ["description"]
  interfaceEntries <- V.fromList <$> many (parseSignal <|> EEnum <$> parseEnum)
  let requests = V.mapMaybe getRequest interfaceEntries
      events = V.mapMaybe getEvent interfaceEntries
  pure InterfaceSpec{ifName, version, requests, events}
 where
  getRequest = \case
    ESignal Request signal -> Just signal
    _ -> Nothing
  getEvent = \case
    ESignal Event signal -> Just signal
    _ -> Nothing

-- ? How to handle "since" ?
parseSignal :: XMLParser InterfaceEntry
parseSignal = elementAttrIn ["request", "event"] $ \name attrMap -> do
  Just sigName <- pure (attrText attrMap "name")
  _ <- optional $ element ["description"]
  arguments <- V.fromList <$> many parseArgument
  pure $ ESignal (signalType name) SignalSpec{sigName, arguments}
 where
  signalType = \case
    "request" -> Request
    "event" -> Event
    _ -> error "blame the cosmic rays flipping random bits"

parseArgument :: XMLParser ArgumentSpec
parseArgument = simpleTag "arg" $ \attrMap -> do
  Just argName <- pure (attrText attrMap "name")
  Just allowNull <- pure (canNull <$> attrFlag attrMap "allow-null")
  let mayInterface = attrText attrMap "interface"
  argType <- case attr2str <$> attrMap M.!? "type" of
    Just "int" -> pure ArgInt
    Just "uint" -> pure ArgUInt
    Just "fixed" -> pure ArgInt -- TODO Fixed type
    Just "object" -> do
      -- TODO Properly handle the null case
      interface <- maybe (pure T.empty) pure mayInterface
      pure $ ArgObject allowNull interface
    Just "new_id" -> do
      interface <- maybe (pure T.empty) pure mayInterface
      pure $ ArgNewID allowNull interface
    Just "string" -> pure $ ArgString allowNull
    Just "array" -> pure $ ArgArray allowNull
    Just "fd" -> pure ArgFd
    _ -> fail $ "invalid type encountered for argument: " <> show argName
  pure ArgumentSpec{argName, argType}
 where
  canNull flag = if flag then Nullable else NonNull

parseEnum :: XMLParser EnumSpec
parseEnum = elementAttrIn ["enum"] $ \_ attrMap -> do
  Just enumName <- pure $ attrText attrMap "name"
  Just enumType <- pure $ enumTypeOf <$> attrFlag attrMap "bitfield"
  _ <- optional $ element ["description"]
  enumEntries <- V.fromList <$> many parseEnumEntry
  pure EnumSpec{enumName, enumType, enumEntries}
 where
  enumTypeOf flag = if flag then BitField else SimpleEnum

parseEnumEntry :: XMLParser EnumEntry
parseEnumEntry = simpleTag "entry" $ \attrMap -> do
  Just entryName <- pure $ attrText attrMap "name"
  Just entryValue <- pure $ fromIntegral <$> attrInt attrMap "value"
  pure EnumEntry{entryName, entryValue}