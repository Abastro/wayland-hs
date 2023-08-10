{-# LANGUAGE StrictData #-}

module Graphics.Wayland.Scanner.Types (
  ProtocolSpec (..),
  InterfaceSpec (..),
  MessageKind (..),
  MessageSpec (..),
  EnumSpec (..),
  EnumEntry (..),
  ArgumentSpec (..),
  CanNull (..),
  EnumType (..),
  ArgumentType (..),
  ArgFlat (..),
  ArgReference (..),
  Description (..),
) where

import Data.Text qualified as T
import Data.Vector qualified as V

data ProtocolSpec = ProtocolSpec
  { prName :: T.Text,
    interfaces :: V.Vector InterfaceSpec
  }
  deriving (Show)

data InterfaceSpec = InterfaceSpec
  { ifName :: T.Text,
    version :: Int,
    ifDescribe :: Maybe Description,
    requests :: V.Vector MessageSpec,
    events :: V.Vector MessageSpec,
    enums :: V.Vector EnumSpec
  }
  deriving (Show)

-- ? How to handle "since" ?

-- TODO Destructor requests
data MessageKind = Request | Event
  deriving (Show)
data MessageSpec = MessageSpec
  { msgName :: T.Text,
    msgType :: Maybe T.Text,
    msgSince :: Maybe Int,
    msgDescribe :: Maybe Description,
    arguments :: V.Vector ArgumentSpec
  }
  deriving (Show)

data EnumType = SimpleEnum | BitField
  deriving (Show)

data EnumSpec = EnumSpec
  { enumName :: T.Text,
    enumSince :: Maybe Int,
    enumType :: EnumType,
    enumDescribe :: Maybe Description,
    enumEntries :: V.Vector EnumEntry
  }
  deriving (Show)
data EnumEntry = EnumEntry
  { entryName :: T.Text,
    entryValue :: Word,
    entrySummary :: Maybe T.Text,
    entrySince :: Maybe Int
  }
  deriving (Show)

data ArgumentSpec = ArgumentSpec
  { argName :: T.Text,
    argType :: ArgumentType,
    argSummary :: Maybe T.Text
  }
  deriving (Show)

data CanNull = NonNull | Nullable
  deriving (Show)

data ArgumentType
  = FlatType ArgFlat
  | RefType CanNull ArgReference
  deriving (Show)

-- | Flat types, not allowing null.
data ArgFlat
  = ArgInt
  | ArgUInt
  | ArgFixed
  | ArgEnum T.Text
  | ArgNewID T.Text
  | ArgNewIDDyn
  | ArgFd
  deriving (Show)

data ArgReference
  = ArgObject T.Text
  | ArgObjectAny
  | ArgString
  | ArgArray
  deriving (Show)

data Description = Description
  { summary :: T.Text,
    describe :: T.Text
  }
instance Show Description where
  show :: Description -> String
  show Description{summary} = "Description " <> show summary
