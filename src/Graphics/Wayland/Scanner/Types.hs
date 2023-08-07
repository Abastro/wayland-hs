{-# LANGUAGE StrictData #-}

module Graphics.Wayland.Scanner.Types (
  ProtocolSpec (..),
  InterfaceSpec (..),
  SignalType (..),
  SignalSpec (..),
  EnumSpec (..),
  EnumEntry (..),
  ArgumentSpec (..),
  CanNull (..),
  EnumType (..),
  ArgumentType (..),
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
    requests :: V.Vector SignalSpec,
    events :: V.Vector SignalSpec,
    enums :: V.Vector EnumSpec
  }
  deriving (Show)

data SignalType = Request | Event
  deriving (Show)
data SignalSpec = SignalSpec
  { sigName :: T.Text,
    sigDescribe :: Maybe Description,
    arguments :: V.Vector ArgumentSpec
  }
  deriving (Show)

data EnumType = SimpleEnum | BitField
  deriving (Show)

data EnumSpec = EnumSpec
  { enumName :: T.Text,
    enumType :: EnumType,
    enumDescribe :: Maybe Description,
    enumEntries :: V.Vector EnumEntry
  }
  deriving (Show)
data EnumEntry = EnumEntry
  { entryName :: T.Text,
    entryValue :: Word,
    entrySummary :: Maybe T.Text
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
  = ArgInt
  | ArgUInt
  | ArgObject CanNull (Maybe T.Text)
  | ArgNewID CanNull (Maybe T.Text)
  | ArgString CanNull
  | ArgArray CanNull
  | ArgFd
  | -- | Considered separately on codegen.
    ArgEnum T.Text
  deriving (Show)

data Description = Description
  { summary :: T.Text,
    describe :: T.Text
  }
instance Show Description where
  show :: Description -> String
  show Description{summary} = "Description " <> show summary
