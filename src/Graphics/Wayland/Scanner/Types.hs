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
) where

import Data.Text qualified as T
import Data.Vector qualified as V

data ProtocolSpec = ProtocolSpec
  { prName :: T.Text,
    interfaces :: V.Vector InterfaceSpec
  }

data InterfaceSpec = InterfaceSpec
  { ifName :: T.Text,
    version :: Int,
    requests :: V.Vector SignalSpec,
    events :: V.Vector SignalSpec
  }

data SignalType = Request | Event
data SignalSpec = SignalSpec
  { sigName :: T.Text,
    arguments :: V.Vector ArgumentSpec
  }

data EnumType = SimpleEnum | BitField

data EnumSpec = EnumSpec
  { enumName :: T.Text,
    enumType :: EnumType,
    enumEntries :: V.Vector EnumEntry
  }
data EnumEntry = EnumEntry
  { entryName :: T.Text,
    entryValue :: Word
  }

data ArgumentSpec = ArgumentSpec
  { argName :: T.Text,
    argType :: ArgumentType
  }

data CanNull = NonNull | Nullable

data ArgumentType
  = ArgInt
  | ArgUInt
  | ArgObject CanNull T.Text
  | ArgNewID CanNull T.Text
  | ArgString CanNull
  | ArgArray CanNull
  | ArgFd
  | -- | Considered separately on codegen.
    ArgEnum EnumType T.Text
