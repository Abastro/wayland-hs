module Graphics.Wayland.Scanner.Marshal (
  AsArguments (..),
  ArgumentAtom (..),
  EnumAtom (..),
  unEnumAtom,
  trivial,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Fixed (Fixed (..))
import Data.Int
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word
import Foreign (nullPtr, peek, with)
import Graphics.Flag
import Graphics.Wayland.Remote
import Graphics.Wayland.Util (Argument, WlArray, WlFixed, argumentToPtr, argumentToWord, ptrToArgument, wordToArgument)
import Language.Haskell.TH qualified as TH
import System.Posix.Types

trivial :: TH.Q [TH.Dec]
trivial = pure []

-- | Arguments record which could be marshalled into wl_argument array.
class AsArguments arg where
  -- | Number of argument atoms in the type.
  argLength :: proxy arg -> Int

  -- | Marshall arguments into Argument list.
  withArgs :: arg -> ([Argument] -> IO a) -> IO a

  -- | Marshall arguments out of Argument list.
  peekArgs :: [Argument] -> IO arg

-- | Types which support the conversion to Argument atom.
class ArgumentAtom atom where
  withAtom :: atom -> (Argument -> IO a) -> IO a
  peekAtom :: Argument -> IO atom

instance ArgumentAtom T.Text where
  withAtom :: T.Text -> (Argument -> IO a) -> IO a
  withAtom txt act = BS.useAsCString (T.encodeUtf8 txt) (act . ptrToArgument)
  peekAtom :: Argument -> IO T.Text
  peekAtom arg = T.decodeUtf8 <$> BS.unsafePackCString (argumentToPtr arg)

instance ArgumentAtom WlArray where
  withAtom :: WlArray -> (Argument -> IO a) -> IO a
  withAtom array act = with array (act . ptrToArgument)
  peekAtom :: Argument -> IO WlArray
  peekAtom arg = peek (argumentToPtr arg)

instance ArgumentAtom Word32 where
  withAtom :: Word32 -> (Argument -> IO a) -> IO a
  withAtom num act = act $ wordToArgument (fromIntegral num)
  peekAtom :: Argument -> IO Word32
  peekAtom arg = pure $ fromIntegral (argumentToWord arg)

instance ArgumentAtom Int32 where
  withAtom :: Int32 -> (Argument -> IO a) -> IO a
  withAtom num act = act $ wordToArgument (fromIntegral num)
  peekAtom :: Argument -> IO Int32
  peekAtom arg = pure $ fromIntegral (argumentToWord arg)

-- Fixed type converts its internals
instance ArgumentAtom WlFixed where
  withAtom :: WlFixed -> (Argument -> IO a) -> IO a
  withAtom (MkFixed inner) act = act $ wordToArgument (fromIntegral inner)
  peekAtom :: Argument -> IO WlFixed
  peekAtom arg = pure $ (MkFixed . fromIntegral) (argumentToWord arg)

instance ArgumentAtom Fd where
  withAtom :: Fd -> (Argument -> IO a) -> IO a
  withAtom fd act = act $ wordToArgument (fromIntegral fd)
  peekAtom :: Argument -> IO Fd
  peekAtom arg = pure $ fromIntegral (argumentToWord arg)

instance (ArgumentAtom t) => ArgumentAtom (Maybe t) where
  withAtom :: (ArgumentAtom t) => Maybe t -> (Argument -> IO a) -> IO a
  withAtom = \case
    Just val -> withAtom val
    Nothing -> \act -> act $ ptrToArgument nullPtr
  peekAtom :: (ArgumentAtom t) => Argument -> IO (Maybe t)
  peekAtom arg = case argumentToPtr arg of
    n | n == nullPtr -> pure Nothing
    _ -> Just <$> peekAtom arg

instance ArgumentAtom (RemoteAny e) where
  withAtom :: RemoteAny e -> (Argument -> IO a) -> IO a
  withAtom (RemoteAny ptr) act = act $ ptrToArgument ptr
  peekAtom :: Argument -> IO (RemoteAny e)
  peekAtom arg = pure . RemoteAny $ argumentToPtr arg

instance (HasInterface a) => ArgumentAtom (Remote e a) where
  withAtom :: Remote e a -> (Argument -> IO b) -> IO b
  withAtom remote = withAtom (untypeRemote remote)
  peekAtom :: Argument -> IO (Remote e a)
  peekAtom arg = typeRemote <$> peekAtom arg

instance ArgumentAtom (NewID e a) where
  withAtom :: NewID e a -> (Argument -> IO b) -> IO b
  withAtom (NewID ident) = withAtom ident
  peekAtom :: Argument -> IO (NewID e a)
  peekAtom arg = NewID <$> peekAtom arg

-- | To have shorter templated code.
newtype EnumAtom a = EnumAtom a

unEnumAtom :: EnumAtom a -> a
unEnumAtom (EnumAtom a) = a

instance (Enum a) => ArgumentAtom (EnumAtom a) where
  withAtom :: (Enum a) => EnumAtom a -> (Argument -> IO b) -> IO b
  withAtom atom act = act $ wordToArgument (fromIntegral . fromEnum . unEnumAtom $ atom)
  peekAtom :: (Enum a) => Argument -> IO (EnumAtom a)
  peekAtom arg = pure $ (EnumAtom . toEnum . fromIntegral) (argumentToWord arg)

instance (Flag a) => ArgumentAtom (Flags a) where
  withAtom :: (Flag a) => Flags a -> (Argument -> IO b) -> IO b
  withAtom flags act = act $ wordToArgument (fromFlags flags)
  peekAtom :: (Flag a) => Argument -> IO (Flags a)
  peekAtom arg = pure $ toFlags (argumentToWord arg)