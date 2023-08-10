module Graphics.Wayland.Scanner.Marshal (
  module Control.Monad.Trans.Class,
  ContT (..),
  evalContT,
  PeelT,
  runPeelT,
  peeling,
  peekOne,
  AsArguments (..),
  EnumAtom (..),
  unEnumAtom,
) where

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Cont
import Control.Monad.Trans.State.Strict
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Fixed (Fixed (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Foreign (Ptr, nullPtr, peek, with)
import Graphics.Flag
import Graphics.Wayland.Remote
import Graphics.Wayland.Util

-- | Monad where one could "peel" away the elements, progressively checking at it.
newtype PeelT e m a = PeelT (StateT [e] m a)
  deriving (Functor, Applicative, Monad)

runPeelT :: (Monad m) => PeelT e m a -> [e] -> m a
runPeelT (PeelT act) = evalStateT act

peeling :: (Monad m) => (e -> m a) -> PeelT e m a
peeling run = PeelT . StateT $ \case
  (x : xs) -> (,xs) <$> run x
  [] -> error "cannot peel from empty list"

-- | Peek one without popping it out.
peekOne :: (Monad m) => PeelT e m e
peekOne = PeelT $ gets head

-- | Arguments record which could be marshalled into wl_argument array.
class AsArguments arg where
  -- | Number of argument atoms in the type.
  argLength :: proxy arg -> Int
  argLength _ = 1 -- default is the singleton case.

  -- | Marshall arguments into Argument list.
  withArgs :: arg -> ContT r IO [Argument]

  -- | Marshall arguments out of Argument list.
  peekArgs :: PeelT Argument IO arg

withArgsPtr :: Ptr a -> ContT r IO [Argument]
withArgsPtr ptr = pure [ptrToArgument ptr]

withArgsWord :: Word -> ContT r IO [Argument]
withArgsWord word = pure [wordToArgument word]

peelingPtr :: (Ptr a -> IO b) -> PeelT Argument IO b
peelingPtr act = peeling (act . argumentToPtr)

peelWord :: PeelT Argument IO Word
peelWord = peeling (pure . argumentToWord)

instance AsArguments T.Text where
  withArgs :: T.Text -> ContT r IO [Argument]
  withArgs txt = withArgsPtr =<< ContT (BS.useAsCString $ T.encodeUtf8 txt)
  peekArgs :: PeelT Argument IO T.Text
  peekArgs = T.decodeUtf8 <$> peelingPtr BS.unsafePackCString

instance AsArguments WlArray where
  withArgs :: WlArray -> ContT r IO [Argument]
  withArgs array = withArgsPtr =<< ContT (with array)
  peekArgs :: PeelT Argument IO WlArray
  peekArgs = peelingPtr peek

instance AsArguments Word32 where
  withArgs :: Word32 -> ContT r IO [Argument]
  withArgs num = withArgsWord (fromIntegral num)
  peekArgs :: PeelT Argument IO Word32
  peekArgs = fromIntegral <$> peelWord

instance AsArguments Int32 where
  withArgs :: Int32 -> ContT r IO [Argument]
  withArgs num = withArgsWord (fromIntegral num)
  peekArgs :: PeelT Argument IO Int32
  peekArgs = fromIntegral <$> peelWord

-- Fixed type converts its internals
instance AsArguments WlFixed where
  withArgs :: WlFixed -> ContT r IO [Argument]
  withArgs (MkFixed inner) = withArgsWord (fromIntegral inner)
  peekArgs :: PeelT Argument IO WlFixed
  peekArgs = MkFixed . fromIntegral <$> peelWord

instance AsArguments Fd where
  withArgs :: Fd -> ContT r IO [Argument]
  withArgs fd = withArgsWord (fromIntegral fd)
  peekArgs :: PeelT Argument IO Fd
  peekArgs = fromIntegral <$> peelWord

-- Nullable case; Simply check if the first atom is null.
instance (AsArguments t) => AsArguments (Maybe t) where
  argLength :: (AsArguments t) => proxy (Maybe t) -> Int
  argLength _ = argLength (Proxy @t)

  withArgs :: (AsArguments t) => Maybe t -> ContT r IO [Argument]
  withArgs = \case
    Just val -> withArgs val
    Nothing -> withArgsPtr nullPtr

  peekArgs :: (AsArguments t) => PeelT Argument IO (Maybe t)
  peekArgs =
    peekOne >>= \case
      arg | argumentToPtr arg == nullPtr -> pure Nothing
      _ -> Just <$> peekArgs

instance AsArguments (RemoteAny e) where
  withArgs :: RemoteAny e -> ContT r IO [Argument]
  withArgs (RemoteAny ptr) = withArgsPtr ptr
  peekArgs :: PeelT Argument IO (RemoteAny e)
  peekArgs = RemoteAny <$> peelingPtr pure

instance (HasInterface a) => AsArguments (Remote e a) where
  withArgs :: (HasInterface a) => Remote e a -> ContT r IO [Argument]
  withArgs remote = withArgs (untypeRemote remote)
  peekArgs :: (HasInterface a) => PeelT Argument IO (Remote e a)
  peekArgs = typeRemote <$> peekArgs

instance AsArguments (NewID e a) where
  withArgs :: NewID e a -> ContT r IO [Argument]
  withArgs (NewID ident) = withArgs ident
  peekArgs :: PeelT Argument IO (NewID e a)
  peekArgs = NewID <$> peekArgs

instance AsArguments (NewIDAny e) where
  argLength :: proxy (NewIDAny e) -> Int
  argLength _ = 3

  withArgs :: NewIDAny e -> ContT r IO [Argument]
  withArgs newID = do
    ifName <- withArgs newID.newIDInterface
    ifVersion <- withArgs newID.newIDVersion
    ident <- withArgs newID.newID
    pure (ifName <> ifVersion <> ident)

  peekArgs :: PeelT Argument IO (NewIDAny e)
  peekArgs = NewIDAny <$> peekArgs <*> peekArgs <*> peekArgs

-- | To have shorter templated code.
newtype EnumAtom a = EnumAtom a

unEnumAtom :: EnumAtom a -> a
unEnumAtom (EnumAtom a) = a

instance (Enum a) => AsArguments (EnumAtom a) where
  withArgs :: (Enum a) => EnumAtom a -> ContT r IO [Argument]
  withArgs atom = withArgsWord (fromIntegral . fromEnum . unEnumAtom $ atom)
  peekArgs :: (Enum a) => PeelT Argument IO (EnumAtom a)
  peekArgs = EnumAtom . toEnum . fromIntegral <$> peelWord

instance (Flag a) => AsArguments (Flags a) where
  withArgs :: (Flag a) => Flags a -> ContT r IO [Argument]
  withArgs flags = withArgsWord (fromFlags flags)
  peekArgs :: (Flag a) => PeelT Argument IO (Flags a)
  peekArgs = toFlags <$> peelWord
