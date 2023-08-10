{-# LANGUAGE DataKinds #-}

module Graphics.Wayland.Remote (
  End (..),
  Remote,
  RemoteAny (..),
  RemoteServer,
  RemoteClient,
  NewID (..),
  untypeRemote,
  typeRemote,
  CInterface (..),
  HasInterface (..),
) where

import Data.Coerce
import Data.Kind
import Foreign (Ptr, Word32)

-- | An end is either client or server.
data End = EndClient | EndServer

-- | Representation of a remote resource.
--
-- The end represents this end.
newtype Remote (e :: End) (a :: Type) = Remote (Ptr (RemoteAny e))

-- | Like Remote, but untyped for usage in foreign interfaces.
newtype RemoteAny (e :: End) = RemoteAny (Ptr (RemoteAny e))

instance Show (RemoteAny e) where
  show :: RemoteAny e -> String
  show _ = "Any"

type RemoteServer = RemoteAny EndServer
type RemoteClient = RemoteAny EndClient

-- | Denotes a new object the server allocates for the client.
--
-- Explicit pointers are passed around on client-allocated objects, so Remote type is used for those.
newtype NewID (e :: End) (a :: Type) = NewID Word32
  deriving (Show)

-- | Cast to untyped.
untypeRemote :: Remote e a -> RemoteAny e
untypeRemote = coerce

-- | Cast to typed.
typeRemote :: RemoteAny e -> Remote e a
typeRemote = coerce

-- | C type for interfaces.
newtype CInterface = CInterface (Ptr CInterface)

-- | Denotes types describing interfaces.
class HasInterface a where
  -- | Haskell-side name of the interface.
  interfaceName :: p a -> String

  interfaceVersion :: p a -> Int

  -- | Obtain the corresponding C Interface.
  asCInterface :: p a -> CInterface

instance (HasInterface a) => Show (Remote e a) where
  show :: (HasInterface a) => Remote e a -> String
  show = interfaceName
