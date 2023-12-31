{-# LANGUAGE DataKinds #-}

module Graphics.Wayland.Remote (
  End (..),
  Remote,
  RemoteAny (..),
  EClient,
  EServer,
  ServerAny,
  ClientAny,
  NewID (..),
  NewIDAny (..),
  untypeRemote,
  typeRemote,
  CInterface (..),
  CMessage (..),
  HasInterface (..),
) where

import Data.Coerce
import Data.Kind
import Data.Text qualified as T
import Foreign (Ptr, Word32, Storable)

-- | An end is either client or server.
data End = EndClient | EndServer

-- | Type alias for EndClient.
type EClient = 'EndClient

-- | Type alias for EndServer.
type EServer = 'EndServer

-- | Representation of a remote resource.
--
-- The end represents this end.
newtype Remote (e :: End) (a :: Type) = Remote (Ptr (RemoteAny e))

-- | Like Remote, but untyped for usage in foreign interfaces.
newtype RemoteAny (e :: End) = RemoteAny (Ptr (RemoteAny e))

instance Show (RemoteAny e) where
  show :: RemoteAny e -> String
  show _ = "Any"

-- | Any remote type on the server.
type ServerAny = RemoteAny EServer

-- | Any remote type on the client.
type ClientAny = RemoteAny EClient

-- | Denotes a new object the server allocates for the client.
--
-- Explicit pointers are passed around on client-allocated objects, so Remote type is used for those.
newtype NewID (e :: End) (a :: Type) = NewID Word32
  deriving (Show)

-- | Blanket new_id type, which also carries the information around.
data NewIDAny (e :: End) = NewIDAny
  { -- | This one denotes the wayland-style interface name.
    newIDInterface :: !T.Text,
    newIDVersion :: !Word32,
    newID :: !Word32
  }
  deriving (Show)

-- | Cast to untyped.
untypeRemote :: Remote e a -> RemoteAny e
untypeRemote = coerce

-- | Cast to typed.
typeRemote :: RemoteAny e -> Remote e a
typeRemote = coerce

-- | C struct for wayland interfaces.
newtype CInterface = CInterface (Ptr CInterface)
  deriving (Storable)

-- | C struct for wayland messages.
newtype CMessage = CMessage (Ptr CMessage)
  deriving (Storable)

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
