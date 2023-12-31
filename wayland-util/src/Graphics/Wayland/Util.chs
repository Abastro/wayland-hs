module Graphics.Wayland.Util (
  createInterface,
  createMessageRaw,
  setMessageTypes,
  WlArray(..), WlArrayPtr,
  WlRes, WlFixed,
  Int32, Word32, Fd,
  Argument(..), ArgumentPtr, ptrToArgument, argumentToPtr, wordToArgument, argumentToWord,
  Dispatcher, CDispatcher, withDispatcher,
)
where

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Fixed
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Posix.Types (Fd)

import Graphics.ForeignUtil
import Graphics.Wayland.Remote

#include <wayland-util.h>
{# context prefix="wl" #}

-- From Remote
{# pointer *message as CMessage newtype nocode #}
{# pointer *interface as CInterface newtype nocode #}

-- | Create an interface. It will never be deallocated.
createInterface :: String -> Int -> [CMessage] -> [CMessage] -> IO CInterface
createInterface name version requests events = do
  interfacePtr <- mallocBytes {# sizeof interface #}
  {# set interface->name #} interfacePtr =<< newCString name
  {# set interface->version #} interfacePtr (fromIntegral $ version)
  -- Hack for encoding array, because normal pointer and array pointer are indistinguishable.
  {# set interface->method_count #} interfacePtr (fromIntegral $ length requests)
  {# set interface->methods #} interfacePtr =<< (CMessage <$> newArray requests)
  {# set interface->event_count #} interfacePtr (fromIntegral $ length events)
  {# set interface->events #} interfacePtr =<< (CMessage <$> newArray events)
  pure (CInterface interfacePtr)

-- | Create a message. It will never be deallocated.
createMessageRaw :: String -> String -> IO CMessage
createMessageRaw name sig = do
  messagePtr <- mallocBytes {# sizeof message #}
  {# set message->name #} messagePtr =<< newCString name
  {# set message->signature #} messagePtr =<< newCString sig
  pure (CMessage messagePtr)

setMessageTypes :: CMessage -> [Maybe CInterface] -> IO ()
setMessageTypes (CMessage messagePtr) interfaces = do
  {# set message->types #} messagePtr =<< newArray (map nullable interfaces)

-- | Named as WlArray to avoid name collision.
newtype WlArray = WlArray BS.ByteString
  deriving (Show)
{# pointer *array as WlArrayPtr -> WlArray #}

instance Storable WlArray where
  sizeOf _ = {# sizeof array #}
  alignment _ = {# alignof array #}
  peek arrPtr = do
    size <- fromIntegral <$> {# get array->size #} arrPtr
    raw <- castPtr <$> {# get array->data #} arrPtr
    WlArray <$> BS.packCStringLen (raw, size)
  poke arrPtr (WlArray bs) = BS.unsafeUseAsCStringLen bs $ \(bsPtr, len) -> do
    {# set array->size #} arrPtr (fromIntegral len)
    {# set array->alloc #} arrPtr (fromIntegral len)
    {# set array->data #} arrPtr (castPtr bsPtr)

data WlRes
instance HasResolution WlRes where
  resolution _ = 256 -- 8 bits of precision
type WlFixed = Fixed WlRes

-- | Data to feed wl_resource_post_event_array.
--
-- Argument data itself could be an integer, and it lags a tag.
-- Hence, it is represented as a WordPtr for seamless conversion.
newtype Argument = Argument WordPtr
{# pointer *argument as ArgumentPtr -> Argument #}

instance Storable Argument where
  sizeOf _ = {# sizeof argument #}
  alignment _ = {# alignof argument #}
  peek argPtr = Argument <$> peek (castPtr argPtr)
  poke argPtr (Argument arg) = poke (castPtr argPtr) arg

ptrToArgument :: Ptr a -> Argument
ptrToArgument ptr = Argument (ptrToWordPtr ptr)

argumentToPtr :: Argument -> Ptr a
argumentToPtr (Argument arg) = wordPtrToPtr arg

wordToArgument :: Word -> Argument
wordToArgument num = Argument (fromIntegral num)

argumentToWord :: Argument ->  Word
argumentToWord (Argument arg) = fromIntegral arg

-- |
-- Dispatcher function type alias
--
-- A dispatcher is a function that handles the emitting of callbacks in client
-- code. For programs directly using the C library, this is done by using
-- libffi to call function pointers. When binding to languages other than C,
-- dispatchers provide a way to abstract the function calling process to be
-- friendlier to other function calling systems.
--
-- A dispatcher takes five arguments: The first is the dispatcher-specific
-- implementation associated with the target object. The second is the object
-- upon which the callback is being invoked (either wl_proxy or wl_resource).
-- The third and fourth arguments are the opcode and the wl_message
-- corresponding to the callback. The final argument is an array of arguments
-- received from the other process via the wire protocol.
--
-- \param "const void *" Dispatcher-specific implementation data
-- \param "void *" Callback invocation target (wl_proxy or `wl_resource`)
-- \param uint32_t Callback opcode
-- \param "const struct wl_message *" Callback message signature
-- \param "union wl_argument *" Array of received arguments
--
-- \return 0 on success, or -1 on failure
--
type Dispatcher end impl = StablePtr impl -> RemoteAny end -> Word32 -> CMessage -> Ptr Argument -> IO Int
type CDispatcher = Ptr () -> Ptr () -> Word32 -> CMessage -> Ptr Argument -> IO CInt
foreign import ccall unsafe "wrapper" makeDispatcher :: CDispatcher -> IO (FunPtr CDispatcher)
withDispatcher :: Dispatcher end impl -> (FunPtr CDispatcher -> IO a) -> IO a
withDispatcher func act = do
  fnPtr <- makeDispatcher $ \impl targetPtr opcode message argPtr ->
    fromIntegral <$> func (castPtrToStablePtr impl) (RemoteAny . castPtr $ targetPtr) opcode message argPtr
  act fnPtr

-- TODO Maybe the dispatcher should be freed properly.
