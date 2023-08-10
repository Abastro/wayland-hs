module Graphics.Wayland.Client.Proxy (
  Proxy(..),
  proxyMarshalArrayFlags,
)
where

import Foreign
-- import Foreign.C.Types

import Graphics.ForeignUtil
import Graphics.Flag
{# import Graphics.Wayland.Util #}

#include <wayland-client.h>
{# context prefix="wl" #}

{# typedef uint32_t Word32 #}

{# pointer *proxy as Proxy newtype #}

data MarshalFlag = MarshalDestroy
instance Flag MarshalFlag where
  flagBits MarshalDestroy = 1 -- c2hs translates WL_MARSHAL_FLAG_DESTROY literally..

flagMarshall :: Flags MarshalFlag -> Word32
flagMarshall = fromIntegral . fromFlags

-- | No documentation.
-- 
-- > struct wl_proxy *
-- > wl_proxy_marshal_array_flags(struct wl_proxy *proxy, uint32_t opcode,
-- >            const struct wl_interface *interface,
-- >            uint32_t version,
-- >            uint32_t flags,
-- >            union wl_argument *args);
{# fun unsafe proxy_marshal_array_flags as ^ {
    `Proxy',
    `Word32',
    nullable `Maybe Interface',
    `Word32',
    flagMarshall `Flags MarshalFlag',
    `ArgumentPtr'
  } -> `Proxy' #}
