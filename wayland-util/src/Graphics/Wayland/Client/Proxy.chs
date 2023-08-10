module Graphics.Wayland.Client.Proxy (
  MarshalFlag(..),
  proxyMarshalArrayFlags,
  proxyGetVersion,
)
where

import Foreign
-- import Foreign.C.Types

import Graphics.ForeignUtil
import Graphics.Flag
import Graphics.Wayland.Remote
{# import Graphics.Wayland.Util #}

#include <wayland-client.h>
{# context prefix="wl" #}

{# typedef uint32_t Word32 #}

{# pointer *proxy as ClientAny newtype nocode #}

data MarshalFlag = MarshalDestroy
instance Flag MarshalFlag where
  flagBits MarshalDestroy = 1 -- c2hs translates WL_MARSHAL_FLAG_DESTROY literally..

flagMarshal :: Flags MarshalFlag -> Word32
flagMarshal = fromIntegral . fromFlags

-- | No documentation.
-- 
-- > struct wl_proxy *
-- > wl_proxy_marshal_array_flags(struct wl_proxy *proxy, uint32_t opcode,
-- >            const struct wl_interface *interface,
-- >            uint32_t version,
-- >            uint32_t flags,
-- >            union wl_argument *args);
{# fun unsafe proxy_marshal_array_flags as ^ {
    `ClientAny',
    `Word32',
    nullable `Maybe Interface',
    `Word32',
    flagMarshal `Flags MarshalFlag',
    `ArgumentPtr'
  } -> `ClientAny' #}

-- | No documentation.
--
-- > uint32_t
-- > wl_proxy_get_version(struct wl_proxy *proxy);
{# fun unsafe proxy_get_version as ^ {
    `ClientAny'    
  } -> `Word32' #}
