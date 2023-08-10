{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Graphics.Wayland.Protocol.Client where

import Graphics.Wayland.Scanner
import Graphics.Wayland.Protocol.Types

$(emitProtocolEnd EndClient "wl" "/usr/share/wayland/wayland.xml")
