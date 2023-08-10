{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Protocol.Server where

import Graphics.Wayland.Scanner
import Graphics.Wayland.Protocol.Types

$(emitProtocolEnd EndServer "wl" "/usr/share/wayland/wayland.xml")
