{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Protocol.Types where

import Graphics.Wayland.Scanner

$(emitProtocolTypes "wl" "/usr/share/wayland/wayland.xml")
