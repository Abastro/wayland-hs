{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Test where

import Graphics.Wayland.Scanner

$(emitProtocolTypes "/usr/share/wayland/wayland.xml")
