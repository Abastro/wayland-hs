{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Protocol.Types where

import Graphics.Wayland.Scanner
import Language.Haskell.TH qualified as TH

$(emitProtocolTypes "/usr/share/wayland/wayland.xml")
