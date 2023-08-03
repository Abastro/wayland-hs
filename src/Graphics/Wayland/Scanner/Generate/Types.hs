{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.Types (
  interfaceTypeName,
  generateInterfaceTypes,
) where

import Foreign
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Naming
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH

-- ? Drop the prefix
interfaceTypeName :: InterfaceSpec -> TH.Name
interfaceTypeName interface = symbol interface.ifName

-- | Simply generate the interface type.
generateInterfaceTypes :: InterfaceSpec -> Scan [TH.Dec]
generateInterfaceTypes interface = do
  decl <-
    TH.newtypeD
      (pure [])
      interfaceType
      []
      Nothing
      (TH.normalC interfaceType [TH.bangType noBang [t|Ptr $(TH.conT interfaceType)|]])
      []
  pure [decl]
 where
  noBang = TH.bang TH.noSourceUnpackedness TH.noSourceStrictness
  interfaceType = interfaceTypeName interface
