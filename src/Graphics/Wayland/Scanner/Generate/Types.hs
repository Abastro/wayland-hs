{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.Types (
  generateInterfaceTypes,
) where

import Foreign
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH
import Graphics.Wayland.Scanner.Marshall (ArgumentAtom(..))

-- ? Drop the prefix

-- | Simply generate the interface type.
generateInterfaceTypes :: InterfaceSpec -> Scan [TH.Dec]
generateInterfaceTypes interface = do
  interfaceType <- scanNewType [interface.ifName]
  let constr = TH.normalC interfaceType [TH.bangType noBang [t|Ptr $(TH.conT interfaceType)|]]
  decl <- TH.newtypeD (pure []) interfaceType [] Nothing constr []
  atomInstance <- [d|
    instance ArgumentAtom $(TH.conT interfaceType) where
      withAtom = $(TH.conE interfaceType)
      peekAtom = undefined
    |]
  pure [decl]
 where
  noBang = TH.bang TH.noSourceUnpackedness TH.noSourceStrictness
