{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Generate.GenTypes (
  generateAllTypes,
  generateInterfaceTypes,
) where

import Foreign
import Graphics.Wayland.Scanner.Env
import Graphics.Wayland.Scanner.Marshall
import Graphics.Wayland.Scanner.Naming
import Graphics.Wayland.Scanner.Types
import Language.Haskell.TH qualified as TH
import Data.Foldable

-- ? Drop the prefix

generateAllTypes :: ProtocolSpec -> Scan [TH.Dec]
generateAllTypes protocol = fold <$> traverse generateInterfaceTypes protocol.interfaces

-- | Simply generate the interface type.
generateInterfaceTypes :: InterfaceSpec -> Scan [TH.Dec]
generateInterfaceTypes interface = do
  interfaceType <- scanNewType (lead interface.ifName)
  let constr = TH.normalC interfaceType [TH.bangType noBang [t|Ptr $(TH.conT interfaceType)|]]
  typeDec <- TH.newtypeD (pure []) interfaceType [] Nothing constr []
  let typeName = TH.nameBase interfaceType
  instances <-
    [d|
      instance Show $(TH.conT interfaceType) where
        show _ = typeName
      instance ArgumentAtom $(TH.conT interfaceType) where
        withAtom = withAtomPtr $(TH.lam1E (TH.conP interfaceType [TH.varP ptr]) (TH.varE ptr))
        peekAtom = peekAtomPtr $(TH.conE interfaceType)
      |]
  pure (typeDec : instances)
 where
  ptr = TH.mkName "ptr"
  noBang = TH.bang TH.noSourceUnpackedness TH.noSourceStrictness
