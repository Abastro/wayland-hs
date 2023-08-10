{-# LANGUAGE TemplateHaskell #-}

module Graphics.Wayland.Protocol.Types where

import Graphics.Wayland.Scanner

$(emitProtocolTypes "wl" "/usr/share/wayland/wayland.xml")

-- What
{-
$( do
    parsed <- protocolFromXML "wayland.xml" <$> TH.runIO (readFile "/usr/share/wayland/wayland.xml")
    case parsed of
      Left err -> fail err
      Right protocol -> runScan (T.pack "wl") $ do
        foldMap simpleGen protocol.interfaces
       where
        derives = TH.derivClause Nothing [[t|ArgumentAtom|]]

        simpleGen :: InterfaceSpec -> Scan [TH.Dec]
        simpleGen interface = do
          interfaceType <- scanNewType (lead interface.ifName)
          let noBang = TH.bang TH.noSourceUnpackedness TH.noSourceStrictness
              constr = TH.normalC interfaceType [TH.bangType noBang [t|Ptr $(TH.conT interfaceType)|]]
          typeDec <- TH.newtypeD (pure []) interfaceType [] Nothing constr [derives]
          docTypeDec <- addDescribe interface.ifDescribe typeDec
          let typeName = TH.nameBase interfaceType
          instances <-
            [d|
              instance Show $(TH.conT interfaceType) where
                show _ = typeName
              |]
          -- meh <- liftQ trivial
          pure (docTypeDec : instances)
 )
-}