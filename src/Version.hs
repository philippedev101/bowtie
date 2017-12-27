{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Version
  ( version
  ) where

import Distribution.PackageDescription.Parse (readGenericPackageDescription)
import Distribution.PackageDescription (specVersion, packageDescription)
import Language.Haskell.TH
import Distribution.Verbosity (deafening) -- try to lessen this later on

version :: Q Exp
version = stringE =<< (runIO $ readGenericPackageDescription deafening "bowtie.cabal"
  >>= \gpd -> return $ show $ specVersion $ packageDescription gpd)
