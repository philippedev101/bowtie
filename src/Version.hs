{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Version
  ( version
  ) where

import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription (specVersion, packageDescription)
import Data.Version (showVersion)
import Language.Haskell.TH
import Distribution.Verbosity (deafening) -- try to lessen this later on

version :: Q Exp
version = stringE =<< (runIO $ readPackageDescription deafening "bowtie.cabal"
  >>= \gpd -> return $ showVersion $ specVersion $ packageDescription gpd)
