{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Proxy
import Data.Vinyl.Field

data Foo
data Bar

main :: IO ()
main = do
  let
    r0 :: Record _
    r0 = Proxy @Foo =: 15 <+> Proxy @Bar =: True

    r1 :: Num n => Record '[Foo ':- n, Bar ':- Bool]
    r1 = Proxy @Foo =: 42 <+> Proxy @Bar =: False

  print r0
  print r1
