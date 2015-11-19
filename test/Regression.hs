{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Data.Vinyl.Field

makeTag "Foo"
makeTag "Bar"

main :: IO ()
main = do
  let
    r0 :: Record _
    r0 = foo =: 15 <+> bar =: True

    r1 :: Num n => Record '[Foo ':- n, Bar ':- Bool]
    r1 = foo =: 42 <+> bar =: False

  print r0
  print r1
