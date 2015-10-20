{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Vinyl.Field

makeTag "Foo"
makeTag "Bar"

type instance FieldType Foo = Integer
type instance FieldType Bar = Bool

main :: IO ()
main = do
  let r = foo =: 15 <+> bar =: True
  print r
