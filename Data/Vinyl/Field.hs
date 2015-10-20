{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Vinyl.Field
  ( FieldType
  , Field(..)
  , (=:)
  , module Data.Vinyl.Core
  , makeTag
  ) where

import qualified Data.Char as Char
import Data.Vinyl.Core
import Data.Proxy
import qualified Language.Haskell.TH as TH

type family FieldType (t :: k) :: *

data Field (t :: k) = Field { getField :: FieldType t }

type Record = Rec Field

(=:) :: proxy (t :: k) -> FieldType t -> Record '[t]
(=:) _ x = Field x :& RNil

instance Show (FieldType t) => Show (Field t) where
  showsPrec n (Field t) = showsPrec n t

emptyDataDecl :: TH.Name -> TH.DecQ
emptyDataDecl name = TH.dataD (return []) name [] [] []

funSimple :: TH.Name -> TH.ExpQ -> TH.DecQ
funSimple name body = TH.funD name [ TH.clause [] (TH.normalB body) [] ]

proxySimple :: TH.Name -> TH.TypeQ -> TH.Q (TH.Dec, TH.Dec)
proxySimple name ty = do
  sig <- TH.sigD name [t| Proxy $ty |]
  val <- funSimple name [e| Proxy |]
  return (sig, val)

makeTag :: String -> TH.DecsQ
makeTag strTagName = do
    tagDecl <- emptyDataDecl tagName
    (tagProxySig, tagProxyVal) <- proxySimple tagProxyName tag
    return [tagDecl, tagProxySig, tagProxyVal]
  where
    (strTagName, strTagProxyName) = case strTagName of
      []     -> ([], [])
      (c:cs) -> (Char.toUpper c : cs, Char.toLower c : cs)
    tagName = TH.mkName strTagName
    tag = TH.conT tagName
    tagProxyName = TH.mkName strTagProxyName
