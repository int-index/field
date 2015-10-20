{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{- |

The @vinyl@ library provides the @ElField@ data type to implement
extensible records. However, @ElField@ uses type-level strings as tags,
effectively giving a rise to a form of duck typing. This module provides
an alternative approach where types serve the role of tags.

-}

module Data.Vinyl.Field
  ( module Data.Vinyl.Core
  , module Data.Vinyl.Lens
  , FieldType
  , Field(..)
  , Record
  , (=:)
  -- * Template Haskell utilities
  , makeTag
  ) where

import qualified Data.Char as Char
import Data.Vinyl.Core
import Data.Vinyl.Lens
import Data.Proxy
import qualified Language.Haskell.TH as TH

-- | Type of the payload associated with a tag.
type family FieldType (t :: k) :: *

-- | Wrapper around `FieldType` so it can be partially applied.
newtype Field (t :: k) = Field { getField :: FieldType t }

-- | A record of fields.
type Record = Rec Field

-- | Construct a `Record` with a single field. Tip: combine with `<+>`.
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

tagMangle :: String -> (TH.Name, TH.Name)
tagMangle str = (tagName, tagProxyName)
  where
    (strTagName, strTagProxyName) = case str of
      []     -> ([], [])
      (c:cs) -> (Char.toUpper c : cs, Char.toLower c : cs)
    tagName = TH.mkName strTagName
    tagProxyName = TH.mkName strTagProxyName

mkTag :: (TH.Name, TH.Name) -> TH.DecsQ
mkTag (tagName, tagProxyName) = do
  tagDecl <- emptyDataDecl tagName
  (tagProxySig, tagProxyVal) <- proxySimple tagProxyName (TH.conT tagName)
  return [tagDecl, tagProxySig, tagProxyVal]

-- |
-- Creates a tag and a value-level proxy for it.
--
-- @'makeTag' \"Foo\"@ generates the following code:
--
-- > data Foo
-- > foo :: Proxy Foo
-- > foo = Proxy
makeTag :: String -> TH.DecsQ
makeTag = mkTag . tagMangle
