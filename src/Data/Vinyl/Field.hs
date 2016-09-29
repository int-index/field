{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , Bundle(..)
  , FieldType
  , Field(..)
  , Record
  , (=:)
  ) where

import Data.Vinyl.Core
import Data.Vinyl.Lens

-- | Bundle a tag with type.
data Bundle k star = k :- star

-- | Extract the type of the field from a `Bundle`.
type family FieldType (b :: Bundle k star) where
  FieldType (t ':- ty) = ty

-- | Reify a `Bundle`.
newtype Field (b :: Bundle k *) = Field { getField :: FieldType b }

-- | A record of fields.
type Record = Rec Field

-- | Construct a `Record` with a single field. Tip: combine with `<+>`.
(=:) :: proxy (t :: k) -> ty -> Record '[t ':- ty]
(=:) _ x = Field x :& RNil

instance Show ty => Show (Field (t ':- ty)) where
  showsPrec n = showsPrec n . getField
