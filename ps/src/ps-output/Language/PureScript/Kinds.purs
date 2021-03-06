-- File auto generated by purescript-bridge! --
module Language.PureScript.Kinds where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Language.PureScript.Names (KindName, ProperName, Qualified)
import Prim (Int)

import Prelude
import Data.Generic (class Generic)

data Kind =
    KUnknown Int
  | Row Kind
  | FunKind Kind Kind
  | NamedKind (Qualified (ProperName KindName))

derive instance genericKind :: Generic Kind


--------------------------------------------------------------------------------
_KUnknown :: Prism' Kind Int
_KUnknown = prism' KUnknown f
  where
    f (KUnknown a) = Just $ a
    f _ = Nothing

_Row :: Prism' Kind Kind
_Row = prism' Row f
  where
    f (Row a) = Just $ a
    f _ = Nothing

_FunKind :: Prism' Kind { a :: Kind, b :: Kind }
_FunKind = prism' (\{ a, b } -> FunKind a b) f
  where
    f (FunKind a b) = Just $ { a: a, b: b }
    f _ = Nothing

_NamedKind :: Prism' Kind (Qualified (ProperName KindName))
_NamedKind = prism' NamedKind f
  where
    f (NamedKind a) = Just $ a
    f _ = Nothing

--------------------------------------------------------------------------------
