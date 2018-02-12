-- File auto generated by purescript-bridge! --
module Language.PureScript.AST.Literals where

import Data.Either (Either)
import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Data.Tuple (Tuple)
import Prim (Array, Boolean, Int, Number, String)

import Prelude
import Data.Generic (class Generic)

data Literal a =
    NumericLiteral (Either Int Number)
  | StringLiteral String
  | CharLiteral String
  | BooleanLiteral Boolean
  | ArrayLiteral (Array a)
  | ObjectLiteral (Array (Tuple String a))

derive instance genericLiteral :: Generic a => Generic (Literal a)


--------------------------------------------------------------------------------
_NumericLiteral :: forall a. Prism' (Literal a) (Either Int Number)
_NumericLiteral = prism' NumericLiteral f
  where
    f (NumericLiteral a) = Just $ a
    f _ = Nothing

_StringLiteral :: forall a. Prism' (Literal a) String
_StringLiteral = prism' StringLiteral f
  where
    f (StringLiteral a) = Just $ a
    f _ = Nothing

_CharLiteral :: forall a. Prism' (Literal a) String
_CharLiteral = prism' CharLiteral f
  where
    f (CharLiteral a) = Just $ a
    f _ = Nothing

_BooleanLiteral :: forall a. Prism' (Literal a) Boolean
_BooleanLiteral = prism' BooleanLiteral f
  where
    f (BooleanLiteral a) = Just $ a
    f _ = Nothing

_ArrayLiteral :: forall a. Prism' (Literal a) (Array a)
_ArrayLiteral = prism' ArrayLiteral f
  where
    f (ArrayLiteral a) = Just $ a
    f _ = Nothing

_ObjectLiteral :: forall a. Prism' (Literal a) (Array (Tuple String a))
_ObjectLiteral = prism' ObjectLiteral f
  where
    f (ObjectLiteral a) = Just $ a
    f _ = Nothing

--------------------------------------------------------------------------------
