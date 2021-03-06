-- File auto generated by purescript-bridge! --
module Language.PureScript.CoreFn.Binders where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import Language.PureScript.AST.Literals (Literal)
import Language.PureScript.Names (ConstructorName, Ident, ProperName, Qualified, TypeName)
import Prim (Array)

import Prelude
import Data.Generic (class Generic)

data Binder a =
    NullBinder a
  | LiteralBinder a (Literal (Binder a))
  | VarBinder a Ident
  | ConstructorBinder a (Qualified (ProperName TypeName)) (Qualified (ProperName ConstructorName)) (Array (Binder a))
  | NamedBinder a Ident (Binder a)

derive instance genericBinder :: Generic a => Generic (Binder a)


--------------------------------------------------------------------------------
_NullBinder :: forall a. Prism' (Binder a) a
_NullBinder = prism' NullBinder f
  where
    f (NullBinder a) = Just $ a
    f _ = Nothing

_LiteralBinder :: forall a. Prism' (Binder a) { a :: a, b :: Literal (Binder a) }
_LiteralBinder = prism' (\{ a, b } -> LiteralBinder a b) f
  where
    f (LiteralBinder a b) = Just $ { a: a, b: b }
    f _ = Nothing

_VarBinder :: forall a. Prism' (Binder a) { a :: a, b :: Ident }
_VarBinder = prism' (\{ a, b } -> VarBinder a b) f
  where
    f (VarBinder a b) = Just $ { a: a, b: b }
    f _ = Nothing

_ConstructorBinder :: forall a. Prism' (Binder a) { a :: a, b :: Qualified (ProperName TypeName), c :: Qualified (ProperName ConstructorName), d :: Array (Binder a) }
_ConstructorBinder = prism' (\{ a, b, c, d } -> ConstructorBinder a b c d) f
  where
    f (ConstructorBinder a b c d) = Just $ { a: a, b: b, c: c, d: d }
    f _ = Nothing

_NamedBinder :: forall a. Prism' (Binder a) { a :: a, b :: Ident, c :: Binder a }
_NamedBinder = prism' (\{ a, b, c } -> NamedBinder a b c) f
  where
    f (NamedBinder a b c) = Just $ { a: a, b: b, c: c }
    f _ = Nothing

--------------------------------------------------------------------------------
