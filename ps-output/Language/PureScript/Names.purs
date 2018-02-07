-- File auto generated by purescript-bridge! --
module Language.PureScript.Names where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))
import GHC.Integer.Type (Integer)
import Prim (Array, String)

import Prelude
import Data.Generic (class Generic)

newtype ProperName 'TypeName =
    ProperName {
      runProperName :: String
    }

derive instance genericProperName :: Generic (ProperName 'TypeName)

derive instance newtypeProperName :: Newtype (ProperName 'TypeName) _


--------------------------------------------------------------------------------
_ProperName :: forall 'TypeName. Iso' (ProperName 'TypeName) { runProperName :: String}
_ProperName = _Newtype

--------------------------------------------------------------------------------
data Ident =
    Ident String
  | GenIdent (Maybe String) Integer

derive instance genericIdent :: Generic Ident


--------------------------------------------------------------------------------
_Ident :: Prism' Ident String
_Ident = prism' Ident f
  where
    f (Ident a) = Just $ a
    f _ = Nothing

_GenIdent :: Prism' Ident { a :: Maybe String, b :: Integer }
_GenIdent = prism' (\{ a, b } -> GenIdent a b) f
  where
    f (GenIdent a b) = Just $ { a: a, b: b }
    f _ = Nothing

--------------------------------------------------------------------------------
data Qualified a =
    Qualified (Maybe ModuleName) a

derive instance genericQualified :: Generic a => Generic (Qualified a)


--------------------------------------------------------------------------------
_Qualified :: forall a. Prism' (Qualified a) { a :: Maybe ModuleName, b :: a }
_Qualified = prism' (\{ a, b } -> Qualified a b) f
  where
    f (Qualified a b) = Just $ { a: a, b: b }

--------------------------------------------------------------------------------
newtype ModuleName =
    ModuleName (Array (ProperName 'Namespace))

derive instance genericModuleName :: Generic ModuleName

derive instance newtypeModuleName :: Newtype ModuleName _


--------------------------------------------------------------------------------
_ModuleName :: Iso' ModuleName (Array (ProperName 'Namespace))
_ModuleName = _Newtype
--------------------------------------------------------------------------------