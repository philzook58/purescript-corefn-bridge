{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}


module Main where

import Lib as Lib


import           Data.Proxy
import           GHC.Generics
import           Data.Typeable
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeParameters (A)
import           Data.Text as T
import           Control.Monad.Reader.Class
import           Control.Lens
--import Language.PureScript.Bridge.SumType (GDataConstructor)

import qualified Language.PureScript.CoreFn.Expr as E
import qualified Language.PureScript.CoreFn.Binders as B
import qualified Language.PureScript.CoreFn.Module as M
import qualified Language.PureScript.Names as N
import           Language.PureScript.Names (ProperNameType(..), OpNameType(..))
import qualified Language.PureScript.Comments as C
import qualified Language.PureScript.Types as TY
import qualified Language.PureScript.Kinds as K
import qualified Language.PureScript.AST.Literals as L
import qualified Language.PureScript.AST.SourcePos as S
import qualified Language.PureScript.Label as Label



vectorBridge :: BridgePart -- Converts Haskell Vector type to Purescript Array type.
vectorBridge = typeName ^== "Vector" >> psArray

psstringBridge :: BridgePart -- Converts Haskell Vector type to Purescript Array type.
psstringBridge = typeName ^== "PSString" >> (return psString)

integerBridge :: BridgePart -- Converts Haskell Vector type to Purescript Array type.
integerBridge = typeName ^== "Integer" >> (return psInt)

charBridge :: BridgePart -- Converts Haskell Vector type to Purescript Array type.
charBridge = typeName ^== "Char" >> (return psString)

--dataKindBridge :: BridgePart -- Converts Haskell Vector type to Purescript Array type.
--dataKindBridge = (typeName . packed . ix 0) ^== "'" >> psArray

myBridge :: BridgePart
myBridge =  charBridge <|> integerBridge <|> psstringBridge <|> vectorBridge <|> defaultBridge <|> dataKindsFixUp 


--dataKind :: forall t. (Generic t, Typeable t, GDataConstructor (Rep t)) => Proxy t -> [SumType 'Haskell]


mkDataKinds p = let SumType ti dcs = (Lib.mkSumType' p) in 
              fmap (\dc -> 
              SumType
			     TypeInfo {
			            _typePackage = "" -- Don't suggest any packages
		              , _typeModule =  ti ^. typeModule 
			          , _typeName    = dc ^. sigConstructor
			          , _typeParameters = []
		         }
		         [
                 DataConstructor {
                   _sigConstructor = T.append "DK" (dc ^. sigConstructor)
                 , _sigValues = Left []
             }

                 ]) dcs


--fixDataKindParameters :: TypeInfo  -> TypeInfo lang
fixDataKindParameters t psArgs = if "'" `T.isPrefixOf` _typeName t
    then TypeInfo {
          _typePackage = "" -- Don't suggest any packages
        , _typeModule = input ^. typeModule -- Don't import any modules
        , _typeName = T.drop 1 (_typeName t)
        , _typeParameters = psArgs
        }
    else TypeInfo {
      _typePackage = ""
    , _typeModule  = input ^. typeModule
    , _typeName    = input ^. typeName
    , _typeParameters = psArgs
    }
    where
       input = t

dataKindsFixUp :: MonadReader BridgeData m => m PSType
dataKindsFixUp = do
  input <- view haskType
  psArgs <- psTypeParameters
  return (fixDataKindParameters input psArgs)

{-
 clearPackageFixUp :: MonadReader BridgeData m => m PSType

clearPackageFixUp = do
  input <- view haskType
  psArgs <- psTypeParameters
  return TypeInfo {
      _typePackage = ""
    , _typeModule  = input ^. typeModule
    , _typeName    = input ^. typeName
    , _typeParameters = psArgs
    }
-}

deriving instance Generic (E.Expr a)
deriving instance Generic (E.Bind a)
deriving instance Generic (E.CaseAlternative a)

deriving instance Generic (M.Module a)

deriving instance Generic (C.Comment)

deriving instance Generic (N.ProperNameType)
deriving instance Generic (N.OpNameType)

deriving instance Generic (B.Binder a)

deriving instance Generic (L.Literal a)

--deriving instance Typeable ('TypeName)

myTypes :: [SumType 'Haskell]
myTypes =  [
              mkSumType (Proxy :: Proxy (E.Expr A))
            , mkSumType (Proxy :: Proxy (E.Bind A))
            --, mkSumType (Proxy :: Proxy (E.Guard A))
            , mkSumType (Proxy :: Proxy (E.CaseAlternative A))

            , mkSumType (Proxy :: Proxy (M.Module A))

--            , mkSumType (Proxy :: Proxy (N.TypeName))
--            , mkSumType (Proxy :: Proxy (N.('ConstructorName)))
            --, mkSumType (Proxy :: Proxy (N.ProperName 'TypeName))
            --, mkSumType (Proxy :: Proxy ('TypeName))
            , mkSumType (Proxy :: Proxy (N.Ident))
            --, mkSumType (Proxy :: Proxy (N.OpName ValueOpName))
            , mkSumType (Proxy :: Proxy (N.Qualified A))
            , mkSumType (Proxy :: Proxy (N.ModuleName))
            , properName
            , opName

            , mkSumType (Proxy :: Proxy (C.Comment))

            , mkSumType (Proxy :: Proxy (TY.Type))
            , mkSumType (Proxy :: Proxy (TY.SkolemScope))
            , mkSumType (Proxy :: Proxy (TY.Constraint))
            , mkSumType (Proxy :: Proxy (TY.ConstraintData))
            , mkSumType (Proxy :: Proxy (L.Literal A))
            , mkSumType (Proxy :: Proxy (B.Binder A))
            , mkSumType (Proxy :: Proxy (S.SourceSpan))
            , mkSumType (Proxy :: Proxy (S.SourcePos))
            , mkSumType (Proxy :: Proxy (K.Kind))
            , mkSumType (Proxy :: Proxy (Label.Label))


            

          ]

-- I didn't need to do it this way. I could have leverage mkSumType and then used lenses to re-edit it.  
-- That is SO much better        
myDataKinds =(mkDataKinds (Proxy :: Proxy (N.ProperNameType))) ++ (mkDataKinds (Proxy :: Proxy (N.OpNameType))) 

--data Dummy :: N.ProperNameType

properName = 
      let st = mkSumType (Proxy :: Proxy (N.ProperName 'TypeName)) in
      set (sumTypeInfo . typeParameters) [TypeInfo {
      _typePackage = ""
    , _typeModule  = ""
    , _typeName    = "a"
    , _typeParameters = []
       }] st

opName = let st = mkSumType (Proxy :: Proxy (N.OpName 'ValueOpName)) in
              set (sumTypeInfo . typeParameters) [TypeInfo {
              _typePackage = ""
            , _typeModule  = ""
            , _typeName    = "a"
            , _typeParameters = []
               }] st

-- fixDataKindParameters . 
main :: IO ()
main = do
  let path = "./ps/src/ps-output"
  writePSTypes path ((buildBridge myBridge)) (myTypes ++ myDataKinds)
