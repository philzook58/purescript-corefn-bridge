{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveGeneric         #-}


module Main where

import Lib


import           Data.Proxy
import           GHC.Generics
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeParameters (A)


import qualified Language.PureScript.CoreFn.Expr as E
import qualified Language.PureScript.CoreFn.Module as M
import qualified Language.PureScript.Names as N
import           Language.PureScript.Names (ProperNameType(..))
import qualified Language.PureScript.Comments as C


vectorBridge :: BridgePart -- Converts Haskell Vector type to Purescript Array type.
vectorBridge = typeName ^== "Vector" >> psArray

myBridge :: BridgePart
myBridge =  vectorBridge <|> defaultBridge


deriving instance Generic (E.Expr a)
deriving instance Generic (E.Bind a)
deriving instance Generic (E.CaseAlternative a)

deriving instance Generic (M.Module a)

deriving instance Generic (C.Comment)

myTypes :: [SumType 'Haskell]
myTypes =  [
              mkSumType (Proxy :: Proxy (E.Expr A))
            , mkSumType (Proxy :: Proxy (E.Bind A))
            , mkSumType (Proxy :: Proxy (E.Guard A))
            , mkSumType (Proxy :: Proxy (E.CaseAlternative A))

            , mkSumType (Proxy :: Proxy (M.Module A))

--            , mkSumType (Proxy :: Proxy (N.TypeName))
--            , mkSumType (Proxy :: Proxy (N.('ConstructorName)))
            , mkSumType (Proxy :: Proxy (N.ProperName 'TypeName))
            , mkSumType (Proxy :: Proxy (N.Ident))
            , mkSumType (Proxy :: Proxy (N.Qualified A))
            , mkSumType (Proxy :: Proxy (N.ModuleName))

            , mkSumType (Proxy :: Proxy (C.Comment))

          ]


main :: IO ()
main = do
  let path = "./ps-output"
  writePSTypes path (buildBridge myBridge) myTypes
