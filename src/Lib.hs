{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib where

import           Control.Lens      hiding (from, to)
import           Data.Proxy
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Typeable
import           Generics.Deriving

import           Language.PureScript.Bridge (SumType(..), DataConstructor(..), RecordEntry(..)) -- hiding (mkSumType)
import Language.PureScript.Bridge.TypeInfo

someFunc :: IO ()
someFunc = putStrLn "someFunc"



mkSumType' :: forall t. (Generic t, Typeable t, GDataConstructor (Rep t))
          => Proxy t -> SumType 'Haskell
mkSumType' p = SumType  (mkTypeInfo p) constructors
  where
    constructors = gToConstructors (from (undefined :: t))


class GDataConstructor f where
  gToConstructors :: f a -> [DataConstructor 'Haskell]

class GRecordEntry f where
  gToRecordEntries :: f a -> [RecordEntry 'Haskell]

instance (Datatype a, GDataConstructor c) =>  GDataConstructor (D1 a c) where
  gToConstructors (M1 c) = gToConstructors c

instance (GDataConstructor a, GDataConstructor b) => GDataConstructor (a :+: b) where
  gToConstructors (_ :: (a :+: b) f) = gToConstructors (undefined :: a f)
                                    ++ gToConstructors (undefined :: b f)

instance (Constructor a, GRecordEntry b) => GDataConstructor (C1 a b) where
  gToConstructors c@(M1 r) = [ DataConstructor { _sigConstructor = constructor
                                               , _sigValues = values }
                             ]
    where
      constructor = T.pack $ conName c
      values = if conIsRecord c
                  then Right $ gToRecordEntries r
                  else Left $ map _recValue $ gToRecordEntries r

instance (GRecordEntry a, GRecordEntry b) => GRecordEntry (a :*: b) where
  gToRecordEntries (_ :: (a :*: b) f) = gToRecordEntries (undefined :: a f)
                                     ++ gToRecordEntries (undefined :: b f)


instance GRecordEntry U1 where
  gToRecordEntries _ = []

instance (Selector a, Typeable t) => GRecordEntry (S1 a (K1 R t)) where
  gToRecordEntries e = [
      RecordEntry { _recLabel = T.pack (selName e)
      , _recValue = mkTypeInfo (Proxy :: Proxy t)
      }
     ]
