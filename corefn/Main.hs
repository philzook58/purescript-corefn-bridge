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

import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.CoreFn.Module
import Data.Version
import Language.PureScript hiding (Module, Error)
import Language.PureScript.CoreFn.Ann
-- Value -> Parser (Stuff)

import Data.Aeson.Parser

import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B

import Data.Aeson
import Data.Maybe (fromMaybe)


--getJSON :: String -> IO B.ByteString
--getJSON fileName = B.readFile fileName

{-
maybecorefn = decodeWith (modulefromJSON json) () jsonbuddy
parse (modulefromJSON json)
jsonbuddy :: Maybe Value
jsonbuddy = decode str 
-}

parseModule' :: Value -> Result (Version, Module Ann)
parseModule' = parse moduleFromJSON

{-
wegotit ::  Result (Version, Module Ann)
wegotit = parseModule jsonbuddy
-}

extractResult (Success x) = show x
extractResult (Error x) = x
-- Result is 
-- Error String  
-- |Success a
decodemyJSON str = do 
        val <- decode str
        return $ extractResult $ parseModule' val


main :: IO ()
main = do
    str <- B.readFile "test.json"
    putStrLn (show str)
    let res = fromMaybe "failed to decode" (decodemyJSON str)  in  -- = maybe "" show (decodemyJSON str) in
     putStrLn res





