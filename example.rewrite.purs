module Rewrite where

--Thoughts on rewrite

foreign import data Rewrite :: Type
foreign import data Inline :: Type

foreign import rewrite :: forall a. a -> a -> Rewrite
foreign import inline :: forall a. a -> Inline

infixr 5 rewrite as :->

-- something like this
rewriterule1 = (tabulate <<< index) :-> id 


inlineadd = inline (+)

