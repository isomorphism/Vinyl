{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
module Data.Vinyl.Case where

import GHC.TypeLits


data (|::) :: Symbol -> * -> * where
  Case :: sy |:: t

instance SingI sy => Show (sy |:: t) where
  show Case = fromSing (sing :: Sing sy)
