{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Vinyl.Opt 
    ( CaseOf(..), (~:)
    , Match(..), match
    , Opt(..), select, switch
    , never, joined
    ) where

import Data.Void
import Data.Vinyl.Rec
import Data.Vinyl.Case
import Data.Vinyl.Witnesses
import GHC.TypeLits

{- Example of use:


_salamander = Case :: "salamander" |:: String
_frog = Case :: "frog" |:: String
_moonCheese = Case :: "moon cheese" |:: Integer

amphibians = switch $ Of
    :| _salamander :~ putStrLn . (++ " . . .")
    :| _frog :~ (\x -> putStrLn x >> putStrLn " *ribbit* ")

green x = select x $ Of
    :| _frog :~ const "ribbit"
    :| _moonCheese :~ const "a legit fact"

-}



data family CaseOf (cs :: [*]) (a :: *)
newtype instance CaseOf cs (sy |:: t) = CaseOf t

(~:) :: (sy |:: t) -> t -> CaseOf cs (sy |:: t)
(~:) _ = CaseOf


infix 2 :~
data Match (sy :: Symbol) t r where
    (:~) :: (sy |:: t) -> (t -> r) -> Match sy t r

instance Functor (Match sy t) where fmap f (c :~ m) = c :~ f.m

match :: (sy |:: a) -> Opt '[sy |:: a] a
match _ = Of :| Case :~ id


infixl 1 :|
data Opt :: [*] -> * -> * where
    Of :: Opt '[] r
    (:|) :: Opt cs r -> Match sy t r -> Opt ((sy |:: t) ': cs) r


select :: IElem (sy |:: t) cs => CaseOf cs (sy |:: t) -> Opt cs r -> r
select = selectAux implicitly

switch :: IElem (sy |:: t) cs => Opt cs r -> CaseOf cs (sy |:: t) -> r
switch = flip $ selectAux implicitly

selectAux :: forall c cs sy t r. (c ~ (sy |:: t)) => Elem c cs -> CaseOf cs c -> Opt cs r -> r
selectAux = go
  where 
        go :: Elem c cs' -> CaseOf cs' c -> Opt cs' r -> r
        go Here (CaseOf o) (_ :| Case :~ f) = f o
        go (There p) (CaseOf o) (cs :| _) = go p (CaseOf o) cs
        go _ _ Of = error "Unintended base case invocation"

never :: (sy |:: Void) -> Opt '[sy |:: Void] r
never _ = Of :| Case :~ absurd

joined :: Opt as r -> Opt bs r -> Opt (as ++ bs) r
joined Of cs2 = cs2
joined (cs1 :| c) cs2 = joined cs1 cs2 :| c


-- TODO : instances

