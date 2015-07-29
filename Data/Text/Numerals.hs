-- |
-- Module      : Text.Numerals
-- Copyright   : (c) 2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
--
--


module Data.Text.Numerals where

import qualified Data.Map as M
import Data.Text.Numerals.Types
import qualified Data.Text.Defs.En as En




spellEn = process En.rule . Just



process :: M.Map Integer [Part] -> Maybe Integer -> String
process rls Nothing = ""
process rls (Just x)
    | Just (base,prc) <- M.lookupLE x rls = process1 base x prc
    | otherwise = ""
    where
          process1 base x prc = concatMap f prc
              where
                diff = x - k*base
                k | base>0    = x `div` base
                  | otherwise = 1
                f (S s) = s
                f (Possible smth) | diff>0 = process1 0 diff smth
                                  | otherwise = ""
                f (Fun Prefix) = process rls (Just k)
                f (Fun Postfix) = process rls (Just diff)
                f Stop = show x


