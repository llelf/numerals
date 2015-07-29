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
import qualified Data.Text.Defs.Fi as Fi



spellEn = spell En.rule
spellFi = spell Fi.rule

spell r = process r . Just



process :: BasesMap -> Maybe Integer -> String
process rls Nothing = ""
process rls (Just x)
    | Just (base,prc) <- M.lookupLE x rls = process1 base x prc
    | otherwise = ""
    where
          process1 base x prc = concatMap f prc
              where
                -- Rule = { n*base   =>  "spelling" }
                -- We have x: x = n*base + diff
                -- TODO this will be wrong for other languages
                diff = x - k*base
                k | base>0    = x `div` base
                  | otherwise = 1
                f (S s) = s
                f (Possible smth) | diff>0 = process1 0 diff smth
                                  | otherwise = ""
                f (Fun Prefix) = process rls (Just k)
                f (Fun Postfix) = process rls (Just diff)
                f Stop = show x


