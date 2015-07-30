{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text (Text,unpack)
import qualified Data.Text as T
import Data.Text.Numerals.Types
import qualified Data.Text.Numerals.Defs.En as En
import qualified Data.Text.Numerals.Defs.Fi as Fi



spellEn = spell En.rule
spellFi = spell Fi.rule

spell (Rule la ru) = process r . Just
    where r = ru M.! "spellout-cardinal"


spelloutsByGender :: [(Maybe Gender, RuleSetName)]
spelloutsByGender = [
 (Just Masculine, "spellout-cardinal-masculine"),
 (Just Feminine,  "spellout-cardinal-feminine"),
 (Just Neuter,    "spellout-cardinal-neuter"),
 (Nothing,        "spellout-cardinal")]




process :: BasesMap -> Maybe Integer -> Text
process rls Nothing = ""
process rls (Just x)
    | Just (base,prc) <- M.lookupLE x rls = process1 base x prc
    | otherwise = ""
    where
          process1 ruleBase x prc = T.concat . map f $ prc
              where
                -- Rule = { (ruleBase = n*base)  ⟹  "spelling" }
                -- We have x: x = k*base + diff
                diff = x - k*base
                k = x `div` base
                base | ruleBase>0 = baseBase 10 ruleBase
                     | otherwise  = 1
                f (S s) = s
                f (Possible smth) | diff>0 = T.concat . map f $ smth
                                  | otherwise = ""
                f (Fun Prefix Default) = process rls (Just k)
                f (Fun Postfix Default) = process rls (Just diff)
                f Stop = T.pack (show x)


-- 12345₁₀ ⟶ 10000
baseBase :: Integer -> Integer -> Integer
baseBase b x = b ^ floor (logBase (fromInteger b) (fromInteger x + 0.5))


