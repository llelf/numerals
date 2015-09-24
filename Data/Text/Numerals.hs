

{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Text.Numerals
-- Copyright   : (c) 2015 Antonio Nikishaev
--
-- License     : BSD-style
-- Maintainer  : me@lelf.lu
--
--


module Data.Text.Numerals
    (spell,spellEn,spellFi,spellIt,spellFr,
     Gender(..),
     module Numeric.Natural) where

import qualified Data.Map as M
import Data.Map (Map,(!))
import Data.Text (Text,unpack)
import qualified Data.Text as T
import Numeric.Natural
import Data.Text.Numerals.Types
import qualified Data.Text.Numerals.Defs as Defs

rulesEn = Defs.rules!"en"
rulesFi = Defs.rules!"fi"
rulesIt = Defs.rules!"it"
rulesFr = Defs.rules!"fr"


spellEn = spell rulesEn Nothing
spellFi = spell rulesFi Nothing
spellIt = spell rulesIt
spellFr = spell rulesFr

spell :: Rule -> Maybe Gender -> Natural -> Text
spell (Rule _ ru) sex = process ru r . Just . fromIntegral
    where r = ru M.! (spelloutsByGender M.! sex)


spelloutsByGender :: M.Map (Maybe Gender) RuleSetName
spelloutsByGender = M.fromList [
 (Just Masculine, "spellout-cardinal-masculine"),
 (Just Feminine,  "spellout-cardinal-feminine"),
 (Just Neuter,    "spellout-cardinal-neuter"),
 (Nothing,        "spellout-cardinal")]




process :: M.Map RuleSetName BasesMap -> BasesMap -> Maybe Integer -> Text
process _ _ Nothing = ""
process allRules rls (Just x)
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
                f (Fun Prefix Default)    = process allRules rls (Just k)
                f (Fun Prefix (Alt alt))  = process allRules (allRules M.! alt) (Just k)
                f (Fun Postfix Default)   = process allRules rls (Just diff)
                f (Fun Postfix (Alt alt)) = process allRules (allRules M.! alt) (Just diff)
                f (Replace (Alt alt))     = process allRules (allRules M.! alt) (Just x)
                f Stop = T.pack (show x)


-- 12345₁₀ ⟶ 10000
baseBase :: Integer -> Integer -> Integer
baseBase b x = b ^ floor (logBase (fromInteger b) (fromInteger x + 0.5))


