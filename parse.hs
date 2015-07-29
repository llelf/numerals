{-# LANGUAGE Arrows, QuasiQuotes #-}
module Main where

import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$),(<$>))
import Text.XML.HXT.Core
import Text.XML.HXT.XPath
import Text.Read
import Data.String.Interpolate

import Data.Text.Numerals.Types



parser :: Parser [Part]
parser = [Stop] <$ string "=#"
         <|> normal
    where normal = many (between (string "[") (string "]") (Possible <$> normal)
                         <|> Fun Prefix <$ string "←←"
                         <|> Fun Postfix <$ string "→→"
                         <|> S <$> many1 (letter<|>space<|>punct))

punct = char '-'


parseRule :: String -> [Part]
parseRule s = either (error $ "parseRule ‘"++s++"’") id . parse parser "" $ s



spellouts :: [(String, Maybe Gender)]
spellouts = [("spellout-cardinal-masculine", Just Masculine),
             ("spellout-cardinal-feminine",  Just Feminine),
             ("spellout-cardinal-neuter",    Just Neuter),
             ("spellout-cardinal",           Nothing)]


purr :: IOSArrow XmlTree Rule
purr = proc z ->
       do ld <- this /> hasName "ldml" -< z
          lang <- this /> getXPathTrees "identity/language" >>> getAttrValue "type" -< ld
          ruleset <- this /> getXPathTrees "rbnf/rulesetGrouping/ruleset"
                     >>> hasAttrValue "type" (=="spellout-cardinal") -< ld
          r <- listA $ this /> hasName "rbnfrule" >>> toRuleset -< ruleset
          returnA -< Rule lang r


toRuleset :: IOSArrow XmlTree (Integer, [Part])
toRuleset = proc r -> do v <- getAttrValue "value" >>> readBase -< r
                         t <- this /> getText -< r
                         returnA -< (v, parseRule t)

readBase :: (ArrowXml cat, ArrowChoice cat) => cat String Integer
readBase = proc x ->
           case readMaybe x of
             Just v -> returnA -< v
             Nothing -> zeroArrow -< ()





en = readDocument [] "definitions/num/en.xml"



main = do
  [Rule la cc] <- runX $ en >>> purr
  let rules = M.fromList cc :: M.Map Int [Part]
  writeFile "Data/Text/Defs/En.hs"
            [i|
module Data.Text.Defs.En where
import Data.Map
import Data.Text.Numerals.Types
rule = #{rules}
|]

