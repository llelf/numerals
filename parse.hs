{-# LANGUAGE Arrows, QuasiQuotes #-}
module Main where

import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$),(<$>))
import Text.XML.HXT.Core
import Text.XML.HXT.XPath
import Text.Read
import Data.Char
import Control.Monad
import Data.String.Interpolate

import Data.Text.Numerals.Types


-- Unicode TR35
-- and
-- http://www.icu-project.org/apiref/icu4c/classicu_1_1RuleBasedNumberFormat.html

-- “un miliardo[ →→];” ⟶ [S "un miliardo", Possible [S " ", Fun Postfix]]
parser :: Parser [Part]
parser = [Stop] <$ string "=#"
         <|> normal
    where normal = many (between (string "[") (string "]") (Possible <$> normal)
                         <|> Fun Prefix <$ string "←←"
                         <|> Fun Postfix <$ string "→→"
                         <|> S <$> many1 (letter<|>space<|>punct))

punct = char '-' <|> char '\173'


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




readDocumentOf lang = readDocument [] [i|definitions/num/#{lang}.xml|]


writeLang lang rules = writeFile [i|Data/Text/Defs/#{mlang}.hs|]
            [i|
module Data.Text.Defs.#{mlang} where
import Data.Map
import Data.Text.Numerals.Types
rule = #{rules}
|] where mlang = toUpper (head lang) : tail lang


parseLang :: String -> IO BasesMap
parseLang lang = do
  [Rule la cc] <- runX $ readDocumentOf lang >>> purr
  return $ M.fromList cc

langs = ["en","fi","hi","hy","id","ja","ms","se"]

main = forM_ langs $ \lang -> parseLang lang >>= writeLang lang


