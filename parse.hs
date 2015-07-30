{-# LANGUAGE Arrows, QuasiQuotes #-}
module Main where

import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$),(<$>),(*>))
import Text.XML.HXT.Core
import Text.XML.HXT.XPath
import Text.Read
import Data.Char
import Data.List
import Control.Monad
import Data.String.Interpolate

import Data.Text.Numerals.Types


-- Unicode TR35
-- and
-- http://www.icu-project.org/apiref/icu4c/classicu_1_1RuleBasedNumberFormat.html

-- “un miliardo[ →→];”   ⟶ [S "un miliardo", Possible [S " ", Fun Postfix Default]]
-- “vent→%%msc-with-i→;” ⟶ [S "vent", Fun Postfix (Alt "msc-with-i")]
parser :: Parser [Part]
parser = [Stop] <$ try (string "=#")
         <|> normal
    where normal = many (between (string "[") (string "]") (Possible <$> normal)
                         <|> Replace <$>     between (char '=') (char '=') ruleRef
                         <|> Fun Prefix <$>  between (char '←') (char '←') ruleRef
                         <|> Fun Postfix <$> between (char '→') (char '→') ruleRef
                         <|> S <$> many1 (letter<|>space<|>punct))

punct = oneOf "-\xad"

ruleRef :: Parser RuleRef
ruleRef = Alt <$> altRuleName <|> Default <$ string ""

altRuleName :: Parser String
altRuleName = char '%' *> (char '%' *> name
                           <|> name)
    where name = many1 (alphaNum <|> char '-')


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
          ruleset <- listA (this /> getXPathTrees "rbnf/rulesetGrouping/ruleset"
                                -- TODO parse rules there
                            >>> hasAttrValue "type" (not . ("ordinal" `isSuffixOf`))
                            >>> toRuleset) -< ld
          returnA -< Rule lang (M.fromList ruleset)



toRuleset :: IOSArrow XmlTree (String,BasesMap)
toRuleset = proc rs -> do typ <- getAttrValue "type" -< rs
                          bases <- listA $ this /> hasName "rbnfrule" >>> toRule -< rs
                          returnA -< (typ, M.fromList bases)

toRule :: IOSArrow XmlTree (Integer, [Part])
toRule = proc r -> do v <- getAttrValue "value" >>> readBase -< r
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
rule :: Rule
rule = #{rules}
|] where mlang = toUpper (head lang) : tail lang


parseLang :: String -> IO Rule
parseLang lang = do
  [rules] <- runX $ readDocumentOf lang >>> purr
  return rules

langs = ["en","fi","hi","hy","id","ms","se"]

main = forM_ langs $ \lang -> parseLang lang >>= writeLang lang


