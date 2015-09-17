{-# LANGUAGE Arrows, QuasiQuotes #-}
module Main where

import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Text
import Control.Applicative ((<$),(<$>),(*>))
import Text.XML.HXT.Core
import Text.XML.HXT.XPath
import Text.Read
import Data.Char
import Data.List
import qualified Data.Text as T
import Control.Monad
import Data.String.Interpolate
import System.Directory

import Data.Text.Numerals.Types


-- Unicode TR35
-- and
-- http://www.icu-project.org/apiref/icu4c/classicu_1_1RuleBasedNumberFormat.html

-- “un miliardo[ →→];”   ⟶ [S "un miliardo", Possible [S " ", Fun Postfix Default]]
-- “vent→%%msc-with-i→;” ⟶ [S "vent", Fun Postfix (Alt "msc-with-i")]
parser :: Parser Spellout
parser = [Stop] <$ try (string "=#" <|> string "ERROR-")
         <|> normal
    where normal = many (between (string "[") (string "]") (Possible <$> normal)
                         <|> Replace <$>     between (char '=') (char '=') ruleRef
                         <|> Fun Prefix <$>  between (char '←') (char '←') ruleRef
                         <|> Fun Postfix <$> between (char '→') (char '→') ruleRef
                         <|> S . T.pack <$> many1 (letter<|>space<|>punct))

punct = oneOf "-\xad"

ruleRef :: Parser RuleRef
ruleRef = Alt <$> altRuleName <|> Default <$ string ""

altRuleName :: Parser T.Text
altRuleName = char '%' *> (char '%' *> name
                           <|> name)
    where name = T.pack <$> many1 (alphaNum <|> char '-')


parseSpellout :: T.Text -> Spellout
parseSpellout s = either (error $ "parseRule ‘"++show s++"’") id . parse parser "" $ s




purr :: IOSArrow XmlTree Rule
purr = proc z ->
       do ld <- this /> hasName "ldml" -< z
          lang <- this /> getXPathTrees "identity/language" >>> getAttrValue "type" -< ld
          ruleset <- listA (this /> getXPathTrees "rbnf/rulesetGrouping/ruleset"
                                -- TODO parse rules there
                            >>> hasAttrValue "type" (not . ("ordinal" `isSuffixOf`))
                            >>> toRuleset) -< ld
          returnA -< Rule (T.pack lang) (M.fromList ruleset)



toRuleset :: IOSArrow XmlTree (T.Text,BasesMap)
toRuleset = proc rs -> do typ <- getAttrValue "type" -< rs
                          bases <- listA $ this /> hasName "rbnfrule" >>> toRule -< rs
                          returnA -< (T.pack typ, M.fromList bases)

toRule :: IOSArrow XmlTree (Integer, Spellout)
toRule = proc r -> do v <- getAttrValue "value" >>> readBase -< r
                      t <- this /> getText -< r
                      returnA -< (v, parseSpellout (T.pack t))

readBase :: (ArrowXml cat, ArrowChoice cat) => cat String Integer
readBase = proc x ->
           case readMaybe x of
             Just v -> returnA -< v
             Nothing -> zeroArrow -< ()




readDocumentOf lang = readDocument [] [i|definitions/num/#{lang}.xml|]


writeLang lang rules = writeFile [i|Data/Text/Numerals/Defs/#{mlang}.hs|]
            [i|
{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Numerals.Defs.#{mlang} where
import Data.Map
import Data.Text.Numerals.Types
rule :: Rule
rule = #{rules}
|] where mlang = toUpper (head lang) : tail lang


parseLang :: String -> IO Rule
parseLang lang = do
  [rules] <- runX $ readDocumentOf lang >>> purr
  return rules

langs = ["en","fi","hi","hy","id","it","ms","se"]

main = do
  forM_ langs $ \lang -> parseLang lang >>= writeLang lang


