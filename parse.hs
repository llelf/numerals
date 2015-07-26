{-# LANGUAGE Arrows #-}
module Main where

import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$),(<$>))
import Text.XML.HXT.Core
import Text.XML.HXT.XPath
import Text.Read




data Fun = Prefix               -- | base digit
         | Postfix              -- | minor digits
           deriving (Eq,Ord,Show)

data Part = S String            -- | literal string
          | Possible [Part]     -- | […]
          | Fun Fun             -- | ←…← / →…→ / …
          | Stop                -- | too big, leave it in numeric form
            deriving (Eq,Ord,Show)

data Gender = Masculine | Feminine | Neuter deriving Show

data Rule = Rule String [(Int, [Part])]

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


toRuleset :: IOSArrow XmlTree (Int, [Part])
toRuleset = proc r -> do v <- getAttrValue "value" >>> readBase -< r
                         t <- this /> getText -< r
                         returnA -< (v, parseRule t)

readBase :: (ArrowXml cat, ArrowChoice cat) => cat String Int
readBase = proc x ->
           case readMaybe x of
             Just v -> returnA -< v
             Nothing -> zeroArrow -< ()

