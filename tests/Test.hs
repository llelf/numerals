{-# LANGUAGE OverloadedStrings #-}
module Test where

import qualified Data.Text as T
import Data.Function
import Data.Monoid
import Data.Text.Numerals

testsFi :: [(Natural,T.Text)]
testsFi = [
 (0, "nolla"),
 (7, "seitsemän"),
 (11, "yksi-toista"),
 (12, "kaksi-toista"),
 (20, "kaksi-kymmentä"),
 (21, "kaksi-kymmentä yksi"),
 (22, "kaksi-kymmentä kaksi"),
 (30, "kolme-kymmentä"),
 (40, "neljä-kymmentä"),
 (90, "yhdeksän-kymmentä"),
 (100, "sata"),
 (200, "kaksisataa"),
 (1000, "tuhat"),
 (2000, "kaksituhatta"),
 (3721,  "kolme-tuhatta-seitsemän-sataa-kaksi-kymmentä-yksi"),
 (1026234, "yksi miljoona kaksikymmentäkuusituhattakaksisataakolmekymmentäneljä")
 ]

testsIt :: [(Gender,Natural,T.Text)]
testsIt = [
 (Masculine, 1, "un"), (Feminine, 1, "una"),
 (Masculine, 33, "trentatré"),
 (Masculine, 77, "settantasette"),
 (Masculine, 100, "cento"), (Feminine, 100, "cento"),
 (Masculine, 200, "duecento"),
 (Masculine, 342, "trecentoquarantadue"),
 (Masculine, 1984, "millenovecentottantaquattro"),
 (Masculine, 2000, "duemila")
 ]

norm = T.concat . T.split (`elem` " -\xad")

test = and [all (f1 spellFi) testsFi,
            all (f2 spellIt) testsIt
           ]
    where f1 sp (n,s) = ((==)`on`norm) (sp n) s || error (show n)
          f2 sp (g,n,s) = ((==)`on`norm) (sp (Just g) n) s || error (show n)

