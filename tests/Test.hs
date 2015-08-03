{-# LANGUAGE OverloadedStrings #-}
module Test where

import qualified Data.Text as T
import Data.Function
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


norm = T.concat . T.split (`elem` " -\xad")

test = all f testsFi
    where f (n,s) = ((==)`on`norm) (spellFi n) s || error (show n)
