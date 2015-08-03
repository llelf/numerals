
{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Numerals.Defs.Ms where
import Data.Map
import Data.Text.Numerals.Types
rule :: Rule
rule = Rule "ms" (fromList [("spellout-cardinal",fromList [(0,[S "kosong"]),(1,[S "satu"]),(2,[S "dua"]),(3,[S "tiga"]),(4,[S "empat"]),(5,[S "lima"]),(6,[S "enam"]),(7,[S "tujuh"]),(8,[S "lapan"]),(9,[S "sembilan"]),(10,[S "sepuluh"]),(11,[S "sebelas"]),(12,[Fun Postfix Default,S " belas"]),(20,[Fun Prefix Default,S " puluh",Possible [S " ",Fun Postfix Default]]),(100,[S "seratus",Possible [S " ",Fun Postfix Default]]),(200,[Fun Prefix Default,S " ratus",Possible [S " ",Fun Postfix Default]]),(1000,[S "seribu",Possible [S " ",Fun Postfix Default]]),(2000,[Fun Prefix Default,S " ribu",Possible [S " ",Fun Postfix Default]]),(1000000,[Fun Prefix Default,S " juta",Possible [S " ",Fun Postfix Default]]),(1000000000,[Fun Prefix Default,S " milyar",Possible [S " ",Fun Postfix Default]]),(1000000000000,[Fun Prefix Default,S " bilyun",Possible [S " ",Fun Postfix Default]]),(1000000000000000,[Fun Prefix Default,S " bilyar",Possible [S " ",Fun Postfix Default]]),(1000000000000000000,[Stop])]),("spellout-numbering",fromList [(0,[Replace (Alt "spellout-cardinal")])]),("spellout-numbering-year",fromList [(0,[Replace (Alt "spellout-numbering")])])])
