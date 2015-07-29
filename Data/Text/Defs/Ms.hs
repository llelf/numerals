
module Data.Text.Defs.Ms where
import Data.Map
import Data.Text.Numerals.Types
rule = fromList [(0,[S "kosong"]),(1,[S "satu"]),(2,[S "dua"]),(3,[S "tiga"]),(4,[S "empat"]),(5,[S "lima"]),(6,[S "enam"]),(7,[S "tujuh"]),(8,[S "lapan"]),(9,[S "sembilan"]),(10,[S "sepuluh"]),(11,[S "sebelas"]),(12,[Fun Postfix,S " belas"]),(20,[Fun Prefix,S " puluh",Possible [S " ",Fun Postfix]]),(100,[S "seratus",Possible [S " ",Fun Postfix]]),(200,[Fun Prefix,S " ratus",Possible [S " ",Fun Postfix]]),(1000,[S "seribu",Possible [S " ",Fun Postfix]]),(2000,[Fun Prefix,S " ribu",Possible [S " ",Fun Postfix]]),(1000000,[Fun Prefix,S " juta",Possible [S " ",Fun Postfix]]),(1000000000,[Fun Prefix,S " milyar",Possible [S " ",Fun Postfix]]),(1000000000000,[Fun Prefix,S " bilyun",Possible [S " ",Fun Postfix]]),(1000000000000000,[Fun Prefix,S " bilyar",Possible [S " ",Fun Postfix]]),(1000000000000000000,[Stop])]
