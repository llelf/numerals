
module Data.Text.Defs.Se where
import Data.Map
import Data.Text.Numerals.Types
rule = fromList [(0,[S "nolla"]),(1,[S "okta"]),(2,[S "guokte"]),(3,[S "golbma"]),(4,[S "njeallje"]),(5,[S "vihtta"]),(6,[S "guhtta"]),(7,[S "\269ie\382a"]),(8,[S "g\225vcci"]),(9,[S "ovcci"]),(10,[S "logi"]),(11,[Fun Postfix,S "\173nuppe\173lohk\225i"]),(20,[Fun Prefix,S "\173logi",Possible [S "\173",Fun Postfix]]),(100,[Fun Prefix,S "\173\269uo\273i",Possible [S "\173",Fun Postfix]]),(1000,[Fun Prefix,S "\173duh\225t",Possible [S " ",Fun Postfix]]),(1000000,[Fun Prefix,S " miljon",Possible [S " ",Fun Postfix]]),(1000000000,[Fun Prefix,S " miljard",Possible [S " ",Fun Postfix]]),(1000000000000,[Fun Prefix,S " biljon",Possible [S " ",Fun Postfix]]),(1000000000000000,[Fun Prefix,S " biljard",Possible [S " ",Fun Postfix]]),(1000000000000000000,[Stop])]
