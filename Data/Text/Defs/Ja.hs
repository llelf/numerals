
module Data.Text.Defs.Ja where
import Data.Map
import Data.Text.Numerals.Types
rule = fromList [(0,[]),(1,[S "\19968"]),(2,[S "\20108"]),(3,[S "\19977"]),(4,[S "\22235"]),(5,[S "\20116"]),(6,[S "\20845"]),(7,[S "\19971"]),(8,[S "\20843"]),(9,[S "\20061"]),(10,[S "\21313",Possible [Fun Postfix]]),(20,[Fun Prefix,S "\21313",Possible [Fun Postfix]]),(100,[S "\30334",Possible [Fun Postfix]]),(200,[Fun Prefix,S "\30334",Possible [Fun Postfix]]),(1000,[S "\21315",Possible [Fun Postfix]]),(2000,[Fun Prefix,S "\21315",Possible [Fun Postfix]]),(10000,[Fun Prefix,S "\19975",Possible [Fun Postfix]]),(100000000,[Fun Prefix,S "\20740",Possible [Fun Postfix]]),(1000000000000,[Fun Prefix,S "\20806",Possible [Fun Postfix]]),(10000000000000000,[Fun Prefix,S "\20140",Possible [Fun Postfix]]),(1000000000000000000,[Stop])]
