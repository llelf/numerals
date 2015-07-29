
module Data.Text.Defs.Ja where
import Data.Map
import Data.Text.Numerals.Types
rule = fromList [(0,[]),(1,[S "\19968"]),(2,[S "\20108"]),(3,[S "\19977"]),(4,[S "\22235"]),(5,[S "\20116"]),(6,[S "\20845"]),(7,[S "\19971"]),(8,[S "\20843"]),(9,[S "\20061"]),(10,[S "\21313",Possible [Fun Postfix Default]]),(20,[Fun Prefix Default,S "\21313",Possible [Fun Postfix Default]]),(100,[S "\30334",Possible [Fun Postfix Default]]),(200,[Fun Prefix Default,S "\30334",Possible [Fun Postfix Default]]),(1000,[S "\21315",Possible [Fun Postfix Default]]),(2000,[Fun Prefix Default,S "\21315",Possible [Fun Postfix Default]]),(10000,[Fun Prefix Default,S "\19975",Possible [Fun Postfix Default]]),(100000000,[Fun Prefix Default,S "\20740",Possible [Fun Postfix Default]]),(1000000000000,[Fun Prefix Default,S "\20806",Possible [Fun Postfix Default]]),(10000000000000000,[Fun Prefix Default,S "\20140",Possible [Fun Postfix Default]]),(1000000000000000000,[Stop])]
