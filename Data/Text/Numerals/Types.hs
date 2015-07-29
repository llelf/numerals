module Data.Text.Numerals.Types where
import Data.Map (Map)

data Fun = Prefix               -- | base digit
         | Postfix              -- | minor digits
           deriving (Eq,Ord,Show)

-- see parse.hs
data Part = S String            -- | literal string
          | Possible [Part]     -- | […]
          | Fun Fun RuleRef           -- | ←…← / →…→ / …
          | Replace RuleRef          -- | =…=
          | Stop                -- | too big, leave it in numeric form
            deriving (Eq,Ord,Show)

data RuleRef = Default | Alt String deriving (Eq,Ord,Show)

data Gender = Masculine | Feminine | Neuter deriving Show

type BasesMap = Map Integer [Part]

data Rule = Rule String [(Integer, [Part])]
