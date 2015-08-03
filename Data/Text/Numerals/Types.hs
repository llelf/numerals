module Data.Text.Numerals.Types where
import Data.Map (Map)
import Data.Text (Text)

data Fun = Prefix               -- | base digit
         | Postfix              -- | minor digits
           deriving (Eq,Ord,Show)

-- see parse.hs
data Part = S Text              -- | literal string
          | Possible [Part]     -- | […]
          | Fun Fun RuleRef           -- | ←…← / →…→ / …
          | Replace RuleRef          -- | =…=
          | Stop                -- | too big, leave it in numeric form
            deriving (Eq,Ord,Show)


type Spellout = [Part]

data RuleRef = Default | Alt Text deriving (Eq,Ord,Show)

data Gender = Masculine | Feminine | Neuter deriving (Eq,Ord,Show)

type BasesMap = Map Integer Spellout

type RuleSetName = Text

data Rule = Rule Text (Map RuleSetName BasesMap) deriving Show
