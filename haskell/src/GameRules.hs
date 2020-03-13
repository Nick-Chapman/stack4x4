
module GameRules(
  Size,
  Pos,
  Move,
  Status(..),
  Outcome(..),
  start,
  apply,
  status,

  ) where

import Data.List.NonEmpty

data Size
data Pos
data Move
data Outcome = Loss | Draw | Win deriving (Eq,Ord)
data Status = Complete Outcome | Continue (NonEmpty Move)

instance Show Move where show = undefined

start :: Size -> Pos
start = undefined

apply :: Move -> Pos
apply = undefined

status :: Pos -> Status
status = undefined
