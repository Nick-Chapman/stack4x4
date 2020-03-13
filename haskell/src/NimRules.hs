
module NimRules( desirable,
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

newtype Size = Size Int deriving (Show,Enum,Num)
newtype Pos = Pos Int
data Move = Move { desc :: MoveDesc, leadsTo :: Pos }
data Outcome = Loss | Draw | Win deriving (Eq,Ord)
data Status = Complete Outcome | Continue (NonEmpty Move)

instance Show Move where show Move{desc} = show desc

start :: Size -> Pos
start (Size n) = Pos n

apply :: Move -> Pos
apply = leadsTo

status :: Pos -> Status
status (Pos n) = if n < 0 then error "nim<0" else case n of
  0 -> Complete Win -- last to remove a match looses!
  1 -> Continue (m1 :| [])
  2 -> Continue (m1 :| [m2])
  _ -> Continue (m1 :| [m2,m3])
  where
    m1 = Move M1 (Pos (n-1))
    m2 = Move M2 (Pos (n-2))
    m3 = Move M3 (Pos (n-3))

data MoveDesc = M1 | M2 | M3 deriving Show

desirable :: Move -> Bool
desirable move = n `mod` 4 == 1 where Pos n = leadsTo move
