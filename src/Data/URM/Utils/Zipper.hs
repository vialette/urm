module Data.URM.Utils.Zipper
(
  -- | opaque data type
  Zipper

  -- * Constructing
, mk

  -- * Querying
, getForward
, getBackward
, focus
, len

  -- * Modifying
, stepForward
, stepBackward
, move
)
where

  import qualified Data.List as L

  -- | zipper type definition
  data Zipper a = Zipper { forward  :: [a]
                         , backward :: [a]
                         } deriving (Show)

  {-|
   The 'mk' function creates a zipper on a list.
  -}
  mk :: [a] -> Zipper a
  mk xs = Zipper { forward = xs, backward = [] }

  {-|
   The 'getForward' function returns the forward elements (the elements
   under focus).
   It returns @Nothing@ in case of exhausted zipper.
  -}
  getForward :: Zipper a -> [a]
  getForward = forward

  {-|
   The 'getBackward' function returns the backward elements (the elements
   before focus) in correct order.
   It returns @Nothing@ in case of exhausted zipper.
  -}
  getBackward :: Zipper a -> [a]
  getBackward = L.reverse . backward

  {-|
   The 'focus' function returns the first element under focus.
   It returns @Nothing@ in case of exhausted zipper.
  -}
  focus :: Zipper a -> Maybe a
  focus Zipper { forward = [] }     = Nothing
  focus Zipper { forward = (x : _)} = Just x

  {-|
   The 'stepForward' function moves the zipper forward. It returns @Nothing@
   in case the zipper is exhausted.
  -}
  stepForward :: Zipper a -> Maybe (Zipper a)
  stepForward Zipper { forward = [] } = Nothing
  stepForward Zipper { forward = (x : xs), backward = ys } =
     Just Zipper { forward = xs, backward = x : ys }

  {-|
   The 'stepBackward' function moves the zipper backward. It returns Nothing
   in case the zipper is exhausted.
  -}
  stepBackward :: Zipper a -> Maybe (Zipper a)
  stepBackward Zipper { backward = [] } = Nothing
  stepBackward Zipper { forward  = xs, backward = (y : ys) } =
     Just Zipper { forward = y : xs, backward = ys }

  {-|
   The 'move' function moves the zipper by 'n' (positif or negative move)
   and returns a new zipper.
   If 'n' is positive it moves 'n' steps in the forward direction.
   If 'n' is negative it moves 'n' steps in the backward direction.
   It returns @Nothing@ in case the zipper is exhausted at any step.
  -}
  move :: Int -> Zipper a -> Maybe (Zipper a)
  move n z
   | n == 0    = Just z
   | n > 0     = stepForward  z >>= move (n-1)
   | otherwise = stepBackward z >>= move (n+1)

  {-|
   The 'len' function returns the number of elements in the zipper.
  -}
  len :: Zipper a -> Int
  len Zipper { forward = xs, backward = ys } = L.length xs + L.length ys
