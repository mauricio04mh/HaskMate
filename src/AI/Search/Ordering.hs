module AI.Search.Ordering
  ( OrderContext (..),
    MoveKey (..),
    emptyOrderContext,
    orderMovesCtx,
    moveKey,
    updateKiller,
    updateHistory,
  )
where

import Board.Query (boardPieceAt)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Ord (Down (..), comparing)
import GameState (GameState, applyMove, gsActiveColor, gsBoard)
import GameState.Validation (boardIsInCheck)
import Move (Move (..))
import Piece (PieceType, getPieceColor)
import Position (Position)

-- Move doesn't have Ord, so we create a comparable key
data MoveKey = MoveKey !Position !Position !(Maybe PieceType)
  deriving (Eq, Ord, Show)

-- | Convert a Move to a MoveKey for use in maps
moveKey :: Move -> MoveKey
moveKey (Move from to promo) = MoveKey from to promo

-- | Context for advanced move ordering
data OrderContext = OrderContext
  { -- | Principal Variation move (best move from previous iteration)
    ocPvMove :: !(Maybe Move),
    -- | Killer moves by ply (stores up to 2 per ply)
    ocKillers :: !(IntMap [Move]),
    -- | History scores for quiet moves
    ocHistory :: !(Map MoveKey Int)
  }
  deriving (Eq, Show)

emptyOrderContext :: OrderContext
emptyOrderContext =
  OrderContext
    { ocPvMove = Nothing,
      ocKillers = IntMap.empty,
      ocHistory = Map.empty
    }

-- | Update killer moves for a given ply
-- Keeps max 2 killers per ply, no duplicates
updateKiller :: Int -> Move -> OrderContext -> OrderContext
updateKiller ply move ctx =
  let killers = IntMap.findWithDefault [] ply (ocKillers ctx)
      newKillers = take 2 $ move : filter (/= move) killers
   in ctx {ocKillers = IntMap.insert ply newKillers (ocKillers ctx)}

-- | Update history heuristic score
-- Uses depth^2 bonus (classic formula)
updateHistory :: Move -> Int -> OrderContext -> OrderContext
updateHistory move depthRemaining ctx =
  let key = moveKey move
      bonus = depthRemaining * depthRemaining
      newScore = Map.findWithDefault 0 key (ocHistory ctx) + bonus
   in ctx {ocHistory = Map.insert key newScore (ocHistory ctx)}

-- | Order moves with context-aware heuristics
-- Priority: PV > Captures > Promotions > Checks > Killers > History > Rest
orderMovesCtx :: OrderContext -> GameState -> Int -> [Move] -> [Move]
orderMovesCtx ctx gameState ply moves =
  sortBy (comparing (Down . movePriority)) moves
  where
    movePriority :: Move -> (Int, Int, Int, Int, Int, Int)
    movePriority move =
      ( isPvMove move,
        isCapture move,
        isPromotion move,
        givesCheck move,
        killerScore move,
        historyScore move
      )

    isPvMove move =
      if ply == 0 && Just move == ocPvMove ctx
        then 1
        else 0

    isCapture move =
      case boardPieceAt (gsBoard gameState) (toPos move) of
        Just piece ->
          if getPieceColor piece /= gsActiveColor gameState
            then 1
            else 0
        Nothing -> 0

    isPromotion move = if isJust (promotion move) then 1 else 0

    givesCheck move =
      case applyMove gameState move of
        Nothing -> 0
        Just nextState ->
          if boardIsInCheck (gsBoard nextState) (gsActiveColor nextState)
            then 1
            else 0

    killerScore move =
      let killers = IntMap.findWithDefault [] ply (ocKillers ctx)
       in if move `elem` killers then 1 else 0

    historyScore move =
      Map.findWithDefault 0 (moveKey move) (ocHistory ctx)
