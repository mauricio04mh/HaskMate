module GameState
  ( module GameState.Types,
    applyMove,
    isLegalMove,
    hasLegalMoves,
    determineResult,
    declareDrawByAgreement,
    isKingInCheck,
  )
where

import GameState.StateUpdate
  ( applyMove,
    declareDrawByAgreement,
    determineResult,
    hasLegalMoves,
    isLegalMove,
  )
import GameState.Validation (isKingInCheck)
import GameState.Types
