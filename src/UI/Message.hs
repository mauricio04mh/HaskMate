module UI.Message
  ( formatStateMessage,
  )
where

import GameState (GameState, Result (..), gsActiveColor, gsResult)

formatStateMessage :: GameState -> String
formatStateMessage gs =
  case gsResult gs of
    Ongoing -> "Turno de " ++ show (gsActiveColor gs)
    Checkmate color -> "¡Jaque mate para " ++ show color ++ "! Presiona 'r' para reiniciar."
    DrawBy50Moves -> "Tablas por regla de 50 jugadas. Presiona 'r' para reiniciar."
    DrawByRepetition -> "Tablas por repetición. Presiona 'r' para reiniciar."
    Stalemate -> "Ahogado. Presiona 'r' para reiniciar."
    DrawByAgreement -> "Acuerdo. Presiona 'r' para reiniciar."
