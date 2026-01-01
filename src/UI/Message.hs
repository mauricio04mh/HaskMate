module UI.Message
  ( formatStateMessage,
  )
where

import GameState (GameState, Result (..), gsActiveColor, gsResult)

formatStateMessage :: GameState -> String
formatStateMessage gs =
  case gsResult gs of
    Ongoing -> "Turno de " ++ show (gsActiveColor gs)
    Checkmate color -> "¡Jaque mate para " ++ show color ++ "!\nPresiona R para reiniciar."
    DrawBy50Moves -> "Tablas por regla de 50 jugadas.\nPresiona R para reiniciar."
    DrawByRepetition -> "Tablas por repetición.\nPresiona R para reiniciar."
    DrawByInsufficientMaterial -> "Tablas por material insuficiente.\nPresiona R para reiniciar."
    Stalemate -> "Ahogado.\nPresiona R para reiniciar."
    DrawByAgreement -> "Acuerdo.\nPresiona R para reiniciar."
