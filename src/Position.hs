module Position where

-- File (column) and Rank (row) constrained to 1-8.
newtype File = File Int
  deriving (Eq, Show)

newtype Rank = Rank Int
  deriving (Eq, Show)

-- Chessboard coordinate.
data Position = Position
  { posFile :: File,
    posRank :: Rank
  }
  deriving (Eq, Show)

mkFile :: Int -> Maybe File
mkFile x
  | x >= 1 && x <= 8 = Just (File x)
  | otherwise = Nothing

mkRank :: Int -> Maybe Rank
mkRank x
  | x >= 1 && x <= 8 = Just (Rank x)
  | otherwise = Nothing

mkPosition :: Int -> Int -> Maybe Position
mkPosition f r = do
  file <- mkFile f
  rank <- mkRank r
  pure $ Position file rank

isValidPosition :: Position -> Bool
isValidPosition (Position (File f) (Rank r)) = f >= 1 && f <= 8 && r >= 1 && r <= 8
