module UI.Assets
  ( Assets,
    loadAssets,
    pieceSprite,
  )
where

import Codec.Picture
  ( DynamicImage,
    convertRGBA8,
    imageHeight,
    imageWidth,
    readImage,
  )
import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Graphics.Gloss (Picture, blank)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Piece (Color (Black, White), Piece (..), PieceType (..))
import Paths_HaskMate (getDataFileName)

type SpriteKey = (Color, PieceType)

newtype Assets = Assets
  (Map.Map SpriteKey (Picture, Float))

loadAssets :: IO Assets
loadAssets = do
  entries <- forM spriteFiles $ \(key, relPath) -> do
    path <- getDataFileName relPath
    picture <- fmap (fromMaybe blank) (loadJuicyPNG path)
    size <- spriteSize path
    pure (key, (picture, size))
  pure $ Assets (Map.fromList entries)

pieceSprite :: Assets -> Piece -> (Picture, Float)
pieceSprite (Assets sprites) piece =
  let key = (pieceColor piece, pieceType piece)
   in Map.findWithDefault (blank, 256) key sprites

spriteSize :: FilePath -> IO Float
spriteSize path = do
  result <- readImage path
  pure $ either (const 256) dynamicImageSize result

dynamicImageSize :: DynamicImage -> Float
dynamicImageSize dyn =
  let rgba = convertRGBA8 dyn
   in max (fromIntegral $ imageWidth rgba) (fromIntegral $ imageHeight rgba)

spriteFiles :: [(SpriteKey, FilePath)]
spriteFiles =
  [ ((White, King), "assets/white_filled_king.png"),
    ((White, Queen), "assets/white_filled_queen.png"),
    ((White, Rook), "assets/white_filled_rook.png"),
    ((White, Bishop), "assets/white_filled_bishop.png"),
    ((White, Knight), "assets/white_filled_knight.png"),
    ((White, Pawn), "assets/white_filled_pawn.png"),
    ((Black, King), "assets/black_king.png"),
    ((Black, Queen), "assets/black_queen.png"),
    ((Black, Rook), "assets/black_rook.png"),
    ((Black, Bishop), "assets/black_bishop.png"),
    ((Black, Knight), "assets/black_knight.png"),
    ((Black, Pawn), "assets/black_pawn.png")
  ]
