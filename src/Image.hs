{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Image (
      toGray
    , toRed
    , toGreen
    , toBlue
    , generateFromList
    , fromGrayImage
    , fromColorImage
    , savePngImageWithTStmp
)where


import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Directory (createDirectoryIfMissing)
import Data.List ((!!))
import Data.Word (Word8)
import Data.Char (isDigit)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
-- import qualified Codec.Picture.Types as M
-- dummy Image
makeDummyImg :: DynamicImage
makeDummyImg = ImageRGB8 (generateImage originalFnc 2400 1200)
    where
        originalFnc :: Int -> Int -> PixelRGB8
        originalFnc x y = 
                let (q, r) = x `quotRem` max 10 y
                    s      = fromIntegral . min 0xff
                in PixelRGB8 (s q) (s r) (s (q + r + 30))

-- 使用可能な色の指定方法（グレー、R系統、G系統、B系統）
getColorMagni :: Int -> Int -> Word8  -- 色の指定範囲から0を起点とした255への範囲を作る倍率を作成
getColorMagni minI maxI = fromIntegral $ floor (255.0 / fromIntegral (abs (maxI - minI)))  
toPixel8 :: Int -> Word8 -> Int -> Pixel8  -- 0が起点ではないのでminIだけ平行移動させて正規化している
toPixel8 minI magni target = (fromIntegral (target - minI)) * magni
toGray = toPixel8
toRed minI magni target = let r = toPixel8 minI magni target in PixelRGB8 r 0 0
toGreen minI magni target = let g = toPixel8 minI magni target in PixelRGB8 0 g 0
toBlue minI magni target = let b = toPixel8 minI magni target in PixelRGB8 0 0 b
getColorIndex :: Pixel a => (Int -> a) -> Int -> Int -> [a] -- 使用可能な色のインデックス
getColorIndex cnv minI maxI = map cnv [minI .. maxI]

-- 画像への変換
-- この関数はいちいち値を習得しているから重いかも
generateFromList :: Pixel a =>
                    Int -> Int -> Int -> Int -> (Int -> a) -> [[Int]] -> Image a
generateFromList w h minI maxI cnv lst = generateImage (\h' w' -> colorIndex !! ( lst !! h' !! w')) w h
                    where
                        colorIndex = getColorIndex cnv minI maxI

-- 画像のデータ形式
fromGrayImage :: Image Pixel8 -> DynamicImage
fromGrayImage img = ImageY8 img
fromColorImage :: Image PixelRGB8 -> DynamicImage
fromColorImage img = ImageRGB8 img



-- 使用可能な拡張子をpngだけにしてタイムスタンプ.pngを吐き出して保存するようにする
getTimePngFilePath :: IO FilePath
getTimePngFilePath = (++ ".png") . filter isDigit . iso8601Show <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime) 
savePngImageWithTStmp :: DynamicImage -> IO FilePath
savePngImageWithTStmp img = do
                                createDirectoryIfMissing False  ".\\work"
                                resPath <- (".\\work\\" ++) <$> getTimePngFilePath 
                                savePngImage resPath img 
                                return resPath


