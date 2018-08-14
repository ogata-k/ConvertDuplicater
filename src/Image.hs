{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Image where


import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Directory (createDirectoryIfMissing)
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
-- 使用可能な拡張子をpngだけにしてタイムスタンプ.pngを吐き出して保存するようにする
getTimePngFilePath :: IO FilePath
getTimePngFilePath = (++ ".png") . filter isDigit . iso8601Show <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime) 
savePngImageWithTStmp :: DynamicImage -> IO FilePath
savePngImageWithTStmp img = do
                                createDirectoryIfMissing False  ".\\work"
                                resPath <- (".\\work\\" ++) <$> getTimePngFilePath 
                                savePngImage resPath img 
                                return resPath

-- 画像への変換 注:使用可能な色数を指定させて変換できるようにすること

