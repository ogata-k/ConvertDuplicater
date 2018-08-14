{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Image where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.FilePath (replaceExtension)
import Data.Char (isDigit)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
-- import qualified Codec.Picture.Types as M

-- 使用可能な色の指定方法（グレー、R系統、G系統、B系統）
-- 使用可能な拡張子をpngだけにしてタイムスタンプ.pngを吐き出して保存するようにする
getTimePngFilePath :: IO FilePath
getTimePngFilePath = (++ ".png") . filter isDigit . iso8601Show <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime) 

-- 画像への変換 注:使用可能な色数を指定させて変換できるようにすること
