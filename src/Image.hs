{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Image where


import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.FilePath (replaceExtension)
import System.Directory (doesDirectoryExist)
import Data.Char (isDigit)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
-- import qualified Codec.Picture.Types as M

-- 使用可能な色の指定方法（グレー、R系統、G系統、B系統）
-- 使用可能な拡張子をpngだけにしてタイムスタンプ.pngを吐き出して保存するようにする
getTimePngFilePath :: IO FilePath
getTimePngFilePath = (++ ".png") . filter isDigit . iso8601Show <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime) 
savePngImageWithTStmp :: FilePath -> DynamicImage -> IO (String, FilePath, Bool)  -- Bool is success or false
savePngImageWithTStmp fpath img = do
                                    isDir <- doesDirectoryExist fpath
                                    if isDir 
                                    then do
                                        resPath <- (fpath ++) <$> getTimePngFilePath 
                                        savePngImage resPath img 
                                        return ("保存に成功しました。", resPath, True)
                                    else return ("指定したディレクトリの指定が間違っているか存在しないディレクトリをしています。", fpath, False)

-- 画像への変換 注:使用可能な色数を指定させて変換できるようにすること

