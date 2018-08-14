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

-- �g�p�\�ȐF�̎w����@�i�O���[�AR�n���AG�n���AB�n���j
-- �g�p�\�Ȋg���q��png�����ɂ��ă^�C���X�^���v.png��f���o���ĕۑ�����悤�ɂ���
getTimePngFilePath :: IO FilePath
getTimePngFilePath = (++ ".png") . filter isDigit . iso8601Show <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime) 

-- �摜�ւ̕ϊ� ��:�g�p�\�ȐF�����w�肳���ĕϊ��ł���悤�ɂ��邱��
