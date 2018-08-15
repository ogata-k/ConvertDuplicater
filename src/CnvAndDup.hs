module CnvAndDup(
  cnvdup
)where
import Data.List (all, transpose)
import Text.Read (readEither)
import Control.Monad
import Image

-- このプログラムは基本cnvdupで使用する関数に正しい引数の指定をするものとする。そのため各関数での場合分けは極力少なくする。

-- データ
type DataList = [[Int]]
showDataList :: DataList -> String
showDataList d = concatMap ((++ "\n") . unwords .(map show)) d
getHeight :: DataList -> Int
getHeight = length
getWidth :: DataList -> Int
getWidth = length . head

data ImageType = Gray | Red | Green | Blue deriving (Read)
-- データ入力
readImageType :: IO ImageType
readImageType = do
                    putStr "グレイスケール（Gray）、赤系統（Red）、緑系統（Green）、青系統（Blue） >> "
                    s <- getLine
                    case (readEither s) of
                        Left _ -> putStrLn "入力が正しい指定ではありません。" >> readImageType
                        Right t -> return t


readIntRange :: Int -> Int -> IO Int
readIntRange minI maxI = do
    putStr $ (show minI) ++ "以上" ++ (show maxI) ++ "以下の整数を入力してください。 >> "
    s <- getLine
    case (readEither s) of
        Left _ -> putStrLn "入力が整数ではありません。" >> (readIntRange minI maxI)
        Right i -> if (minI <= i && i <= maxI)
            then (return i)
            else putStrLn "入力が範囲外です。" >> (readIntRange minI maxI)

readIntOver :: Int -> IO Int
readIntOver minI = do
    putStr $ (show minI) ++ "以上の整数を入力してください。 >> "
    s <- getLine
    case (readEither s) of
        Left _ -> putStrLn "入力が整数ではありません。" >> (readIntOver minI)
        Right i -> if (minI <= i)
            then (return i)
            else putStrLn "入力が範囲外です。" >> (readIntOver minI)

convert2IntList :: [[String]] -> Either String [[Int]]
convert2IntList ssList = if isAllRight (map (map (readEither :: String -> Either String Int)) ssList)
                            then Right (map (map read) ssList)
                            else Left "入力データに整数以外の文字や全角の整数が入り込んでいます。"
    where
        isRight :: Either String a -> Bool
        isRight (Right _) = True
        isRight (Left _) = False
        isAllRightLine :: [Either String a] -> Bool
        isAllRightLine = all isRight
        isAllRight :: [[Either String a]] -> Bool
        isAllRight = and . map isAllRightLine

convert2DataList :: Int -> Int -> Int -> Int -> [[Int]] -> Either String DataList
convert2DataList h w minI maxI dat
            | not (inRange dat) = Left $ "入力データに"++ (show minI) ++ "未満か"  ++ (show maxI) ++ "より大きい整数が含まれています。"
            | not (eqHeight dat) = Left $ "入力データが" ++ (show h) ++ "行と一致しません。"
            | not (eqWidth dat) = Left $ "入力データが" ++ (show w) ++ "列でない行が存在します。。"
            | otherwise = Right dat
            
            where
                inRange' :: Int -> Bool
                inRange' n = minI <= n && n <= maxI
                inRange :: [[Int]] -> Bool
                inRange = and . map (all inRange')
                eqHeight, eqWidth :: [[Int]] -> Bool
                eqHeight = (== h) . length
                eqWidth = all (== w) . map length

readListRange :: Int -> Int -> Int -> Int -> IO DataList
readListRange h w minI maxI = do
    putStrLn $ "半角スペース区切りで" ++ (show minI) ++ "以上" ++ (show maxI) ++ "以下の整数からなる高さ" ++ (show h) ++ "で幅" ++ (show w) ++ "からなるリストを入力してください。改行後は行の変更はできません。"
    status <- convert2IntList <$> (replicateM h $ words <$> getLine)
    case status of
        Left s -> putStrLn (s ++ "今データの最初から入力しなおしてください。") >> readListRange h w minI maxI
        Right iList -> case (convert2DataList h w minI maxI iList) of
                            Left s -> putStrLn (s ++ "今データの最初から入力しなおしてください。") >> readListRange h w minI maxI
                            Right result -> return result

-- 変換と合成
-- 正しい範囲指定で使われると仮定する。minI <= n <= maxI
converter :: Int -> Int -> [DataList] -> DataList -> DataList
converter minI maxI targetList target = compose $ convert target targetList
    where
        getCnvData :: Int -> [DataList] -> DataList
        getCnvData n datas = datas !! (n - minI)
        convert :: DataList -> [DataList] -> [[DataList]]
        convert init cnvs = flip map init $ map (flip getCnvData cnvs)
        compose :: [[DataList]] -> DataList
        compose = map concat . concatMap transpose

convertRep :: Int -> Int -> Int -> [DataList] -> DataList -> DataList
convertRep n minI maxI lst target
    | n == 0 = target
    | n > 0 = convertRep (n - 1) minI maxI lst (converter minI maxI lst target)

-- main
cnvdup :: IO ()  -- 画像への変換などは別モジュールへ？上の変換でもいいかも
cnvdup = do
    putStrLn "\n最初に使用可能な整数を決定します。"
    putStrLn "使用する最小の整数を入力してください。"
    minI' <- readIntOver 0
    putStrLn "使用する最大の整数を入力してください。"
    maxI' <- readIntOver 0
    let minI = minimum [minI', maxI']
    let maxI = maximum [minI', maxI']
    putStrLn "\n次に変換元の初期データを入力します。"
    putStrLn "行数を入力してください。"
    hInit <- readIntOver 1
    putStrLn "列数を入力してください。"
    wInit <- readIntOver 1
    initList <- readListRange hInit wInit minI maxI
    putStrLn "\n次に変換方法を入力します。"
    putStrLn "各整数の変換後のデータの行数を入力してください。"
    hCnv <- readIntOver 1
    putStrLn "各整数の変換後のデータの列数を入力してください。"
    wCnv <- readIntOver 1
    let targetList' = flip map [minI .. maxI] $ \n ->
                        putStrLn ("\n" ++ (show n) ++ "の変換先を入力してください。") >> readListRange hCnv wCnv minI maxI
    targetList <- sequence targetList'
    putStrLn "変換回数を入力してください。"
    countCnv <- readIntOver 0
    result <- if countCnv < 1
            then (return initList)
            else foldl (\target' n -> id $! do
                target <- target'
                let converted = converter minI maxI targetList target
                return converted
            ) (return initList) [1 .. countCnv]
    putStrLn "\nここから変換後の処理に移ります。"
    let magni = getColorMagni minI maxI
    let h = getHeight result
    let w = getWidth result
    putStrLn "変換後のデータに対して画像としての保存方法を入力してください。"
    typ <- readImageType
    let dImg = case typ of
                        Gray -> fromGrayImage $ generateFromList w h minI maxI (toGray minI magni) result   
                        Red -> fromColorImage $ generateFromList w h minI maxI (toRed minI magni) result
                        Green -> fromColorImage $ generateFromList w h minI maxI (toGreen minI magni) result
                        Blue -> fromColorImage $ generateFromList w h minI maxI (toBlue minI magni) result
    putStrLn "以上で入力は終了です。"
    putStrLn "\n変換と保存を開始します。時間がかかるので保存の終了まで少々お待ちください。" 
    file <- savePngImageWithTStmp dImg
    putStrLn $ file ++ "に保存しました。"
    putStrLn "以上で終了です。"
