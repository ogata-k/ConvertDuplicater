module CnvAndDup(
  cnvdup
)where
import Data.List (all, transpose)
import Text.Read (readEither)
import Control.Monad


-- このプログラムは基本cnvdupで使用する関数に正しい引数の指定をするものとする。そのため各関数での場合分けは極力少なくする。

-- データ
type DataList = [[Int]]
showDataList :: DataList -> String
showDataList d = concatMap ((++ "\n") . unwords .(map show)) d

-- データ入力
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
converter :: Int -> Int -> DataList -> [DataList] -> DataList
converter minI maxI target = compose . convert target
    where
        getCnvData :: Int -> [DataList] -> DataList
        getCnvData n datas = datas !! (n - minI)
        convert :: DataList -> [DataList] -> [[DataList]]
        convert init cnvs = flip map init $ map (flip getCnvData cnvs)
        compose :: [[DataList]] -> DataList
        compose = map concat . concatMap transpose

-- main
cnvdup :: IO (DataList)  -- 画像への変換などは別モジュールへ？上の変換でもいいかも
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
    putStrLn "入力お疲れさまでした。"
    putStrLn "\n変換を開始します。"
    putStrLn "変換を適用する前の初期データです。"
    putStrLn (showDataList initList)
    result <- if countCnv < 1
            then (return initList)
            else foldl (\target' n -> do
                target <- target'
                putStrLn ("\n" ++ (show n) ++ "回目の変換を実行しています。")
                let converted = converter minI maxI target targetList
                putStrLn "変換が終了しました。"
                putStrLn ((show n) ++ "回変換を適用した後のデータです。")
                putStrLn (showDataList converted)
                return converted
            ) (return initList) [1 .. countCnv]
    putStrLn "すべての変換を終了しました。"
    return result
    -- TODO 画像にして吐き出したり保存したり、などの処理。
