module Main where

import System.Environment
import System.Exit

checkNb :: [Char] -> [[Char]]-> Bool
checkNb [] av = checkArg av
checkNb (z:zs) av
    | z >= '0' && z <= '9' = checkNb zs av
    | otherwise = False


checkArg :: [[Char]] -> Bool
checkArg [] = True
checkArg [_] = False
checkArg ("--rule":b:zs) = checkNb b zs
checkArg ("--line":b:zs) = checkNb b zs
checkArg ("--start":b:zs) = checkNb b zs
checkArg ("--move":b:zs) = checkNb b zs
checkArg ("--window":b:zs) = checkNb b zs
checkArg _ = False


stockRule :: [String] -> Int
stockRule [] = -1
stockRule ("--rule":b:zs) = read b :: Int
stockRule (b:zs) = stockRule zs


stockLines :: [String] -> Int
stockLines [] = -1
stockLines ("--lines":b:zs) = read b :: Int
stockLines (b:zs) = stockLines zs


stockWindow :: [String] -> Int
stockWindow [] = 80
stockWindow ("--window":b:zs) = read b :: Int
stockWindow (b:zs) = stockWindow zs


stockStart :: [String] -> Int
stockStart [] = 0
stockStart ("--start":b:zs) = read b :: Int
stockStart (b:zs) = stockStart zs


stockMove :: [String] -> Int
stockMove [] = 0
stockMove ("--move":b:zs) = read b :: Int
stockMove (b:zs) = stockMove zs



drawRule30 :: String -> String
drawRule30 ('*':'*':'*':zs) = " " ++ drawRule30('*':'*':zs)
drawRule30 ('*':'*':' ':zs) = " " ++ drawRule30('*':' ':zs)
drawRule30 ('*':' ':'*':zs) = " " ++ drawRule30(' ':'*':zs)
drawRule30 ('*':' ':' ':zs) = "*" ++ drawRule30(' ':' ':zs)
drawRule30 (' ':'*':'*':zs) = "*" ++ drawRule30('*':'*':zs)
drawRule30 (' ':'*':' ':zs) = "*" ++ drawRule30('*':' ':zs)
drawRule30 (' ':' ':'*':zs) = "*" ++ drawRule30(' ':'*':zs)
drawRule30 (' ':' ':' ':zs) = " " ++ drawRule30(' ':' ':zs)
drawRule30 _ = " "

    

drawRule90 :: String -> String
drawRule90 ('*':'*':'*':zs) = " " ++ drawRule90('*':'*':zs)
drawRule90 ('*':'*':' ':zs) = "*" ++ drawRule90('*':' ':zs)
drawRule90 ('*':' ':'*':zs) = " " ++ drawRule90(' ':'*':zs)
drawRule90 ('*':' ':' ':zs) = "*" ++ drawRule90(' ':' ':zs)
drawRule90 (' ':'*':'*':zs) = "*" ++ drawRule90('*':'*':zs)
drawRule90 (' ':'*':' ':zs) = " " ++ drawRule90('*':' ':zs)
drawRule90 (' ':' ':'*':zs) = "*" ++ drawRule90(' ':'*':zs)
drawRule90 (' ':' ':' ':zs) = " " ++ drawRule90(' ':' ':zs)
drawRule90 _ = " "


drawRule110 :: String -> String
drawRule110 ('*':'*':'*':zs) = " " ++ drawRule110('*':'*':zs)
drawRule110 ('*':'*':' ':zs) = "*" ++ drawRule110('*':' ':zs)
drawRule110 ('*':' ':'*':zs) = "*" ++ drawRule110(' ':'*':zs)
drawRule110 ('*':' ':' ':zs) = " " ++ drawRule110(' ':' ':zs)
drawRule110 (' ':'*':'*':zs) = "*" ++ drawRule110('*':'*':zs)
drawRule110 (' ':'*':' ':zs) = "*" ++ drawRule110('*':' ':zs)
drawRule110 (' ':' ':'*':zs) = "*" ++ drawRule110(' ':'*':zs)
drawRule110 (' ':' ':' ':zs) = " " ++ drawRule110(' ':' ':zs)
drawRule110 _ = " "


firstLine :: Int -> Int-> String -> String
firstLine 0 _ _ = ""
firstLine nb cpt str
    | cpt == (nb `div` 2) = firstLine nb (cpt + 1) (str ++ "*")
    | cpt < nb = firstLine nb (cpt + 1) (str ++ " ")
    | otherwise = str

secondLine30 :: Int -> Int -> String -> IO[Int]
secondLine30 nb totalLine firstLine = do
    if (nb /= 0) then do
        putStrLn (drop totalLine (take (length firstLine - totalLine) firstLine))
        secondLine30 (nb-1) (totalLine+1) (drawRule30 ("  " ++ firstLine ++ " "))
    else
        exitSuccess


secondStart30 :: Int -> Int -> Int -> String -> IO[Int]
secondStart30 nb totalLine cpt firstLine
    | cpt /= 0 = secondStart30 nb (totalLine+1) (cpt-1) (drawRule30 ("  " ++ firstLine ++ " "))
    |   otherwise = secondLine30 nb totalLine firstLine


secondStart90 :: Int -> Int -> Int -> String -> IO[Int]
secondStart90 nb totalLine cpt firstLine
    | cpt /= 0 = secondStart90 nb (totalLine+1) (cpt-1) (drawRule90 ("  " ++ firstLine ++ " "))
    |   otherwise = secondLine90 nb totalLine firstLine


secondStart110 :: Int -> Int -> Int -> String -> IO[Int]
secondStart110 nb totalLine cpt firstLine
    | cpt /= 0 = secondStart110 nb (totalLine+1) (cpt-1) (drawRule110 ("  " ++ firstLine ++ " "))
    |   otherwise = secondLine110 nb totalLine firstLine


secondLine110 :: Int -> Int -> String -> IO[Int]
secondLine110 nb totalLine firstLine = do
    if (nb /= 0) then do
        putStrLn (drop totalLine (take (length firstLine - totalLine) firstLine))
        secondLine110 (nb-1) (totalLine+1) (drawRule110 ("  " ++ firstLine ++ " "))
    else
        exitSuccess

secondLine90 :: Int -> Int -> String -> IO[Int]
secondLine90 nb totalLine firstLine = do
    if (nb /= 0) then do
        putStrLn (drop totalLine (take (length firstLine - totalLine) firstLine))
        secondLine90 (nb-1) (totalLine+1) (drawRule90 ("  " ++ firstLine ++ " "))
    else
        exitSuccess

main :: IO[Int]
main = do
    tab_args <- getArgs
    let rule = stockRule tab_args
    let window = firstLine (stockWindow tab_args) 0 ""
    let move = stockMove tab_args
    let start = stockStart tab_args
    let line = stockLines tab_args
    if rule == 30 then
        secondStart30 line 0 start window
    else if rule == 90 then
        secondStart90 line 0 start window
    else if rule == 110 then
        secondStart110 line 0 start window
    else if (checkArg tab_args == False) then
        exitWith (ExitFailure 84)
    else exitSuccess

    -- parcourir la 1re ligne
    -- draw new line par rapport à la 1re ligne
    --
    --
    --
    --

--traverseFirstLine :: window -> wi
--traverseFirstLine 


