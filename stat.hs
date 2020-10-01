{- 
 - quick statistical analysis tool
 - takes a file of the format:
 -
 - TITLE
 - number
 - number
 - etc...
 -
 - TODO:
 - integrate list of outliers into english sentence
 -}

import System.Environment
import System.IO
import Data.Char
import Data.List

getMean :: [Float] -> Float
getMean d = total / size
   where
      total = sum d
      size  = read (show $ length d) :: Float

getMedian :: [Float] -> Float
getMedian d
   | even $ length d = ((d !! middle) + (d !! (middle-1))) / 2
   | otherwise       = d !! middle
   where
      middle = length d `div` 2

-- left half of a list
dataLeft :: [a] -> [a]
dataLeft d = take half d
   where
      half = length d `div` 2

-- right half of a list
dataRight :: [a] -> [a]
dataRight d = drop half d
   where
      dlen = length d
      hlen = dlen `div` 2
      half
         | even dlen = hlen
         | otherwise = hlen + 1

getQ1 :: [Float] -> Float
getQ1 d = getMedian $ dataLeft d

getQ3 :: [Float] -> Float
getQ3 d = getMedian $ dataRight d

getIQR :: [Float] -> Float
getIQR d = getQ3 d - getQ1 d

getLF :: [Float] -> Float
getLF d = getQ1 d - (getIQR d * 1.5)

getUF :: [Float] -> Float
getUF d = getQ3 d + (getIQR d * 1.5)

outliers :: [Float] -> [Float]
outliers d = filter (< lf) d ++ filter (> uf) d
   where
      lf = getLF d
      uf = getUF d

strFloat :: [String] -> [Float]
strFloat [x] = [flt]
   where
      flt = read x :: Float
strFloat (x:xs) = flt : strFloat xs
   where
      flt = read x :: Float

pluralize :: Int -> String -> String
pluralize n s
   | n == 1    = base
   | otherwise = base ++ "s"
   where
      base = show n ++ " " ++ s

cuss :: String -> [Float] -> String
cuss s d
   | null ol   = "The distribution of " ++ s ++ " has no outliers."
   | otherwise = "The distribution of " ++ s ++ " has " ++ num ++ " at " ++ engList ol
   where
      ol  = outliers d
      num = pluralize (length ol) "outlier"

engList :: [Float] -> String
engList l
   | ln == 0   = "nothing"
   | ln == 1   = dropDec $ show $ head l
   | ln == 2   = dropDec (show $ head l) ++ " and " ++ dropDec (show $ last l)
   | otherwise = concat $ chNth (ln*2-3) ", and " $ intersperse ", " $ map (dropDec . show) l
   where
      ln = length l

chNth :: Int -> a -> [a] -> [a]
chNth i b l = left ++ [b] ++ right
   where
      left  = take i l
      right = drop (i + 1) l

dropDec :: String -> String
dropDec s
   | ls == '0' && ls2 == '.' = init $ init s
   | otherwise               = s
   where
      ls  = last s
      ls2 = last $ init s

getMode :: [Float] -> Float
getMode d = undefined
   -- | -- single
   -- | -- multiple
   where
      md = sort $ group d

main :: IO ()
main = do
   args <- getArgs
   file <- readFile $ head args
   let title = head $ lines file
   let set   = sort $ strFloat $ tail $ lines file
   --hPutStrLn stderr $ "\ESC[31mfor data:\n" ++ (show set) ++ "\ESC[m\n"
   putStrLn $ "Minimum:  " ++ dropDec (show $ minimum   set)
   putStrLn $ "Q1:       " ++ dropDec (show $ getQ1     set)
   putStrLn $ "Median:   " ++ dropDec (show $ getMedian set)
   putStrLn $ "Q3:       " ++ dropDec (show $ getQ3     set)
   putStrLn $ "Maximum:  " ++ dropDec (show $ maximum   set)
   putStrLn $ "Mean:     " ++ dropDec (show $ getMean   set)
   putStrLn $ "Outliers: " ++ unwords (map (dropDec . show) (outliers set))
   hPutStrLn stderr $ "\ESC[34;1m" ++ cuss title set ++ "\ESC[m"
