-- Student name : Ruiqi Li
-- Student ID : D18125180
-- Course : ASD DT228A
-- Lecturer : Dr. Pierpaolo Dondio 
-- Haskell Assignment

import System.IO
import System.Environment
import Data.Char
import Data.List

#!/usr/bin/env stack
-- stack --install-ghc runghc

main :: IO ()
main = do
 putStrLn "Hello World!"

-- Q1
is_square :: Int -> Bool
is_square 0 = True
is_square n = 
 let root = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)
 in root * root == n

-- Q2
mygroup :: [Char] -> [[Char]]
mygroup [] = []
mygroup [x] = [[x]]
mygroup (x:y:ys) = 
 let (head:tail) = mygroup (y:ys)
 in if x /= y then [x]:head:tail 
     else (x:head):tail

qsort :: [Char] -> [Char]
qsort [] = []
qsort (x:xs) = 
 qsort smaller ++ [x] ++ qsort larger
 where smaller = [a | a<- xs, a<=x]
       larger = [b | b<- xs, b>x]

freq_letter_pc :: String -> [(Char,Float)]
freq_letter_pc str = 
 let string = map toLower $ filter (/= ' ') str
     size = length string
 in map (\x -> (head x, fromIntegral (length x *10 `div` size)/10 )) $ mygroup $ qsort string

-- Q3
cities :: [(Int,String,Int,Int)] 
cities = [(1,"Paris",7000000,1),(2,"London",8000000,2),(1,"Rome",3000000,3),
 (1,"Edinburgh",500000,2),(1,"Florence",50000,3),(1,"Venice",200000,3), 
 (1,"Lyon",1000000,1),(1,"Milan",3000000,3),(1,"Madrid",6000000,4),(1,"Barcelona",5000000,4)]

countries :: [(Int,String)]
countries = [(1,"UK"), (2,"France"), (3,"Italy"), (4,"Spain")] 

qsort_i :: [Int] -> [Int]
qsort_i [] = []
qsort_i (x:xs) = 
 qsort_i smaller ++ [x] ++ qsort_i larger
 where smaller = [a | a<- xs, a<=x]
       larger = [b | b<- xs, b>x]

mygroup_i :: [Int] -> [[Int]]
mygroup_i [] = []
mygroup_i [x] = [[x]]
mygroup_i (x:y:ys) = 
 let (head:tail) = mygroup_i (y:ys)
 in if x /= y then [x]:head:tail 
     else (x:head):tail

-- a.
get_city_above :: Int -> [String]
get_city_above n = [name | (_,name,population,_) <- cities, population >= n]

-- b.
get_city :: String -> [String]
get_city cname = [name | (_,name,_,cid) <- cities, cid == (get_cid cname)]

get_cid :: String -> Int
get_cid cname = head [id | (id,name) <- countries, name == cname]

-- c.
num_city :: [(String,Int)]
num_city = [(y, length x) | x <- mygroup_i $ qsort_i [cid | (_,_,_,cid) <- cities], (id,y) <- countries, id == (head x)]

-- Q4
type Vector = [Float]
eucl_dist :: Vector -> Vector -> Float
eucl_dist x y = 
 let pair = zip [x1 | x1 <- x] [y1 | y1 <- y]
 in sqrt $ sum $ map (\(x1,y1) -> (x1 - y1) ^ 2) pair 

-- Q5
eng_freq :: [Float]
eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]

pt_freq :: [Float]
pt_freq = [12.21, 1.01, 3.35, 4.21, 13.19, 1.07, 1.08, 1.22, 5.49, 0.30, 0.13, 3.00, 5.07, 5.02, 10.22, 3.01, 1.10, 6.73, 7.35, 5.07, 4.46, 1.72, 0.05, 0.28, 0.04, 0.45]

get_lang :: IO ()
get_lang = do
 [filename] <- getArgs -- :run get_lang dorian.txt
 text <- readFile filename -- example "dorian.txt" with 331410 characters
 putStrLn ("Identify the language for " ++ filename)
 if freq_letter_text text < 0
  then putStrLn "The text is in English"
 else putStrLn "The text is in Portuguese" 

freq_letter_text :: String -> Float
freq_letter_text str = 
 let string = map toLower $ filter (/= ' ') str
     size = length string
 in  sum [ abs (x1-x2) | (x1,x2) <- zip eng_freq (frequence string size)] 
  - sum [ abs (x1-x2) | (x1,x2) <- zip pt_freq (frequence string size)] 

frequence :: String -> Int -> [Float]
frequence string size = [ fromIntegral ( (length $ filter (== x) string) * 10000 `div` size )/ 100 | x <- ['a'..'z'] ]

-- Q6 
caesar_cypher  :: IO()
caesar_cypher = do
 putStrLn "Enter a encrypted file: "
 filename <- getLine
 putStrLn "Enter the index of the cypher: "
 index <- getLine
 contents <- readFile filename 
 writeFile (applyNtimes 4 init filename) (c_decrypt (read index :: Int) contents)
 putStrLn "The file has decrypted and saved as txt file."

c_decrypt :: Int -> String -> String
c_decrypt index encrypt = [if x == ' ' then ' ' else chr $ ((ord x - ord 'a' - index) `mod` 26) + ord 'a' | x <- encrypt ]

applyNtimes :: (Num n, Ord n) => n -> (String -> String) -> String -> String 
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)

-- Q7 
-- Part 1.

-- method 1. It costs about 18 mins to finish dictionary creation
build_dict_1:: IO()
build_dict_1 = do
 putStrLn "create dictionary..."
 novel1 <- readFile "dorian.txt"
 novel2 <- readFile "pride.txt"
 novel3 <- readFile "ulysses.txt"
 writeFile "dict.txt" (unwords $ unique_1 (novel1 ++ " " ++ novel2 ++ " " ++ novel3)) 
 putStrLn "The dictionary has created." 

unique_1 :: String -> [String]
unique_1 novel = 
 let word_list = words novel
 in [w | (w,i) <- zip word_list [0..], w `notElem` (take i word_list)]

-- method 2. It costs about 5 mins to finish dictionary creation
build_dict_2 :: IO()
build_dict_2 = do
 putStrLn "create dictionary..."
 novel1 <- readFile "dorian.txt"
 novel2 <- readFile "pride.txt"
 novel3 <- readFile "ulysses.txt"
 writeFile "dict2.txt" (unwords $ unique_2 (novel1 ++ " " ++ novel2 ++ " " ++ novel3) )
 putStrLn "The dictionary has created." 

unique_2 :: String -> [String]
unique_2 novel = nub $ words novel

-- Part 2.
-- it costs about 10 secoinds for guessing
guess_index :: IO()
guess_index = do
 putStrLn "load dictionary..."
 dictionary <- readFile "dict.txt" 
 putStrLn "Enter the encryptd file: "
 filename <- getLine
 encrypt <- readFile filename
 putStrLn "guess index:"
 putStrLn (show (guess dictionary encrypt))
 writeFile "decrypted_file.txt" (c_decrypt (guess dictionary encrypt) encrypt)
 putStrLn "The file has decrypted and saved as decrypted_file.txt"

guess :: String -> String -> Int
guess dict encrypt = find_max_index [ length ( (words dict) `intersect` (words $ c_decrypt index encrypt) ) | index <- [1..26] ]

find_max_index :: [Int] -> Int
find_max_index xs = maximum [ if max == (maximum xs) then index else 0| (max,index) <- zip xs [1..] ]


-- Q8 
montecarlo_area :: Float
montecarlo_area = (fromIntegral $ length [(x,y) | x <- map (/1000) [0..1000], y <- map (/1000) [0..1000], x*x + y*y < 1] ) / 1000000


-- Q9
-- sample series
math_series :: (Float -> Float) -> Int -> Float
math_series f 1 = f 0
math_series f n = f (fromIntegral n-1) + math_series f (n-1)

sample_series :: Float -> Float
sample_series k = 1/(2 ** k)

-- pi series
math_series_2 :: (Float -> Float) -> Int -> Float
math_series_2 f 1 = f 1
math_series_2 f n = f (fromIntegral n) + math_series_2 f (n-1)

pi_series :: Float -> Float
pi_series k = ((-1) ** (k+1)) * (4.0 / (2*k - 1))

--  math_series2 pi_series 1000 = 3.1405926
--  math_series2 pi_series 100000 = 3.1415858
--  so when k goes to infinity, the sum of series get closer to pi


-- Q10
integral :: (Float -> Float) -> Float -> Float -> Int -> Float  
integral f x1 x2 n = 
 let interval = (x2-x1) / fromIntegral n 
 in sum [ f x * interval | x <- [x1, (x1 + interval)..(x2 - interval)] ]

sample_f :: Float -> Float
sample_f x = 0.5 * x

-- integral sample_f 1 20 9 = 89.72223
-- integral sample_f 1 20 9 = 99.73897
