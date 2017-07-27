module ExerciseOne
    ( validate
    , towersOfHanoi
    , towersOfHanoiFour
    ) 
where

validate :: Int -> Bool
validate n = (((sumDigits . doubleEveryOther . toDigits) n) `mod` 10) == 0

toDigitsRev :: Int -> [Int]
toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigitsRev(n `div` 10)

toDigits = reverse . toDigitsRev 

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther xs = reverse (doubleEveryOther' (reverse xs))
  where
    doubleEveryOther' :: [Int] -> [Int]
    doubleEveryOther' []           = []
    doubleEveryOther' (x:[])       = [x]
    doubleEveryOther' (x1:(x2:xs)) = x1:(x2 * 2):(doubleEveryOther' xs)

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits xs = sum [sum . toDigits $ x | x <- xs]

type Peg  = String
type Move = (Peg, Peg)

towersOfHanoi :: Int -> Peg -> Peg -> Peg -> [Move]
towersOfHanoi n source target temp
    | n <= 1    = [(source, target)]
    | otherwise = (towersOfHanoi (n - 1) source temp target)
                  ++  [(source, target)] 
                  ++ (towersOfHanoi (n - 1) temp target source)


towersOfHanoiFour :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
towersOfHanoiFour n source target temp temp2
    | n < 1     = []
    | n == 1    = [(source, target)]
    | otherwise = towersOfHanoiFour (n - 2) source temp2 target temp
                  ++ [(source, temp)]
                  ++ [(source, target)]
                  ++ [(temp, target)]
                  ++ towersOfHanoiFour (n - 2) temp2 target temp source
