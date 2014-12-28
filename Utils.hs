module Utils 
(print64, randoms64, linToRF, rfToLin, random64ToSquare, bitScanForward, sparseRand, bbToList,
  printLikeBoard, parseMove)
where

import Types
import Data.Bits
import System.Random as Rand
import Data.Array.Unboxed as UArray
import Text.Printf
import Control.Exception
import Data.Maybe
import Data.Char

import Debug.Trace

-- converts a linear index into a rank-file pair
linToRF :: Int -> (Int, Int)
linToRF sq = quotRem sq 8

-- converts a rank-file pair to a linear index
rfToLin :: Int -> Int -> Int
rfToLin rank file = rank*8 + file

-- print a bitboard as a chess-board reads
-- I wrote this when I was much worse at Haskell... (just terrible)
print64 :: BB -> IO ()
print64 b = let l64 = replicate 64 b
                shifts = 1:(take 63 (map (shift 1) [1..])) :: [BB]
                inter = zipWith (.&.) shifts l64
                result = (map fromIntegral) $ zipWith shift inter (map (*(-1))[0..63]) :: [Int]
            in printLikeABoardIterate 0 result


-- helper function for above
printLikeABoardIterate :: Int -> [Int] -> IO ()
printLikeABoardIterate 64 _ = putStr "\n"
printLikeABoardIterate i xs = do let realIndex = \a -> 64 - 8*( truncate ((fromIntegral (a+8))/8) ) + (a `rem` 8) :: Int
                                 if i `rem` 8 == 0
                                     then putStr $ "\n"
                                     else return ()
                                 let x = (xs !! (realIndex i))
                                 putStr $ ( show x ) ++ " "
                                 printLikeABoardIterate (i+1) xs

-- an infinite list of random bitboards
randoms64 :: Int -> [BB]
randoms64 = fromRandoms . Rand.randoms . mkStdGen

-- throws away a lot of randomness to get a number between 1 and 64, pretty ugly
random64ToSquare :: BB -> Int
random64ToSquare r = abs(fromIntegral r) `rem` 64

-- combines three numbers into one, designed to combine several low-bit randoms into one random BB
fromRandoms :: [Int] -> [BB]
fromRandoms (a:b:c:xs) = (    (fromIntegral a) 
	                      .|. (shiftL (fromIntegral b) 30)
                          .|. (shiftL (fromIntegral c) 60) ):(fromRandoms xs)

-- takes several random numbers and ANDs them to make them sparse (1/4 chance of a 1)
sparseRand :: StdGen -> (BB, StdGen)
sparseRand gen = let (r1,g1) = (Rand.random gen) :: (BB, StdGen)
                     (r2,g2) = (Rand.random g1) :: (BB, StdGen)
                     --(r3,g3) = (Rand.random g2) :: (BB, StdGen)
                 in (r1 .&. r2, g2)

-- used in bitscan forward hacks
index64 :: UArray BB Int
index64 = UArray.listArray (0,63) [0,  1, 48,  2, 57, 49, 28,  3,
                                   61, 58, 50, 42, 38, 29, 17,  4,
                                   62, 55, 59, 36, 53, 51, 43, 22,
                                   45, 39, 33, 30, 24, 18, 12,  5,
                                   63, 47, 56, 27, 60, 41, 37, 16,
                                   54, 35, 52, 21, 44, 32, 23, 11,
                                   46, 26, 40, 15, 34, 20, 31, 10,
                                   25, 14, 19,  9, 13,  8,  7,  6]

-- debruijn method for bitscan forward, found online and adapted to Haskell
bitScanForward :: BB -> Int
bitScanForward bb = let debruijn64 = 0x03f79d71b4cb0a89 :: BB
                    in index64 UArray.! (((bb .&. (-bb)) * debruijn64) `shiftR` 58)

bbToList :: BB -> BB -> [Char]
bbToList wocc bocc = bbToListW [] 0 wocc bocc

bbToListW :: [Char] -> Int -> BB -> BB -> [Char]
bbToListW accum 64 _ _ = accum
bbToListW accum i wocc bocc = let (r,f) = i `quotRem` 8
                                  ind = 8*r + (7-f)
                                  wpiece = (wocc .&. (bit ind)) `shiftR` ind
                                  bpiece = (bocc .&. (bit ind)) `shiftR` ind
                              in if wpiece == 1
                                    then bbToListW ('w':accum) (i+1) wocc bocc
                                    else if bpiece == 1
                                         then bbToListW ('b':accum) (i+1) wocc bocc
                                         else bbToListW ('-':accum) (i+1) wocc bocc

printLikeBoard :: [Char] -> IO ()
printLikeBoard arr = do printLikeBoardW 0 arr
                        putStr "    a  b  c  d  e  f  g  h\n"


printLikeBoardW :: Int -> [Char] -> IO ()
printLikeBoardW i [] = return ()
printLikeBoardW i (x:xs) = do if (i `rem` 8) == 0 then putStr $ " " ++ (show $ (71-i) `quot` 8) ++ "  "
                                                  else return ()
                              putStr $ [x] ++ "  "
                              if (i `rem` 8) == 7 then putStr "\n"
                                                  else return ()
                              printLikeBoardW (i+1) xs


parseMove :: [Char] -> Maybe Int
parseMove (fc:rc:[]) = if ('A' <= (toUpper fc) && (toUpper fc) <= 'H') 
                          && ('1' <= rc && rc <= '8')
                     then let file = (ord $ toUpper fc) - 65
                              rank = digitToInt rc - 1
                          in return $ rfToLin rank file
                     else Nothing
parseMove _ = Nothing









