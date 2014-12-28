import Magic
import Types
import Utils
import Moves
import System.Random as Rand
import Data.Array.Unboxed as UArray
import Data.Bits
import Data.Tuple
import Data.Char
import System.TimeIt


main :: IO ()
main = do gameLoop startwocc startbocc True

startwocc :: BB
startwocc = bit 27 .|. bit 36

startbocc :: BB
startbocc = bit 35 .|. bit 28

-- informal main game loop, computer plays black
gameLoop :: BB -> BB -> Bool -> IO ()
gameLoop wocc bocc toMove = 
  do if toMove then putStrLn "white's move" else putStrLn "black's move"
     let occ = bocc .|. wocc
     printLikeBoard (bbToList wocc bocc)
     putStrLn "searching..."
     let (pv, score) = search wocc bocc toMove 12
         GameState _ _ _ bestmove = (pv !! 1)
     if toMove then return () else timeIt $ putStrLn $ "Computer calculates advantage: " ++ (show score)
     move <- (if toMove then do putStr "\nReady for input:\n"
                                input <- getLine
                                return (parseMove input)
                                  
                        else return (Just bestmove))
     case move of
      Nothing -> do putStrLn "Invalid move format"
                    gameLoop wocc bocc (toMove)
      Just sq -> 
        do let valid = if toMove then isValid wocc bocc sq 
                                 else isValid bocc wocc sq
               (r,f) = linToRF sq
               (nwocc, nbocc) = if valid
                                 then if toMove then makeMove wocc bocc sq 
                                                else swap $ makeMove bocc wocc sq        
                                 else (wocc, bocc)
               ntoMove = if valid then not toMove else toMove
           putStrLn $ [chr $ f + (ord 'a')] ++ [chr $ r + (ord '1')] 
           putStrLn $ "valid = " ++ (show valid)
           gameLoop nwocc nbocc ntoMove


