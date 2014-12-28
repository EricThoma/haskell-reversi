module Moves (
genMoves,
isValid,
makeMove,
search,
GameState (GameState)
) where

import Types
import Magic
import Utils
import Data.Bits
import Data.Array as Array
import Data.Array.Unboxed as UArray
import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout
import Debug.Trace

-- used purely for interfacing with Game_tree library
data GameState = GameState BB BB Bool Int deriving (Show)

instance Game_tree (GameState) where
    is_terminal (GameState wocc bocc toMove _) = if toMove then genMoves (wocc .|. bocc) wocc bocc == 0
                                                           else genMoves (wocc .|. bocc) bocc wocc == 0
    node_value (GameState wocc bocc _ _) = eval wocc bocc
    children (GameState wocc bocc toMove sq) = 
       let moves = if toMove then genMoves (wocc .|. bocc) wocc bocc
                             else genMoves (wocc .|. bocc) bocc wocc
       in childrenNodes (GameState wocc bocc toMove sq) moves []

-- worker for finding the children of a GameState
childrenNodes :: GameState -> BB -> [GameState] -> [GameState]
childrenNodes gmstate 0 accum = accum
childrenNodes gmstate moves accum = 
    let sq = bitScanForward moves
    in childrenNodes gmstate (moves .&. (moves-1)) ((makeMoveFormal gmstate sq):accum)

-- makeMove made to work nice with GameState
makeMoveFormal :: GameState -> Int -> GameState
makeMoveFormal (GameState wocc bocc toMove _) sq = 
    if toMove
       then let (nwocc, nbocc) = makeMove wocc bocc sq
            in GameState nwocc nbocc (not toMove) sq
       else let (nbocc, nwocc) = makeMove bocc wocc sq
            in GameState nwocc nbocc (not toMove) sq

-- generates white's moves (switch argument order for black's moves)
-- serialization of bitboard needed to get square numbers
genMoves :: BB -> BB -> BB -> BB
genMoves occ wocc bocc = let nocc = complement occ
                             nbocc = complement bocc
                         in (genMovesW wocc nbocc 0) .&. nocc

-- worker for genMoves
genMovesW :: BB -> BB -> BB -> BB
genMovesW 0 _ accum = accum
genMovesW wocc nbocc accum = let sq = bitScanForward wocc
                                 rays = (magicQueenMoves nbocc sq) .&. (nadjacents UArray.! sq)
                             in genMovesW (wocc .&. (wocc-1)) nbocc (accum .|. rays)

-- checks naively by generating all possible moves
isValid :: BB -> BB -> Int -> Bool
isValid wocc bocc sq = let occ = wocc .|. bocc
                       in ((genMoves occ wocc bocc) .&. (bit sq)) /= 0

-- execute a move
-- process: 
-- find queen moves from destination square that are not adjecent
-- intersect with white pieces
-- for each white piece, find the ray between the square we are moving on and that piece
-- make all pieces along the union of the rays white
makeMove :: BB -> BB -> Int -> (BB,BB)
makeMove wocc bocc sq = let nbocc = complement bocc
                            dests = (magicQueenMoves nbocc sq) .&. wocc .&. (nadjacents UArray.! sq)
                            rays = computeRays dests sq 0
                        in (wocc .|. rays, bocc .&. (complement rays))

computeRays :: BB -> Int -> BB -> BB
computeRays 0 _ accum = accum
computeRays dests sq accum = let destSq = bitScanForward dests
                                 ray = rayBetween UArray.! (destSq + sq*64)
                             in computeRays (dests .&. (dests-1)) sq (accum .|. ray)

-- very naive hash table for rays between two squares
rayBetween :: UArray Int BB
rayBetween = UArray.listArray (0,64*64-1) [computeRayBetween (i `quot` 64) (i `rem` 64) | i<-[0..(64*64-1)]] 

-- if the elements are on the same diagonal, row, column, it makes a BB of the ray between, inclusive
computeRayBetween :: Int -> Int -> BB
computeRayBetween sq1 sq2 = if sq1 == sq2
                               then 0
                               else let (r1,f1) = linToRF sq1
                                        (r2,f2) = linToRF sq2
                                        go = \(fr,ff) -> squaresToBB (computeRayBetweenW (r1,f1) (r2,f2) [] fr ff) 0
                                    in if r1==r2
                                          then if f1 < f2
                                               then go (id, (\x -> x+1))
                                               else go (id, (\x -> x-1))
                                          else if f1 == f2
                                               then if r1 < r2
                                                    then go ((\x -> x+1), id)
                                                    else go ((\x -> x-1), id)
                                               else if abs(f1-f2) == abs(r1-r2) -- diagonal
                                                    then if f2>f1 && r2>r1
                                                       then go (\x -> x+1, \x -> x+1)
                                                       else if f2>f1 && r2<r1
                                                          then go (\x -> x-1, \x -> x+1)
                                                          else if f2 < f1 && r2>r1
                                                            then go (\x -> x+1, \x -> x-1)
                                                            else go (\x -> x-1, \x -> x-1)
                                                    else 0

-- worker for computeRayBetween, fills a bitboard with the ray
computeRayBetweenW :: (Int,Int) -> (Int,Int) -> [Int] -> (Int -> Int) -> (Int -> Int) -> [Int]
computeRayBetweenW (r1,f1) (r2,f2) accum fr ff =
     if r1 == r2 && f1 == f2 then (rfToLin r1 f1):accum
                             else let r1n = fr r1
                                      f1n = ff f1
                                  in computeRayBetweenW (r1n, f1n) (r2,f2) ((rfToLin r1 f1):accum) fr ff 

-- squares that are not adjacent to a given square
nadjacents :: UArray Int BB
nadjacents = UArray.listArray (0,63) [complement $ go i | i <- [0..63]]
            where go sq = let (r,f) = linToRF sq
                              l1 = if r<7 then (rfToLin (r+1) f):[] else []
                              l2 = if r>0 then (rfToLin (r-1) f):l1 else l1
                              l3 = if f<7 then (rfToLin r (f+1)):l2 else l2
                              l4 = if f>0 then (rfToLin r (f-1)):l3 else l3
                              l5 = if r>0 && f>0 then (rfToLin (r-1) (f-1)):l4 else l4
                              l6 = if r>0 && f<7 then (rfToLin (r-1) (f+1)):l5 else l5
                              l7 = if r<7 && f>0 then (rfToLin (r+1) (f-1)):l6 else l6
                              l8 = if r<7 && f<7 then (rfToLin (r+1) (f+1)):l7 else l7
                          in squaresToBB l8 0

-- list of squares into a BB
squaresToBB :: [Int] -> BB -> BB
squaresToBB [] accum = accum
squaresToBB (x:xs) accum = squaresToBB xs ((bit x) .|. accum)

-- super basic evaluation
-- piece-square tables is likely an easy way to increase engine strength
eval :: BB -> BB -> Int
eval wocc bocc = popCount wocc - popCount bocc

-- search based on the Game_tree library, quick converter into GameState formalism
-- we use 0 as the root "last move" (arbitrary)
search wocc bocc toMove depth = alpha_beta_search (GameState wocc bocc toMove 0) depth





