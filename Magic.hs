-- in theory I need only to reveal the first three functions for the rest of the engine
-- more is exported now for testing purposes, convenience, etc.
module Magic (
magicBishopMoves,
magicRookMoves,
magicQueenMoves,
bishopMovesNoLookup,
rookMovesNoLookup,
genOccupancies,
bishopMasks,
rookMasks,
genMagic,
bishopMagics,
rookMagics,
getRookMask,
printMagics
) where

import Types
import Utils
import Data.Array as Array
import Data.Array.Unboxed as UArray
import System.Random as Rand
import Data.Maybe
import Data.Bits
import Debug.Trace



-- magic bitboard bishop interface
magicBishopMoves :: BB -> Int -> BB
magicBishopMoves occ sq = 
                          let MagicEntry mask magic shift lookup = bishopMagics UArray.! sq
                              om = occ .&. mask
                              opm = om * magic
                              ind = (fromIntegral $ opm `unsafeShiftR` shift)::Int
                          in lookup UArray.! ind

-- magic bitboard rook interface
magicRookMoves :: BB -> Int -> BB
magicRookMoves occ sq = 
                          let MagicEntry mask magic shift lookup = rookMagics UArray.! sq
                              om = occ .&. mask
                              opm = om * magic
                              ind = (fromIntegral $ opm `unsafeShiftR` shift)::Int
                          in lookup UArray.! ind

-- composition of the above two for queen moves, interface
magicQueenMoves :: BB -> Int -> BB
magicQueenMoves occ sq = (magicBishopMoves occ sq) .|. (magicRookMoves occ sq)

-- list of bishop magics for each square
-- commented out line is the function that recomputes magics each time
bishopMagics :: Array Int MagicEntry
bishopMagics = UArray.listArray (0,63) $
               [f a | (f,a) <- zip (map ($ True) $ map genMagicGivenMagic bishopMagicNumbers) [0..63]]
--bishopMagics = UArray.listArray (0,63) [genMagic True i | i <- [0..63]]

-- list of rook magics for each square
-- commented out line is the function that recomputes magics each time
rookMagics :: Array Int MagicEntry
rookMagics = UArray.listArray (0,63) $
               [f a | (f,a) <- zip (map ($ False) $ map genMagicGivenMagic rookMagicNumbers) [0..63]]
--rookMagics = UArray.listArray (0,63) [genMagic False i | i <- [0..63]]

-- generated using the code here, pasted for quickness
bishopMagicNumbers :: [BB]
bishopMagicNumbers = [379459123718811778,613660812742558082,7237570560186687616,13967950013397999884,1264425315787433472,2326575733672000,5196034229341786149,1594982405301667874,5199977552759235652,17983192514145158796,10358367430308306962,4976761305227858971,13876202125073914913,9304447309947406732,41098662357123072,11531607901366911668,5807871175822214152,227431850170188100,4541842018346016,2420702394078871584,6130666194113399056,3929109277230449408,1172062670820870178,2486559497905767504,4046528727245431057,2702731540455009432,1567692479439045137,124130740211302912,1236664708249985026,4579467305452036,649649765484102147,4579467305452036,7570836916975734796,2686696315861598490,288321154585200960,2882349943220600960,1352242072271454272,4758070740237361280,13515295730721051,1263277850304357132,2326575733672000,2326575733672000,1225015521712820232,11251478546616424454,4971991624022626816,2420702394078871584,3519581609370094088,7237570560186687616,5196034229341786149,12142907469970219015,13916123415922088117,225331715080594948,4774171921170695024,5199977552759235652,10358367430308306962,613660812742558082,1594982405301667874,11531607901366911668,1172062670820870178,14991438763125671936,13890620779114857732,12335720037306336516,5199977552759235652,379459123718811778]

-- generated using the code here, pasted for quickness
rookMagicNumbers :: [BB]
rookMagicNumbers = [12718168114208194560,144152573635604544,12321883771438649600,3530832007826243616,5908724910470012936,216181896386710528,14123297234037113104,144115747025782850,6629439389523132417,1749930205104931622,2594636404675854466,594615957029140480,322570358240612364,289919234602178577,289919234602178577,163396225062671616,6212012548342941600,76570814404632578,2311562669242581122,1153204080711041048,8512507534202931200,2450240772247654936,2909488087843309584,2452300157258072155,15024150075796373509,14632759109947130626,4687687019512484385,16226470017910343681,867461904200892544,289919234602178577,289919234602178577,1225562982270207044,2314964282942423178,76570814404632578,9476171751571329280,594615957029140480,4639288330146548992,41939849527168000,343577630521426192,163396225062671616,2356051383206182913,216529161588588544,5836951815266041905,4827931514347061256,14422215219089113104,256745036594020624,1266198725227970630,149757624076861441,7503034925265064448,7503034925265064448,6507842817520307328,1418871377401971328,867461904200892544,1388234735461411584,10952771989736064000,306262939772651008,11674175086457274546,11674175086457274546,651333235561759554,4629991379764838405,9659659214644078618,6647876051782864898,1324216633140578988,15078056719357592578]

printMagics :: [MagicEntry] -> IO ()
printMagics [] = return ()
printMagics ((MagicEntry _ magic _ _):xs) = do putStr $ (show magic) ++ ","
                                               printMagics xs

-- generate a magic number for the square
-- process is as follows: 
-- generate masks
-- generate all possible mask occupancies
-- plug those occupancies into the old-fashioned move generation for attack maps
-- generate a magic number that is correct via tryMagics
genMagic :: Bool -> Int -> MagicEntry
genMagic bishop sq = let mask = if bishop
                                   then bishopMasks UArray.! sq
                                   else rookMasks UArray.! sq
                         nbits = popCount mask
                         occs = genOccupancies mask sq
                         attmap = if bishop
                                     then (UArray.listArray (0,(bit nbits)-1) $
                                          map (\x -> bishopMovesNoLookup x sq) occs)::UArray Int BB
                                     else UArray.listArray (0,(bit nbits)-1) $ 
                                          map (\x -> rookMovesNoLookup x sq) occs
                         (magic,result) = tryMagics sparseRand (Rand.mkStdGen 2135632) attmap occs nbits
                     in MagicEntry mask magic (64-nbits) (UArray.array (0,(bit nbits)-1) result)
                        --then let mask = bishopMasks UArray.! sq
                        --else let mask = bishopMasks UArray.! sq

-- same as genMagic except it takes the magics from a list and then generates the entries
genMagicGivenMagic :: BB -> Bool -> Int -> MagicEntry
genMagicGivenMagic num bishop sq = let mask = if bishop
                                               then bishopMasks UArray.! sq
                                               else rookMasks UArray.! sq
                                       nbits = popCount mask
                                       occs = genOccupancies mask sq
                                       attmap = if bishop
                                                 then (UArray.listArray (0,(bit nbits)-1) $
                                                      map (\x -> bishopMovesNoLookup x sq) occs)::UArray Int BB
                                                 else UArray.listArray (0,(bit nbits)-1) $ 
                                                      map (\x -> rookMovesNoLookup x sq) occs
                                       (magic,result) = tryMagics (\x -> (num,x)) (Rand.mkStdGen 2135632) attmap occs nbits
                                   in MagicEntry mask magic (64-nbits) (UArray.array (0,(bit nbits)-1) result)

-- try out magics until one works, use sparse random numbers for the magics
tryMagics :: (StdGen -> (BB,StdGen)) -> StdGen -> UArray Int BB -> [BB] -> Int -> (BB,[(Int, BB)])
tryMagics rand gen attmap occs indexBits = let usedmap = (UArray.listArray (0,(bit indexBits)-1) (replicate (bit indexBits) False))::UArray Int Bool
                                               (magic, ngen) = rand gen
                                               (res, success) = genMagicW attmap usedmap occs 0 indexBits magic []
                                           in if success
                                                then (magic,res)
                                                else tryMagics rand ngen attmap occs indexBits

-- worker function for try magics; sees if a given magic number works
-- process: compute the index from the given magic and verify that index isn't used yet
genMagicW :: UArray Int BB -> UArray Int Bool -> [BB] -> Int -> Int -> BB -> [(Int,BB)] -> ([(Int, BB)], Bool)
genMagicW _ _ [] _ _ _ accum = (accum,True)
genMagicW attmap usedmap (occ:occs) occno indexBits magic accum =
          let index = (fromIntegral $ (occ*magic) `shiftR` (64-indexBits))::Int
          in if (usedmap UArray.! index)
                then (accum,False)
                else genMagicW attmap (usedmap UArray.// [(index,True)]) occs (occno+1) indexBits magic ((index, attmap UArray.! occno):accum)

-- generate all occupancies in a given mask, square
genOccupancies :: BB -> Int -> [BB]
genOccupancies mask sq = let nbits = popCount mask
                         in genOccupanciesW mask nbits sq 0 []

-- worker function for genOccupancies
genOccupanciesW :: BB -> Int -> Int -> Int -> [BB] -> [BB]
genOccupanciesW mask nbits sq iter accum = if iter >= (bit nbits)
                                              then accum
                                              else genOccupanciesW mask nbits sq (iter+1) ((getOcc mask nbits iter 0 0):accum)
    

getOcc :: BB -> Int -> Int -> Int -> BB -> BB
getOcc mask nbits i j occ = if j >= nbits
                               then occ
                               else let nmask = mask .&. (mask-1)
                                        nocc = if (i `unsafeShiftR` j) .&. 1 /= 0
                                               then occ .|. (bit $ bitScanForward mask)
                                               else occ
                                    in getOcc nmask nbits i (j+1) nocc

-- list of bishop masks (bishop moves on an empty board minus edges)
-- the hex number is simply the board minus edges
bishopMasks :: UArray Int BB
bishopMasks = UArray.listArray (0,63) [(0x007E7E7E7E7E7E00::BB) .&. (bishopMovesNoLookup 0 i) | i <- [0..63]]

-- list of the rook masks (rook moves on an empty board minus edges)
rookMasks :: UArray Int BB
rookMasks = UArray.listArray (0,63) [getRookMask i | i <- [0..63]]

-- compute the rook mask for a given square
getRookMask :: Int -> BB
getRookMask sq = let (r,f) = linToRF sq
                     n = \(ri, fi) -> (ri+1, fi, ri+1 < 6)
                     e = \(ri, fi) -> (ri, fi+1, fi+1 < 6)
                 in    (genWrapper n (0, f, True) 0
                    .|. genWrapper e (r, 0, True) 0)
                    .&. (complement $ bit sq)

-- computes naively the bishop moves by defining directions and calling a generic looping function
-- simplification proposal:
-- make a direction by specifying a row/col funcs Int->Int and have generic bound conditions
bishopMovesNoLookup :: BB -> Int -> BB
bishopMovesNoLookup occ sq = let (r, f) = linToRF sq
                                 ne = \(ri, fi) -> (ri+1, fi+1, ri+1<7 && fi+1<7 && (not $ testBit occ $ rfToLin (ri+1) (fi+1)))
                                 nw = \(ri, fi) -> (ri+1, fi-1, ri+1<7 && fi-1>0 && (not $ testBit occ $ rfToLin (ri+1) (fi-1)))
                                 se = \(ri, fi) -> (ri-1, fi+1, ri-1>0 && fi+1<7 && (not $ testBit occ $ rfToLin (ri-1) (fi+1)))
                                 sw = \(ri, fi) -> (ri-1, fi-1, ri-1>0 && fi-1>0 && (not $ testBit occ $ rfToLin (ri-1) (fi-1)))
                                 third = \(_,_,x) -> x
                             in      genWrapper ne (r, f, r<7 && f<7) 0
                                 .|. genWrapper nw (r, f, r<7 && f>0) 0
                                 .|. genWrapper se (r, f, r>0 && f<7) 0
                                 .|. genWrapper sw (r, f, r>0 && f>0) 0

-- computes naively the rook moves by defining directions and calling a generic looping function
rookMovesNoLookup :: BB -> Int -> BB
rookMovesNoLookup occ sq = let (r, f) = linToRF sq
                               n = \(ri, fi) -> (ri+1, fi, ri+1 < 7 && (not $ testBit occ $ rfToLin (ri+1) (fi)))
                               w = \(ri, fi) -> (ri, fi-1, fi-1 > 0 && (not $ testBit occ $ rfToLin (ri) (fi-1)))
                               s = \(ri, fi) -> (ri-1, fi, ri-1 > 0 && (not $ testBit occ $ rfToLin (ri-1) (fi)))
                               e = \(ri, fi) -> (ri, fi+1, fi+1 < 7 && (not $ testBit occ $ rfToLin (ri) (fi+1)))
                           in      genWrapper n (r, f, r<7) 0
                               .|. genWrapper w (r, f, f>0) 0
                               .|. genWrapper s (r, f, r>0) 0
                               .|. genWrapper e (r, f, f<7) 0

-- generic looping function that iterates over sliding moves, used for naive move generation
genWrapper :: ((Int, Int) -> (Int, Int, Bool)) -> (Int, Int, Bool) -> BB -> BB
genWrapper func (_, _, False) att = att
genWrapper func (rank, file, True) att = let (rn, fn, b) = func (rank,file)
                                         in genWrapper func (rn,fn,b) (att .|. (bit $ rfToLin rn fn))

