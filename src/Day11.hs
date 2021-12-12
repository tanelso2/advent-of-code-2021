module Day11
  (
  doDay11
  ) where

import Lib
import Grid
 
import Data.Foldable
import Data.Maybe

type Day11Input = Grid Int

type Loc = (Int,Int)
type Octo = (Int, Loc, Bool)

parseInput :: String -> Either () Day11Input
parseInput s = Right $ grid res
  where l = lines s
        f :: String -> [Int]
        f x = map (read . (:[])) x
        res = map f l

makeOctos :: Grid Int -> Grid Octo
makeOctos g = mapi (\v (x,y) -> (v, (x,y), False)) g

gonnaFlash :: Octo -> Bool
gonnaFlash (_, _, True) = False -- they already flashed this round, ignore
gonnaFlash (v, _, False) = v > 9

resetOcto :: Octo -> Octo
resetOcto (_,p,True) = (0,p,False) -- they flashed this round, reset them back to zero
resetOcto (v,p,False) = (v,p,False)

incOcto :: Octo -> Octo
incOcto (v,p,b) = (v+1,p,b)

didFlash :: Octo -> Bool
didFlash (_,_,b) = b

allFlashed :: Grid Octo -> Bool
allFlashed g = all didFlash g

markFlashed :: Octo -> Octo
markFlashed (v,p,_) = (v,p,True)

handleFlashes :: (Grid Octo, Int) -> (Grid Octo, Int)
handleFlashes (g,acc) =
  case find gonnaFlash g of
    Nothing -> (g,acc)
    Just (_,p,_) -> handleFlashes (g'', acc+1)
      where g' = alter markFlashed p g
            ns = neighbors g p
            g'' = alterBy (\_ currP -> currP `elem` ns) incOcto g'

handleRound :: (Grid Octo, Int) -> (Grid Octo, Int)
handleRound (g,acc) = (g''', acc')
  where
    g' = fmap resetOcto g
    g'' = fmap incOcto g'
    (g''', acc') = handleFlashes (g'',acc)

doDay11 :: IO ()
doDay11 = doDay 11 parseInput part1 part2

part1 :: Day11Input -> Result Int
part1 xs = Verified 1627 $ flashes
  where
    octos = makeOctos $ xs
    gens = iterate handleRound (octos, 0)
    (_, flashes) = head $ drop 100 gens

part2 :: Day11Input -> Result Int
part2 xs = Verified 329 $ i
  where
    octos = makeOctos $ xs
    gens = iterate handleRound (octos, 0)
    indexedGens = zip gens [0..]
    (_, i) = fromJust $ find f indexedGens
    f ((g,_),_) = allFlashed g

