import System.IO
import Text.Printf
import Text.Regex (mkRegex, splitRegex)
import Data.Maybe (fromJust, isJust)

type SlotTable = [(Int, [String])]

timeTable slot time = do
  withFile "slot_table_en.csv" ReadMode $ \handle -> do
      table <- fmap parse $ hGetContents handle
      let mins = time2mins (h, m)
          s = (fromJust $ getSlotNumber slot table) - 1
          times = map mins2time $ scanl (+) mins $ repeat 10
          slots = drop s $ cycle table
      mapM_ (putStrLn . formatting) . take 88 $ zip times slots

formatting :: (PrintfType t) => ((Int, Int), (Int, [String])) -> t
formatting ((h,m), (n, slots)) = 
    let slots' = concatMap (printf "%-12s") slots :: String
    in printf "%3d:%02d  [%2d]  %s" h m n slots'

parse :: String -> SlotTable
parse str = map (g . f) $ lines str
    where f = splitRegex (mkRegex ",[ \t]*")
          g (x:xs) = (read x, xs)

getSlotNumber :: String -> [(Int, [String])] -> Maybe Int
getSlotNumber _ []            = Nothing
getSlotNumber str ((n, x):xs) = if elem str x then Just n
                                else getSlotNumber str xs

mins2time :: Integral a => a -> (a, a)
mins2time n = divMod n 60

time2mins :: Integral a => (a, a) -> a
time2mins (h, m) = h * 60 + m
