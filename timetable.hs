import System.IO
import System.Environment (getArgs)
import Data.Maybe
import Text.Printf
import Text.Regex (mkRegex, splitRegex)

type SlotTable = [(Int, [String])]

main = do
  (time, slot) <- fmap parseArgs getArgs
  timeTable time slot

timeTable time slot = do
  withFile "slot_table_en.csv" ReadMode $ \handle -> do
      table <- fmap parseCsv $ hGetContents handle
      let mins  = time2mins time
          snum_ = getSlotNumber slot table
          snum  = fromJust snum_
          times = map mins2time . scanl (+) mins $ repeat 10
          slots = drop (snum-1) $ cycle table
      if isNothing snum_ then error "slot not found"
      else mapM_ (putStrLn . formatting) . take 88 $ zip times slots

formatting :: (PrintfType t) => ((Int, Int), (Int, [String])) -> t
formatting ((h,m), (n, slots)) = 
    let slots' = concatMap (printf "%-12s") slots :: String
    in printf "%3d:%02d [%2d] %s" h m n slots'

parseCsv :: String -> SlotTable
parseCsv str = map (g . f) $ lines str
    where f = splitRegex (mkRegex ",[ \t]*")
          g (x:xs) = (read x, xs)

parseArgs :: [String] -> ((Int, Int), String)
parseArgs (time:slot:_) = let (h, (_:m)) = break (==':') time
                          in ((read h, read m), slot)

getSlotNumber :: String -> [(Int, [String])] -> Maybe Int
getSlotNumber _ []           = Nothing
getSlotNumber str ((n,x):xs) = if elem str x then Just n
                                else getSlotNumber str xs

mins2time :: Integral a => a -> (a, a)
mins2time n = divMod n 60

time2mins :: Integral a => (a, a) -> a
time2mins (h, m) = h * 60 + m
