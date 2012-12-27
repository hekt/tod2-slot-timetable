import System.IO
import System.Environment (getArgs)
import Text.Printf
import Text.Regex (mkRegex, splitRegex)
import Data.Maybe (fromJust, isJust)

type SlotTable = [(Int, [String])]

main = do
  (time, slot) <- fmap parseArgs getArgs
  timeTable slot time

timeTable slot time = do
  withFile "slot_table_en.csv" ReadMode $ \handle -> do
      table <- fmap parseCsv $ hGetContents handle
      let mins  = time2mins time
          s_    = getSlotNumber slot table
          s     = fromJust (if isJust s_ then s_ 
                            else error "invalid slot") - 1
          times = map mins2time $ scanl (+) mins $ repeat 10
          slots = drop s $ cycle table
      mapM_ (putStrLn . formatting) . take 88 $ zip times slots

formatting :: (PrintfType t) => ((Int, Int), (Int, [String])) -> t
formatting ((h,m), (n, slots)) = 
    let slots' = concatMap (printf "%-12s") slots :: String
    in printf "%3d:%02d  [%2d]  %s" h m n slots'

parseCsv :: String -> SlotTable
parseCsv str = map (g . f) $ lines str
    where f = splitRegex (mkRegex ",[ \t]*")
          g (x:xs) = (read x, xs)

parseArgs :: [String] -> ((Int, Int), String)
parseArgs (time:slot:_) = let (h, m_) = break (==':') time
                              m       = tail m_
                          in ((read h, read m), slot)

getSlotNumber :: String -> [(Int, [String])] -> Maybe Int
getSlotNumber _ []            = Nothing
getSlotNumber str ((n, x):xs) = if elem str x then Just n
                                else getSlotNumber str xs

mins2time :: Integral a => a -> (a, a)
mins2time n = divMod n 60

time2mins :: Integral a => (a, a) -> a
time2mins (h, m) = h * 60 + m
