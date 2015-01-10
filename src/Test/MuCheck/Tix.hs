{-# LANGUAGE MultiWayIf #-}
-- | Read the HPC Tix and Mix files.
module Test.MuCheck.Tix where
import Trace.Hpc.Tix
import Trace.Hpc.Mix
import Trace.Hpc.Util

-- | Span info - same as HpcPos
-- but we have to make it separately because HpcPos constructors are hidden and
-- hence Generic deriving tricker does not work.
data Span = P !Int !Int !Int !Int deriving (Eq, Ord)

instance Show Span where
  show (P l1 c1 l2 c2) = show l1 ++ ':' : show c1 ++ '-' : show l2 ++ ':' : show c2

-- | 'fromSpan' explodes the Span into line:column-line:colunm
fromSpan :: Span -> (Int,Int,Int,Int)
fromSpan (P l1 c1 l2 c2) = (l1,c1,l2,c2)
--
-- | 'toSpan' implodes to Span, from line:column-line:colunm
toSpan :: (Int,Int,Int,Int) -> Span
toSpan (l1,c1,l2,c2) = P l1 c1 l2 c2


-- | asks the question, is the first argument inside the second argument.
insideSpan :: Span -> Span -> Bool
insideSpan small big =
	     sl1 >= bl1 &&
	     (sl1 /= bl1 || sc1 >= bc1) &&
	     sl2 <= bl2 &&
	     (sl2 /= bl2 || sc2 <= bc2)
  where (sl1,sc1,sl2,sc2) = fromSpan small
        (bl1,bc1,bl2,bc2) = fromSpan big

-- | Whether a line is covered or not
data TCovered = TCovered
              | TNotCovered
  deriving (Eq, Show)

-- | Whether a line is covered or not
isCovered :: TCovered -> Bool
isCovered TCovered = True
isCovered _  = False

-- | `mixTix` joins together the location and coverage data.
mixTix :: String -> Mix -> TixModule -> (String, [(Span, TCovered)])
mixTix s (Mix _fp _int _h _i mixEntry) tix = (s, zipWith toLocC mymixes mytixes)
  where mytixes = tixModuleTixs tix
        mymixes = mixEntry
        toLocC (hpos, _) covT = (toSpan (fromHpcPos hpos), isCov covT)
        isCov 0 = TNotCovered
        isCov _ = TCovered

-- | reads a tix file. The tix is named for the binary run, and contains a list
-- of modules involved.
parseTix :: String -> IO [TixModule]
parseTix path = do
  tix <- readTix path
  case tix of
    Nothing -> return []
    Just (Tix tms) -> return tms

-- | Read the corresponding Mix file to a TixModule
getMix :: TixModule -> IO Mix
getMix tm = readMix [".hpc"] (Right tm)

-- | return the tix and mix information
getMixedTix :: String -> IO [(String, [(Span, TCovered)])]
getMixedTix file = do
  tms <- parseTix file
  mixs <- mapM getMix tms
  let names = map tixModuleName tms
  return $ zipWith3 mixTix names mixs tms
{- getMixedTix "tests.tix"
[("Main",[
 (11:12-11:26,TNotCovered),
 (11:3-11:26,TNotCovered),
 (10:9-11:26,TNotCovered),
 (10:1-11:26,TNotCovered),
 (6:14-6:44,TCovered),
 (6:3-6:44,TCovered),
 (7:14-7:46,TCovered),
 (7:3-7:46,TCovered),
 (8:14-8:46,TCovered),
 (8:3-8:46,TCovered),
 (5:8-8:46,TCovered),
 (5:1-8:46,TCovered)])]
-}
-- [10:1-11:26]
-- | getUnCoveredPatches returns the largest parts of the program that are not
-- covered.
getUnCoveredPatches :: String -> String -> IO (Maybe [Span])
getUnCoveredPatches file name = do
  val <- getMixedTix file
  let modSpan = getNamedModule name val
      uncovSpan = filter (not . isCovered . snd) modSpan
  return $ case val of
            [] -> Nothing
            _ -> Just $ removeRedundantSpans $ map fst uncovSpan

-- | Get the span and covering information of the given module
getNamedModule :: String -> [(String, [(Span,TCovered)])] -> [(Span,TCovered)]
getNamedModule mname val = snd . head $ filter (\(a, _b) -> a == mname) val

-- | Remove spans which are contained within others of same kind.
removeRedundantSpans :: [Span] -> [Span]
removeRedundantSpans [] = []
removeRedundantSpans [x] = [x]
removeRedundantSpans (a:b:cde) = if | insideSpan a b -> removeRedundantSpans (b:cde)
                                    | otherwise      -> a : removeRedundantSpans (b:cde)

