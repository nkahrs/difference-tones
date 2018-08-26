import Difftones
import Data.List

twelve = [0..11]

nine = fmap ((flip modR 12) . (+ 2) . (* (4/3))) [0..8]

tn = sort $ nub $ mappend (map fromInteger twelve) nine

-- arbitrary combinations of twelve-tet and 9-tet, filtered by proximity to +-14c, 50c

allPCs = let
  foo = map (flip Pitch 6) tn
  in
    sort $ filter (not . isNaN) $
    nub $ map ((flip modR 1) . pc) $
    difftone <$> foo <*> foo

relevantp x = (0.13 < x && x < 0.2) ||
  (0.45 < x && x < 0.55) ||
  (0.82 < x && x < 0.87)

-- that gets us a list of pitch classes, but there are some tedious adaptations before it can actually get us the source pitches

--type SourceDft = ((Pitch, Pitch), (Pitch, Double))

data SourceDftMt = SourceDftMt {p1 :: Pitch, p2 :: Pitch, result :: Pitch, microtone :: Double}
  deriving Show

-- we want to be able to remove extraneous values---just one representative combo per potential microtone relative to 12tet. In order to do this, we define two difference tones (wrapped in a container with source pitches, result, and microtone) to be equal if the microtones are the same

instance Eq SourceDftMt where
--  sdm1 == sdm2 = ((p1 sdm1 == p1 sdm2) && (p2 sdm1 == p2 sdm2)) || ((p1 sdm1 == p2 sdm2) && (p2 sdm1 == p1 sdm2))
  sdm1 == sdm2 = (microtone sdm1) == (microtone sdm2)

instance Ord SourceDftMt where
  compare sdm1 sdm2 = compare (microtone sdm1) (microtone sdm2)

dftSDM p1 p2 = let dft = difftone p1 p2 in
  SourceDftMt p1 p2 dft ((flip modR 1) $ pc $ dft)

pcsSDM = let
  foo = map (flip Pitch 6) tn in
    filter (relevantp . microtone) $
    sort $ filter (not . isNaN . microtone) $ nub $
    dftSDM <$> foo <*> foo
