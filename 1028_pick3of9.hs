--import Difftones
import Data.List
import Data.Monoid
-- start of second section: I need to grind through a lot of orderings/registral choices to transition from fewest difference tones to most difference tones among adjacent pairs

-- first, let's get adjacent pairs of a list
adjPairs xs = case xs of
  (a:(b:c)) -> ([a,b]: adjPairs (b:c))
  (a:[]) -> []
  [] -> []

-- then, let's pick 3 elements out of a 7-element set
-- from https://stackoverflow.com/questions/21265454/subsequences-of-length-n-from-list-performance/21288092#21288092
sublistofsize 0 _        = [[]]
sublistofsize _ []       = []
sublistofsize n (x : xs) = sublistsThatStartWithX ++ sublistsThatDontStartWithX
  where sublistsThatStartWithX = map (x:) $ sublistofsize (n-1) xs
        sublistsThatDontStartWithX = sublistofsize n xs

septa = [4,5,7,8,9,11,12] -- our working septachord
trichordsOf = sublistofsize 3

complement set universe = filter (\x -> not $ elem x set) universe

-- trichord -> trichord on bottom, then C#/D#, then complement on top. ' is complement on bottom, trichord on top
tcOrdering tc = tc <> [1,3] <> complement tc septa
tcOrdering' tc = complement tc septa <> [1,3] <> tc

dftPairs = [[4,8], [9,11], [5,7], [8,12]]
-- countAdjs: count how many linearly adjacent elements are one of our chosen difftones (as in dftPairs above)
countAdjs xs = foldl (+) 0 $ map (\x -> if x then 1 else 0) $ isInfixOf <$> dftPairs <*> pure xs

allCombos = (map tcOrdering $ trichordsOf septa) <> (map tcOrdering' $ trichordsOf septa) -- all ways of splitting trichords above/below

-- all with n adjacencies
withAdjs n = filter (\x -> countAdjs x == n) allCombos


-- we can now live entirely below here
-- it's clumsy to work with these full collections. Let's just work with trichords
countAdjs_tri = countAdjs . tcOrdering
triWithAdjs n = filter (\x -> countAdjs_tri x == n) $ trichordsOf septa

-- it would be helpful to have  tiebreaker. Let's break ties if an entire major third is in one, but split.

-- subsetp: return whether xs is a subset of ys
subsetp xs ys = foldl (&&) True $ map (flip elem ys) xs

-- does a trichord  or its complement contain one of two major thirds while it isn't an infix?
maj3p tri = foldl (||) False $ map (\xs -> (subsetp [4,8] xs && (not $ isInfixOf [4,8] xs)) || (subsetp [8,12] xs && (not $ isInfixOf [8,12] xs))) -- this could be rewritten better
  [tri, complement tri septa]

-- now, take n = how many adjacencies, boolean True if we want hidden thirds
triWith n b = filter (\x -> maj3p x == b) $ triWithAdjs n

zeroWith = filter (not . maj3p) $ triWithAdjs 0
-- triWith 0 False, triWith 0 True, etc, for our ordering. #s: 4, 8, 4, 4, 6, 6, 2, 1.
-- for "musical" (ie extrinsic to the process) reasons, sort each one by descending highest pitch, then descending second-highest pitch, etc. So, replace each trichord with its two realizations

trisToConfigs = (reverse . sortBy (\x y -> compare (reverse x) (reverse y))) . --concat . (map (\x -> [tcOrdering x, tcOrdering' x])) previous version
  (map tcOrdering) -- 10/29 alternate version, trichords-on-bottom only

c00 = triWith 0 False
c01 = triWith 0 True
c10 = triWith 1 False
c11 = triWith 1 True
c20 = triWith 2 False
c21 = triWith 2 True
c30 = triWith 3 False
c31 = triWith 3 True
c40 = triWith 4 False
c41 = triWith 4 True

allColls = map trisToConfigs [c00, c01, c10, c11, c20, c21, c30, c31, c40, c41]
-- now I have all of my collections in a nice order, so I can hopefully figure out how to export them and everything. But it's ordered!

-- ordToPitch: convert a list like [5,8,9,1,2,4,7,11,12] to useful pitches (this time in MIDI)
ordToPitches xs = let
  split = splitAt (if (xs !! 3 == 1) then 3 else 4) xs
  in
  (map (+ 84) (fst split)) <> (map (+ 96) (snd split))

triMidicents = (map ((map (* 100)) . ordToPitches)) . trisToConfigs

collMidicents = (map ((map (* 100)) . ordToPitches))

lispShowList xs = "(" ++ (foldl (++) "" $ map ((++ " ") . show) xs) ++ ")"

-- lispShowListLists xs = foldl (++) "\n" $ map ((++ "\n") . lispShowList) xs

-- omShowColl = putStr . lispShowListLists . collMidicents

-- omShowCollConcat = putStrLn . lispShowList . concat . collMidicents

-- after playing with Bach library, we actually want each measure to be its own list sublist

-- "show list by": pick the show
lispShowListBy myShow xs = "(" ++ (foldl (++) "" $ map ((++ " ") . myShow) xs) ++ ")"

allColls_llll = lispShowListBy (lispShowListBy (lispShowListBy show)) $ map collMidicents allColls

coll_llll coll = lispShowListBy (lispShowListBy show) $ collMidicents coll

-- a bit more automation to get the best possible output
-- as structural markers, add a measure of 9 "0"s to the end of each collection

allCollsSepd = concat $ map (<> [[0,0,0,0,0,0,0,0,0]]) allColls

rhythms = "(" ++ (concat $ map (\x -> "(1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8) ") allCollsSepd) ++ ")"
