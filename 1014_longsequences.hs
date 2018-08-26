import Difftones
import Data.List
import Data.Monoid
import qualified Data.MultiSet as MultiSet -- installed via cabal install multiset, used to identify repetitions in lists

-- to avoid repetition of the same difference tone, we'll package our difference tones in containers
-- type SourceDft = (Pitch 1, Pitch 2, dft in Hz, dft Pitch)

data SourceDft = SourceDft {p1 :: Pitch, p2 :: Pitch, resF :: Double, resP :: Pitch}
  deriving Show

-- are two pitches basically the same (within 5 cents)?
closeEnough p1un p2un = let
  p1 = normalize p1un
  p2 = normalize p2un
  in
    (oct p1 == oct p2) && ((abs (pc p1 - pc p2)) < 0.05)

swapSources sdft = SourceDft (p2 sdft) (p1 sdft) (resF sdft) (resP sdft)

ceDFT x1 x2 = (closeEnough (p1 x1) (p1 x2)) && (closeEnough (p2 x1) (p2 x2))

instance Eq SourceDft where
  x1 == x2 = (ceDFT x1 x2) || (ceDFT x1 $ swapSources x2)-- || ((resF x1 == 0) && (resF x2 == 0))

  --two zeros are always equal if you remove the --

instance Ord SourceDft where
  compare x1 x2 = compare (resF x1) (resF x2)

dft p1 p2 = let freq = difftoneFreq p1 p2 in
  SourceDft p1 p2 freq $ freqPitch freq

-- now, we need to be able to find all the internal difference tones of some list of notes

dfts xs = sort $ nub $ dft <$> xs <*> xs

-- the actual collections
-- o6: make a list of numbers into pitches in octave 6
o6 = map (flip Pitch 6)

--collections for 7:5, 6:5, 5:4, 3:2, 2:1
-- shorthands: 7, 6, 5, 3, 2
-- naming is c[numbers] for minimal overlap between things
-- or c[number] for just one
-- common points first

cAll = [5.5, 5.86]
c765 = [6]
c763 = [0.5, 0.67]
c7532 = [7, 7.17]

c7 = o6 $ c763 <> cAll <> c765 <> c7532
c6 = o6 $ c763 <> [2.5, 2.86] <> cAll <> c765
c5 = o6 $ cAll <> c765 <> c7532
c3 = o6 $ c763 <> cAll <> c7532
c2 = o6 $ cAll <> c7532

-- for easier use, enable an alternate method of sorting SourceDfts: by sources instead of by pitch

sortSources x1 x2 =
  if p1 x1 == p1 x2 then
    compare (p2 x1) (p2 x2)
  else
    compare (p1 x1) (p1 x2)
    
-- to get approximate frequencies in sequential order corresponding to 10/14/17-1 sketch, we can use something like


c7_test = map (round . resF) . (sortBy sortSources) . dfts $ c7

-- at this point, my only way forwards is eyeballing to see if certain pitches "seem viable," so [shrug].

-- I still need second-order difference tones. So...
pitchIsNaN = isNaN . pc

-- list of sources/difference tones to list of pitches
sdftsToPitches xs = filter (not . pitchIsNaN) $ map resP xs

-- all of the output is illegible, so let's do nicer formatting

-- round a double to have two decimal places
round2 x = (fromInteger $ round $ x * (10^2)) / (10.0^^2)

-- show a pitch more nicely
showPitch (Pitch a b) = "(" ++ show (round2 a) ++ " " ++ show b ++ ")"

-- show a SourceDft more nicely
showSdft (SourceDft p1 p2 rf rp) = let tab = ['\t'] in
  showPitch p1 ++ " " ++ tab ++ showPitch p2 ++ " " ++ tab ++ (show $ round2 rf) ++ tab ++ showPitch rp ++ ['\n']

-- show a list of them with tabs and newlines.
-- almost there, except we have inconsistent tab sizes

showSdfts = putStr . concat . map showSdft

--main1 (for now): consume a list of pitches, return nicely formatted difftones (1 = first order)
main1 = showSdfts . dfts

--second-order difference tones
main2 = showSdfts . dfts . sdftsToPitches . dfts

-- third order for shits and gigs
main3 = showSdfts . dfts . sdftsToPitches . dfts . sdftsToPitches . dfts

-- we theoretically could generalize to nth order but there's no good reason for that

-- the real question here is why we get so many repeated values. For now, to abstract away from

samePitch x1 x2 = (round2 . resF $ x1) == (round2 . resF $ x2)
noDups = nubBy samePitch

-- now, we can eliminate duplicates like so

main1' = showSdfts . noDups . dfts
main2' = showSdfts . noDups . dfts . sdftsToPitches . noDups . dfts
main3' = showSdfts . noDups . dfts . sdftsToPitches . noDups . dfts . sdftsToPitches . noDups . dfts

-- the current issue is that it doesn't contain the entire history for each difference tone. I have to manually trace backwards. That could be considered good or bad. Implementing the whole history for everything would require me to reinvent these data structures recursively, at which point this is no longer "composing first."
-- I should figure out why we get so many duplicated difference tones, though.

-- after some investigation, it's most interesting when the difference tones loop back on themselves---ie we find second-order difference tones from the original sequence. Let's look for those specifically, with "main2''".

--main2'' xs = filter (\p -> elem (round2 . resF $ p) (map (round2 . pitchFreq) ps)) (dfts . sdftsToPitches . noDups . dfts $ ps)

main2''_innards xs = 
  let ps = sdftsToPitches . noDups . dfts $ xs in --ps is the set of first difference tones
    filter (\p -> elem (round2 . resF $ p)
             (map (round2 . pitchFreq) ps)) -- filter by whether they're in the set of first-order difference tones
    (dfts $ ps) -- set of second-order difference tones

main2'' = showSdfts . main2''_innards -- IO action

-- this actually does what I want. Given a list of pitches, it tells me all the ways I can express the first-order difference tones as difference tones between themselves.

--really, this is begging to be refactored.

-- instead of refactoring, we'll also have a main2''' that gives the dfts in the first one that aren't in the second

main2'''_innards xs ys = filter (not . flip elem (main2''_innards ys)) $ main2''_innards xs

main2''' xs ys = showSdfts $ main2'''_innards xs ys

-- now I want to count total mentions in all 3 pitch columns.  First, combine and sort.

sdftsAllPitches xs = (map p1 xs) <> (map p2 xs) <> (map resP xs)

main2'''_count xs ys = MultiSet.fromList $ sort $ map (\p -> (Pitch (round2 $ pc p) (oct p))) $ sdftsAllPitches $ main2'''_innards xs ys
-- I shouldn't need sort, but there's a glitch somewhere

-- example usage: main2'''_count c3 c2 shows you the second-order difference tones that also first-order difference tones in collection c3 (3/2) but not in c2 (2/1), and the format tells you how many times they appear total (ie as result and as source)

-- notably, one impediment I've found is that I should be able to doubly eta-reduce the above, but I can't.

{-
10/17: another issue is that we get redundancies of the specific form that (a,b->c) gets flipped to (a,c->b) since a-b=c means a-c=b. I can easily discount this by just ignoring the second half of a list, though.

another issue: I want c7 relative to combined difftones of c5 and c3 and c2, but not difftones of combined c5 and c3 and c2. I can sort this out with some ingenuity...-}

main2'''c7_innards = sort $ nub $ (main2'''_innards c7 c3) <> (main2'''_innards c7 c5) <> (main2'''_innards c7 c2)

main2'''c7 = showSdfts $ main2'''c7_innards

main2'''c7_count = MultiSet.fromList $ sort $ map (\p -> (Pitch (round2 $ pc p) (oct p))) $ sdftsAllPitches main2'''c7_innards


-- for OpenMusic use:
pitchMidiCents p = round $ ((pc p) * 100) + (fromIntegral (6000 + 1200 * (oct p - 4)))

omShowSdft (SourceDft x y _ z) = let showmc = show . pitchMidiCents in
  "(" ++ (showmc x) ++ " " ++ (showmc y) ++ " " ++ (showmc z) ++ ") "

omShowSdfts xs = putStrLn $ "(" ++ (concat $ map omShowSdft xs) ++ ")"


-- 10/25/17 adaptations: for each collection, I want to see all of the first- and second-order difference tones, sorted by average pitch.

-- honestly probably the easiest way to do this with the functions I've already defined (though it involves a bit of computational redundancy) is to average the three notes as midicents, and sort by that.

-- sdft to average midicents
sdftAvgMc x = ((/ 3)) $ foldl (+) 0 $ map pitchFreq
  [p1 x, p2 x, resP x]

-- first-order difference tones and second-order dfts with result pitch also a first-order dft, for a given [Pitch] (ie "c2")

-- first and second-order difference tones sorted high to low by pitch centroid
sorted12_general f xs = -- f is the [Pitch]->[Sdfts] we want, ie main2''_innards
  nubBy (\x y -> ((resP x == p1 y) && (p2 x == p2 y)) ||
                 ((resP x == p2 y) && (p1 x == p2 y))) $
  -- eliminate quasi-duplicates in which the source of one is the result of another and the third is the same
  sortBy (\y x -> compare (sdftAvgMc x) (sdftAvgMc y)) $ -- swapped so it's high->low
  filter (\x -> (resF x /= 0)) $ dfts xs <> f xs -- formerly "firstAndSecond"

sorted12 = sorted12_general main2''_innards

-- recall c23567
-- try, say, omShowSdfts $ sorted12 c2


-- now, we want ''' instead (exclude those of a second set listed)

sorted12''' included excluded =
  sorted12_general (flip main2'''_innards excluded) included

--- also the whole c7 thing (see well above)
sorted12'''c7 =
    nubBy (\x y -> ((resP x == p1 y) && (p2 x == p2 y)) ||
                 ((resP x == p2 y) && (p1 x == p2 y))) $
  sortBy (\y x -> compare (sdftAvgMc x) (sdftAvgMc y)) $
  filter (\x -> (resF x /= 0)) $ dfts c7 <> main2'''c7_innards

-- these tools now work pretty well. The primary remaining issue is that I'm using pitches I at some point decided to exclude... I should add in another filter for that.

-- 10/28: fixing the issue described in the previous comment. I need to filter by (is each of these notes in the collection we care about?).

-- ipn is important pitches for collection n
ip2 = sort $ nub $ c7 <> c6 <> [Pitch 1.9 3, Pitch 11.9 2{-avoiding a real bug fix-}, Pitch 0 3, Pitch 9.8 2, Pitch 10.6 0, Pitch 11 (-1)]

ip3 = ip2 <> [Pitch 4.5 (-1), Pitch 5 4, Pitch 5.5 4, Pitch 6.5 4, Pitch 10.5 4, Pitch 10.8 4, Pitch 11 4]

ip5 = ip2 <> [Pitch 6.5 (-1), Pitch 4.4 1, Pitch 5.1 2, Pitch 8 2]

ip6 = ip2 <> [Pitch 10.7 2, Pitch 0.14 3, Pitch 2 3, Pitch 3.2 3, Pitch 7.6 3, Pitch 9.7 3, Pitch 10 3, Pitch 10.9 3, Pitch 11.8 3, Pitch 0.6 4, Pitch 5 4, Pitch 5.4 4, Pitch 7 4, Pitch 7.5 4, {-change 10/31-} Pitch 6.5 (-1), Pitch 4.5 (-1), Pitch 7.6 0]

ip7 = ip3 <> ip5 <> [Pitch 7 4]--, Pitch 7.5 4]. I'm making an executive decision to kill that particular quarter-tone.

-- from 10 28 program
subsetp xs ys = foldl (&&) True $ map (flip elem ys) xs

sdftList s = [p1 s, p2 s, resP s]
relevantOnly sdfts rvtps = filter ((flip subsetp rvtps) . sdftList) sdfts

-- example usage: showSdfts $ relevantOnly (sorted12''' c6 c2) ip6 --- this actually miracuously seems to work!

-- 10/29, more automation for output to Bach

reducedDfts = map (uncurry relevantOnly) [
  (sorted12 c2, ip2),
  (sorted12''' c3 c2, ip3),
  (sorted12''' c5 c3, ip5),
  (sorted12''' c6 c7, ip6),
  (sorted12'''c7, ip7)
  ]

  
bachShowSdfts xs = "((" ++ (concat $ map omShowSdft xs) ++ "))"
