import Difftones
import Data.List

-- 11/27/17: return all the sidebands from modulating one pitch by another pitch. Use partials 1 through 8.

-- first, do everything in terms of frequency

-- overtones fund n: return the first n overtones of fundamental fund
overtones fund n = map (* fund) [1..n]

-- sidebands: carrier and modulator are frequencies, n is how many overtones to consider
sidebands carrier modulator n = (sort . nub . (map abs)) $
  map (+ carrier) $ (*) <$> [-1,1] <*> overtones modulator n

-- now, with pitches

sidebands'freq carrier modulator =
  sidebands (pitchFreq carrier) (pitchFreq modulator)

sidebands' x y z= map freqPitch $ sidebands'freq x y z

-- next step: make the output of this usable in Bach library or OM (ie as midicents in ()s)

pitchMidiCents p = round $ ((pc p) * 100) + (fromIntegral (6000 + 1200 * (oct p - 4)))

omShowListMC :: Show a => [a] -> [Char]
omShowListMC = (++ ")") . foldl (++) "( " . map ((++ " " ) . show)

omSidebandsStr x y z = omShowListMC $ map pitchMidiCents $ sidebands' x y z

omSidebands x y z = putStrLn $ (++ ", bang") $ omSidebandsStr x y z


-- 12/11/17, let's suppose we want to get all possible permutations of modulator/carrier among a chord.

-- first, let's do something that Haskell makes easier for us: all possible carrier/modulator combos

allCombs chord1 chord2 n = ((\x y -> sidebands' x y n) <$> chord1 <*> chord2)

omAllCombs x y z=  putStrLn $ (("(" ++) . (++ "), bang") ) $ concat $ ((++ " ") . omShowListMC . map pitchMidiCents) <$> allCombs x y z
