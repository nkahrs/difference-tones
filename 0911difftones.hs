module Difftones where

data Pitch = Pitch Double Int
  deriving Show

c4 = 440 * (2 ** (-9/12))

-- get pc and octave
pc (Pitch pc _) = pc

oct (Pitch _ oct) = oct

-- normalization via remainders
modR x n = (fromIntegral (mod flx n)) + dfx where
  flx = floor x
  dfx = x - (fromIntegral flx)

-- and quotients too
quotR x n
  | x >= 0 = quot (floor x) n
  | otherwise = quot (ceiling x) n - 1 -- this is knarly and causes problems
-- elaboration: floor caused issues with -11 to -12
  -- ceiling is ok with -11.5
  -- still bad when it's -12, but that's not as bad

normalize :: Pitch -> Pitch
normalize (Pitch pc oct)
  | (0 <= pc) && (pc < 12) = (Pitch pc oct)
  | otherwise = (Pitch (modR pc 12) (oct + (quotR pc 12)))

instance Eq Pitch where
  p1 == p2 =
    (pc1n == pc2n) && (oct1n == oct2n) where
    p1n = normalize p1
    pc1n = pc p1n
    oct1n = oct p1n
    p2n = normalize p2
    pc2n = pc p2n
    oct2n = oct p2n

-- plus and minus

-- pitch to frequency
pitchFreq (Pitch pc oct) =
  (2 ** (pc/12)) *
  (2 ** ((fromIntegral oct) - 4)) * c4

-- reallog: not needed because we get NaN so this typechecker doesn't complain

-- frequency to pitch

freqPitch f0 =
  let abovec4 = 12 * (log (f0 / c4)) / (log 2) in
    normalize (Pitch abovec4 4)

-- difference tones
difftoneFreq p1 p2 = abs $ (pitchFreq p1) - (pitchFreq p2)
difftone p1 p2 = freqPitch $ difftoneFreq p1 p2


-- now, some specific stuff for this new task: some scale constructors
-- first, define an ordering on pitches
instance Ord Pitch where
  p1 <= p2 =
    (isNaN $ pc p1) ||
    (oct p1n < oct p2n) ||
    ((oct p1n == oct p2n) && (pc p1n <= pc p2n))
    where
      p1n = normalize p1
      p2n = normalize p2

-- let intervals just be represented by real numbers carrying direction
applyInterval interval (Pitch pc oct) =
  normalize $ Pitch (pc + interval) oct

evenScale start step len =
  take len $ map ((flip applyInterval) start) (map (* step) [0..])

-- get all difftones in a list, this would take some thought about how best to sort things


{-
tasks as of 09/11/17:
see the sequence we get from just 4/3-semitones.
then, straight-up 12tet.
then, what if we combine those two systems?
We get certain notes to jump up octaves from 12tet, what if we combine just those with 12tet?
-}

difftonesEven start step len = map (difftone start) $ evenScale start step len
