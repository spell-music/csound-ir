-- | Searches for IR-files in the user's home directory  in the directory
-- "HOME_USER/.csound-ir/samplicity_t600_v1.3/Samplicity T600 - 02 - Wave, 24 bit, 48 khz"
module Csound.Ir.T6.V48_24(
    -- * Halls
    hsDense , hSmall , hAmbient , sAcousticGuitar , hFrontPiano , hShort , hJoy
    , hBandLeader , hlClub , hStage , hlJazzScene , hBrightTheatre , hPiano
    , hOverheadMics , hVocalSlapback , hSmokey , hArena , hWarmVenue , hVocal
    , hmVocal , hLarge , hMedium , hmString , hVocalBalad , hBackWall , hGuitarBright
    , hShow , hVocalDeepMale , hVocalFemale , hNatural , hArenaEmpty , hEuropean
    , hVocalGrand , hlWarm , hSlapBright , hCathedral , hGospel , hlClear , hSlapWarm
    , hPianoChurch , hChurch , hlOrchestra , hNewAge , hWarmCathedral

    -- * Rooms
    , rDrum , rCloset , rRhodesThicken , rsBooth , rTightRound , rsWooden , rBlackVelvet
    , rSmall , rsPercussion , rCoffeeHouse , rGuitarClear , rGuitarAcoustic , rDrumExpander
    , rLiveClub , rShortSlapbackVerb , rStudio , rWideAmbient , rBandRehearsal , rClear
    , rHardDrum , rTomTomReverb , rBossaNovaPercussion , rmBasement , rMedium , rRapClub
    , rBrightSnare , rPianoSlapback , rStudio20x20 , rPercussion , rStudio40x40 , rFatSnare
    , rVintageSnare , rSlowSnare , rStraightTailPerc

    -- * Plates
    , pFatDrum , pDrumWood , pAmbient , pPiano , pStairway , pSlapback , pBright
    , pSilkyGold , pSoftDrumPerc
    -- * Effects
    , sOutInTheDark , sSteelDrum
) where

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Csound.Base

rootDir = ".csound-ir" </> "samplicity_t600_v1.3" </> "Samplicity T600 - 06 - Wave, 24 bit, 48 khz"

getDirs :: String -> (String, String)
getDirs dir = (fileL, fileR)
    where
        home = unsafePerformIO getHomeDirectory
        fileL = fileBy mkLeft
        fileR = fileBy mkRight

        fileBy mk = home </> rootDir </> dir </> mk dir
        mkLeft = mkBy "L"
        mkRight = mkBy "R"
        mkBy suf x = x ++ " " ++ suf ++ ".wav"

genT6 :: MixAt Sig2 Sig2 a => String -> Sig -> Sig -> a -> AtOut Sig2 Sig2 a
genT6 file k ratio ain = mixAt ratio (mul k . stereoIR2 dirs) ain
    where dirs = getDirs file


-- | Reverb for preset: T600 - 001 Small Dense Hall.
hsDense :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hsDense = genT6 "T600 - 001 Small Dense Hall" 0.1


-- | Reverb for preset: T600 - 002 Small Hall.
hSmall :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hSmall = genT6 "T600 - 002 Small Hall" 0.1


-- | Reverb for preset: T600 - 003 Ambient Hall.
hAmbient :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hAmbient = genT6 "T600 - 003 Ambient Hall" 0.1


-- | Reverb for preset: T600 - 004 Acoustic Guitar Space.
sAcousticGuitar :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sAcousticGuitar = genT6 "T600 - 004 Acoustic Guitar Space" 0.1


-- | Reverb for preset: T600 - 005 Piano Hall Front Row.
hFrontPiano :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hFrontPiano = genT6 "T600 - 005 Piano Hall Front Row" 0.1


-- | Reverb for preset: T600 - 006 Short Hall.
hShort :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hShort = genT6 "T600 - 006 Short Hall" 0.1


-- | Reverb for preset: T600 - 007 Joy Hall.
hJoy :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hJoy = genT6 "T600 - 007 Joy Hall" 0.1


-- | Reverb for preset: T600 - 008 Band Leader.
hBandLeader :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hBandLeader = genT6 "T600 - 008 Band Leader" 0.1


-- | Reverb for preset: T600 - 009 Big Empty Club.
hlClub :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hlClub = genT6 "T600 - 009 Big Empty Club" 0.1


-- | Reverb for preset: T600 - 010 Stage and Hall.
hStage :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hStage = genT6 "T600 - 010 Stage and Hall" 0.1


-- | Reverb for preset: T600 - 011 Big Jazz Scene.
hlJazzScene :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hlJazzScene = genT6 "T600 - 011 Big Jazz Scene" 0.1


-- | Reverb for preset: T600 - 012 Bright Theatre.
hBrightTheatre :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hBrightTheatre = genT6 "T600 - 012 Bright Theatre" 0.1


-- | Reverb for preset: T600 - 013 Concert Piano.
hPiano :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hPiano = genT6 "T600 - 013 Concert Piano" 0.1


-- | Reverb for preset: T600 - 014 Overhead Mics.
hOverheadMics :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hOverheadMics = genT6 "T600 - 014 Overhead Mics" 0.1


-- | Reverb for preset: T600 - 015 Slapback Vocal Hall.
hVocalSlapback :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hVocalSlapback = genT6 "T600 - 015 Slapback Vocal Hall" 0.1


-- | Reverb for preset: T600 - 016 Smokey Hall.
hSmokey :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hSmokey = genT6 "T600 - 016 Smokey Hall" 0.1


-- | Reverb for preset: T600 - 017 Concert Arena.
hArena :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hArena = genT6 "T600 - 017 Concert Arena" 0.1


-- | Reverb for preset: T600 - 018 Warm Venue.
hWarmVenue :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hWarmVenue = genT6 "T600 - 018 Warm Venue" 0.1


-- | Reverb for preset: T600 - 019 Vocal Hall.
hVocal :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hVocal = genT6 "T600 - 019 Vocal Hall" 0.1


-- | Reverb for preset: T600 - 020 Medium Vocal Hall.
hmVocal :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hmVocal = genT6 "T600 - 020 Medium Vocal Hall" 0.1


-- | Reverb for preset: T600 - 021 Large Hall.
hLarge :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hLarge = genT6 "T600 - 021 Large Hall" 0.1


-- | Reverb for preset: T600 - 022 Medium Hall.
hMedium :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hMedium = genT6 "T600 - 022 Medium Hall" 0.1


-- | Reverb for preset: T600 - 023 Medium String Hall.
hmString :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hmString = genT6 "T600 - 023 Medium String Hall" 0.1


-- | Reverb for preset: T600 - 024 Balad Vocal Hall.
hVocalBalad :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hVocalBalad = genT6 "T600 - 024 Balad Vocal Hall" 0.1


-- | Reverb for preset: T600 - 025 Back Wall Hall.
hBackWall :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hBackWall = genT6 "T600 - 025 Back Wall Hall" 0.1


-- | Reverb for preset: T600 - 026 Bright Guitar Hall.
hGuitarBright :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hGuitarBright = genT6 "T600 - 026 Bright Guitar Hall" 0.1


-- | Reverb for preset: T600 - 027 Show Hall.
hShow :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hShow = genT6 "T600 - 027 Show Hall" 0.1


-- | Reverb for preset: T600 - 028 Deep Male Vocal Hall.
hVocalDeepMale :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hVocalDeepMale = genT6 "T600 - 028 Deep Male Vocal Hall" 0.1


-- | Reverb for preset: T600 - 029 Female Vocal Hall.
hVocalFemale :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hVocalFemale = genT6 "T600 - 029 Female Vocal Hall" 0.1


-- | Reverb for preset: T600 - 030 Natural Hall.
hNatural :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hNatural = genT6 "T600 - 030 Natural Hall" 0.1


-- | Reverb for preset: T600 - 031 Empty Arena.
hArenaEmpty :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hArenaEmpty = genT6 "T600 - 031 Empty Arena" 0.1


-- | Reverb for preset: T600 - 032 European Hall.
hEuropean :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hEuropean = genT6 "T600 - 032 European Hall" 0.1


-- | Reverb for preset: T600 - 033 Grand Vocal Hall.
hVocalGrand :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hVocalGrand = genT6 "T600 - 033 Grand Vocal Hall" 0.1


-- | Reverb for preset: T600 - 034 Large Warm Hall.
hlWarm :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hlWarm = genT6 "T600 - 034 Large Warm Hall" 0.1


-- | Reverb for preset: T600 - 035 Bright Slap Hall.
hSlapBright :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hSlapBright = genT6 "T600 - 035 Bright Slap Hall" 0.1


-- | Reverb for preset: T600 - 036 Cathedral.
hCathedral :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hCathedral = genT6 "T600 - 036 Cathedral" 0.1


-- | Reverb for preset: T600 - 037 Gospel Hall.
hGospel :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hGospel = genT6 "T600 - 037 Gospel Hall" 0.1


-- | Reverb for preset: T600 - 038 Large Clear Hall.
hlClear :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hlClear = genT6 "T600 - 038 Large Clear Hall" 0.1


-- | Reverb for preset: T600 - 039 Warm Slap Hall.
hSlapWarm :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hSlapWarm = genT6 "T600 - 039 Warm Slap Hall" 0.1


-- | Reverb for preset: T600 - 040 Church Piano.
hPianoChurch :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hPianoChurch = genT6 "T600 - 040 Church Piano" 0.1


-- | Reverb for preset: T600 - 041 Church.
hChurch :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hChurch = genT6 "T600 - 041 Church" 0.1


-- | Reverb for preset: T600 - 042 Big Orchestra Hall.
hlOrchestra :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hlOrchestra = genT6 "T600 - 042 Big Orchestra Hall" 0.1


-- | Reverb for preset: T600 - 043 New Age Hall.
hNewAge :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hNewAge = genT6 "T600 - 043 New Age Hall" 0.1


-- | Reverb for preset: T600 - 044 Warm Cathedral.
hWarmCathedral :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hWarmCathedral = genT6 "T600 - 044 Warm Cathedral" 0.1


-- | Reverb for preset: T600 - 045 Drum Bin.
rDrum :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rDrum = genT6 "T600 - 045 Drum Bin" 0.1


-- | Reverb for preset: T600 - 046 Closet.
rCloset :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rCloset = genT6 "T600 - 046 Closet" 0.1


-- | Reverb for preset: T600 - 047 Rhodes Thicken.
rRhodesThicken :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rRhodesThicken = genT6 "T600 - 047 Rhodes Thicken" 0.1


-- | Reverb for preset: T600 - 048 Small Booth.
rsBooth :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rsBooth = genT6 "T600 - 048 Small Booth" 0.1


-- | Reverb for preset: T600 - 049 Tight and Round.
rTightRound :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rTightRound = genT6 "T600 - 049 Tight and Round" 0.1


-- | Reverb for preset: T600 - 050 Small Wooden Room.
rsWooden :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rsWooden = genT6 "T600 - 050 Small Wooden Room" 0.1


-- | Reverb for preset: T600 - 051 Black Velvet.
rBlackVelvet :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rBlackVelvet = genT6 "T600 - 051 Black Velvet" 0.1


-- | Reverb for preset: T600 - 052 Small Room.
rSmall :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rSmall = genT6 "T600 - 052 Small Room" 0.1


-- | Reverb for preset: T600 - 053 Small Percussion Room.
rsPercussion :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rsPercussion = genT6 "T600 - 053 Small Percussion Room" 0.1


-- | Reverb for preset: T600 - 054 Coffee House.
rCoffeeHouse :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rCoffeeHouse = genT6 "T600 - 054 Coffee House" 0.1


-- | Reverb for preset: T600 - 055 Clear Guitar Room.
rGuitarClear :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rGuitarClear = genT6 "T600 - 055 Clear Guitar Room" 0.1


-- | Reverb for preset: T600 - 056 Acoustic Guitar Room.
rGuitarAcoustic :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rGuitarAcoustic = genT6 "T600 - 056 Acoustic Guitar Room" 0.1


-- | Reverb for preset: T600 - 057 Drum Expander Room.
rDrumExpander :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rDrumExpander = genT6 "T600 - 057 Drum Expander Room" 0.1


-- | Reverb for preset: T600 - 058 Generic Live Club.
rLiveClub :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rLiveClub = genT6 "T600 - 058 Generic Live Club" 0.1


-- | Reverb for preset: T600 - 059 Short Slapback Verb.
rShortSlapbackVerb :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rShortSlapbackVerb = genT6 "T600 - 059 Short Slapback Verb" 0.1


-- | Reverb for preset: T600 - 060 The Studio.
rStudio :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudio = genT6 "T600 - 060 The Studio" 0.1


-- | Reverb for preset: T600 - 061 Wide Ambient Chamber.
rWideAmbient :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rWideAmbient = genT6 "T600 - 061 Wide Ambient Chamber" 0.1


-- | Reverb for preset: T600 - 062 Band Rehearsal.
rBandRehearsal :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rBandRehearsal = genT6 "T600 - 062 Band Rehearsal" 0.1


-- | Reverb for preset: T600 - 063 Clear Room.
rClear :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rClear = genT6 "T600 - 063 Clear Room" 0.1


-- | Reverb for preset: T600 - 064 Hard Drum Space.
rHardDrum :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rHardDrum = genT6 "T600 - 064 Hard Drum Space" 0.1


-- | Reverb for preset: T600 - 065 Tom Tom Reverb.
rTomTomReverb :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rTomTomReverb = genT6 "T600 - 065 Tom Tom Reverb" 0.1


-- | Reverb for preset: T600 - 066 Bossa Nova Percussion.
rBossaNovaPercussion :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rBossaNovaPercussion = genT6 "T600 - 066 Bossa Nova Percussion" 0.1


-- | Reverb for preset: T600 - 067 Medium Basement.
rmBasement :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rmBasement = genT6 "T600 - 067 Medium Basement" 0.1


-- | Reverb for preset: T600 - 068 Medium Room.
rMedium :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rMedium = genT6 "T600 - 068 Medium Room" 0.1


-- | Reverb for preset: T600 - 069 Rap Club.
rRapClub :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rRapClub = genT6 "T600 - 069 Rap Club" 0.1


-- | Reverb for preset: T600 - 070 Bright Snare Room.
rBrightSnare :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rBrightSnare = genT6 "T600 - 070 Bright Snare Room" 0.1


-- | Reverb for preset: T600 - 071 Slapback Piano.
rPianoSlapback :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rPianoSlapback = genT6 "T600 - 071 Slapback Piano" 0.1


-- | Reverb for preset: T600 - 072 Studio 20 x 20.
rStudio20x20 :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudio20x20 = genT6 "T600 - 072 Studio 20 x 20" 0.1


-- | Reverb for preset: T600 - 073 Percussion Room.
rPercussion :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rPercussion = genT6 "T600 - 073 Percussion Room" 0.1


-- | Reverb for preset: T600 - 074 Studio 40 x 40.
rStudio40x40 :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudio40x40 = genT6 "T600 - 074 Studio 40 x 40" 0.1


-- | Reverb for preset: T600 - 075 Fat Snare Room.
rFatSnare :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rFatSnare = genT6 "T600 - 075 Fat Snare Room" 0.1


-- | Reverb for preset: T600 - 076 Vintage Snare Room.
rVintageSnare :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rVintageSnare = genT6 "T600 - 076 Vintage Snare Room" 0.1


-- | Reverb for preset: T600 - 077 Slow Snare.
rSlowSnare :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rSlowSnare = genT6 "T600 - 077 Slow Snare" 0.1


-- | Reverb for preset: T600 - 078 Straight Tail Perc.
rStraightTailPerc :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStraightTailPerc = genT6 "T600 - 078 Straight Tail Perc" 0.1


-- | Reverb for preset: T600 - 079 Fat Drum Plate.
pFatDrum :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pFatDrum = genT6 "T600 - 079 Fat Drum Plate" 0.1


-- | Reverb for preset: T600 - 080 Drum Wood Plate.
pDrumWood :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pDrumWood = genT6 "T600 - 080 Drum Wood Plate" 0.1


-- | Reverb for preset: T600 - 081 Ambient Plate.
pAmbient :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pAmbient = genT6 "T600 - 081 Ambient Plate" 0.1


-- | Reverb for preset: T600 - 082 Piano Plate.
pPiano :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pPiano = genT6 "T600 - 082 Piano Plate" 0.1


-- | Reverb for preset: T600 - 083 Stairway Plate.
pStairway :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pStairway = genT6 "T600 - 083 Stairway Plate" 0.1


-- | Reverb for preset: T600 - 084 Slapback Plate.
pSlapback :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSlapback = genT6 "T600 - 084 Slapback Plate" 0.1


-- | Reverb for preset: T600 - 085 Bright Plate.
pBright :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pBright = genT6 "T600 - 085 Bright Plate" 0.1


-- | Reverb for preset: T600 - 086 Silky Gold Plate.
pSilkyGold :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSilkyGold = genT6 "T600 - 086 Silky Gold Plate" 0.1


-- | Reverb for preset: T600 - 087 Soft Drum Perc.
pSoftDrumPerc :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSoftDrumPerc = genT6 "T600 - 087 Soft Drum Perc" 0.1


-- | Reverb for preset: T600 - 088 Out in the Dark.
sOutInTheDark :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sOutInTheDark = genT6 "T600 - 088 Out in the Dark" 0.1


-- | Reverb for preset: T600 - 089 Steel Drum.
sSteelDrum :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sSteelDrum = genT6 "T600 - 089 Steel Drum" 0.1

