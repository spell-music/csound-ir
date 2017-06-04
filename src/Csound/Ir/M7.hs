-- | Searches for IR-files in the user's home directory  in the directory
-- ".csound-ir/Samplicity M7 Main - 01 - Wave, 32 bit, 44.1 Khz, v1.1"
module Csound.Ir.M7(
    -- * Halls
      hLarge , hMedium , hSmall , hlNear , hmNear , hsNear , hDark
    , hlDeep , hmDeep , hConcert , hGold , hSandors , hDense , hClear
    , hBrass , hAmsterdam , hBerliner , hBostonA , hBostonB , hChicago
    , hVienna , hWorcester , hDuke , hTroy , hSylvain , hMechanics , hGerold
    -- * Plates
    , pBright , pDark , pLondon , pSnareA , pSnareB , pVocal , pOld , pRich
    , pGold , pDense , pSilver , pPercussion , pEcho , pcdA , pcdB , pLarge
    , pSmall , pFat , pCrystal , pSunA , pSunB , pSunC , pVocalB
    -- * Rooms
    , rStudioA, rStudioClose , rStudioFar , rStudioC , rStudioD , rStudioE , rDeepStone
    , rMusic , rHeavy , rlWooden , rsWooden , rlTiled , rmTiled , rsTiled
    , rDrumChamber , rDjango , rsVox , rGlass , rPercussion , rMarble , rLarge
    , rSmall , rLargeRed , rRed , rBlue , rLargeB , rSmallB , rFront , rCenter
    , rBack , rStudioK , rWaits , rCorn , rOakland , rPerf
    -- * Chambers
    , cLarge , cMedium
    , cSmall , clDark , csDark , clBright , csBright , cKick , cSnare , cVocal
    , cAM , cCD , cOld , cDeep , cAmbA , cAmbB , cSunset , clAmbience , cmAmbience
    , csAmbience
    -- * Ambiences
    , alDark , amDark , asDark , alBright , amBright , asBright , aDeep
    , aLong , aClear , aHeavy , aBassHuge , aPercussion

    -- * Spaces
    , sNorthChurch , sEastChurch
    , sSouthChurch , sWestChurch , sCinema , sScoringStage , sBathHouse , sCarPark
    , sArena , sRedwoodValley , sTanglewood , sAcademyYard , sHillside , sCavern , sStoneQuarry
    , sEuropa , sGatedSpace
) where

import System.Directory
import System.FilePath
import System.IO.Unsafe

import Csound.Base

rootDir = ".csound-ir" </> "Samplicity M7 Main - 01 - Wave, 32 bit, 44.1 Khz, v1.1"

getDirs :: String -> (String, String)
getDirs dir = (fileL, fileR)
    where
        home = unsafePerformIO getHomeDirectory
        fileL = fileBy mkLeft
        fileR = fileBy mkRight

        fileBy mk = home </> rootDir </> dir </> mk dir
        mkLeft = mkBy "L"
        mkRight = mkBy "R"
        mkBy suf x = drop 5 x ++ " " ++ suf ++ ".wav"

genM7 :: MixAt Sig2 Sig2 a => String -> Sig -> Sig -> a -> AtOut Sig2 Sig2 a
genM7 file k ratio ain = mixAt ratio (mul k . stereoIR2 dirs) ain
    where dirs = getDirs file

-- | Reverb for preset: M7 - 1 Halls 01 Large Hall.
hLarge :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hLarge = genM7 "M7 - 1 Halls 01 Large Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 02 Medium Hall.
hMedium :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hMedium = genM7 "M7 - 1 Halls 02 Medium Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 03 Small Hall.
hSmall :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hSmall = genM7 "M7 - 1 Halls 03 Small Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 04 Large & Near.
hlNear :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hlNear = genM7 "M7 - 1 Halls 04 Large & Near" 0.05


-- | Reverb for preset: M7 - 1 Halls 05 Medium & Near.
hmNear :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hmNear = genM7 "M7 - 1 Halls 05 Medium & Near" 0.05


-- | Reverb for preset: M7 - 1 Halls 06 Small & Near.
hsNear :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hsNear = genM7 "M7 - 1 Halls 06 Small & Near" 0.05


-- | Reverb for preset: M7 - 1 Halls 07 Large & Dark.
hDark :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hDark = genM7 "M7 - 1 Halls 07 Large & Dark" 0.05


-- | Reverb for preset: M7 - 1 Halls 08 Large & Deep.
hlDeep :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hlDeep = genM7 "M7 - 1 Halls 08 Large & Deep" 0.05


-- | Reverb for preset: M7 - 1 Halls 09 Medium & Deep.
hmDeep :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hmDeep = genM7 "M7 - 1 Halls 09 Medium & Deep" 0.05


-- | Reverb for preset: M7 - 1 Halls 10 Concert Hall.
hConcert :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hConcert = genM7 "M7 - 1 Halls 10 Concert Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 11 Gold Hall.
hGold :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hGold = genM7 "M7 - 1 Halls 11 Gold Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 12 Sandors Hall.
hSandors :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hSandors = genM7 "M7 - 1 Halls 12 Sandors Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 13 Dense Hall.
hDense :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hDense = genM7 "M7 - 1 Halls 13 Dense Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 14 Clear Hall.
hClear :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hClear = genM7 "M7 - 1 Halls 14 Clear Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 15 Brass Hall.
hBrass :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hBrass = genM7 "M7 - 1 Halls 15 Brass Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 16 Amsterdam Hall.
hAmsterdam :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hAmsterdam = genM7 "M7 - 1 Halls 16 Amsterdam Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 17 Berliner Hall.
hBerliner :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hBerliner = genM7 "M7 - 1 Halls 17 Berliner Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 18 Boston Hall A.
hBostonA :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hBostonA = genM7 "M7 - 1 Halls 18 Boston Hall A" 0.05


-- | Reverb for preset: M7 - 1 Halls 19 Boston Hall B.
hBostonB :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hBostonB = genM7 "M7 - 1 Halls 19 Boston Hall B" 0.05


-- | Reverb for preset: M7 - 1 Halls 20 Chicago Hall.
hChicago :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hChicago = genM7 "M7 - 1 Halls 20 Chicago Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 21 Vienna Hall.
hVienna :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hVienna = genM7 "M7 - 1 Halls 21 Vienna Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 22 Worcester Hall.
hWorcester :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hWorcester = genM7 "M7 - 1 Halls 22 Worcester Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 23 The ArchDuke.
hDuke :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hDuke = genM7 "M7 - 1 Halls 23 The ArchDuke" 0.05


-- | Reverb for preset: M7 - 1 Halls 24 Troy Music Hall.
hTroy :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hTroy = genM7 "M7 - 1 Halls 24 Troy Music Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 25 Saint Sylvain.
hSylvain :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hSylvain = genM7 "M7 - 1 Halls 25 Saint Sylvain" 0.05


-- | Reverb for preset: M7 - 1 Halls 26 Mechanics Hall.
hMechanics :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hMechanics = genM7 "M7 - 1 Halls 26 Mechanics Hall" 0.05


-- | Reverb for preset: M7 - 1 Halls 27 Saint Gerold.
hGerold :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
hGerold = genM7 "M7 - 1 Halls 27 Saint Gerold" 0.05


-- | Reverb for preset: M7 - 2 Plates 01 Bright Plate.
pBright :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pBright = genM7 "M7 - 2 Plates 01 Bright Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 02 Dark Plate.
pDark :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pDark = genM7 "M7 - 2 Plates 02 Dark Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 03 London Plate.
pLondon :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pLondon = genM7 "M7 - 2 Plates 03 London Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 04 Snare Plate A.
pSnareA :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSnareA = genM7 "M7 - 2 Plates 04 Snare Plate A" 0.05


-- | Reverb for preset: M7 - 2 Plates 05 Snare Plate B.
pSnareB :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSnareB = genM7 "M7 - 2 Plates 05 Snare Plate B" 0.05


-- | Reverb for preset: M7 - 2 Plates 06 Vocal Plate.
pVocal :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pVocal = genM7 "M7 - 2 Plates 06 Vocal Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 07 Old Plate.
pOld :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pOld = genM7 "M7 - 2 Plates 07 Old Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 08 Rich Plate.
pRich :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pRich = genM7 "M7 - 2 Plates 08 Rich Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 09 Gold Plate.
pGold :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pGold = genM7 "M7 - 2 Plates 09 Gold Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 10 Dense Plate.
pDense :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pDense = genM7 "M7 - 2 Plates 10 Dense Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 11 Silver Plate.
pSilver :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSilver = genM7 "M7 - 2 Plates 11 Silver Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 12 Percussion Plate.
pPercussion :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pPercussion = genM7 "M7 - 2 Plates 12 Percussion Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 13 Echo Plate.
pEcho :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pEcho = genM7 "M7 - 2 Plates 13 Echo Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 14 CD Plate A.
pcdA :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pcdA = genM7 "M7 - 2 Plates 14 CD Plate A" 0.05


-- | Reverb for preset: M7 - 2 Plates 15 CD Plate B.
pcdB :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pcdB = genM7 "M7 - 2 Plates 15 CD Plate B" 0.05


-- | Reverb for preset: M7 - 2 Plates 16 Large Plate.
pLarge :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pLarge = genM7 "M7 - 2 Plates 16 Large Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 17 Small Plate.
pSmall :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSmall = genM7 "M7 - 2 Plates 17 Small Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 18 Fat Plate.
pFat :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pFat = genM7 "M7 - 2 Plates 18 Fat Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 19 Crystal Plate.
pCrystal :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pCrystal = genM7 "M7 - 2 Plates 19 Crystal Plate" 0.05


-- | Reverb for preset: M7 - 2 Plates 20 Sun Plate A.
pSunA :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSunA = genM7 "M7 - 2 Plates 20 Sun Plate A" 0.05


-- | Reverb for preset: M7 - 2 Plates 21 Sun Plate B.
pSunB :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSunB = genM7 "M7 - 2 Plates 21 Sun Plate B" 0.05


-- | Reverb for preset: M7 - 2 Plates 22 Sun Plate C.
pSunC :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pSunC = genM7 "M7 - 2 Plates 22 Sun Plate C" 0.05


-- | Reverb for preset: M7 - 2 Plates 23 Vocal Plate B.
pVocalB :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
pVocalB = genM7 "M7 - 2 Plates 23 Vocal Plate B" 0.05


-- | Reverb for preset: M7 - 3 Rooms 01 Studio A.
rStudioA :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudioA = genM7 "M7 - 3 Rooms 01 Studio A" 0.05


-- | Reverb for preset: M7 - 3 Rooms 02 Studio B Close.
rStudioClose :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudioClose = genM7 "M7 - 3 Rooms 02 Studio B Close" 0.05


-- | Reverb for preset: M7 - 3 Rooms 03 Studio B Far.
rStudioFar :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudioFar = genM7 "M7 - 3 Rooms 03 Studio B Far" 0.05


-- | Reverb for preset: M7 - 3 Rooms 04 Studio C.
rStudioC :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudioC = genM7 "M7 - 3 Rooms 04 Studio C" 0.05


-- | Reverb for preset: M7 - 3 Rooms 05 Studio D.
rStudioD :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudioD = genM7 "M7 - 3 Rooms 05 Studio D" 0.05


-- | Reverb for preset: M7 - 3 Rooms 06 Studio E.
rStudioE :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudioE = genM7 "M7 - 3 Rooms 06 Studio E" 0.05


-- | Reverb for preset: M7 - 3 Rooms 07 Deep Stone.
rDeepStone :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rDeepStone = genM7 "M7 - 3 Rooms 07 Deep Stone" 0.05


-- | Reverb for preset: M7 - 3 Rooms 08 Music Room.
rMusic :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rMusic = genM7 "M7 - 3 Rooms 08 Music Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 09 Heavy Room.
rHeavy :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rHeavy = genM7 "M7 - 3 Rooms 09 Heavy Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 10 Large Wooden Room.
rlWooden :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rlWooden = genM7 "M7 - 3 Rooms 10 Large Wooden Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 11 Small Wooden Room.
rsWooden :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rsWooden = genM7 "M7 - 3 Rooms 11 Small Wooden Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 12 Large Tiled Room.
rlTiled :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rlTiled = genM7 "M7 - 3 Rooms 12 Large Tiled Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 13 Medium Tiled Room.
rmTiled :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rmTiled = genM7 "M7 - 3 Rooms 13 Medium Tiled Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 14 Small Tiled Room.
rsTiled :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rsTiled = genM7 "M7 - 3 Rooms 14 Small Tiled Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 15 Drum & Chamber.
rDrumChamber :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rDrumChamber = genM7 "M7 - 3 Rooms 15 Drum & Chamber" 0.05


-- | Reverb for preset: M7 - 3 Rooms 16 Djangos Room.
rDjango :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rDjango = genM7 "M7 - 3 Rooms 16 Djangos Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 17 Small Vox Room.
rsVox :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rsVox = genM7 "M7 - 3 Rooms 17 Small Vox Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 18 Glass Room.
rGlass :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rGlass = genM7 "M7 - 3 Rooms 18 Glass Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 19 Percussion Room.
rPercussion :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rPercussion = genM7 "M7 - 3 Rooms 19 Percussion Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 20 Marble Foyer.
rMarble :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rMarble = genM7 "M7 - 3 Rooms 20 Marble Foyer" 0.05


-- | Reverb for preset: M7 - 3 Rooms 21 Large & Room.
rLarge :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rLarge = genM7 "M7 - 3 Rooms 21 Large & Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 22 Small & Room.
rSmall :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rSmall = genM7 "M7 - 3 Rooms 22 Small & Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 23 Large Red Room.
rLargeRed :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rLargeRed = genM7 "M7 - 3 Rooms 23 Large Red Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 24 Red Room.
rRed :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rRed = genM7 "M7 - 3 Rooms 24 Red Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 25 Blue Room.
rBlue :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rBlue = genM7 "M7 - 3 Rooms 25 Blue Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 26 Large Room.
rLargeB :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rLargeB = genM7 "M7 - 3 Rooms 26 Large Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 27 Small Room.
rSmallB :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rSmallB = genM7 "M7 - 3 Rooms 27 Small Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 28 Front Room.
rFront :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rFront = genM7 "M7 - 3 Rooms 28 Front Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 29 Center Room.
rCenter :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rCenter = genM7 "M7 - 3 Rooms 29 Center Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 30 Back Room.
rBack :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rBack = genM7 "M7 - 3 Rooms 30 Back Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 31 Studio K.
rStudioK :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rStudioK = genM7 "M7 - 3 Rooms 31 Studio K" 0.05


-- | Reverb for preset: M7 - 3 Rooms 32 Waits Room.
rWaits :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rWaits = genM7 "M7 - 3 Rooms 32 Waits Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 33 Corn Room.
rCorn :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rCorn = genM7 "M7 - 3 Rooms 33 Corn Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 34 Oakland Room.
rOakland :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rOakland = genM7 "M7 - 3 Rooms 34 Oakland Room" 0.05


-- | Reverb for preset: M7 - 3 Rooms 35 SF Perf Room.
rPerf :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
rPerf = genM7 "M7 - 3 Rooms 35 SF Perf Room" 0.05


-- | Reverb for preset: M7 - 4 Chambers 01 Large Chamber.
cLarge :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cLarge = genM7 "M7 - 4 Chambers 01 Large Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 02 Medium Chamber.
cMedium :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cMedium = genM7 "M7 - 4 Chambers 02 Medium Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 03 Small Chamber.
cSmall :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cSmall = genM7 "M7 - 4 Chambers 03 Small Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 04 Large & Dark.
clDark :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
clDark = genM7 "M7 - 4 Chambers 04 Large & Dark" 0.05


-- | Reverb for preset: M7 - 4 Chambers 05 Small & Dark.
csDark :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
csDark = genM7 "M7 - 4 Chambers 05 Small & Dark" 0.05


-- | Reverb for preset: M7 - 4 Chambers 06 Large & Bright.
clBright :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
clBright = genM7 "M7 - 4 Chambers 06 Large & Bright" 0.05


-- | Reverb for preset: M7 - 4 Chambers 07 Small & Bright.
csBright :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
csBright = genM7 "M7 - 4 Chambers 07 Small & Bright" 0.05


-- | Reverb for preset: M7 - 4 Chambers 08 Kick Chamber.
cKick :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cKick = genM7 "M7 - 4 Chambers 08 Kick Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 09 Snare Chamber.
cSnare :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cSnare = genM7 "M7 - 4 Chambers 09 Snare Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 10 Vocal Chamber.
cVocal :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cVocal = genM7 "M7 - 4 Chambers 10 Vocal Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 11 A&M Chamber.
cAM :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cAM = genM7 "M7 - 4 Chambers 11 A&M Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 12 CD Chamber.
cCD :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cCD = genM7 "M7 - 4 Chambers 12 CD Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 13 Old Chamber.
cOld :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cOld = genM7 "M7 - 4 Chambers 13 Old Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 14 Deep Chamber.
cDeep :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cDeep = genM7 "M7 - 4 Chambers 14 Deep Chamber" 0.05


-- | Reverb for preset: M7 - 4 Chambers 15 Amb Chamber A.
cAmbA :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cAmbA = genM7 "M7 - 4 Chambers 15 Amb Chamber A" 0.05


-- | Reverb for preset: M7 - 4 Chambers 16 Amb Chamber B.
cAmbB :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cAmbB = genM7 "M7 - 4 Chambers 16 Amb Chamber B" 0.05


-- | Reverb for preset: M7 - 4 Chambers 17 Sunset Chamber.
cSunset :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cSunset = genM7 "M7 - 4 Chambers 17 Sunset Chamber" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 01 Large Ambience.
clAmbience :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
clAmbience = genM7 "M7 - 5 Ambiences 01 Large Ambience" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 02 Medium Ambience.
cmAmbience :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
cmAmbience = genM7 "M7 - 5 Ambiences 02 Medium Ambience" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 03 Small Ambience.
csAmbience :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
csAmbience = genM7 "M7 - 5 Ambiences 03 Small Ambience" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 04 Large & Dark.
alDark :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
alDark = genM7 "M7 - 5 Ambiences 04 Large & Dark" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 05 Medium & Dark.
amDark :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
amDark = genM7 "M7 - 5 Ambiences 05 Medium & Dark" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 06 Small & Dark.
asDark :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
asDark = genM7 "M7 - 5 Ambiences 06 Small & Dark" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 07 Large & Bright.
alBright :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
alBright = genM7 "M7 - 5 Ambiences 07 Large & Bright" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 08 Medium & Bright.
amBright :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
amBright = genM7 "M7 - 5 Ambiences 08 Medium & Bright" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 09 Small & Bright.
asBright :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
asBright = genM7 "M7 - 5 Ambiences 09 Small & Bright" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 10 Deep Ambience.
aDeep :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
aDeep = genM7 "M7 - 5 Ambiences 10 Deep Ambience" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 11 Long Ambience.
aLong :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
aLong = genM7 "M7 - 5 Ambiences 11 Long Ambience" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 12 Clear Ambience.
aClear :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
aClear = genM7 "M7 - 5 Ambiences 12 Clear Ambience" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 13 Heavy Ambience.
aHeavy :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
aHeavy = genM7 "M7 - 5 Ambiences 13 Heavy Ambience" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 14 Bass XXL.
aBassHuge :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
aBassHuge = genM7 "M7 - 5 Ambiences 14 Bass XXL" 0.05


-- | Reverb for preset: M7 - 5 Ambiences 15 Percussion Air.
aPercussion :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
aPercussion = genM7 "M7 - 5 Ambiences 15 Percussion Air" 0.05


-- | Reverb for preset: M7 - 6 Spaces 01 North Church.
sNorthChurch :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sNorthChurch = genM7 "M7 - 6 Spaces 01 North Church" 0.05


-- | Reverb for preset: M7 - 6 Spaces 02 East Church.
sEastChurch :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sEastChurch = genM7 "M7 - 6 Spaces 02 East Church" 0.05


-- | Reverb for preset: M7 - 6 Spaces 03 South Church.
sSouthChurch :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sSouthChurch = genM7 "M7 - 6 Spaces 03 South Church" 0.05


-- | Reverb for preset: M7 - 6 Spaces 04 West Church.
sWestChurch :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sWestChurch = genM7 "M7 - 6 Spaces 04 West Church" 0.05


-- | Reverb for preset: M7 - 6 Spaces 05 Cinema Room.
sCinema :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sCinema = genM7 "M7 - 6 Spaces 05 Cinema Room" 0.05


-- | Reverb for preset: M7 - 6 Spaces 06 Scoring Stage.
sScoringStage :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sScoringStage = genM7 "M7 - 6 Spaces 06 Scoring Stage" 0.05


-- | Reverb for preset: M7 - 6 Spaces 07 Bath House.
sBathHouse :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sBathHouse = genM7 "M7 - 6 Spaces 07 Bath House" 0.05


-- | Reverb for preset: M7 - 6 Spaces 08 Car Park.
sCarPark :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sCarPark = genM7 "M7 - 6 Spaces 08 Car Park" 0.05


-- | Reverb for preset: M7 - 6 Spaces 09 Arena.
sArena :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sArena = genM7 "M7 - 6 Spaces 09 Arena" 0.05


-- | Reverb for preset: M7 - 6 Spaces 10 Redwood Valley.
sRedwoodValley :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sRedwoodValley = genM7 "M7 - 6 Spaces 10 Redwood Valley" 0.05


-- | Reverb for preset: M7 - 6 Spaces 11 Tanglewood.
sTanglewood :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sTanglewood = genM7 "M7 - 6 Spaces 11 Tanglewood" 0.05


-- | Reverb for preset: M7 - 6 Spaces 12 Academy Yard.
sAcademyYard :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sAcademyYard = genM7 "M7 - 6 Spaces 12 Academy Yard" 0.05


-- | Reverb for preset: M7 - 6 Spaces 13 Hillside.
sHillside :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sHillside = genM7 "M7 - 6 Spaces 13 Hillside" 0.05


-- | Reverb for preset: M7 - 6 Spaces 14 Cavern.
sCavern :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sCavern = genM7 "M7 - 6 Spaces 14 Cavern" 0.05


-- | Reverb for preset: M7 - 6 Spaces 15 Stone Quarry.
sStoneQuarry :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sStoneQuarry = genM7 "M7 - 6 Spaces 15 Stone Quarry" 0.05


-- | Reverb for preset: M7 - 6 Spaces 16 Europa.
sEuropa :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sEuropa = genM7 "M7 - 6 Spaces 16 Europa" 0.05


-- | Reverb for preset: M7 - 6 Spaces 17 Gated Space.
sGatedSpace :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
sGatedSpace = genM7 "M7 - 6 Spaces 17 Gated Space" 0.05
