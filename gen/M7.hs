{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module M7 where

import Prelude

import Control.Applicative
import Control.Monad
import Data.List
import Data.String

import System.Directory
import System.FilePath

import Text.InterpolatedString.Perl6 (q,qc)

import Shelly hiding (FilePath)
import qualified Data.Text as T
default (T.Text)

main = do
  writeFile "res.txt" =<< (fmap unlines $ liftA2 (zipWith mkFun) funNames dirNames)

dirNames :: IO [String]
dirNames = fmap (sort . fmap takeBaseName) files

funNames :: IO [String]
funNames = fmap lines $ readFile namesFileT6

namesFileM7 = "m7-names.txt"

namesFileT6 = "t6-names.txt"

rootDir :: IsString a => a
rootDir = rootDirT6

rootDirM7 :: IsString a => a
rootDirM7 = "/home/anton/music/raga/rever-ir/Samplicity M7 Main - 01 - Wave, 32 bit, 44.1 Khz, v1.1/"

rootDirT6 :: IsString a => a
rootDirT6 = "/home/anton/music/raga/rever-ir/samplicity_t600_v1.3/Samplicity T600 - 02 - Wave, 32 bit, 48 khz/"


files :: IO [FilePath]
files = shelly $ do
  cd rootDir
  dirs <- (filterM test_d =<<) $ ls $ fromText $ fromString "."
  return $ fmap fromShFilePath dirs

fromShFilePath = T.unpack . toTextIgnore

mkFun :: String -> String -> String
mkFun name dir =
  [qc|
-- | Reverb for preset: {dir}.
{name} :: MixAt Sig2 Sig2 a => Sig -> a -> AtOut Sig2 Sig2 a
{name} = genT6 "{dir}" 0.1
|]

exportNames = fmap (unwords . intersperse ",") funNames