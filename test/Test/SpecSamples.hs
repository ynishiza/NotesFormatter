{-# LANGUAGE QuasiQuotes #-}

module Test.SpecSamples (
  spec,
  rtfFilePaths,
  rtfdFilePaths,
) where

import Control.Monad
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Haskell.TH
import Notes.File.RTF
import Notes.Process
import Notes.RTFDoc
import Test.Hspec hiding (runIO)
import Test.Utils
import Notes.RTFFile

expectedRTFFiles :: Int
expectedRTFFiles = 41

expectedRTFDFiles :: Int
expectedRTFDFiles = 17

rtfFilePaths :: [FilePath]
rtfFilePaths =
  $( do
      contents <-
        runIO $ listFilesRecursive (\v _ -> isRTF v) (not . isRTFD) $ dataPath </> "database"
      [|contents|]
   )

rtfdFilePaths :: [FilePath]
rtfdFilePaths =
  $( do
      contents <-
        runIO $ listFilesRecursive (\v _ -> isRTFD v) (const True) $ dataPath </> "database"
      [|contents|]
   )

spec :: SpecWith ()
spec =
  describe "Samples" $ do
    let testWith :: IO Text -> IO ()
        testWith readContent = do
          src <- readContent
          parsed <- expectToRTFDocSuccess @RTFDoc src
          let srcNormalized = normalizeRTFForTest src
              rendered = render parsed
              renderedNormalized = normalizeRTFForTest rendered

          when (renderedNormalized /= srcNormalized) $ do
            T.writeFile "file_src.rtf" src
            T.writeFile "file_rendered.rtf" rendered
            T.writeFile "file_src_normalized.rtf" srcNormalized
            T.writeFile "file_rendered_normalized.rtf" renderedNormalized
          renderedNormalized `shouldBe` srcNormalized
          T.length renderedNormalized `shouldBe` T.length srcNormalized

    describe "RTF" $ do
      let
        testRTFSampleFile :: FilePath -> Spec
        testRTFSampleFile path = it ("RTF sample: " <> path) $ testWith (readRTFFile $ rtfFile path)
      it "Tests RTF samples" $ do
        length rtfFilePaths `shouldBe` expectedRTFFiles

      foldr (\x r -> r >> testRTFSampleFile x) (pure ()) rtfFilePaths

    describe "RTFD" $ do
      let
        testRTFDSampleFile :: FilePath -> Spec
        testRTFDSampleFile path = it ("RTFD sample: " <> path) $ testWith (readRTFFile $ rtfdFile path)

      it "Tests RTFD samples" $ do
        length rtfdFilePaths `shouldBe` expectedRTFDFiles

      foldr (\x r -> r >> testRTFDSampleFile x) (pure ()) rtfdFilePaths
