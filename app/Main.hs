{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as B
import System.IO (FilePath)
import Data.Aeson (decode, FromJSON)
import GHC.Generics
import qualified System.Random as R
import Data.Set (Set)
import qualified Data.Set as S

import qualified Debug.Trace as DT

newtype Name = Name Text
  deriving (Eq, Ord, Show, Read, Generic)

getName :: Name -> Text
getName (Name name) = name

data Person = Person {
  name :: Name,
  seedChoice :: Integer,
  slackUsername :: Text
} deriving (Eq, Ord, Show, Read, Generic)

data Config = Config {
  people :: [Person]
} deriving (Eq, Show, Read, Generic)

instance FromJSON Name
instance FromJSON Person
instance FromJSON Config

readPeople :: FilePath -> IO (Maybe Config)
readPeople fileName = decode <$> B.readFile fileName

randomPairs :: R.StdGen -> Int -> Set Person -> [Person] -> [(Set Person, (Person, Person))]
randomPairs rndGen pplLength seen people = 
    if first /= second && (S.notMember first seen) && (S.notMember second seen)
      then (newSeen, (first, second)):(randomPairs nextGen' pplLength newSeen people)
      else randomPairs nextGen' pplLength seen people
  where (idx, nextGen)   = R.next rndGen
        (idx', nextGen') = R.next nextGen
        getItem          = \i -> people !! (i `mod` pplLength)
        first            = getItem idx
        second           = getItem idx'
        newSeen          = S.insert first $ S.insert second $ seen

genPairs :: R.StdGen -> [Person] -> [(Person, Person)]
genPairs rndGen people = map snd $ randomPairs rndGen (length people) S.empty people

formatPairs :: [(Person, Person)] -> Text
formatPairs = foldl (\acc (first, second) -> 
    T.concat [acc,  (format first),  " <---> ",  (format second),  "\n"]) ""
  where format = \person -> T.concat [slackUsername person, " (", getName $ name person, ")"]

main :: IO ()
main = do
  parsedPeople <- readPeople "people.json"
  case parsedPeople of
    Nothing -> fail "wrong filename format?"
    Just config -> do
      let seeds = map (seedChoice) ppl
          seed = fromIntegral $ foldl (\acc s -> acc + s `mod` 12345123) 0 seeds
          stdGen = R.mkStdGen seed
          ppl = people config

      putStrLn $ "We have " ++ (show $ length ppl) ++ " people playing"
      if length ppl `mod` 2 /= 0
        then fail "we need the number of people divisible by two"
        else putStrLn . T.unpack . formatPairs $ take (length ppl `quot` 2) (genPairs stdGen ppl)


