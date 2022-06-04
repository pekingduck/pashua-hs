{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.Pashua.Parser
  ( Err(..)
  , parseText
  , parseFloat
  , parseBool
  , parseInt
  , assoc
  , parse
  , R.Reader
  ) where

import           Data.Bifunctor     (first)
import           Data.List          (find)
import           Data.Text          (Text, head, tail)
import           Data.Text.Read     as R
import           Graphics.UI.Pashua (Result (..))
import           Prelude            hiding (head, tail)

data Err a = FormCancelled | ParseError a String deriving Show

parse :: Eq a => R.Reader b -> a -> [(a, Text)] -> Either (Err a) (b, Text)
parse reader key aList =
  case assoc key aList of
    Nothing -> Left FormCancelled
    Just s  -> first (ParseError key) $ reader s

parseText :: Eq a => a -> Result a -> Either (Err a) Text
parseText key aList = fst <$> parse (\s -> Right (s, "")) key aList

parseFloat :: Eq a => a -> Result a -> Either (Err a) Float
parseFloat key aList = fromRational . fst <$> parse R.rational key aList

parseBool :: Eq a => a -> Result a -> Either (Err a) Bool
parseBool key aList = fst <$> parse readBool key aList
  where
    readBool :: R.Reader Bool
    readBool s =
      if s == ""
      then Left "Empty string"
      else
        let (h, t) = (head s, tail s)
        in case h of
          '0' -> Right (False, t)
          '1' -> Right (True, t)
          _   -> Left "Failed to parse boolean"

parseInt :: Eq a => a -> Result a -> Either (Err a) Int
parseInt key aList = fromIntegral . fst <$> parse R.decimal key aList

assoc :: Eq a => a -> [(a, b)] -> Maybe b
assoc key xs = snd <$> find ((== key) . fst) xs
