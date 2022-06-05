{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.Pashua.Parser
  ( Err(..)
  , IR.IParser(..)
  , R.Reader
  , R.decimal
  , R.rational
  , R.signed
  , R.hexadecimal
  , R.double
  , takeAll
  , matchText
  , bool
  , eos
  , parseText
  , parseFloat
  , parseBool
  , parseInt
  , assoc
  , parse
  ) where

import           Data.Bifunctor          (first)
import           Data.List               (find)
import           Data.Text
    ( Text
    , drop
    , head
    , isPrefixOf
    , length
    , tail
    )
import           Data.Text.Internal.Read as IR
import           Data.Text.Read          as R
import           Graphics.UI.Pashua      (Result (..))
import           Prelude                 hiding (drop, head, length, tail)

data Err a = FormCancelled | ParseError a String deriving Show

type Parser a = IR.IParser Text a

parse :: Eq a => Parser b -> a -> [(a, Text)] -> Either (Err a) (b, Text)
parse p key aList =
  case assoc key aList of
    Nothing -> Left FormCancelled
    Just s  -> first (ParseError key) $ runP p s

matchText :: Text -> R.Reader Text
matchText prefix s =
  if prefix `isPrefixOf` s
  then Right (prefix, drop (length prefix) s)
  else Left $ "Failed to match " <> show prefix

takeAll :: R.Reader Text
takeAll s = Right (s, "")

bool :: R.Reader Bool
bool s =
  if s == ""
  then Left "Empty string"
  else
    let (h, t) = (head s, tail s)
    in case h of
         '0' -> Right (False, t)
         '1' -> Right (True, t)
         _   -> Left "Failed to parse boolean"

eos :: R.Reader ()
eos s = if s == "" then Right ((), "") else Left "End-of-string expected"

parseText :: Eq a => a -> Result a -> Either (Err a) Text
parseText key aList = fst <$> parse (P takeAll) key aList

parseFloat :: Eq a => a -> Result a -> Either (Err a) Float
parseFloat key aList =
  let p = IR.P R.rational <* IR.P eos
  in fromRational . fst <$> parse p key aList

parseBool :: Eq a => a -> Result a -> Either (Err a) Bool
parseBool key aList =
  let p = IR.P bool <* IR.P eos
  in fst <$> parse p key aList

parseInt :: Eq a => a -> Result a -> Either (Err a) Int
parseInt key aList =
  let p = IR.P R.decimal <* IR.P eos
  in fromIntegral . fst <$> parse p key aList

assoc :: Eq a => a -> [(a, b)] -> Maybe b
assoc key xs = snd <$> find ((== key) . fst) xs
