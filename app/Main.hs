{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Monad             (forM_)
import           Data.Functor              ((<&>))
import qualified Data.List.NonEmpty        as NL
import           Data.Maybe                (fromJust)
import           Data.Text                 as T
import           Graphics.UI.Pashua
import           Graphics.UI.Pashua.Parser

data SomeID = Radio | TxtField deriving (Show, Eq)

data Food = Sushi | Tofu deriving (Show, Eq, Enum, Read)

main = do
  let
    w = window { title = Just "One Weird Trick"
               , transparency = Just 0.9
               }
    f = mkForm ( Just w) $
        [ radioButton Radio (mkOptionListFromEnum (Nothing :: Maybe Food))
        , (textField TxtField) { label_ = Just "Enter a number" } ]
    parseFood :: SomeID -> Result SomeID -> Either (Err SomeID) Food
    parseFood x y = mkEnumParser ("Invalid food: " <>) x y
  case f of
    Just f' -> do
      result <- runPashua f'
      print result
      print $ parseFood Radio result
      print $ parseInt TxtField result
      simpleMessage "Title" "A text box"
    _ -> putStrLn "Unable to make Form"
