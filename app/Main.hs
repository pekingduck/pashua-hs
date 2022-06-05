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

--import qualified Turtle             as TT

data SomeID =
  Passwd | Txt | Pop | OkButton | OtherButton |
  Combo | Radio | Browser | Save | Cal |
  TxtField | TxtBox | Img
  deriving (Show, Eq)

data Food = Sushi | Tofu deriving (Show, Eq)

food :: Reader Food
food str =
  case matchText "Sushi" str <> matchText "Tofu" str <> Left "" of
    Right ("Sushi", b) -> Right (Sushi, b)
    Right ("Tofu", b)  -> Right (Tofu, b)
    _                  -> Left "Invalid Food"
  -- if | T.isPrefixOf "Sushi" s -> go Sushi "Sushi" s
  --    | T.isPrefixOf "Tofu" s  -> go Tofu "Tofu" s
  --    | otherwise              -> Left "Invalid food"
  -- where
  --   go a pre s = Right (a, T.drop (T.length pre) s)

parseFood key result = fst <$> parse (P food <* P eos) key result

main :: IO ()
main = do
  let
    l :: ListWithDefault
    l = fromJust (mkListWithDefault (Just Sushi) (Sushi NL.:| [Tofu]))
    imageWidth = fromJust $ mkPixel 250
    b :: [Widget SomeID]
    b = [ radioButton Radio l
    --  button OtherButton "Don't Click!"
    --     , (comboBox Combo ("Two" NL.:| [ "Worlds", "Collide" ]))
    --       { completion = Just CaseSensitive }

    --     , (openBrowser Browser)
    --       { fileType = Just (Extensions ("jpg" NL.:| ["dhall"])) }
    --     , (saveBrowser Save)
    --       { fileExtension = Just "cabal" }
    --     , popup Pop l
    --     , (defaultButton OkButton) { label_ = Just "Subscribe" }
        , (textField TxtField) { label_ = Just "Enter a number" }
    --     , (password Passwd) { label_ = Just "Tell me your password" }
    --     , (date Cal) { choice = Just DateOnly
    --                         , default_ = Just "1997-07-01"
    --                         , style = Just Textual }
    --     , (text_ Txt "OK\nOK") { tooltip = Just "Tip of the iceberg"
    --                            , relY = mkRelY (negate 20) }
    --     , (textBox TxtBox) { default_ = Just "Hmm\nmumm"
    --                               , disabled = Just False
    --                               , fontType = Just Fixed }
    --     , (image Img "test.jpg") { dimensions = Just (Width imageWidth Nothing)
    --                              , relY = mkRelY 300 }
        ]
    w = window { title = Just "One Weird Trick"
                      , transparency = Just 0.9
                      }
    f = Form (Just w) b
  result <- runPashua f
  print result
  print $ parseFood Radio result
  print $ parseInt TxtField result
