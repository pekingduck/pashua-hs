{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
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

readFood :: Reader Food
readFood =
  \case
    "Sushi" -> Right (Sushi, "")
    "Tofu"  -> Right (Tofu, "")
    s       -> Left "Invalid food"

parseFood key result = fst <$> parse readFood key result

main :: IO ()
main = do
  let
    l :: ListWithDefault
    l = fromJust (mkListWithDefault (Just Sushi) (Sushi NL.:| [Tofu]))
    imageWidth = fromJust $ mkPixel 250
    b :: [Widget SomeID]
    b = [ button OtherButton "Don't Click!"
        , (comboBox Combo ("Two" NL.:| [ "Worlds", "Collide" ]))
          { completion = Just CaseSensitive }
        , radioButton Radio l
        , (openBrowser Browser)
          { fileType = Just (Extensions ("jpg" NL.:| ["dhall"])) }
        , (saveBrowser Save)
          { fileExtension = Just "cabal" }
        , popup Pop l
        , (defaultButton OkButton) { label_ = Just "Subscribe" }
        , (textField TxtField) { default_ = Just "Black Friday" }
        , (password Passwd) { label_ = Just "Tell me your password" }
        , (date Cal) { choice = Just DateOnly
                            , default_ = Just "1997-07-01"
                            , style = Just Textual }
        , (text_ Txt "OK\nOK") { tooltip = Just "Tip of the iceberg"
                               , relY = mkRelY (negate 20) }
        , (textBox TxtBox) { default_ = Just "Hmm\nmumm"
                                  , disabled = Just False
                                  , fontType = Just Fixed }
        , (image Img "test.jpg") { dimensions = Just (Width imageWidth Nothing)
                                 , relY = mkRelY 300 }
        ]
    w = window { title = Just "One Weird Trick"
                      , transparency = Just 0.9
                      }
    f = Form (Just w) b
  result <- runPashua f
  print result
  print $ parseFood Radio result
