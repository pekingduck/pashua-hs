{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Monad      (forM_)
import qualified Data.List.NonEmpty as NL
import           Data.Maybe         (fromJust)
import           Data.Text          as T
import           Data.Text.IO       as TIO
import           Pashua

data SomeID =
  Passwd | Txt | Pop | OkButton | OtherButton |
  Combo | Gaga | Browser | Save | Cal
  deriving Show

main :: IO ()
main = do
  let
    l :: ListWithDefault
    l = fromJust (mkListWithDefault (Just "Gaga") ("Radio" NL.:| ["Gaga"]))
    b :: [Widget SomeID]
    b = [ (defaultButton OtherButton "Don't Click!")
        , (defaultCombobox Combo ("Two" NL.:| [ "Worlds", "Collide" ]))
          { completion = Just CaseSensitive }
        , (defaultRadioButton Gaga l)
        , (defaultOpenBrowser Browser)
          { fileType = Just (Extensions ("jpg" NL.:| ["dhall"])) }
        , (defaultSaveBrowser Save)
          { fileExtension = Just "cabal" }
        , defaultPopup Pop l
        , (defaultDefaultButton OkButton) { label_ = Just "Subscribe" }
        , (defaultTextField Txt) { default_ = Just "Black Friday" }
        , (defaultPassword Passwd) { label_ = Just "Tell me your password" }
        , (defaultDate Cal) { choice = Just DateOnly
                            , default_ = Just "1997-07-01"
                            , style = Just Textual }
        ]
    w = defaultWindow { title = Just "One Weird Trick" } -- { appearance = Just Metal }
  --  forM_ b (print . serialize)
  TIO.putStrLn $ T.intercalate "\n" $ runForm $ Form (Just w) b
