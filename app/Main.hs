{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Monad      (forM_)
import           Data.Functor       ((<&>))
import qualified Data.List.NonEmpty as NL
import           Data.Maybe         (fromJust)
import           Data.Text          as T
import           Graphics.UI.Pashua
--import qualified Turtle             as TT

data SomeID =
  Passwd | Txt | Pop | OkButton | OtherButton |
  Combo | Gaga | Browser | Save | Cal |
  TxtField | TxtBox
  deriving (Show, Eq)

-- runPashuaTurtle :: TT.MonadIO m => [Text] -> m [Text]
-- runPashuaTurtle xs = do
--   -- inproc expects trailing "\n" in lines fed to stdin
--   let pashuaOut = TT.inproc "/Applications/Pashua.app/Contents/MacOS/Pashua"
--                   ["-"] $ TT.toLines $ TT.select $ xs <&> \x -> x <> "\n"
--       f = TT.Fold (\x a -> TT.lineToText a : x) [] id
--   TT.reduce f pashuaOut


main :: IO ()
main = do
  let
    l :: ListWithDefault
    l = fromJust (mkListWithDefault (Just "Gaga") ("Radio" NL.:| ["Gaga"]))
    b :: [Widget SomeID]
    b = [ defaultButton OtherButton "Don't Click!"
        , (defaultCombobox Combo ("Two" NL.:| [ "Worlds", "Collide" ]))
          { completion = Just CaseSensitive }
        , defaultRadioButton Gaga l
        , (defaultOpenBrowser Browser)
          { fileType = Just (Extensions ("jpg" NL.:| ["dhall"])) }
        , (defaultSaveBrowser Save)
          { fileExtension = Just "cabal" }
        , defaultPopup Pop l
        , (defaultDefaultButton OkButton) { label_ = Just "Subscribe" }
        , (defaultTextField TxtField) { default_ = Just "Black Friday" }
        , (defaultPassword Passwd) { label_ = Just "Tell me your password" }
        , (defaultDate Cal) { choice = Just DateOnly
                            , default_ = Just "1997-07-01"
                            , style = Just Textual }
        , (defaultText_ Txt "OK\nOK") { tooltip = Just "Tip of the iceberg" }
        , (defaultTextBox TxtBox) { default_ = Just "Hmm\nmumm"
                                  , disabled = Just False
                                  , fontType = Just Fixed }
        ]
    w = defaultWindow { title = Just "One Weird Trick"
                      , transparency = Just 0.9
                      }
    f = Form (Just w) b
  runPashua f >>= print
