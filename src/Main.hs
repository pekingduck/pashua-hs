{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Monad      (forM_)
import           Data.Functor       ((<&>))
import qualified Data.List.NonEmpty as NL
import           Data.Maybe         (fromJust)
import           Data.Text          as T
import           Data.Text.IO       as TIO
import           Pashua
import qualified Turtle             as TT

data SomeID =
  Passwd | Txt | Pop | OkButton | OtherButton |
  Combo | Gaga | Browser | Save | Cal
  deriving Show

runPashua :: TT.MonadIO m => [Text] -> m [(Text, Text)]
runPashua xs = do
  let split l =
          let (k, v) = breakOn "=" $ TT.lineToText l
          in (k, if T.null v then "" else T.tail v)
      -- inproc expects trailing "\n" in lines fed to stdin
      pashuaOut = TT.inproc "/Applications/Pashua.app/Contents/MacOS/Pashua" ["-"]
                  $ TT.toLines (TT.select (xs <&> \x -> x <> "\n"))
      f = TT.Fold (\x a->split a : x) [] id
  TT.reduce f pashuaOut
--    TT.liftIO $ TIO.putStrLn $ k <> "-->" <> v'


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
    w = defaultWindow { title = Just "One Weird Trick"
                      , transparency = Just 0.5
                      }
  TIO.putStrLn $ T.intercalate "\n" $ runForm $ Form (Just w) b
  let xs = runForm $ Form (Just w) b
  l <- runPashua xs
  print l
