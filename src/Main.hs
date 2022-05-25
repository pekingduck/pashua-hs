{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Monad      (forM_)
import           Data.Functor       ((<&>))
import qualified Data.List.NonEmpty as NL
import           Data.Maybe         (fromJust)
import           Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Pashua
import           System.IO          (hClose)
import           System.Process
    ( CreateProcess (..)
    , StdStream (..)
    , createProcess
    , proc
    )
--import qualified Turtle             as TT

data SomeID =
  Passwd | Txt | Pop | OkButton | OtherButton |
  Combo | Gaga | Browser | Save | Cal
  deriving (Show, Eq)

-- runPashuaTurtle :: TT.MonadIO m => [Text] -> m [Text]
-- runPashuaTurtle xs = do
--   -- inproc expects trailing "\n" in lines fed to stdin
--   let pashuaOut = TT.inproc "/Applications/Pashua.app/Contents/MacOS/Pashua"
--                   ["-"] $ TT.toLines $ TT.select $ xs <&> \x -> x <> "\n"
--       f = TT.Fold (\x a -> TT.lineToText a : x) [] id
--   TT.reduce f pashuaOut

pashuaExec :: String
pashuaExec = "/Applications/Pashua.app/Contents/MacOS/Pashua"

runPashuaStock :: [Text] -> IO [Text]
runPashuaStock xs = do
  createProcess (proc pashuaExec ["-"])
    { std_in = CreatePipe, std_out = CreatePipe }>>= \case
    (Just stdin', Just stdout', _, _) -> do
      forM_ xs $ TIO.hPutStrLn stdin'
      hClose stdin'
      T.lines <$> TIO.hGetContents stdout'
    _ -> error "Can't create Pashua process"

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
        , (defaultTextField Txt) { default_ = Just "Black Friday" }
        , (defaultPassword Passwd) { label_ = Just "Tell me your password" }
        , (defaultDate Cal) { choice = Just DateOnly
                            , default_ = Just "1997-07-01"
                            , style = Just Textual }
        ]
    w = defaultWindow { title = Just "One Weird Trick"
                      , transparency = Just 0.9
                      }
    f = Form (Just w) b
  --TIO.putStrLn $ T.intercalate "\n" $ runForm f
  let xs = runForm f
--  rs <- runPashuaTurtle xs
  rs <- runPashuaStock xs
  print $ parseResult f rs
