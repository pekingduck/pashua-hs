{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Graphics.UI.Pashua
  ( Widget(..)
  , WindowAppearance(..)
  , Completion(..)
  , Window(..)
  , FileType(..)
  , FontSize(..)
  , FontType(..)
  , DateChoice(..)
  , DateStyle(..)
  , ImageDimensions(..)
  , Form
  , RelX
  , RelY
  , Result
  , OL.OptionList(OL.default_, OL.items)
  , mkForm
  , OL.mkOptionList
  , OL.mkOptionListFromEnum
  , button
  , window
  , cancelButton
  , checkbox
  , comboBox
  , radioButton
  , popup
  , openBrowser
  , saveBrowser
  , defaultButton
  , textField
  , password
  , date
  , image
  , text_
  , textBox
  , serialize
  , runForm
  , mkRelY
  , mkPixel
  , parseResult
  , withPashua
  , runPashua
  , simpleMessage
  , unreplaceNL
  ) where

import           Control.Monad                 (forM_, void)
import           Data.Functor                  ((<&>))
import           Data.List                     (find, nub)
import qualified Data.List.NonEmpty            as NL
import           Data.Text
    ( Text
    , breakOn
    , lines
    , null
    , replace
    , tail
    , unwords
    )
import qualified Data.Text.IO                  as TIO
import           Formatting                    (sformat, (%))
import           Formatting.Formatters         (int, stext, string)
import           Graphics.UI.Pashua.OptionList as OL
import           Prelude                       hiding
    ( lines
    , null
    , tail
    , unwords
    )
import           System.IO                     (hClose)
import           System.Process
    ( CreateProcess (..)
    , StdStream (..)
    , createProcess
    , proc
    )


type ID = Text

type Attribute = Text

type Coord = (Int, Int)

type RelX = Int

newtype Pixel = Pixel Int

instance Show Pixel where
  show (Pixel i) = show i

newtype RelY = RelY Int

instance Show RelY where
  show (RelY i) = show i

data FontSize = Regular | Small | Mini

instance Show FontSize where
  show Regular = "regular"
  show Small   = "small"
  show Mini    = "mini"

data FontType = Fixed

instance Show FontType where
  show Fixed = "fixed"

data WindowAppearance = Metal

instance Show WindowAppearance where
  show Metal = "metal"

data Completion = NoCompletion | CaseSensitive | CaseInsensitive

instance Show Completion where
  show NoCompletion    = "0"
  show CaseSensitive   = "1"
  show CaseInsensitive = "2"

data FileType = Directory | Extensions (NL.NonEmpty Text) deriving Show

data DateStyle = Textual | Graphical

instance Show DateStyle where
  show Textual   = "1"
  show Graphical = "0"

data DateChoice = DateOnly | TimeOnly | DateTime deriving Show

-- Bool - upscale
data ImageDimensions
  = Height Pixel (Maybe Bool)
  | MaxHeight Pixel
  | Width Pixel (Maybe Bool)
  | MaxWidth Pixel
  | WidthHeight Pixel Pixel (Maybe Bool)
  | MaxWidthMaxHeight Pixel Pixel
  | MaxWidthHeight Pixel Pixel
  | WidthMaxHeight Pixel Pixel
  deriving Show

type Result a = [(a, Text)]

class Serializable w where
  serialize :: w -> [[Text]]

-- Window is not a Widget because it doesn't need an ID
data Window = Window { appearance    :: Maybe WindowAppearance
                     , autoCloseTime :: Maybe Int
                     , autoSaveKey   :: Maybe Text
                     , floating      :: Maybe Bool
                     , title         :: Maybe Text
                     , transparency  :: Maybe Double
                     , xy            :: Maybe Coord
                     }
  deriving Show

-- id_ is needed only for keying into the final results returned by pashua
-- the real ids being passed to pashua are auto-generated ("widget0", "widget1"..)
data Widget a =
  Button
  { id_      :: a
  , label    :: Text
  , xy       :: Maybe Coord
  , disabled :: Maybe Bool
  , tooltip  :: Maybe Text } |
  CancelButton
  { id_      :: a
  , label_   :: Maybe Text
  , disabled :: Maybe Bool
  , tooltip  :: Maybe Text } |
  Checkbox
  { id_      :: a
  , label    :: Text
  , checked  :: Maybe Bool
  , disabled :: Maybe Bool
  , tooltip  :: Maybe Text
  , xy       :: Maybe Coord
  , relX     :: Maybe RelX
  , relY     :: Maybe RelY
  } |
  ComboBox
  { id_         :: a
  , label_      :: Maybe Text
  , options     :: OL.OptionList
  , completion  :: Maybe Completion
  , mandatory   :: Maybe Bool
  , rows        :: Maybe Int
  , placeholder :: Maybe Text
  , disabled    :: Maybe Bool
  , tooltip     :: Maybe Text
  , width       :: Maybe Int
  , xy          :: Maybe Coord
  , relX        :: Maybe RelX
  , relY        :: Maybe RelY
  } |
  RadioButton
  { id_       :: a
  , label_    :: Maybe Text
  , options_  :: OL.OptionList
  , default_  :: Maybe Text
  , mandatory :: Maybe Bool
  , disabled  :: Maybe Bool
  , tooltip   :: Maybe Text
  , xy        :: Maybe Coord
  , relX      :: Maybe RelX
  , relY      :: Maybe RelY
  } |
  Popup
  { id_       :: a
  , label_    :: Maybe Text
  , options_  :: OL.OptionList
  , default_  :: Maybe Text
  , mandatory :: Maybe Bool
  , disabled  :: Maybe Bool
  , tooltip   :: Maybe Text
  , xy        :: Maybe Coord
  , relX      :: Maybe RelX
  , relY      :: Maybe RelY
  } |
  OpenBrowser
  { id_         :: a
  , label_      :: Maybe Text
  , width       :: Maybe Int
  , fileType    :: Maybe FileType
  , defaultPath :: Maybe Text
  , placeholder :: Maybe Text
  , mandatory   :: Maybe Bool
  , xy          :: Maybe Coord
  , relX        :: Maybe RelX
  , relY        :: Maybe RelY
  } |
  SaveBrowser
  { id_           :: a
  , label_        :: Maybe Text
  , width         :: Maybe Int
  , fileExtension :: Maybe Text
  , defaultPath   :: Maybe Text
  , placeholder   :: Maybe Text
  , mandatory     :: Maybe Bool
  , xy            :: Maybe Coord
  , relX          :: Maybe RelX
  , relY          :: Maybe RelY
  } |
  DefaultButton
  { id_      :: a
  , label_   :: Maybe Text
  , disabled :: Maybe Bool
  , tooltip  :: Maybe Text
  } |
  TextField
  { id_         :: a
  , label_      :: Maybe Text
  , default_    :: Maybe Text
  , mandatory   :: Maybe Bool
  , placeholder :: Maybe Text
  , disabled    :: Maybe Bool
  , tooltip     :: Maybe Text
  , width       :: Maybe Int
  , xy          :: Maybe Coord
  , relX        :: Maybe RelX
  , relY        :: Maybe RelY
  } |
  Password
  { id_       :: a
  , label_    :: Maybe Text
  , default_  :: Maybe Text
  , mandatory :: Maybe Bool
  , disabled  :: Maybe Bool
  , tooltip   :: Maybe Text
  , width     :: Maybe Int
  , xy        :: Maybe Coord
  , relX      :: Maybe RelX
  , relY      :: Maybe RelY
  } |
  Date
  { id_      :: a
  , label_   :: Maybe Text
  , default_ :: Maybe Text
  , style    :: Maybe DateStyle
  , choice   :: Maybe DateChoice
  , disabled :: Maybe Bool
  , tooltip  :: Maybe Text
  , xy       :: Maybe Coord
  } |
  Image
  { id_        :: a
  , path       :: Text
  , label_     :: Maybe Text
  , border     :: Maybe Bool
  , dimensions :: Maybe ImageDimensions
  , tooltip    :: Maybe Text
  , xy         :: Maybe Coord
  , relX       :: Maybe RelX
  , relY       :: Maybe RelY
  } |
  Text_
  { id_      :: a
  , text     :: Text
  , label_   :: Maybe Text
  , disabled :: Maybe Bool
  , tooltip  :: Maybe Text
  , xy       :: Maybe Coord
  , relX     :: Maybe RelX
  , relY     :: Maybe RelY
  } |
  TextBox
  { id_       :: a
  , default_  :: Maybe Text
  , label_    :: Maybe Text
  , width     :: Maybe Int
  , height    :: Maybe Int
  , fontSize  :: Maybe FontSize
  , fontType  :: Maybe FontType
  , disabled  :: Maybe Bool
  , mandatory :: Maybe Bool
  , tooltip   :: Maybe Text
  , xy        :: Maybe Coord
  , relX      :: Maybe RelX
  , relY      :: Maybe RelY
  }
  deriving (Functor, Show) -- Functor because we need Widget Text for serialization

data Form a = Form (Maybe Window) [Widget a] deriving Show

coordFmt :: ID -> (Attribute, Attribute) -> Maybe Coord -> [Text]
coordFmt wid (x, y)=
  maybe [] $ \(a, b) ->
               [ sformat (stext % "." % stext % "=" % int) wid x a
               , sformat (stext % "." % stext % "=" % int) wid y b
               ]

textFmt :: ID -> Attribute -> Maybe Text -> [Text]
textFmt wid attr = maybe [] $ \a ->
                     [ sformat (stext % "." % stext % "=" % stext)
                       wid attr a ]

showFmt :: Show a => ID -> Attribute -> Maybe a -> [Text]
showFmt wid attr =
  maybe [] $ \a ->
               [ sformat (stext % "." % stext % "=" % string)
                 wid attr (show a) ]

instance Serializable Window where
  serialize Window {..} =
    [ showFmt "*" "appearance" appearance
    , showFmt "*" "autoclosetime" autoCloseTime
    , textFmt "*" "autosavekey" autoSaveKey
    , showFmt "*" "floating" (boolToInt <$> floating)
    , textFmt "*" "title" title
    , showFmt "*" "transparency" transparency
    , coordFmt "*" ("x", "y") xy
    ]

-- Only Widget Text is allowed to be serializable since
-- serialize will fmap Widget a to Widget Text anyway
instance Serializable (Widget Text) where
  serialize Button {..} =
    [ textFmt id_ "type" (Just "button")
    , textFmt id_ "label" (Just label)
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    ]
  serialize CancelButton {..} =
    [ textFmt id_ "type" (Just "cancelbutton")
    , textFmt id_ "label" label_
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    ]
  serialize Checkbox {..} =
    [ textFmt id_ "type" (Just "checkbox")
    , textFmt id_ "label" (Just label)
    , showFmt id_ "default" (boolToInt <$> checked)
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ]
  serialize ComboBox {..} =
    [ textFmt id_ "type" (Just "combobox")
    , textFmt id_ "label" label_
    , showFmt id_ "width" width
    , showFmt id_ "rows" rows
    , textFmt id_ "placeholder" placeholder
    , showFmt id_ "completion" completion
    , showFmt id_ "mandatory" mandatory
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ] <> NL.toList (OL.items options <&> textFmt id_ "option" . Just)
  serialize RadioButton {..} =
    [ textFmt id_ "type" (Just "radiobutton")
    , textFmt id_ "label" label_
    , showFmt id_ "mandatory" mandatory
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ] <>
    let
      --x = options :: _
      def = OL.default_ options_
      opts = OL.items options_
    in
      NL.toList (opts <&> textFmt id_ "option" . Just) <>
      flip (maybe []) def \x -> [ textFmt id_ "default" (Just x) ]
  serialize Popup {..} =
    [ textFmt id_ "type" (Just "popup")
    , textFmt id_ "label" label_
    , showFmt id_ "mandatory" mandatory
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ] <> let def = OL.default_ options_
             opts = OL.items options_
         in
           NL.toList (opts <&> textFmt id_ "option" . Just) <>
           flip (maybe []) def \x -> [ textFmt id_ "default" (Just x) ]
  serialize OpenBrowser {..} =
    [ textFmt id_ "type" (Just "openbrowser")
    , textFmt id_ "label" label_
    , showFmt id_ "width" width
    , textFmt id_ "default" defaultPath
    , showFmt id_ "mandatory" mandatory
    , textFmt id_ "placeholder" placeholder
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ] <> flip (maybe []) fileType \case
    Directory -> [ textFmt id_ "filetype" (Just "directory") ]
    Extensions es ->
      [ textFmt id_ "filetype" ((Just . unwords . NL.toList) es) ]
  serialize SaveBrowser {..} =
    [ textFmt id_ "type" (Just "openbrowser")
    , textFmt id_ "label" label_
    , showFmt id_ "width" width
    , textFmt id_ "filetype" fileExtension
    , textFmt id_ "default" defaultPath
    , showFmt id_ "mandatory" mandatory
    , textFmt id_ "placeholder" placeholder
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ]
  serialize DefaultButton {..} =
    [ textFmt id_ "type" (Just "defaultbutton")
    , textFmt id_ "label" label_
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    ]
  serialize TextField {..} =
    [ textFmt id_ "type" (Just "textfield")
    , textFmt id_ "label" label_
    , textFmt id_ "default" default_
    , showFmt id_ "width" width
    , textFmt id_ "placeholder" placeholder
    , showFmt id_ "mandatory" mandatory
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ]
  serialize Password {..} =
    [ textFmt id_ "type" (Just "password")
    , textFmt id_ "label" label_
    , textFmt id_ "default" default_
    , showFmt id_ "width" width
    , showFmt id_ "mandatory" mandatory
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ]
  serialize Date {..} =
    [ textFmt id_ "type" (Just "date")
    , textFmt id_ "label" label_
    , textFmt id_ "default" default_
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "textual" style
    ] <> flip (maybe []) choice
    \x ->
      let (date, time) = case x of
            DateOnly -> (1, 0) :: (Int, Int)
            TimeOnly -> (0, 1)
            DateTime -> (1, 1)
      in [ showFmt id_ "date" (Just date)
         , showFmt id_ "time" (Just time)
         ]
  serialize Text_ {..} =
    [ textFmt id_ "type" (Just "text")
    , textFmt id_ "label" label_
    , textFmt id_ "text" ((Just . replaceNL) text)
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ]
  serialize TextBox {..} =
    [ textFmt id_ "type" (Just "textbox")
    , textFmt id_ "label" label_
    , showFmt id_ "height" height
    , showFmt id_ "width" width
    , showFmt id_ "fontsize" fontSize
    , showFmt id_ "fonttype" fontType
    , textFmt id_ "default" (replaceNL <$> default_)
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , showFmt id_ "mandatory" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ]
  serialize Image {..} =
    [ textFmt id_ "type" (Just "image")
    , textFmt id_ "path" (Just path)
    , textFmt id_ "label" label_
    , showFmt id_ "border" (boolToInt <$> border)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , showFmt id_ "relx" relX
    , showFmt id_ "rely" relY
    ] <> case dimensions of
           Just (Height h upscale) ->
             [ showFmt id_ "height" (Just h)
             , showFmt id_ "upscale" (boolToInt <$> upscale) ]
           Just (MaxHeight mh) -> [ showFmt id_ "maxheight" (Just mh) ]
           Just (Width w upscale) ->
             [ showFmt id_ "width" (Just w)
             , showFmt id_ "upscale" (boolToInt <$> upscale) ]
           Just (MaxWidth mw) -> [ showFmt id_ "maxwidth" (Just mw) ]
           Just (WidthHeight w h upscale) ->
             [ showFmt id_ "height" (Just h)
             , showFmt id_ "width" (Just w)
             , showFmt id_ "upscale" (boolToInt <$> upscale) ]
           Just (MaxWidthMaxHeight mw mh) ->
             [ showFmt id_ "maxwidth" (Just mw)
             , showFmt id_ "maxheight" (Just mh) ]
           Just (MaxWidthHeight mw h) ->
             [ showFmt id_ "maxwidth" (Just mw)
             , showFmt id_ "height" (Just h) ]
           Just (WidthMaxHeight w mh) ->
             [ showFmt id_ "maxwidth" (Just w)
             , showFmt id_ "height" (Just mh) ]
           Nothing -> []


mkWidgetID :: Int -> a -> Text
mkWidgetID i _  = sformat ("widget" % int) i

fmapWidget :: (a -> b) -> Widget a -> Widget b
fmapWidget f widget = widget { id_ = f (id_ widget) }

instance Serializable (Form a) where
  serialize (Form w' widgets) =
    maybe [] serialize w' <> mconcat (fmap serialize widgets')
    where
      widgets' = zip [0..] widgets <&> \(i, w) -> fmapWidget (mkWidgetID i) w

window :: Window
window =
  Window
  { appearance    = Nothing
  , autoCloseTime = Nothing
  , autoSaveKey   = Nothing
  , floating      = Nothing
  , title         = Nothing
  , transparency  = Nothing
  , xy            = Nothing
  }

button :: a -> Text -> Widget a
button id_ label =
  Button
  { id_ = id_
  , label = label
  , xy = Nothing
  , disabled = Nothing
  , tooltip = Nothing }


cancelButton :: a -> Widget a
cancelButton id_ =
  CancelButton
  { id_ = id_
  , label_ = Nothing
  , disabled = Nothing
  , tooltip = Nothing }

checkbox :: a -> Text -> Widget a
checkbox id_ label =
  Checkbox
  { id_ = id_
  , label = label
  , disabled = Nothing
  , tooltip = Nothing
  , checked = Nothing
  , xy = Nothing
  , relX = Nothing
  , relY = Nothing
  }

comboBox :: a -> OL.OptionList -> Widget a
comboBox id_ optionList =
  ComboBox
  { id_ = id_
  , label_ = Nothing
  , options = optionList
  , completion = Nothing
  , mandatory = Nothing
  , rows = Nothing
  , placeholder = Nothing
  , disabled = Nothing
  , tooltip = Nothing
  , width = Nothing
  , xy = Nothing
  , relX = Nothing
  , relY = Nothing
  }

radioButton :: a -> OL.OptionList -> Widget a
radioButton id_ optionList =
  RadioButton
  { id_ = id_
  , label_ = Nothing
  , default_ = Nothing
  , options_ = optionList
  , mandatory = Nothing
  , disabled = Nothing
  , tooltip = Nothing
  , xy = Nothing
  , relX = Nothing
  , relY = Nothing
  }

popup :: a -> OL.OptionList -> Widget a
popup id_ optionList =
  Popup
  { id_ = id_
  , label_ = Nothing
  , default_ = Nothing
  , options_ = optionList
  , mandatory = Nothing
  , disabled = Nothing
  , tooltip = Nothing
  , xy = Nothing
  , relX = Nothing
  , relY = Nothing
  }

openBrowser :: a -> Widget a
openBrowser id_ =
  OpenBrowser
  { id_ = id_
  , label_ = Nothing
  , width = Nothing
  , fileType = Nothing
  , defaultPath = Nothing
  , placeholder = Nothing
  , mandatory = Nothing
  , xy = Nothing
  , relX = Nothing
  , relY = Nothing
  }

saveBrowser :: a -> Widget a
saveBrowser id_ =
  SaveBrowser
  { id_ = id_
  , label_ = Nothing
  , width = Nothing
  , fileExtension = Nothing
  , defaultPath = Nothing
  , placeholder = Nothing
  , mandatory = Nothing
  , xy = Nothing
  , relX = Nothing
  , relY = Nothing
  }

defaultButton :: a -> Widget a
defaultButton id_ =
  DefaultButton
  { id_ = id_
  , label_ = Nothing
  , disabled = Nothing
  , tooltip = Nothing
  }

textField :: a -> Widget a
textField id_ =
  TextField
  { id_         = id_
  , label_      = Nothing
  , default_    = Nothing
  , mandatory   = Nothing
  , placeholder = Nothing
  , disabled    = Nothing
  , tooltip     = Nothing
  , width       = Nothing
  , xy          = Nothing
  , relX = Nothing
  , relY = Nothing
  }

password :: a -> Widget a
password id_ =
  Password
  { id_         = id_
  , label_      = Nothing
  , default_    = Nothing
  , mandatory   = Nothing
  , disabled    = Nothing
  , tooltip     = Nothing
  , width       = Nothing
  , xy          = Nothing
  , relX = Nothing
  , relY = Nothing
  }

date :: a -> Widget a
date id_ =
  Date
  { id_      = id_
  , label_   = Nothing
  , default_ = Nothing
  , style    = Nothing
  , choice   = Nothing
  , disabled = Nothing
  , tooltip  = Nothing
  , xy       = Nothing
  }

image :: a -> Text -> Widget a
image id_ path =
  Image
  { id_ = id_
  , path = path
  , label_    = Nothing
  , border = Nothing
  , dimensions = Nothing
  , tooltip = Nothing
  , xy        = Nothing
  , relX      = Nothing
  , relY      = Nothing
  }

text_ :: a -> Text -> Widget a
text_ id_ text =
  Text_
  { id_ = id_
  , text = text
  , label_      = Nothing
  , disabled    = Nothing
  , tooltip     = Nothing
  , xy          = Nothing
  , relX = Nothing
  , relY = Nothing
  }

textBox :: a -> Widget a
textBox id_ =
  TextBox
  { id_ = id_
  , default_    = Nothing
  , label_      = Nothing
  , width       = Nothing
  , height      = Nothing
  , fontSize    = Nothing
  , fontType    = Nothing
  , mandatory   = Nothing
  , disabled    = Nothing
  , tooltip     = Nothing
  , xy          = Nothing
  , relX = Nothing
  , relY = Nothing
  }

-- Ensure all provided Widget IDs are unique
mkForm :: (Eq a, Show a) => Maybe Window -> [Widget a] -> Maybe (Form a)
mkForm _ [] = Nothing
mkForm win ws =
  let ids = fmap (show . id_) ws
      uniqIds = nub ids
--      b = and $ fmap (all isAlphaNum) ids
  in if ids == uniqIds then Just (Form win ws) else Nothing

runForm :: Form a -> [Text]
runForm = mconcat . serialize

parseResult :: Eq a => Form a -> [Text] -> Result a
parseResult (Form _ widgets) inputLines =
  -- lookupTable [ ("widget0", id0), ("widget1", id1), ...]
  let lookupTable = zip [0..] widgets <&> \(i, w) -> (mkWidgetID i id_, id_ w)
  in inputLines <&> \line ->
    let (widgetID', value) = split line
    in
      case find (\(widgetID, _) -> widgetID' == widgetID) lookupTable of
        Nothing     -> error $ "Widget ID " <> show widgetID' <> " not found!"
        Just (_, a) -> (a, value)
  where
    -- Splits line (e.g. "a=b") by "=", return ("a", "b")
    split l = let (k, v) = breakOn "=" l in (k, if null v then "" else tail v)

----------------------
-- Helper functions --
----------------------
pashuaExec :: String
pashuaExec = "/Applications/Pashua.app/Contents/MacOS/Pashua"

withPashua :: Eq a => String -> Form a -> IO (Result a)
withPashua _ (Form _ []) = return []
withPashua pashua f = do
  createProcess (proc pashua ["-"])
    { std_in = CreatePipe, std_out = CreatePipe } >>=
    \case
      (Just stdin', Just stdout', _, _) -> do
        forM_ (runForm f) $ TIO.hPutStrLn stdin'
        hClose stdin'
        (lines <$> TIO.hGetContents stdout') <&> parseResult f
      _ -> error "Can't create Pashua process"

runPashua :: Eq a => Form a -> IO (Result a)
runPashua = withPashua pashuaExec

simpleMessage :: Text -> Text -> IO ()
simpleMessage title body =
  let w = window { title = Just title }
      f = Form (Just w) [ text_ () body ]
  in void $ runPashua f

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

replaceNL :: Text -> Text
replaceNL = replace "\n" "[return]"

unreplaceNL :: Text -> Text
unreplaceNL = replace "[return]" "\n"

mkRelY :: Int -> Maybe RelY
mkRelY i | i < negate 20 = Nothing
         | otherwise = Just $ RelY i

mkPixel :: Int -> Maybe Pixel
mkPixel i | i < 0 = Nothing
          | otherwise = Just $ Pixel i
