{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Pashua
  ( Widget(..)
  , WindowAppearance(..)
  , Completion(..)
  , Window(..)
  , FileType(..)
  , FontSize(..)
  , FontType(..)
  , DateChoice(..)
  , DateStyle(..)
  , Form(..)
  , ListWithDefault
  , defaultButton
  , defaultWindow
  , defaultCancelButton
  , defaultCheckbox
  , defaultCombobox
  , defaultRadioButton
  , defaultPopup
  , defaultOpenBrowser
  , defaultSaveBrowser
  , defaultDefaultButton
  , defaultTextField
  , defaultPassword
  , defaultDate
  , defaultImage
  , defaultText_
  , defaultTextBox
  , serialize
  , runForm
  , mkListWithDefault
  , parseResult
  ) where

import           Data.Functor          ((<&>))
import           Data.List             (find)
import qualified Data.List.NonEmpty    as NL
import           Data.Text             (Text, breakOn, null, tail, unwords)
import           Formatting            (sformat, (%))
import           Formatting.Formatters (int, stext, string)
import           Prelude               hiding (null, tail, unwords)


type ID = Text

type Attribute = Text

type Coord = (Int, Int)

data ListWithDefault = ListWithDefault (Maybe Text) (NL.NonEmpty Text)
  deriving Show

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
  , relXY    :: Maybe Coord
  } |
  Combobox
  { id_         :: a
  , label_      :: Maybe Text
  , options     :: NL.NonEmpty Text
  , completion  :: Maybe Completion
  , mandatory   :: Maybe Bool
  , rows        :: Maybe Int
  , placeholder :: Maybe Text
  , disabled    :: Maybe Bool
  , tooltip     :: Maybe Text
  , width       :: Maybe Int
  , xy          :: Maybe Coord
  , relXY       :: Maybe Coord
  } |
  RadioButton
  { id_       :: a
  , label_    :: Maybe Text
  , options_  :: ListWithDefault
  , default_  :: Maybe Text
  , mandatory :: Maybe Bool
  , disabled  :: Maybe Bool
  , tooltip   :: Maybe Text
  , xy        :: Maybe Coord
  , relXY     :: Maybe Coord
  } |
  Popup
  { id_       :: a
  , label_    :: Maybe Text
  , options_  :: ListWithDefault
  , default_  :: Maybe Text
  , mandatory :: Maybe Bool
  , disabled  :: Maybe Bool
  , tooltip   :: Maybe Text
  , xy        :: Maybe Coord
  , relXY     :: Maybe Coord
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
  , relXY       :: Maybe Coord
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
  , relXY         :: Maybe Coord
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
  , relXY       :: Maybe Coord
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
  , relXY     :: Maybe Coord
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
  Image { id_ :: a }|
  Text_ { id_ :: a }|
  TextBox { id_ :: a }
  deriving (Functor, Show) -- Functor because we need Widget Text for serialization

data Form a = Form (Maybe Window) [Widget a]

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

coordFmt :: ID -> (Attribute, Attribute) -> Maybe Coord -> [Text]
coordFmt wid (x, y)= maybe [] $ \(a, b) ->
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
    , coordFmt id_ ("relx", "rely") relXY
    ]
  serialize Combobox {..} =
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
    , coordFmt id_ ("relx", "rely") relXY
    ] <> NL.toList (options <&> textFmt id_ "option" . Just)
  serialize RadioButton {..} =
    [ textFmt id_ "type" (Just "radiobutton")
    , textFmt id_ "label" label_
    , showFmt id_ "mandatory" mandatory
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , coordFmt id_ ("relx", "rely") relXY
    ] <> let ListWithDefault def opts = options_ in
           NL.toList (opts <&> textFmt id_ "option" . Just) <>
           flip (maybe []) def \x -> [ textFmt id_ "default" (Just x) ]
  serialize Popup {..} =
    [ textFmt id_ "type" (Just "popup")
    , textFmt id_ "label" label_
    , showFmt id_ "mandatory" mandatory
    , showFmt id_ "disabled" (boolToInt <$> disabled)
    , textFmt id_ "tooltip" tooltip
    , coordFmt id_ ("x", "y") xy
    , coordFmt id_ ("relx", "rely") relXY
    ] <> let ListWithDefault def opts = options_ in
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
    , coordFmt id_ ("relx", "rely") relXY
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
    , coordFmt id_ ("relx", "rely") relXY
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
    , coordFmt id_ ("relx", "rely") relXY
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
    , coordFmt id_ ("relx", "rely") relXY
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
  -- TODO: image, text, textbox
  serialize _ = []

mkWidgetID :: Int -> a -> Text
mkWidgetID i _  = sformat ("widget" % int) i

fmapWidget :: (a -> b) -> Widget a -> Widget b
fmapWidget f widget = widget { id_ = f (id_ widget) }

instance Serializable (Form a) where
  serialize (Form w' widgets) =
    maybe [] serialize w' <> mconcat (fmap serialize widgets')
    where
      widgets' = zip [0..] widgets <&> \(i, w) -> fmapWidget (mkWidgetID i) w

defaultWindow :: Window
defaultWindow =
  Window
  { appearance    = Nothing
  , autoCloseTime = Nothing
  , autoSaveKey   = Nothing
  , floating      = Nothing
  , title         = Nothing
  , transparency  = Nothing
  , xy            = Nothing
  }

defaultButton :: a -> Text -> Widget a
defaultButton id_ label =
  Button
  { id_ = id_
  , label = label
  , xy = Nothing
  , disabled = Nothing
  , tooltip = Nothing }


defaultCancelButton :: a -> Widget a
defaultCancelButton id_ =
  CancelButton
  { id_ = id_
  , label_ = Nothing
  , disabled = Nothing
  , tooltip = Nothing }

defaultCheckbox :: a -> Text -> Widget a
defaultCheckbox id_ label =
  Checkbox
  { id_ = id_
  , label = label
  , disabled = Nothing
  , tooltip = Nothing
  , checked = Nothing
  , xy = Nothing
  , relXY = Nothing }

defaultCombobox :: a -> NL.NonEmpty Text -> Widget a
defaultCombobox id_ optionList =
  Combobox
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
  , relXY = Nothing
  }

defaultRadioButton :: a -> ListWithDefault -> Widget a
defaultRadioButton id_ optionList =
  RadioButton
  { id_ = id_
  , label_ = Nothing
  , default_ = Nothing
  , options_ = optionList
  , mandatory = Nothing
  , disabled = Nothing
  , tooltip = Nothing
  , xy = Nothing
  , relXY = Nothing
  }

defaultPopup :: a -> ListWithDefault -> Widget a
defaultPopup id_ optionList =
  Popup
  { id_ = id_
  , label_ = Nothing
  , default_ = Nothing
  , options_ = optionList
  , mandatory = Nothing
  , disabled = Nothing
  , tooltip = Nothing
  , xy = Nothing
  , relXY = Nothing
  }

defaultOpenBrowser :: a -> Widget a
defaultOpenBrowser id_ =
  OpenBrowser
  { id_ = id_
  , label_ = Nothing
  , width = Nothing
  , fileType = Nothing
  , defaultPath = Nothing
  , placeholder = Nothing
  , mandatory = Nothing
  , xy = Nothing
  , relXY = Nothing
  }

defaultSaveBrowser :: a -> Widget a
defaultSaveBrowser id_ =
  SaveBrowser
  { id_ = id_
  , label_ = Nothing
  , width = Nothing
  , fileExtension = Nothing
  , defaultPath = Nothing
  , placeholder = Nothing
  , mandatory = Nothing
  , xy = Nothing
  , relXY = Nothing
  }

defaultDefaultButton :: a -> Widget a
defaultDefaultButton id_ =
  DefaultButton
  { id_ = id_
  , label_ = Nothing
  , disabled = Nothing
  , tooltip = Nothing
  }

defaultTextField :: a -> Widget a
defaultTextField id_ =
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
  , relXY       = Nothing
  }

defaultPassword :: a -> Widget a
defaultPassword id_ =
  Password
  { id_         = id_
  , label_      = Nothing
  , default_    = Nothing
  , mandatory   = Nothing
  , disabled    = Nothing
  , tooltip     = Nothing
  , width       = Nothing
  , xy          = Nothing
  , relXY       = Nothing
  }

defaultDate :: a -> Widget a
defaultDate id_ =
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

defaultImage :: a -> Widget a
defaultImage id_ =
  Image { id_ = id_ }

defaultText_ :: a -> Widget a
defaultText_ id_ =
  Text_ { id_ = id_ }

defaultTextBox :: a -> Widget a
defaultTextBox id_ =
  TextBox { id_ = id_ }

runForm :: Form a -> [Text]
runForm = mconcat . serialize

parseResult :: Eq a => Form a -> [Text] -> [(a, Text)]
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

-- Smart constructor
-- default_ must be in items if it's not Nothing
mkListWithDefault :: Maybe Text -> NL.NonEmpty Text -> Maybe ListWithDefault
mkListWithDefault default_ items =
  case default_ of
    Nothing -> Just $ ListWithDefault Nothing items
    Just x -> if x `elem` items
              then Just $ ListWithDefault default_ items
              else Nothing
