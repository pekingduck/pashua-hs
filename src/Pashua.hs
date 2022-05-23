{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  , defaultCombobox
  , defaultRadioButton
  , defaultPopup
  , defaultOpenBrowser
  , defaultSaveBrowser
  , defaultDefaultButton
  , defaultTextField
  , defaultPassword
  , defaultDate
  , serialize
  , runForm
  , mkListWithDefault
  ) where

import           Data.Functor          ((<&>))
import qualified Data.List.NonEmpty    as NL
import           Data.String           (IsString)
import           Data.Text             (Text, intercalate, pack)
import           Formatting            (sformat, (%))
import           Formatting.Formatters (float, int, stext, string)

type ID = String

type Attribute = Text

type Coord = (Int, Int)

data ListWithDefault = ListWithDefault (Maybe Text) (NL.NonEmpty Text)

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

data FileType = Directory | Extensions (NL.NonEmpty Text)

data DateStyle = Textual | Graphical

instance Show DateStyle where
  show Textual   = "1"
  show Graphical = "0"

data DateChoice = DateOnly | TimeOnly | DateTime

class Serializable a where
  serialize :: a -> [[Text]]

-- Window is not in Widget because it would be difficult to assign an ID to it
-- for any a, and that ID would have to be shown as "*"
data Window = Window { appearance    :: Maybe WindowAppearance
                     , autoCloseTime :: Maybe Int
                     , autoSaveKey   :: Maybe Text
                     , floating      :: Maybe Bool
                     , title         :: Maybe Text
                     , transparency  :: Maybe Double
                     , xy            :: Maybe Coord
                     }

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
  Image |
  Text_ |
  TextBox




data Form a = Form (Maybe Window) [Widget a]

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

maybe' = maybe []

coordFmt :: ID -> (Attribute, Attribute) -> Maybe Coord -> [Text]
coordFmt wid (x, y)= maybe [] $ \(a, b) ->
              [ sformat (string % "." % stext % "=" % int) wid x a
              , sformat (string % "." % stext % "=" % int) wid y b
              ]

textFmt :: ID -> Attribute -> Maybe Text -> [Text]
textFmt wid attr = maybe [] $ \a ->
                     [ sformat (string % "." % stext % "=" % stext)
                       wid attr a ]

showFmt :: Show a => ID -> Attribute -> Maybe a -> [Text]
showFmt wid attr = maybe [] $ \a ->
                                [ sformat (string % "." % stext % "=" % string)
                                  wid attr (show a) ]

labelFmt :: ID -> Text -> [Text]
labelFmt wid label = [ sformat (string % ".label=" % stext) wid label]


instance Serializable Window where
  serialize (Window {..}) =
    [ showFmt "*" "appearance" appearance
    , showFmt "*" "autoclosetime" autoCloseTime
    , textFmt "*" "autosavekey" autoSaveKey
    , showFmt "*" "floating" (boolToInt <$> floating)
    , textFmt "*" "title" title
    , showFmt "*" "transparency" transparency
    , coordFmt "*" ("x", "y") xy
    ]


instance Show a => Serializable (Widget a) where
  serialize (Button {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "button")
      , labelFmt widgetID label
      , coordFmt widgetID ("x", "y") xy
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      ]
  serialize (CancelButton {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "cancelbutton")
      , textFmt widgetID "label" label_
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      ]
  serialize (Checkbox {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "checkbox")
      , labelFmt widgetID label
      , showFmt widgetID "default" (boolToInt <$> checked)
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      , coordFmt widgetID ("x", "y") xy
      , coordFmt widgetID ("relx", "rely") relXY
      ]
  serialize (Combobox {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "combobox")
      , textFmt widgetID "label" label_
      , showFmt widgetID "width" width
      , showFmt widgetID "rows" rows
      , textFmt widgetID "placeholder" placeholder
      , showFmt widgetID "completion" completion
      , showFmt widgetID "mandatory" mandatory
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      , coordFmt widgetID ("x", "y") xy
      , coordFmt widgetID ("relx", "rely") relXY
      ] <> NL.toList (options <&> textFmt widgetID "option" . Just)
  serialize (RadioButton {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "radiobutton")
      , textFmt widgetID "label" label_
      , showFmt widgetID "mandatory" mandatory
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      , coordFmt widgetID ("x", "y") xy
      , coordFmt widgetID ("relx", "rely") relXY
      ] <> let ListWithDefault def opts = options_ in
             NL.toList (opts <&> textFmt widgetID "option" . Just) <>
             flip (maybe []) def \x -> [ textFmt widgetID "default" (Just x) ]
  serialize (Popup {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "popup")
      , textFmt widgetID "label" label_
      , showFmt widgetID "mandatory" mandatory
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      , coordFmt widgetID ("x", "y") xy
      , coordFmt widgetID ("relx", "rely") relXY
      ] <> let ListWithDefault def opts = options_ in
             NL.toList (opts <&> textFmt widgetID "option" . Just) <>
             flip (maybe []) def \x -> [ textFmt widgetID "default" (Just x) ]
  serialize (OpenBrowser {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "openbrowser")
      , textFmt widgetID "label" label_
      , showFmt widgetID "width" width
      , textFmt widgetID "default" defaultPath
      , showFmt widgetID "mandatory" mandatory
      , textFmt widgetID "placeholder" placeholder
      , coordFmt widgetID ("x", "y") xy
      , coordFmt widgetID ("relx", "rely") relXY
      ] <> flip (maybe []) fileType \case
      Directory -> [ textFmt widgetID "filetype" (Just "directory") ]
      Extensions es ->
        [ textFmt widgetID "filetype" ((Just . intercalate " " . NL.toList) es) ]
  serialize (SaveBrowser {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "openbrowser")
      , textFmt widgetID "label" label_
      , showFmt widgetID "width" width
      , textFmt widgetID "filetype" fileExtension
      , textFmt widgetID "default" defaultPath
      , showFmt widgetID "mandatory" mandatory
      , textFmt widgetID "placeholder" placeholder
      , coordFmt widgetID ("x", "y") xy
      , coordFmt widgetID ("relx", "rely") relXY
      ]
  serialize (DefaultButton {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "defaultbutton")
      , textFmt widgetID "label" label_
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      ]
  serialize (TextField {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "textfield")
      , textFmt widgetID "label" label_
      , textFmt widgetID "default" default_
      , showFmt widgetID "width" width
      , textFmt widgetID "placeholder" placeholder
      , showFmt widgetID "mandatory" mandatory
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      , coordFmt widgetID ("x", "y") xy
      , coordFmt widgetID ("relx", "rely") relXY
      ]
  serialize (Password {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "password")
      , textFmt widgetID "label" label_
      , textFmt widgetID "default" default_
      , showFmt widgetID "width" width
      , showFmt widgetID "mandatory" mandatory
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      , coordFmt widgetID ("x", "y") xy
      , coordFmt widgetID ("relx", "rely") relXY
      ]
  serialize (Date {..}) =
    let widgetID = show id_ in
      [ textFmt widgetID "type" (Just "date")
      , textFmt widgetID "label" label_
      , textFmt widgetID "default" default_
      , showFmt widgetID "disabled" (boolToInt <$> disabled)
      , textFmt widgetID "tooltip" tooltip
      , coordFmt widgetID ("x", "y") xy
      , showFmt widgetID "textual" style
      ] <> flip (maybe []) choice
      \x ->
        let (date, time) = case x of
              DateOnly -> (1, 0)
              TimeOnly -> (0, 1)
              DateTime -> (1, 1)
        in [ showFmt widgetID "date" (Just date)
           , showFmt widgetID "time" (Just time)
           ]
  -- TODO: image, text, textbox
  serialize _ = []


instance Show a => Serializable (Form a) where
  serialize (Form w' widgets) =
    maybe [] serialize w' <> mconcat (fmap serialize widgets)

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

runForm :: Show a => Form a -> [Text]
runForm = mconcat . serialize

mkListWithDefault :: Maybe Text -> NL.NonEmpty Text -> Maybe ListWithDefault
mkListWithDefault default_ items =
  case default_ of
    Nothing -> Just $ ListWithDefault Nothing items
    Just x -> if x `elem` items
              then Just $ ListWithDefault default_ items
              else Nothing
