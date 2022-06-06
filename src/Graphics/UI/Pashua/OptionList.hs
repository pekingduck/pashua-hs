module Graphics.UI.Pashua.OptionList
  ( OptionList(default_, items)
  , mkOptionList
  , mkOptionListFromEnum
  ) where

import qualified Data.List.NonEmpty as NL
import           Data.Text          (Text, pack)

data OptionList = OptionList { default_ :: Maybe Text
                             , items    :: NL.NonEmpty Text
                             } deriving Show

mkOptionList :: (Eq a, Show a) => Maybe a -> NL.NonEmpty a -> Maybe OptionList
mkOptionList default_ xs =
  let toText = pack . show
      ts = fmap toText xs
  in
    case default_ of
    Nothing -> Just $ OptionList { default_ = Nothing
                                   , items = ts}
    Just x -> if x `elem` xs
              then Just $ OptionList { default_ = Just (toText x)
                                     , items = ts}
              else Nothing

mkOptionListFromEnum
  :: (Eq a, Enum a, Show a)
  => Maybe a -> a -> Maybe OptionList
mkOptionListFromEnum default_ = mkOptionList default_ . NL.fromList . enumFrom
