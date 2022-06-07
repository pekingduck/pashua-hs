module Graphics.UI.Pashua.OptionList
  ( OptionList(default_, items)
  , mkOptionList
  , mkOptionListFromEnum
  ) where

import qualified Data.List.NonEmpty as NL
import           Data.Proxy         (Proxy (Proxy))
import           Data.Text          (Text, pack)

data OptionList = OptionList { default_ :: Maybe Text
                             , items    :: NL.NonEmpty Text
                             } deriving Show

toText :: Show a => a -> Text
toText = pack . show

mkOptionList :: (Eq a, Show a) => Maybe a -> NL.NonEmpty a -> Maybe OptionList
mkOptionList default_ xs =
  let
      ts = fmap toText xs
  in
    case default_ of
    Nothing -> Just $ OptionList { default_ = Nothing
                                   , items = ts}
    Just x -> if x `elem` xs
              then Just $ OptionList { default_ = Just (toText x)
                                     , items = ts}
              else Nothing

-- Doesn't need to return maybe for enum since a is guaranteed to be in the list
mkOptionListFromEnum
  :: (Eq a, Enum a, Show a)
  => Maybe a -> OptionList
mkOptionListFromEnum default_ =
  -- Need the maybe b to help infer [b]
  let go :: Enum b => Maybe b -> [b]
      go _ = enumFrom $ toEnum 0
  in OptionList { default_ = toText <$> default_
                , items = toText <$> NL.fromList (go default_)
                }
