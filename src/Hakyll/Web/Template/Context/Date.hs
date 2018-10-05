module Hakyll.Web.Template.Context.Date
    ( formatDateFieldValue
    ) where

import Hakyll.Web.Template.Context (Context(..), ContextField(..))
import Hakyll.Core.Metadata (getMetadataField)
import Hakyll.Core.Item (itemIdentifier)
import Control.Applicative (empty)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime)

-- | Replace metadata date format with a new formatted date or do nothing
formatDateFieldValue :: String    -- ^ Field name
                     -> String    -- ^ Current format string
                     -> String    -- ^ New format string
                     -> Context a -- ^ Resulting context
formatDateFieldValue name currFmt newFmt = Context $ \k _ i ->
  if k == name
  then (do value <- getMetadataField (itemIdentifier i) k
           maybe empty (\v -> do
                           let mSDate = parseAndFormat newFmt v
                           case mSDate of
                             (Just sDate) -> (return . StringField) sDate
                             Nothing  -> empty
                       ) value
       )
  else empty
  where
    parseAndFormat :: String -> String -> Maybe String
    parseAndFormat fmt' v' = do
      let timeV = parseTimeM True defaultTimeLocale currFmt v' :: Maybe UTCTime
      case timeV of
        (Just t) -> Just $ formatTime defaultTimeLocale fmt' t
        Nothing   -> Nothing
