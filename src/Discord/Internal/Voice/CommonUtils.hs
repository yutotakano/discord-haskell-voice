{-# LANGUAGE ImportQualifiedPost #-}
module Discord.Internal.Voice.CommonUtils where

import Control.Concurrent
import Control.Concurrent.Async ( race )
import Control.Lens
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Data.Time

-- | @tshow@ is a shorthand alias for @T.pack . show@.
tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | @maybeToRight@ puts the maybe value into the right hand side of the Either,
-- with a default value provided for the Left as the first argument.
maybeToRight :: a -> Maybe b -> Either a b
maybeToRight a = maybe (Left a) Right

-- | @doOrTimeout@ performs an IO action for a maximum of @millisec@ milliseconds.
doOrTimeout :: Int -> IO a -> IO (Maybe a)
doOrTimeout millisec longAction = (^? _Right) <$> race waitSecs longAction
  where
    waitSecs :: IO (Maybe b)
    waitSecs = threadDelay (millisec * 10^(3 :: Int)) >> pure Nothing
