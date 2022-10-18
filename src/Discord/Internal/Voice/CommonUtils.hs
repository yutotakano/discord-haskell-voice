{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Discord.Internal.Voice.CommonUtils
Description : Strictly for internal use only. See Discord.Voice for the public interface.
Copyright   : (c) 2021-2022 Yuto Takano
License     : MIT
Maintainer  : moa17stock@gmail.com

= WARNING

This module is considered __internal__.

The Package Versioning Policy __does not apply__.

The contents of this module may change __in any way whatsoever__ and __without__
__any warning__ between minor versions of this package.

= Description

This module provides useful utility functions used in discord-haskell-voice.
-}
module Discord.Internal.Voice.CommonUtils
    ( module Discord.Internal.Voice.CommonUtils
    ) where

import Control.Concurrent
import Data.Text qualified as T
import GHC.Weak
import System.Timeout ( timeout )

-- | @tshow@ is a shorthand alias for @T.pack . show@.
tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | @maybeToRight@ puts the maybe value into the right hand side of the Either,
-- with a default value provided for the Left as the first argument.
maybeToRight :: a -> Maybe b -> Either a b
maybeToRight a = maybe (Left a) Right

-- | @timeoutMs@ performs an IO action for a maximum of @millisec@ milliseconds.
timeoutMs :: Int -> IO a -> IO (Maybe a)
timeoutMs millisec = timeout (millisec * 10^(3 :: Int))

-- | @killWkThread@ kills a thread referenced by Weak ThreadId. If the thread is
-- no longer alive (that is, if @deRefWeak@ is Nothing), this function will do
-- nothing.
killWkThread :: Weak ThreadId -> IO ()
killWkThread tid = deRefWeak tid >>= \case
    Nothing -> pure ()
    Just x  -> killThread x
