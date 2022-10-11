{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-|
Module      : Discord.Internal.Voice.Encryption
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

This module provides encryption and decryption of secretbox schemes, abstracting
over either Saltine/Libsodium (standard) or Shecretbox/Cryptonite.
-}
module Discord.Internal.Voice.Encryption
    ( module Discord.Internal.Voice.Encryption
    ) where

#ifdef USE_SHECRETBOX
import Crypto.PubKey.Curve25519 qualified as X25519
import Crypto.SecretBox qualified as SecretBox
import Crypto.Error ( maybeCryptoError )
#else
import Crypto.Saltine.Core.SecretBox
    ( secretboxOpen
    , secretbox
    )
import Crypto.Saltine.Class qualified as SC
#endif

import Data.ByteString qualified as B
import Data.Maybe ( fromJust )
import Data.Word ( Word8 )


-- | Decrypt a sound packet using the provided Discord key and header nonce. The
-- argument is strict because it has to be strict when passed to Saltine anyway,
-- and having the same type signature leaves room for the caller to choose.
--
-- This does no error handling on misformatted key/nonce since this function is
-- only used in contexts where we are guaranteed they are valid.
decrypt :: [Word8] -> B.ByteString -> B.ByteString -> Maybe B.ByteString
#ifdef USE_SHECRETBOX
decrypt byteKey nonce ciphertext = SecretBox.open ciphertext nonce key
  where
    key = fromJust $ maybeCryptoError $ X25519.dhSecret $ B.pack byteKey
#else
decrypt byteKey byteNonce og = secretboxOpen key nonce og
  where
    key = fromJust $ SC.decode $ B.pack byteKey
    nonce = fromJust $ SC.decode byteNonce
#endif

-- | Encrypt a strict sound packet using the provided Discord key and header
-- nonce. The argument is strict because it has to be converted to strict
-- before passing onto Saltine anyway, and it leaves room for the caller of the
-- function to choose which laziness to use.
--
-- As with decryption, this function does no error handling on the format of the
-- key and nonce (key = 32 bytes, nonce = 24 bytes).
encrypt :: [Word8] -> B.ByteString -> B.ByteString -> B.ByteString
#ifdef USE_SHECRETBOX
encrypt byteKey nonce message = SecretBox.create message nonce key
  where
    key = fromJust $ maybeCryptoError $ X25519.dhSecret $ B.pack byteKey
#else
encrypt byteKey byteNonce og = secretbox key nonce og
  where
    key = fromJust $ SC.decode $ B.pack byteKey
    nonce = fromJust $ SC.decode byteNonce
#endif
