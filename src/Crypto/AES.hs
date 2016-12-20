{-# LANGUAGE OverloadedStrings #-}

module Crypto.AES
    ( cryptKey128
    , decryptKey128
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

--import qualified Crypto.Cipher as C
import Crypto.Cipher.Types
import Crypto.Cipher

import Utils.Elmify ((|>))

-- the bytestring need to have a length of 16 bytes
-- otherwise the simplified error handling will raise an exception.
initAES128 :: B.ByteString -> AES128
initAES128 = either (error . show) cipherInit . makeKey

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
initAES256 :: B.ByteString -> AES256
initAES256 = either (error . show) cipherInit . makeKey

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
cryptKey256 key msg = ecbEncrypt ctx msg
  where ctx = initAES256 key

decryptKey128 key msg = ecbDecrypt ctx msg
  where ctx = initAES128 key

cryptKey128 key msg = ecbEncrypt ctx msg
  where ctx = initAES128 key

{-
decryptECB :: BL.ByteString -> BL.ByteString -> BL.ByteString
decryptECB bs k =
  let
    cipher =
      BL.toStrict k
      |> C.makeKey
  in
    case cipher of
      Right key ->
        BL.toStrict bs
        |> C.ecbDecrypt key
        |> (\b -> [b])
        |> BL.fromChunks
      Left e ->
        BL.fromChunks [show e]
-}
