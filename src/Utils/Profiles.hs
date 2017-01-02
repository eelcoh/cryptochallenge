{-# LANGUAGE OverloadedStrings #-}

module Utils.Profiles
  ( parse_kvs_string
  , parse_kv_string
  , profile_for
  , profile
  , profile_for'
  , encode
  , attack
  ) where

import qualified Data.ByteString as B
import Data.String.Utils (split, strip, join)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Crypto.AES as AES
import qualified Utils.Bytes as Bytes
import Utils.Elmify ((|>))

parse_kvs_string :: String -> Map.Map String String
parse_kvs_string str =
  strip str
  |> split "&"
  |> map parse_kv_string
  |> catMaybes
  |> Map.fromList

parse_kv_string :: String -> Maybe (String, String)
parse_kv_string str =
  let
    t =
      strip str
      |> split "="
  in
    case t of
      k:v:[] ->
        Just (k,v)
      _ ->
        Nothing

profile :: String -> Map.Map String String
profile address =
  let
    address' =
      filter (\c -> not (elem c ['=', '&'])) address
  in
    [ ( "email", address' )
    , ( "uid", "10" )
    , ( "role", "user" )
    ]
    |> Map.fromList

encode :: Map.Map String String -> String
encode m =
  let
    e =
      Map.lookup "email" m
    u =
      Map.lookup "uid" m
    r =
      Map.lookup "role" m
  in
    catMaybes [e, u, r]
    |> zip ["email", "uid", "role"]
    |> map encode_kv
    |> join "&"

encode_kv :: (String, String) -> String
encode_kv (k, v) =
  join "=" [k, v]

profile_for :: String -> B.ByteString
profile_for address =
  profile address
  |> encode
  |> Bytes.stringToByteString

profile_for' :: B.ByteString -> String -> B.ByteString
profile_for' key address =
  address
  |> profile
  |> encode
  |> Bytes.stringToByteString
  |> AES.ecb_encrypt key


{-
-- email=a@b.com&&uid=10&role=user
-- 1234567890123456123456789012345612345
-- 1               2               3               4

-- email=fool@bar.com=&uid=10&role=user
-- 12345678901234561234567890123456123456789
-- 1               2               3               4

-- email=john12345@gmail.com&uid=10&role=user
-- 1234567890123456123456789012345612345678901234561
-- 1               2               3               4

-- email=johndoe12345@gmail.com&=&uid=10&role=user
-- 1234567890123456123456789012345612345678901234561234
-- 1               2               3               4
-}

attack :: (String -> B.ByteString) -> String -> B.ByteString
attack encrypt_fn address =
  let
    prof =
      encrypt_fn address

    prof_segment =
      prof
      |> Bytes.blocks 16
      |> take 2 -- we take the first 3 blocks
      |> B.concat

    admin_string =
      B.concat [(B.replicate 10 0x04), "admin", (B.replicate 11 0x04), "@gmail.com"]
      -- B.append (Bytes.pad 16 ("admin"::B.ByteString)) ("@gmail.com"::B.ByteString)

    admin_segment =
      Bytes.byteStringToString admin_string
      |> encrypt_fn
      |> Bytes.blocks 16
      |> drop 1 -- we ignore the first block
    --  |> take 1 -- and take the second
      |> B.concat

    --m_key_size =
    --  AES.detect_ecb_key_size encrypt_fn prof

    --enc_prof_size =
    --  encrypt_fn prof
    --  |> B.length

    --prof_size =
    --  B.length prof

    --enc_profile =
    --  encrypt_fn prof

  in
    B.append prof_segment admin_segment
    -- (prof_segment, admin_segment)
