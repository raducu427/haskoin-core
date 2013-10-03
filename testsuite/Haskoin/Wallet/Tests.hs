module Haskoin.Wallet.Tests (tests) where

import Test.QuickCheck.Property hiding ((.&.))
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Control.Monad
import Control.Applicative

import Data.Bits
import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

import Haskoin.Wallet
import Haskoin.Wallet.ScriptParser
import Haskoin.Wallet.Arbitrary
import Haskoin.Util

tests :: [Test]
tests = 
    [ testGroup "HDW Extended Keys"
        [ testProperty "prvSubKey(k,c)*G = pubSubKey(k*G,c)" subkeyTest
        , testProperty "decode( encode(prvKey) ) = prvKey" binXPrvKey
        , testProperty "decode( encode(pubKey) ) = pubKey" binXPubKey
        , testProperty "fromB58( toB58(prvKey) ) = prvKey" b58PrvKey
        , testProperty "fromB58( toB58(pubKey) ) = pubKey" b58PubKey
        ]
    , testGroup "Script Parser"
        [ testProperty "decode( encode(sighash) ) = sighash" binSigHash
        , testProperty "decode( encode(tsig) ) = tsig" binTxSig
        , testProperty "encode decode ScriptOutput" testScriptOutput
        , testProperty "encode decode ScriptInput" testScriptInput
        , testProperty "encode decode ScriptHashInput" testScriptHashInput
        ]
    ]

{- HDW Extended Keys -}

subkeyTest :: XPrvKey -> Word32 -> Bool
subkeyTest k i = fromJust $ liftM2 (==) 
    (deriveXPubKey <$> prvSubKey k i') (pubSubKey (deriveXPubKey k) i')
    where i' = fromIntegral $ i .&. 0x7fffffff -- make it a public derivation

binXPrvKey :: XPrvKey -> Bool
binXPrvKey k = (decode' $ encode' k) == k

binXPubKey :: XPubKey -> Bool
binXPubKey k = (decode' $ encode' k) == k

b58PrvKey :: XPrvKey -> Bool
b58PrvKey k = (fromJust $ xPrvImport $ xPrvExport k) == k

b58PubKey :: XPubKey -> Bool
b58PubKey k = (fromJust $ xPubImport $ xPubExport k) == k

{- Script Parser -}

binSigHash :: SigHash -> Bool
binSigHash sh = (decode' $ encode' sh) == sh

binTxSig :: TxSignature -> Bool
binTxSig ts = (decode' $ encode' ts) == ts

testScriptOutput :: ScriptOutput -> Bool
testScriptOutput so = (decodeOutput $ encodeOutput so) == so

testScriptInput :: ScriptInput -> Bool
testScriptInput si = (decodeInput $ encodeInput si) == si

testScriptHashInput :: ScriptHashInput -> Bool
testScriptHashInput sh = 
    (fromJust $ decodeScriptHash $ encodeScriptHash sh) == sh


