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

import QuickCheckUtils
import Haskoin.Wallet

tests :: [Test]
tests = 
    [ testGroup "HDW Extended Keys"
        [ testProperty "subkey(k,c)*G = subkey(k*G,c)" subkeyTest
        , testProperty "decode( encode(wallet) ) = wallet" encDecWallet
        , testProperty "fromB58( toB58(wallet) ) = wallet" b58Wallet
        ]
    ]

subkeyTest :: PrivateWallet -> Word32 -> Bool
subkeyTest (PrivateWallet w) i = fromJust $ liftM2 (==) 
    (publicWallet <$> subkey w i') (subkey (publicWallet w) i')
    where i' = i .&. 0x7fffffff -- make it a public derivation

encDecWallet :: Wallet -> Bool
encDecWallet w = (decode (encode w)) == w

b58Wallet :: Wallet -> Bool
b58Wallet w = (fromJust $ walletFromBase58 $ walletToBase58 w) == w

