{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Haskoin.Constants
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

Network constants for various networks, including Bitcoin SegWit (BTC), Bitcoin
Cash (BCH), and corresponding public test and private regression test networks.
-}
module Network.Haskoin.Constants
    ( Network(..)
    , btc
    , btcTest
    , btcRegTest
    , bch
    , bchTest
    , bchRegTest
    , ltc
    , ltcTest
    , ltcRegTest
    , allNets
    , netByName
    , netByIdent
    ) where

import           Data.ByteString              (ByteString)
import           Data.List
import           Data.Maybe
import           Data.Serialize
import           Data.String
import           Data.Text                    (Text)
import           Data.Version
import           Data.Word                    (Word32, Word64, Word8)
import           GHC.Generics                 (Generic)
import           Network.Haskoin.Block.Common
import           Paths_haskoin_core
import           Text.Read

-- | Version of Haskoin Core package.
versionString :: IsString a => a
versionString = fromString (showVersion version)

-- | Constants for network.
data Network = Network
    { -- | lowercase alphanumeric and dashes
      getNetworkName              :: !String
      -- | network Haskell identifier
    , getNetworkIdent             :: !String
      -- | prefix for 'Base58' P2PKH addresses
    , getAddrPrefix               :: !Word8
      -- | prefix for 'Base58' P2SH addresses
    , getScriptPrefix             :: !Word8
      -- | prefix for WIF private key
    , getSecretPrefix             :: !Word8
      -- | prefix for extended public key
    , getExtPubKeyPrefix          :: !Word32
      -- | prefix for extended private key
    , getExtSecretPrefix          :: !Word32
      -- | network magic
    , getNetworkMagic             :: !Word32
      -- | genesis block header
    , getGenesisHeader            :: !BlockHeader
      -- | maximum block size in bytes
    , getMaxBlockSize             :: !Int
      -- | maximum amount of satoshi
    , getMaxSatoshi               :: !Word64
      -- | user agent string
    , getHaskoinUserAgent         :: !ByteString
      -- | default port for P2P connections
    , getDefaultPort              :: !Int
      -- | allow min difficulty blocks (testnet)
    , getAllowMinDifficultyBlocks :: !Bool
      -- | do not retarget difficulty (regtest)
    , getPowNoRetargetting        :: !Bool
      -- | proof-of-work target higest possible value
    , getPowLimit                 :: !Integer
      -- | block at which BIP34 activates
    , getBip34Block               :: !(BlockHeight, BlockHash)
      -- | block at which BIP65 activates
    , getBip65Height              :: !BlockHeight
      -- | block at which BIP66 activates
    , getBip66Height              :: !BlockHeight
      -- | time between difficulty retargets
    , getTargetTimespan           :: !Word32
      -- | time between blocks
    , getTargetSpacing            :: !Word32
      -- | checkpoints
    , getCheckpoints              :: ![(BlockHeight, BlockHash)]
      -- | BIP44 derivation path root
    , getBip44Coin                :: !Word32
      -- | peer-to-peer network seeds
    , getSeeds                    :: ![String]
      -- | fork id for replay protection
    , getSigHashForkId            :: !(Maybe Word32)
      -- | EDA start block height
    , getEdaBlockHeight           :: !(Maybe Word32)
      -- | DAA start block height
    , getDaaBlockHeight           :: !(Maybe Word32)
      -- | segregated witness active
    , getSegWit                   :: !Bool
      -- | 'CashAddr' prefix (for Bitcoin Cash)
    , getCashAddrPrefix           :: !(Maybe Text)
      -- | 'Bech32' prefix (for SegWit network)
    , getBech32Prefix             :: !(Maybe Text)
      -- | Replace-By-Fee (BIP-125)
    , getReplaceByFee             :: !Bool
      -- | Subsidy halving interval
    , getHalvingInterval          :: !Word32
    } deriving (Eq, Generic)

instance Serialize Network where
    put net =
        putWord32be $ getNetworkMagic net
    get = do
        magic <- getWord32be
        case find ((== magic) . getNetworkMagic) allNets of
            Nothing  -> fail $ "Network magic unknown: " <> show magic
            Just net -> return net

instance Show Network where
    show = getNetworkIdent

instance Read Network where
    readPrec = do
        Ident str <- lexP
        maybe pfail return (netByIdent str)

instance IsString Network where
    fromString = fromMaybe (error "Network name invalid") . netByName

-- | Query known networks by name.
netByName :: String -> Maybe Network
netByName str = find ((== str) . getNetworkName) allNets

-- | Query known networks by Haskell identifier.
netByIdent :: String -> Maybe Network
netByIdent str = find ((== str) . getNetworkIdent) allNets

-- | Bitcoin SegWit network. Symbol: BTC.
btc :: Network
btc =
    Network
    { getNetworkName = "btc"
    , getNetworkIdent = "btc"
    , getAddrPrefix = 0
    , getScriptPrefix = 5
    , getSecretPrefix = 128
    , getExtPubKeyPrefix = 0x0488b21e
    , getExtSecretPrefix = 0x0488ade4
    , getNetworkMagic = 0xf9beb4d9
    , getGenesisHeader =
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1231006505
              0x1d00ffff
              2083236893
            -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent =
          "/haskoin-btc:" <> versionString <> "/"
    , getDefaultPort = 8333
    , getAllowMinDifficultyBlocks = False
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 227931
          , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8")
    , getBip65Height = 388381
    , getBip66Height = 363725
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 11111
            , "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d")
          , ( 33333
            , "000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6")
          , ( 74000
            , "0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20")
          , ( 105000
            , "00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97")
          , ( 134444
            , "00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe")
          , ( 168000
            , "000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763")
          , ( 193000
            , "000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317")
          , ( 210000
            , "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e")
          , ( 216116
            , "00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e")
          , ( 225430
            , "00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932")
          , ( 250000
            , "000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214")
          , ( 279000
            , "0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40")
          , ( 295000
            , "00000000000000004d9b4ef50f0f9d686fd69db2e03af35a100370c64632a983")
          ]
    , getSeeds =
          [ "seed.bitcoin.sipa.be" -- Pieter Wuille
          , "dnsseed.bluematt.me" -- Matt Corallo
          , "dnsseed.bitcoin.dashjr.org" -- Luke Dashjr
          , "seed.bitcoinstats.com" -- Chris Decker
          , "seed.bitcoin.jonasschnelli.ch" -- Jonas Schnelli
          , "seed.btc.petertodd.org" -- Peter Todd
          , "seed.bitcoin.sprovoost.nl" -- Sjors Provoost
          ]
    , getBip44Coin = 0
    , getSigHashForkId = Nothing
    , getEdaBlockHeight = Nothing
    , getDaaBlockHeight = Nothing
    , getSegWit = True
    , getCashAddrPrefix = Nothing
    , getBech32Prefix = Just "bc"
    , getReplaceByFee = True
    , getHalvingInterval = 210000
    }

-- | Testnet for Bitcoin SegWit network.
btcTest :: Network
btcTest =
    Network
    { getNetworkName = "btctest"
    , getNetworkIdent = "btcTest"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0x0b110907
    , getGenesisHeader =
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              486604799
              414098458
            -- Hash 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent = "/haskoin-btc-test:" <> versionString <> "/"
    , getDefaultPort = 18333
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 21111
          , "0000000023b3a96d3484e5abb3755c413e7d41500f8e2a5c3f0dd01299cd8ef8")
    , getBip65Height = 581885
    , getBip66Height = 330776
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 546
            , "000000002a936ca763904c3c35fce2f3556c559c0214345d31b1bcebf76acb70")
          ]
    , getSeeds =
          [ "testnet-seed.bitcoin.jonasschnelli.ch"
          , "seed.tbtc.petertodd.org"
          , "seed.testnet.bitcoin.sprovoost.nl"
          , "testnet-seed.bluematt.me"
          ]
    , getBip44Coin = 1
    , getSigHashForkId = Nothing
    , getEdaBlockHeight = Nothing
    , getDaaBlockHeight = Nothing
    , getSegWit = True
    , getCashAddrPrefix = Nothing
    , getBech32Prefix = Just "tb"
    , getReplaceByFee = True
    , getHalvingInterval = 210000
    }

-- | RegTest for Bitcoin SegWit network.
btcRegTest :: Network
btcRegTest =
    Network
    { getNetworkName = "btcreg"
    , getNetworkIdent = "btcRegTest"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0xfabfb5da
    , getGenesisHeader =
          BlockHeader
           -- 0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              0x207fffff
              2
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent = "/haskoin-btc-regtest:" <> versionString <> "/"
    , getDefaultPort = 18444
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = True
    , getPowLimit =
          0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 100000000
          , "0000000000000000000000000000000000000000000000000000000000000000")
    , getBip65Height = 1351
    , getBip66Height = 1251
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints = []
    , getSeeds = ["localhost"]
    , getBip44Coin = 1
    , getSigHashForkId = Nothing
    , getEdaBlockHeight = Nothing
    , getDaaBlockHeight = Nothing
    , getSegWit = True
    , getCashAddrPrefix = Nothing
    , getBech32Prefix = Just "bcrt"
    , getReplaceByFee = True
    , getHalvingInterval = 150
    }

-- | Bitcoin Cash network. Symbol: BCH.
bch :: Network
bch =
    Network
    { getNetworkName = "bch"
    , getNetworkIdent = "bch"
    , getAddrPrefix = 0
    , getScriptPrefix = 5
    , getSecretPrefix = 128
    , getExtPubKeyPrefix = 0x0488b21e
    , getExtSecretPrefix = 0x0488ade4
    , getNetworkMagic = 0xe3e1f3e8
    , getGenesisHeader =
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1231006505
              0x1d00ffff
              2083236893
            -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    , getMaxBlockSize = 32000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent = "/haskoin-bch:" <> versionString <> "/"
    , getDefaultPort = 8333
    , getAllowMinDifficultyBlocks = False
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 227931
          , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8")
    , getBip65Height = 388381
    , getBip66Height = 363725
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 11111
            , "0000000069e244f73d78e8fd29ba2fd2ed618bd6fa2ee92559f542fdb26e7c1d")
          , ( 33333
            , "000000002dd5588a74784eaa7ab0507a18ad16a236e7b1ce69f00d7ddfb5d0a6")
          , ( 74000
            , "0000000000573993a3c9e41ce34471c079dcf5f52a0e824a81e7f953b8661a20")
          , ( 105000
            , "00000000000291ce28027faea320c8d2b054b2e0fe44a773f3eefb151d6bdc97")
          , ( 134444
            , "00000000000005b12ffd4cd315cd34ffd4a594f430ac814c91184a0d42d2b0fe")
          , ( 168000
            , "000000000000099e61ea72015e79632f216fe6cb33d7899acb35b75c8303b763")
          , ( 193000
            , "000000000000059f452a5f7340de6682a977387c17010ff6e6c3bd83ca8b1317")
          , ( 210000
            , "000000000000048b95347e83192f69cf0366076336c639f9b7228e9ba171342e")
          , ( 216116
            , "00000000000001b4f4b433e81ee46494af945cf96014816a4e2370f11b23df4e")
          , ( 225430
            , "00000000000001c108384350f74090433e7fcf79a606b8e797f065b130575932")
          , ( 250000
            , "000000000000003887df1f29024b06fc2200b55f8af8f35453d7be294df2d214")
          , ( 279000
            , "0000000000000001ae8c72a0b0c301f67e3afca10e819efa9041e458e9bd7e40")
          , ( 295000
            , "00000000000000004d9b4ef50f0f9d686fd69db2e03af35a100370c64632a983")
            -- UAHF fork block.
          , ( 478559
            , "000000000000000000651ef99cb9fcbe0dadde1d424bd9f15ff20136191a5eec")
            -- Nov, 13 DAA activation block.
          , ( 504031
            , "0000000000000000011ebf65b60d0a3de80b8175be709d653b4c1a1beeb6ab9c")
          ]
    , getSeeds =
          [ "seed.bitcoinabc.org"
          , "seed-abc.bitcoinforks.org"
          , "btccash-seeder.bitcoinunlimited.info"
          , "seed.bitprim.org"
          , "seed.deadalnix.me"
          , "seeder.criptolayer.net"
          ]
    , getBip44Coin = 145
    , getSigHashForkId = Just 0
    , getEdaBlockHeight = Just 478559
    , getDaaBlockHeight = Just 404031
    , getSegWit = False
    , getCashAddrPrefix = Just "bitcoincash"
    , getBech32Prefix = Nothing
    , getReplaceByFee = False
    , getHalvingInterval = 210000
    }

-- | Testnet for Bitcoin Cash network.
bchTest :: Network
bchTest =
    Network
    { getNetworkName = "bchtest"
    , getNetworkIdent = "bchTest"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0xf4e5f3f4
    , getGenesisHeader =
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              486604799
              414098458
            -- Hash 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
    , getMaxBlockSize = 32000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent = "/haskoin-bch-test:" <> versionString <> "/"
    , getDefaultPort = 18333
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 21111
          , "0000000023b3a96d3484e5abb3755c413e7d41500f8e2a5c3f0dd01299cd8ef8")
    , getBip65Height = 581885
    , getBip66Height = 330776
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 546
            , "000000002a936ca763904c3c35fce2f3556c559c0214345d31b1bcebf76acb70")
            -- UAHF fork block.
          , ( 1155876
            , "00000000000e38fef93ed9582a7df43815d5c2ba9fd37ef70c9a0ea4a285b8f5")
            -- Nov, 13. DAA activation block.
          , ( 1188697
            , "0000000000170ed0918077bde7b4d36cc4c91be69fa09211f748240dabe047fb")
          ]
    , getSeeds =
          [ "testnet-seed.bitcoinabc.org"
          , "testnet-seed-abc.bitcoinforks.org"
          , "testnet-seed.bitprim.org"
          , "testnet-seed.deadalnix.me"
          , "testnet-seeder.criptolayer.net"
          ]
    , getBip44Coin = 1
    , getSigHashForkId = Just 0
    , getEdaBlockHeight = Just 1155876
    , getDaaBlockHeight = Just 1188697
    , getSegWit = False
    , getCashAddrPrefix = Just "bchtest"
    , getBech32Prefix = Nothing
    , getReplaceByFee = False
    , getHalvingInterval = 210000
    }

-- | RegTest for Bitcoin Cash network.
bchRegTest :: Network
bchRegTest =
    Network
    { getNetworkName = "bchreg"
    , getNetworkIdent = "bchRegTest"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0xdab5bffa
    , getGenesisHeader =
          BlockHeader
           -- 0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              0x207fffff
              2
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 2100000000000000
    , getHaskoinUserAgent = "/haskoin-bch-regtest:" <> versionString <> "/"
    , getDefaultPort = 18444
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = True
    , getPowLimit =
          0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 100000000
          , "0000000000000000000000000000000000000000000000000000000000000000")
    , getBip65Height = 1351
    , getBip66Height = 1251
    , getTargetTimespan = 14 * 24 * 60 * 60
    , getTargetSpacing = 10 * 60
    , getCheckpoints =
          [ ( 0
            , "0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206")
          ]
    , getSeeds = ["localhost"]
    , getBip44Coin = 1
    , getSigHashForkId = Just 0
    , getEdaBlockHeight = Nothing
    , getDaaBlockHeight = Just 0
    , getSegWit = False
    , getCashAddrPrefix = Just "bchreg"
    , getBech32Prefix = Nothing
    , getReplaceByFee = False
    , getHalvingInterval = 150
    }

-- | Litecoin SegWit network. Symbol: LTC.
ltc :: Network
ltc =
    Network
    { getNetworkName = "ltc"
    , getNetworkIdent = "ltc"
    , getAddrPrefix = 30
    , getScriptPrefix = 48
    -- , getScriptPrefix = 50  -- new
    , getSecretPrefix = 176
    , getExtPubKeyPrefix = 0x0488b21e
    , getExtSecretPrefix = 0x0488ade4
    , getNetworkMagic = 0xfbc0b6db
    , getGenesisHeader =
          BlockHeader
              0x01
              "12a765e31ffd4059bada1e25190f6e98c99d9714d334efa41a195a7e7e04bfe2"
              "97ddfbbae6be97fd6cdf3e7ca13232a3afff2353e29badfab7f73011edd4ced9"
              1231006505
              0x1e0ffff0
              2083236893
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 8400000000000000
    , getHaskoinUserAgent =
          "/haskoin-ltc:" <> versionString <> "/"
    , getDefaultPort = 9333
    , getAllowMinDifficultyBlocks = False
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 710000
          , "fa09d204a83a768ed5a7c8d441fa62f2043abf420cff1226c7b4329aeb9d51cf")
    , getBip65Height = 918684
    , getBip66Height = 811879
    , getTargetTimespan = 3 * 24 * 60 * 60 + 12 * 60 * 60
    , getTargetSpacing = 2 * 60 + 30
    , getCheckpoints =
          [ ( 1500
            , "841a2965955dd288cfa707a755d05a54e45f8bd476835ec9af4402a2b59a2967")
          , ( 4032
            , "9ce90e427198fc0ef05e5905ce3503725b80e26afd35a987965fd7e3d9cf0846")
          , ( 8064
            , "eb984353fc5190f210651f150c40b8a4bab9eeeff0b729fcb3987da694430d70")
          , ( 16128
            , "602edf1859b7f9a6af809f1d9b0e6cb66fdc1d4d9dcd7a4bec03e12a1ccd153d")
          , ( 23420
            , "d80fdf9ca81afd0bd2b2a90ac3a9fe547da58f2530ec874e978fce0b5101b507")
          , ( 50000
            , "69dc37eb029b68f075a5012dcc0419c127672adb4f3a32882b2b3e71d07a20a6")
          , ( 80000
            , "4fcb7c02f676a300503f49c764a89955a8f920b46a8cbecb4867182ecdb2e90a")
          , ( 120000
            , "bd9d26924f05f6daa7f0155f32828ec89e8e29cee9e7121b026a7a3552ac6131")
          , ( 161500
            , "dbe89880474f4bb4f75c227c77ba1cdc024991123b28b8418dbbf7798471ff43")
          , ( 179620
            , "2ad9c65c990ac00426d18e446e0fd7be2ffa69e9a7dcb28358a50b2b78b9f709")
          , ( 240000
            , "7140d1c4b4c2157ca217ee7636f24c9c73db39c4590c4e6eab2e3ea1555088aa")
          , ( 383640
            , "2b6809f094a9215bafc65eb3f110a35127a34be94b7d0590a096c3f126c6f364")
          , ( 409004
            , "487518d663d9f1fa08611d9395ad74d982b667fbdc0e77e9cf39b4f1355908a3")
          , ( 456000
            , "bf34f71cc6366cd487930d06be22f897e34ca6a40501ac7d401be32456372004")
          , ( 638902
            , "15238656e8ec63d28de29a8c75fcf3a5819afc953dcd9cc45cecc53baec74f38")
          , ( 721000
            , "198a7b4de1df9478e2463bd99d75b714eab235a2e63e741641dc8a759a9840e5")
          ]
    , getSeeds =
          [ "seed-a.litecoin.loshan.co.uk"
          , "dnsseed.thrasher.io"
          , "dnsseed.litecointools.com"
          , "dnsseed.litecoinpool.org"
          , "dnsseed.koin-project.com"
          ]
    , getBip44Coin = 2
    , getSigHashForkId = Nothing
    , getEdaBlockHeight = Nothing
    , getDaaBlockHeight = Nothing
    , getSegWit = True
    , getCashAddrPrefix = Nothing
    , getBech32Prefix = Just "ltc"
    , getReplaceByFee = False
    , getHalvingInterval = 840000
    }


-- | Testnet for Litecoin SegWit network.
ltcTest :: Network
ltcTest =
    Network
    { getNetworkName = "ltcTest"
    , getNetworkIdent = "ltcTest"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    -- , getScriptPrefix = 58  -- new
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0xfabfb5da
    , getGenesisHeader =
          BlockHeader
              0x01
              "530827f38f93b43ed12af0b3ad25a288dc02ed74d6d7857862df51fc56c416f9"
              "97ddfbbae6be97fd6cdf3e7ca13232a3afff2353e29badfab7f73011edd4ced9"
              1486949366
              0x207fffff
              293345
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 8400000000000000
    , getHaskoinUserAgent =
          "/haskoin-ltcTest:" <> versionString <> "/"
    , getDefaultPort = 19335
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = False
    , getPowLimit =
          0x00000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 76
          , "8075c771ed8b495ffd943980a95f702ab34fce3c8c54e379548bda33cc8c0573")
    , getBip65Height = 76
    , getBip66Height = 76
    , getTargetTimespan = 3 * 24 * 60 * 60 + 12 * 60 * 60
    , getTargetSpacing = 2 * 60 + 30
    , getCheckpoints =
          [ ( 2056
            , "17748a31ba97afdc9a4f86837a39d287e3e7c7290a08a1d816c5969c78a83289")

          ]
    , getSeeds =
          [ "testnet-seed.litecointools.com"
          , "seed-b.litecoin.loshan.co.uk"
          , "dnsseed-testnet.thrasher.io"
          ]
    , getBip44Coin = 2
    , getSigHashForkId = Nothing
    , getEdaBlockHeight = Nothing
    , getDaaBlockHeight = Nothing
    , getSegWit = True
    , getCashAddrPrefix = Nothing
    , getBech32Prefix = Just "tltc"
    , getReplaceByFee = True
    , getHalvingInterval = 840000
    }

-- | Regression testnet for Litecoin SegWit network.
ltcRegTest :: Network
ltcRegTest =
    Network
    { getNetworkName = "ltcReg"
    , getNetworkIdent = "ltcRegTest"
    , getAddrPrefix = 111
    , getScriptPrefix = 196
    -- , getScriptPrefix = 58  -- new
    , getSecretPrefix = 239
    , getExtPubKeyPrefix = 0x043587cf
    , getExtSecretPrefix = 0x04358394
    , getNetworkMagic = 0xfabfb5da
    , getGenesisHeader =
          BlockHeader
              0x01
              "530827f38f93b43ed12af0b3ad25a288dc02ed74d6d7857862df51fc56c416f9"
              "97ddfbbae6be97fd6cdf3e7ca13232a3afff2353e29badfab7f73011edd4ced9"
              1296688602
              0x207fffff
              0
    , getMaxBlockSize = 1000000
    , getMaxSatoshi = 8400000000000000
    , getHaskoinUserAgent =
          "/haskoin-ltcRegTest:" <> versionString <> "/"
    , getDefaultPort = 19444
    , getAllowMinDifficultyBlocks = True
    , getPowNoRetargetting = True
    , getPowLimit =
          0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =
          ( 100000000
          , "")
    , getBip65Height = 1351
    , getBip66Height = 1251
    , getTargetTimespan = 3 * 24 * 60 * 60 + 12 * 60 * 60
    , getTargetSpacing = 2 * 60 + 30
    , getCheckpoints =
          [ ( 0
            , "530827f38f93b43ed12af0b3ad25a288dc02ed74d6d7857862df51fc56c416f9")

          ]
    , getSeeds = []
    , getBip44Coin = 2
    , getSigHashForkId = Nothing
    , getEdaBlockHeight = Nothing
    , getDaaBlockHeight = Nothing
    , getSegWit = True
    , getCashAddrPrefix = Nothing
    , getBech32Prefix = Just "rltc"
    , getReplaceByFee = True
    , getHalvingInterval = 150
    }


-- | List of all networks supported by this library.
allNets :: [Network]
allNets = [btc, bch, btcTest, bchTest, btcRegTest, bchRegTest, ltc, ltcTest, ltcRegTest]
