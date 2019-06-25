{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Haskoin.Script.Standard
Copyright   : No rights reserved
License     : UNLICENSE
Maintainer  : xenog@protonmail.com
Stability   : experimental
Portability : POSIX

Standard scripts like pay-to-public-key, pay-to-public-key-hash,
pay-to-script-hash, pay-to-multisig and corresponding SegWit variants.
-}
module Network.Haskoin.Script.Standard
    ( ScriptInput(..)
    , ScriptInputNonStd(..)
    , SimpleInput(..)
    , SimpleInputNonStd(..)
    , RedeemScript
    , RedeemScriptNonStd
    , encodeInput
    , encodeInputBS
    , decodeInput
    , decodeInputBS
    , encodeInput'
    , encodeInputBS'
    , decodeInput'
    , decodeInputBS'
    , sortMulSig
    , scriptOpToInt
    , isSpendPK
    , isSpendPKHash
    , isSpendMulSig
    , isScriptHashInput
    ) where

import           Control.Applicative            ((<|>))
import           Control.Monad                  (guard, (<=<))
import           Data.ByteString                (ByteString)
import           Data.Function                  (on)
import           Data.List                      (sortBy)
import           Data.Serialize                 (decode, encode)
import           Network.Haskoin.Constants
import           Network.Haskoin.Keys.Common
import           Network.Haskoin.Script.Common
import           Network.Haskoin.Script.SigHash
import           Network.Haskoin.Util


-- | Data type describing standard transaction input scripts. Input scripts
-- provide the signing data required to unlock the coins of the output they are
-- trying to spend, except in pay-to-witness-public-key-hash and
-- pay-to-script-hash transactions.
data SimpleInput
      -- | spend pay-to-public-key output
    = SpendPK { getInputSig :: !TxSignature
                  -- ^ transaction signature
               }
      -- | spend pay-to-public-key-hash output
    | SpendPKHash { getInputSig :: !TxSignature
                      -- ^ embedded signature
                  , getInputKey :: !PubKeyI
                      -- ^ public key
                   }
      -- | spend multisig output
    | SpendMulSig { getInputMulSigKeys :: ![TxSignature]
                      -- ^ list of signatures
                   }
    deriving (Eq, Show)

data SimpleInputNonStd
      -- | spend pay-to-public-key output
    = SpendPKNonStd { getInputSigNonStd :: !TxSignature
                      -- ^ transaction signature
                    }
      -- | spend pay-to-public-key-hash output
    | SpendPKHashNonStd { getInputSigNonStd :: !TxSignature
                        -- ^ embedded signature
                        , getInputKeyNonStd :: !PubKeyI
                        -- ^ public key
                        }

   | SpendScriptHashNonStd { getInputSigNonStd :: !TxSignature
                           -- ^ embedded signature
                           , getInputDataNonStd :: ![ByteString]
                           -- ^ public key
                           }
      -- | spend multisig output
    | SpendMulSigNonStd { getInputMulSigKeysNonStd :: ![TxSignature]
                      -- ^ list of signatures
                   }
    deriving (Eq, Show)


-- | Returns true if the input script is spending from a pay-to-public-key
-- output.
isSpendPK :: ScriptInput -> Bool
isSpendPK (RegularInput (SpendPK _)) = True
isSpendPK _                          = False

-- | Returns true if the input script is spending from a pay-to-public-key-hash
-- output.
isSpendPKHash :: ScriptInput -> Bool
isSpendPKHash (RegularInput (SpendPKHash _ _)) = True
isSpendPKHash _                                = False

-- | Returns true if the input script is spending a multisig output.
isSpendMulSig :: ScriptInput -> Bool
isSpendMulSig (RegularInput (SpendMulSig _)) = True
isSpendMulSig _                              = False

-- | Returns true if the input script is spending a pay-to-script-hash output.
isScriptHashInput :: ScriptInput -> Bool
isScriptHashInput (ScriptHashInput _ _) = True
isScriptHashInput _                     = False

-- | A redeem script is the output script serialized into the spending input
-- script. It must be included in inputs that spend pay-to-script-hash outputs.
type RedeemScript = ScriptOutput
type RedeemScriptNonStd = ScriptOutputNonStd

-- | Standard input script high-level representation.
data ScriptInput
    = RegularInput    {
        getRegularInput     :: SimpleInput
            -- ^ get wrapped simple input
        }
    | ScriptHashInput {
                        getScriptHashInput  :: SimpleInput
                        -- ^ get simple input associated with redeem script
                      , getScriptHashRedeem :: RedeemScript
                        -- ^ redeem script
                      }
    deriving (Eq, Show)

data ScriptInputNonStd
    = RegularInputNonStd {
        getRegularInputNonStd :: SimpleInputNonStd
            -- ^ get wrapped simple input
        }
    | ScriptHashInputNonStd {
                              getScriptHashInputNonStd  :: SimpleInputNonStd
                              -- ^ get simple input associated with redeem script
                            , getScriptHashRedeemNonStd :: RedeemScriptNonStd
                              -- ^ redeem script
                            }
    deriving (Eq, Show)

-- | Heuristic to decode an input script into one of the standard types.
decodeSimpleInput :: Network -> Script -> Either String SimpleInput
decodeSimpleInput net (Script ops) =
    maybeToEither errMsg $ matchPK ops <|> matchPKHash ops <|> matchMulSig ops
  where
    matchPK [op] = SpendPK <$> f op
    matchPK _    = Nothing
    matchPKHash [op, OP_PUSHDATA pub _] =
        SpendPKHash <$> f op <*> eitherToMaybe (decode pub)
    matchPKHash _ = Nothing
    matchMulSig (x:xs) = do
        guard $ x == OP_0
        SpendMulSig <$> mapM f xs
    matchMulSig _ = Nothing
    f OP_0                    = return TxSignatureEmpty
    f (OP_PUSHDATA "" OPCODE) = f OP_0
    f (OP_PUSHDATA bs _)      = eitherToMaybe $ decodeTxSig net bs
    f _                       = Nothing
    errMsg = "decodeInput: Could not decode script input"

decodeSimpleInput' :: Network -> Script -> Either String SimpleInputNonStd
decodeSimpleInput' net (Script ops) =
    maybeToEither errMsg $ matchPK ops <|> matchPKHash ops <|> matchMulSig ops <|> matchScriptHash ops
  where
    matchPK [op] = SpendPKNonStd <$> f op
    matchPK _    = Nothing
    matchPKHash [op, OP_PUSHDATA pub _] =
        SpendPKHashNonStd <$> f op <*> eitherToMaybe (decode pub)
    matchPKHash _ = Nothing
    matchMulSig (x:xs) = do
        guard $ x == OP_0
        SpendMulSigNonStd <$> mapM f xs
    matchMulSig _ = Nothing
    matchScriptHash (x:xs) =
      let l = filter isPushData xs
      in SpendScriptHashNonStd <$> (f x) <*> (mapM (Just . getData) (init l))
    matchScriptHash _ = Nothing
    f OP_0                    = return TxSignatureEmpty
    f (OP_PUSHDATA "" OPCODE) = f OP_0
    f (OP_PUSHDATA bs _)      = eitherToMaybe $ decodeTxSig net bs
    f _                       = Nothing
    errMsg = "decodeInput: Could not decode script input"


-- | Heuristic to decode a 'ScriptInput' from a 'Script'. This function fails if
-- the script can not be parsed as a standard script input.
decodeInput :: Network -> Script -> Either String ScriptInput
decodeInput net s@(Script ops) =
    maybeToEither errMsg $ matchSimpleInput <|> matchPayScriptHash
  where
    matchSimpleInput =
        RegularInput <$> eitherToMaybe (decodeSimpleInput net s)
    matchPayScriptHash =
        case splitAt (length (scriptOps s) - 1) ops of
            (is, [OP_PUSHDATA bs _]) -> do
                rdm <- eitherToMaybe $ decodeOutputBS bs
                inp <- eitherToMaybe $ decodeSimpleInput net $ Script is
                return $ ScriptHashInput inp rdm
            _ -> Nothing
    errMsg = "decodeInput: Could not decode script input"


decodeInput' :: Network -> Script -> Either String ScriptInputNonStd
decodeInput' net s@(Script ops) =
    maybeToEither errMsg $ matchSimpleInput <|> matchPayScriptHash
  where
    matchSimpleInput =
        RegularInputNonStd <$> eitherToMaybe (decodeSimpleInput' net s)
    matchPayScriptHash =
        case splitAt (length (scriptOps s) - 1) ops of
            (is, [OP_PUSHDATA bs _]) -> do
                inp <- eitherToMaybe $ decodeSimpleInput' net $ Script is
                rdm <- eitherToMaybe $ decodeOutputBS' (getInputDataNonStd inp) bs
                return $ ScriptHashInputNonStd inp rdm
            _ -> Nothing
    errMsg = "decodeInput: Could not decode script input"

-- | Like 'decodeInput' but decodes directly from a serialized script
-- 'ByteString'.
decodeInputBS :: Network -> ByteString -> Either String ScriptInput
decodeInputBS net = decodeInput net <=< decode

decodeInputBS' :: Network -> ByteString -> Either String ScriptInputNonStd
decodeInputBS' net = decodeInput' net <=< decode

-- | Encode a standard input into a script.
encodeInput :: ScriptInput -> Script
encodeInput s = case s of
    RegularInput ri -> encodeSimpleInput ri
    ScriptHashInput i o -> Script $
        scriptOps (encodeSimpleInput i) ++ [opPushData $ encodeOutputBS o]

encodeInput' :: ScriptInputNonStd -> Script
encodeInput' s = case s of
    RegularInputNonStd ri -> encodeSimpleInput' ri
    ScriptHashInputNonStd i o -> Script $
        scriptOps (encodeSimpleInput' i) ++ [opPushData $ encodeOutputBS' o]

-- | Similar to 'encodeInput' but encodes directly to a serialized script
-- 'ByteString'.
encodeInputBS :: ScriptInput -> ByteString
encodeInputBS = encode . encodeInput

encodeInputBS' :: ScriptInputNonStd -> ByteString
encodeInputBS' = encode . encodeInput'

-- | Encode a standard 'SimpleInput' into opcodes as an input 'Script'.
encodeSimpleInput :: SimpleInput -> Script
encodeSimpleInput s =
    Script $
    case s of
        SpendPK ts       -> [f ts]
        SpendPKHash ts p -> [f ts, opPushData $ encode p]
        SpendMulSig xs   -> OP_0 : map f xs
  where
    f TxSignatureEmpty = OP_0
    f ts               = opPushData $ encodeTxSig ts

encodeSimpleInput' :: SimpleInputNonStd -> Script
encodeSimpleInput' s =
    Script $
      case s of
          SpendPKNonStd ts             -> [f ts]
          SpendPKHashNonStd ts p       -> [f ts, opPushData $ encode p]
          SpendScriptHashNonStd ts bss -> [f ts] <> (opPushData <$> bss)
          SpendMulSigNonStd xs         -> OP_0 : map f xs
  where
    f TxSignatureEmpty = OP_0
    f ts               = opPushData $ encodeTxSig ts

-- | Sort the public keys of a multisig output in ascending order by comparing
-- their compressed serialized representations. Refer to BIP-67.
sortMulSig :: ScriptOutput -> ScriptOutput
sortMulSig out = case out of
  PayMulSig keys r -> PayMulSig (sortBy (compare `on` encode) keys) r
  _ -> error "Can only call orderMulSig on PayMulSig scripts"
