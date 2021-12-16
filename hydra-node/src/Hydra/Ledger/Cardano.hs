{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.Cardano (
  module Hydra.Ledger.Cardano,
  module Cardano.Api,
  module Cardano.Api.Shelley,
  Ledger.ShelleyGenesis (..),
) where

import Hydra.Prelude hiding (id)

import Cardano.Api hiding (UTxO)
import qualified Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley
import Cardano.Binary (decodeAnnotator, serialize, serialize')
import qualified Cardano.Crypto.DSIGN as CC
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger.Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Ledger (StandardCrypto)
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary as Ledger.Mary hiding (Value)
import qualified Cardano.Ledger.Mary.Value as Ledger.Mary
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.API.Mempool as Ledger
import qualified Cardano.Ledger.Shelley.Address.Bootstrap as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Ledger
import qualified Cardano.Ledger.Shelley.LedgerState as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import qualified Cardano.Ledger.Slot as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Cardano.Slotting.EpochInfo as Slotting
import qualified Cardano.Slotting.Time as Slotting
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (serialise)
import Control.Arrow (left)
import Control.Monad (foldM)
import qualified Control.State.Transition as Ledger
import qualified Data.ByteString.Lazy as BL
import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Formatting.Buildable (build)
import Hydra.Ledger (IsTx (..), Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano.Orphans ()
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import qualified Test.Cardano.Ledger.Alonzo.AlonzoEraGen as Ledger.Alonzo
import qualified Test.Cardano.Ledger.Shelley.Generator.Constants as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.Core as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.EraGen as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.Presets as Ledger.Generator
import qualified Test.Cardano.Ledger.Shelley.Generator.Utxo as Ledger.Generator
import Test.QuickCheck (
  choose,
  getSize,
  scale,
  shrinkList,
  shrinkMapBy,
  suchThat,
  vectorOf,
 )

type Era = AlonzoEra

type LedgerCrypto = Ledger.StandardCrypto

type LedgerEra = Ledger.Alonzo.AlonzoEra LedgerCrypto

type CardanoTx = Tx Era

-- TODO(SN): Pre-validate transactions to get less confusing errors on
-- transactions which are not expected to working on a layer-2
cardanoLedger :: Ledger CardanoTx
cardanoLedger =
  Ledger
    { applyTransactions = applyAll
    , initUtxo = mempty
    }
 where
  -- NOTE(SN): See full note on 'applyTx' why we only have a single transaction
  -- application here.
  applyAll utxo = \case
    [] -> Right utxo
    (tx : txs) -> do
      utxo' <- left (first fromLedgerTx) $ fromLedgerUtxo <$> applyTx ledgerEnv (toLedgerUtxo utxo) (toLedgerTx tx)
      applyAll utxo' txs

  -- NOTE(SN): This is will fail on any transaction requiring the 'DPState' to be
  -- in a certain state as we do throw away the resulting 'DPState' and only take
  -- the ledger's 'UTxO' forward.
  --
  -- We came to this signature of only applying a single transaction because we
  -- got confused why a sequence of transactions worked but sequentially applying
  -- single transactions didn't. This was because of this not-keeping the'DPState'
  -- as described above.
  applyTx ::
    ( Ledger.ApplyTx era
    , Default (Ledger.State (Ledger.EraRule "PPUP" era))
    ) =>
    Ledger.LedgerEnv era ->
    Ledger.UTxO era ->
    Ledger.Tx era ->
    Either (Ledger.Tx era, ValidationError) (Ledger.UTxO era)
  applyTx env utxo tx =
    case Ledger.applyTxsTransition globals env (pure tx) memPoolState of
      Left err -> Left (tx, toValidationError err)
      Right (ls, _ds) -> Right $ Ledger._utxo ls
   where
    toValidationError = ValidationError . show
    memPoolState = (def{Ledger._utxo = utxo}, def)

signWith ::
  forall era.
  (IsShelleyBasedEra era) =>
  TxId ->
  (VerificationKey PaymentKey, SigningKey PaymentKey) ->
  KeyWitness era
signWith (TxId h) (PaymentVerificationKey vk, PaymentSigningKey sk) =
  ShelleyKeyWitness (shelleyBasedEra @era) $
    Ledger.Shelley.WitVKey
      (Ledger.asWitness vk)
      (Ledger.signedDSIGN @Ledger.StandardCrypto sk h)

--
-- Type conversions & plumbing
--

--
-- Utxo
--

type Utxo = Utxo' (TxOut CtxUTxO Era)

-- | Newtype with phantom types mostly required to work around the poor interface
-- of 'Ledger.UTXO'and provide 'Monoid' and 'Foldable' instances to make utxo
-- manipulation bareable.
newtype Utxo' out = Utxo
  { utxoMap :: Map TxIn out
  }
  deriving newtype (Eq, Show)

instance ToCBOR Utxo where
  toCBOR = toCBOR . toLedgerUtxo
  encodedSizeExpr sz _ = encodedSizeExpr sz (Proxy @(Ledger.UTxO LedgerEra))

instance FromCBOR Utxo where
  fromCBOR = fromLedgerUtxo <$> fromCBOR
  label _ = label (Proxy @(Ledger.UTxO LedgerEra))

instance Functor Utxo' where
  fmap fn (Utxo u) = Utxo (fmap fn u)

instance Foldable Utxo' where
  foldMap fn = foldMap fn . utxoMap
  foldr fn zero = foldr fn zero . utxoMap

instance Semigroup Utxo where
  Utxo uL <> Utxo uR = Utxo (uL <> uR)

instance Monoid Utxo where
  mempty = Utxo mempty

instance ToJSON Utxo where
  toJSON = toJSON . utxoMap

instance FromJSON Utxo where
  parseJSON = fmap Utxo . parseJSON

instance Arbitrary Utxo where
  shrink = shrinkUtxo

  -- TODO: Use Alonzo generators!
  -- probably: import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
  arbitrary =
    fmap
      (fromLedgerUtxo . Ledger.UTxO . Map.map fromMaryTxOut . Ledger.unUTxO)
      arbitrary

utxoPairs :: Utxo' out -> [(TxIn, out)]
utxoPairs = Map.toList . utxoMap

prettyUtxo :: (TxIn, TxOut ctx era) -> Text
prettyUtxo (k, TxOut _ (txOutValueToValue -> v) _) =
  T.drop 54 (renderTxIn k) <> " ↦ " <> prettyValue v

utxoFromTx :: CardanoTx -> Utxo
utxoFromTx (Tx body@(ShelleyTxBody _ ledgerBody _ _ _ _) _) =
  let txOuts = toList $ Ledger.Alonzo.outputs' ledgerBody
      txIns =
        [ Ledger.TxIn (toLedgerTxId $ getTxId body) ix
        | ix <- [0 .. fromIntegral (length txOuts)]
        ]
   in fromLedgerUtxo $ Ledger.UTxO $ Map.fromList $ zip txIns txOuts

-- | Select the minimum (by TxIn) utxo entry from the Utxo map.
--
-- This function is partial.
utxoMin :: Utxo -> Utxo
utxoMin = Utxo . uncurry Map.singleton . Map.findMin . utxoMap

--
-- Transaction Construction
--

type TxBuilder = TxBodyContent BuildTx Era

-- TODO: This is copied straight from 'cardano-api', could be exposed upstream.
type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]

-- | Construct a transction from a builder. It is said 'unsafe' because the
-- underlying implementation will perform some sanity check on a transaction;
-- for example, check that it has at least one input, that no outputs are
-- negatives and whatnot.
--
-- We use the builder only internally for on-chain transaction crafted in the
-- context of Hydra.
unsafeBuildTransaction :: HasCallStack => TxBuilder -> CardanoTx
unsafeBuildTransaction builder =
  either
    (\txBodyError -> bug $ InvalidTransactionException{txBodyError, builder})
    (`Tx` mempty)
    . makeTransactionBody
    $ builder

-- | A runtime exception to capture (programmer) failures when building
-- transactions. This should never happened in practice (famous last words...)!
data InvalidTransactionException = InvalidTransactionException
  { txBodyError :: TxBodyError
  , builder :: TxBuilder
  }
  deriving (Show)

instance Exception InvalidTransactionException

-- | An empty 'TxBodyContent' with all empty/zero values to be extended using
-- record updates.
--
-- FIXME: 'makeTransactionBody' throws when one tries to build a transaction
-- with scripts but no collaterals. This is unfortunate because collaterals are
-- currently added after by out integrated wallet... We may want to revisit our
-- flow to avoid this exception and have the wallet work from a TxBuilder instead
-- of fiddling with a sealed 'CardanoTx'.
--
-- Similarly, 'makeTransactionBody' throws when building a transaction
-- with scripts and no protocol parameters (needed to compute the script
-- integrity hash). This is also added by our wallet at the moment so
-- hopefully, this ugly work-around will be removed eventually.
--
-- So we currently bypass this by having default but seemingly innofensive
-- values for collaterals and protocol params in the 'empty' value
emptyTxBody :: TxBuilder
emptyTxBody =
  TxBodyContent
    mempty
    (TxInsCollateral CollateralInAlonzoEra mempty)
    mempty
    (TxFeeExplicit TxFeesExplicitInAlonzoEra 0)
    (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
    TxMetadataNone
    TxAuxScriptsNone
    TxExtraKeyWitnessesNone
    (BuildTxWith $ Just $ fromLedgerPParams ShelleyBasedEraAlonzo def) -- FIXME
    TxWithdrawalsNone
    TxCertificatesNone
    TxUpdateProposalNone
    TxMintNone
    TxScriptValidityNone

-- | Add new inputs to an ongoing builder.
addInputs :: TxIns BuildTx Era -> TxBuilder -> TxBuilder
addInputs ins tx =
  tx{txIns = txIns tx <> ins}

-- | Like 'addInputs' but only for vk inputs which requires no additional data.
addVkInputs :: [TxIn] -> TxBuilder -> TxBuilder
addVkInputs ins =
  addInputs ((,BuildTxWith $ KeyWitness KeyWitnessForSpending) <$> ins)

-- | Append new outputs to an ongoing builder.
addOutputs :: [TxOut CtxTx Era] -> TxBuilder -> TxBuilder
addOutputs outputs tx =
  tx{txOuts = txOuts tx <> outputs}

--
-- Tx
--

instance IsTx CardanoTx where
  type TxIdType CardanoTx = TxId
  type UtxoType CardanoTx = Utxo
  type ValueType CardanoTx = Value

  txId = getTxId . getTxBody
  balance = foldMap (\(TxOut _ value _) -> txOutValueToValue value)

instance ToCBOR CardanoTx where
  toCBOR = CBOR.encodeBytes . serialize' . toLedgerTx

instance FromCBOR CardanoTx where
  fromCBOR = do
    bs <- CBOR.decodeBytes
    decodeAnnotator "CardanoTx" fromCBOR (fromStrict bs)
      & either
        (fail . toString . toLazyText . build)
        (pure . fromLedgerTx)

instance ToJSON CardanoTx where
  toJSON = toJSON . toLedgerTx

instance FromJSON CardanoTx where
  parseJSON = fmap fromLedgerTx . parseJSON

instance Arbitrary CardanoTx where
  -- TODO: shrinker!
  arbitrary = genUtxo >>= genTx

-- | Show (compact) information about a Cardano transaction for debugging purpose.
--
-- NOTE: The function is 'incomplete' but can easily be extended to cover new
-- needs.
describeCardanoTx :: CardanoTx -> Text
describeCardanoTx (Tx body _wits) =
  unlines
    [ show (getTxId body)
    , "  Inputs (" <> show (length inputs) <> ")"
    , "  Outputs (" <> show (length outputs) <> ")"
    , "    total number of assets: " <> show totalNumberOfAssets
    , "  Scripts (" <> show (length scripts) <> ")"
    , "    total size (bytes):  " <> show totalScriptSize
    ]
 where
  ShelleyTxBody _era lbody scripts _scriptsData _auxData _validity = body
  outputs = Ledger.Alonzo.outputs' lbody
  inputs = Ledger.Alonzo.inputs' lbody
  totalScriptSize = sum $ BL.length . serialize <$> scripts
  totalNumberOfAssets =
    sum $
      [ foldl' (\n inner -> n + Map.size inner) 0 outer
      | Ledger.Alonzo.TxOut _ (Ledger.Mary.Value _ outer) _ <- toList outputs
      ]

-- | Create a zero-fee, payment cardano transaction.
mkSimpleCardanoTx ::
  (TxIn, TxOut CtxUTxO Era) ->
  -- | Recipient address and amount.
  (AddressInEra Era, Value) ->
  -- | Sender's signing key.
  SigningKey PaymentKey ->
  Either TxBodyError CardanoTx
mkSimpleCardanoTx (txin, TxOut owner txOutValueIn datum) (recipient, valueOut) sk = do
  body <- makeTransactionBody txBodyContent
  let witnesses = [makeShelleyKeyWitness body (WitnessPaymentKey sk)]
  pure $ makeSignedTransaction witnesses body
 where
  valueIn = txOutValueToValue txOutValueIn

  txBodyContent =
    emptyTxBody
      { txIns = map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) [txin]
      , txOuts
      , txFee = TxFeeExplicit TxFeesExplicitInAlonzoEra fee
      }

  txOuts =
    TxOut @CtxTx recipient (TxOutValue MultiAssetInAlonzoEra valueOut) TxOutDatumNone :
      [ TxOut @CtxTx owner (TxOutValue MultiAssetInAlonzoEra $ valueIn <> negateValue valueOut) (toTxDatum datum)
      | valueOut /= valueIn
      ]

  fee = Lovelace 0

-- XXX(SN): replace with Cardano.Api.TxBody.lovelaceToTxOutValue when available
lovelaceToTxOutValue :: Lovelace -> TxOutValue AlonzoEra
lovelaceToTxOutValue lovelace = TxOutValue MultiAssetInAlonzoEra (lovelaceToValue lovelace)

toTxDatum :: TxOutDatum CtxUTxO Era -> TxOutDatum CtxTx Era
toTxDatum = \case
  TxOutDatumNone -> TxOutDatumNone
  TxOutDatumHash sdsie ha -> TxOutDatumHash sdsie ha

-- | Convert an existing @cardano-api@'s 'Tx' to a @cardano-ledger-specs@ 'Tx'
toLedgerTx :: CardanoTx -> Ledger.Tx LedgerEra
toLedgerTx = \case
  Tx (ShelleyTxBody _era body scripts scriptsData auxData validity) vkWits ->
    let (datums, redeemers) =
          case scriptsData of
            TxBodyScriptData _ ds rs -> (ds, rs)
            TxBodyNoScriptData -> (mempty, Ledger.Alonzo.Redeemers mempty)
     in Ledger.Alonzo.ValidatedTx
          { Ledger.Alonzo.body =
              body
          , Ledger.Alonzo.isValid =
              toLedgerScriptValidity validity
          , Ledger.Alonzo.auxiliaryData =
              maybeToStrictMaybe auxData
          , Ledger.Alonzo.wits =
              Ledger.Alonzo.TxWitness
                { Ledger.Alonzo.txwitsVKey =
                    toLedgerKeyWitness vkWits
                , Ledger.Alonzo.txwitsBoot =
                    toLedgerBootstrapWitness vkWits
                , Ledger.Alonzo.txscripts =
                    fromList [(Ledger.hashScript @LedgerEra s, s) | s <- scripts]
                , Ledger.Alonzo.txdats =
                    datums
                , Ledger.Alonzo.txrdmrs =
                    redeemers
                }
          }
 where
  toLedgerScriptValidity :: TxScriptValidity Era -> Ledger.Alonzo.IsValid
  toLedgerScriptValidity =
    Ledger.Alonzo.IsValid . \case
      TxScriptValidityNone -> True
      TxScriptValidity _ ScriptValid -> True
      TxScriptValidity _ ScriptInvalid -> False

-- | Convert an existing @cardano-ledger-specs@'s 'Tx' into a @cardano-api@'s 'Tx'
fromLedgerTx :: Ledger.Tx LedgerEra -> CardanoTx
fromLedgerTx (Ledger.Alonzo.ValidatedTx body wits isValid auxData) =
  Tx
    (ShelleyTxBody era body scripts scriptsData (strictMaybeToMaybe auxData) validity)
    (fromLedgerTxWitness wits)
 where
  era =
    ShelleyBasedEraAlonzo
  scripts =
    Map.elems $ Ledger.Alonzo.txscripts' wits
  scriptsData =
    TxBodyScriptData
      ScriptDataInAlonzoEra
      (Ledger.Alonzo.txdats' wits)
      (Ledger.Alonzo.txrdmrs' wits)
  validity = case isValid of
    Ledger.Alonzo.IsValid True ->
      TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptValid
    Ledger.Alonzo.IsValid False ->
      TxScriptValidity TxScriptValiditySupportedInAlonzoEra ScriptInvalid

--
-- TxIn
--

fromLedgerTxIn :: Ledger.TxIn Ledger.StandardCrypto -> TxIn
fromLedgerTxIn = fromShelleyTxIn

toLedgerTxIn :: TxIn -> Ledger.TxIn Ledger.StandardCrypto
toLedgerTxIn = toShelleyTxIn

--
-- TxId
--

toLedgerTxId :: TxId -> Ledger.TxId Ledger.StandardCrypto
toLedgerTxId (TxId h) =
  Ledger.TxId (Ledger.unsafeMakeSafeHash (CC.castHash h))

fromLedgerTxId :: Ledger.TxId Ledger.StandardCrypto -> TxId
fromLedgerTxId (Ledger.TxId h) =
  TxId (CC.castHash (Ledger.extractHash h))

--
-- Scripts
--

fromPlutusScript :: Plutus.Script -> Script PlutusScriptV1
fromPlutusScript =
  PlutusScript PlutusScriptV1 . fromPlutusScript'

fromPlutusScript' :: Plutus.Script -> PlutusScript PlutusScriptV1
fromPlutusScript' script =
  PlutusScriptSerialised bytes
 where
  bytes = toShort . fromLazy . serialise $ script

--
-- Keys
--

toPlutusKeyHash :: Hash PaymentKey -> Plutus.PubKeyHash
toPlutusKeyHash (PaymentKeyHash vkh) =
  Ledger.transKeyHash vkh

--
-- Address
--

-- | Create an (undelegated) address from a verificaton key.
--
-- TODO: 'NetworkId' here is an annoying API because it requires a network magic
-- for testnet addresses. Nevertheless, the network magic is only needed for
-- Byron addresses; Shelley addresses use a different kind of network
-- discriminant which is currently fully captured as 'Mainnet | Testnet'.
--
-- So, it would be a slightly better DX to use Mainnet | Testnet as an interface
-- here since we are only constructing Shelley addresses.
mkVkAddress ::
  IsShelleyBasedEra era =>
  NetworkId ->
  VerificationKey PaymentKey ->
  AddressInEra era
mkVkAddress networkId vk =
  makeShelleyAddressInEra
    networkId
    (PaymentCredentialByKey $ verificationKeyHash vk)
    NoStakeAddress

-- | Create an (undelegated) address from a script.
--
-- TODO: See remark on 'mkVkAddress' about 'NetworkId'
mkScriptAddress ::
  IsShelleyBasedEra era =>
  NetworkId ->
  Script lang ->
  AddressInEra era
mkScriptAddress networkId script =
  makeShelleyAddressInEra
    networkId
    (PaymentCredentialByScript $ hashScript script)
    NoStakeAddress

toLedgerAddr :: AddressInEra Era -> Ledger.Addr Ledger.StandardCrypto
toLedgerAddr = \case
  AddressInEra ByronAddressInAnyEra (ByronAddress addr) ->
    Ledger.AddrBootstrap (Ledger.BootstrapAddress addr)
  AddressInEra (ShelleyAddressInEra _) (ShelleyAddress ntwrk creds stake) ->
    Ledger.Addr ntwrk creds stake

--
-- TxOut
--

txOutValue :: TxOut ctx Era -> Value
txOutValue (TxOut _ value _) =
  txOutValueToValue value

toMaryTxOut :: Ledger.TxOut LedgerEra -> Ledger.Mary.TxOut (Ledger.Mary.MaryEra Ledger.StandardCrypto)
toMaryTxOut = \case
  Ledger.Alonzo.TxOutCompact addr value ->
    Ledger.Shelley.TxOutCompact addr value
  Ledger.Alonzo.TxOutCompactDH addr value _datum ->
    Ledger.Shelley.TxOutCompact addr value

fromMaryTxOut :: Ledger.Mary.TxOut (Ledger.Mary.MaryEra Ledger.StandardCrypto) -> Ledger.TxOut LedgerEra
fromMaryTxOut = \case
  Ledger.Shelley.TxOutCompact addr value ->
    Ledger.Alonzo.TxOutCompact addr value

--
-- Datums & Redeemers
--

mkTxOutDatum :: Plutus.ToData a => a -> TxOutDatum CtxTx Era
mkTxOutDatum =
  TxOutDatum ScriptDataInAlonzoEra . fromPlutusData . Plutus.toData

mkDatumForTxIn :: Plutus.ToData a => a -> ScriptDatum WitCtxTxIn
mkDatumForTxIn =
  ScriptDatumForTxIn . fromPlutusData . Plutus.toData

mkRedeemerForTxIn :: Plutus.ToData a => a -> ScriptRedeemer
mkRedeemerForTxIn =
  fromPlutusData . Plutus.toData

--
-- Utxo
--

toLedgerUtxo :: Utxo -> Ledger.UTxO LedgerEra
toLedgerUtxo =
  Ledger.UTxO . Map.foldMapWithKey fn . utxoMap
 where
  fn ::
    TxIn ->
    TxOut CtxUTxO Era ->
    Map (Ledger.TxIn Ledger.StandardCrypto) (Ledger.TxOut LedgerEra)
  fn i o =
    Map.singleton (toLedgerTxIn i) (toLedgerTxOut o)

fromLedgerUtxo :: Ledger.UTxO LedgerEra -> Utxo
fromLedgerUtxo =
  Utxo . Map.foldMapWithKey fn . Ledger.unUTxO
 where
  fn ::
    Ledger.TxIn Ledger.StandardCrypto ->
    Ledger.TxOut LedgerEra ->
    Map TxIn (TxOut CtxUTxO Era)
  fn i o =
    Map.singleton (fromLedgerTxIn i) (fromLedgerTxOut o)

toLedgerTxOut :: TxOut CtxUTxO Era -> Ledger.TxOut (ShelleyLedgerEra Era)
toLedgerTxOut = toShelleyTxOut shelleyBasedEra

fromLedgerTxOut :: Ledger.TxOut (ShelleyLedgerEra Era) -> TxOut ctx Era
fromLedgerTxOut = fromShelleyTxOut shelleyBasedEra

fromCardanoApiUtxo :: Cardano.Api.UTxO AlonzoEra -> Utxo
fromCardanoApiUtxo = coerce

--
-- Witnesses
--

-- TODO: This could be made available upstream...
class IsScriptWitnessInCtx ctx where
  scriptWitnessCtx :: ScriptWitnessInCtx ctx

instance IsScriptWitnessInCtx WitCtxTxIn where
  scriptWitnessCtx = ScriptWitnessForSpending

instance IsScriptWitnessInCtx WitCtxMint where
  scriptWitnessCtx = ScriptWitnessForMinting

instance IsScriptWitnessInCtx WitCtxStake where
  scriptWitnessCtx = ScriptWitnessForStakeAddr

mkScriptWitness ::
  forall ctx.
  (IsScriptWitnessInCtx ctx) =>
  PlutusScript PlutusScriptV1 ->
  ScriptDatum ctx ->
  ScriptRedeemer ->
  Witness ctx Era
mkScriptWitness script datum redeemer =
  ScriptWitness scriptWitnessCtx witness
 where
  witness =
    PlutusScriptWitness
      PlutusScriptV1InAlonzo
      PlutusScriptV1
      script
      datum
      redeemer
      (ExecutionUnits 0 0)

toLedgerKeyWitness ::
  [KeyWitness era] ->
  Set (Ledger.Shelley.WitVKey 'Ledger.Witness Ledger.StandardCrypto)
toLedgerKeyWitness vkWits =
  fromList [w | ShelleyKeyWitness _ w <- vkWits]

toLedgerBootstrapWitness ::
  [KeyWitness era] ->
  Set (Ledger.BootstrapWitness Ledger.StandardCrypto)
toLedgerBootstrapWitness vkWits =
  fromList [w | ShelleyBootstrapWitness _ w <- vkWits]

fromLedgerTxWitness :: Ledger.Alonzo.TxWitness LedgerEra -> [KeyWitness Era]
fromLedgerTxWitness wits =
  Set.foldr ((:) . ShelleyKeyWitness era) [] (Ledger.Alonzo.txwitsVKey' wits)
    ++ Set.foldr ((:) . ShelleyBootstrapWitness era) [] (Ledger.Alonzo.txwitsBoot' wits)
 where
  era =
    ShelleyBasedEraAlonzo

--
-- Value
--

mkTxOutValue :: Value -> TxOutValue Era
mkTxOutValue =
  TxOutValue MultiAssetInAlonzoEra

fromLedgerValue :: Ledger.Mary.Value Ledger.StandardCrypto -> Value
fromLedgerValue =
  fromMaryValue

--
-- Formatting
--

-- TODO: Maybe consider using 'renderValue' from cardano-api instead?
prettyValue :: Value -> Text
prettyValue value =
  let Lovelace lovelace = fromMaybe 0 (valueToLovelace value)
      (ada, decimal) = lovelace `quotRem` 1000000
      n = length (valueToList value) - 1 -- Discarding ADA
   in unwords $
        [ show ada <> "." <> padLeft '0' 6 (show decimal)
        , "₳"
        ]
          ++ if n == 0
            then mempty
            else ["and", show n, "asset(s)"]

-- | Pad a text-string to left with the given character until it reaches the given
-- length.
--
-- NOTE: Truncate the string if longer than the given length.
-- TODO: Move into a separate module.
padLeft :: Char -> Int -> Text -> Text
padLeft c n str = T.takeEnd n (T.replicate n (T.singleton c) <> str)

--
-- Generators
--

genKeyPair :: Gen (VerificationKey PaymentKey, SigningKey PaymentKey)
genKeyPair = do
  -- NOTE: not using 'genKeyDSIGN' purposely here, it is not pure and does not
  -- play well with pure generation from seed.
  sk <- fromJust . CC.rawDeserialiseSignKeyDSIGN . fromList <$> vectorOf 64 arbitrary
  let vk = CC.deriveVerKeyDSIGN sk
  pure (PaymentVerificationKey (Ledger.VKey vk), PaymentSigningKey sk)

-- TODO: Generate non-genesis transactions for better coverage.
-- TODO: Enable Alonzo-specific features. We started off in the Mary era, and
-- some of our tests / interfaces aren't fully ready for Alonzo-specific
-- details. We later changed the ledger's internals to work with Alonzo-era
-- specific types, and, to make this in incremental steps, this function still
-- generates Mary transactions, but cast them to Alonzo's.
genTx :: Utxo -> Gen CardanoTx
genTx utxos = do
  fromLedgerTx <$> Ledger.Generator.genTx genEnv ledgerEnv (utxoState, dpState)
 where
  utxoState = def{Ledger._utxo = toLedgerUtxo utxos}
  dpState = Ledger.DPState def def

  -- NOTE(AB): This sets some parameters for the tx generator that will
  -- affect the structure of generated trasactions. In our case, we want
  -- to remove "special" capabilities which are irrelevant in the context
  -- of a Hydra head
  -- see https://github.com/input-output-hk/cardano-ledger-specs/blob/nil/shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Generator/Constants.hs#L10
  genEnv =
    (Ledger.Generator.genEnv Proxy)
      { Ledger.Generator.geConstants = noPPUpdatesNoScripts
      }
   where
    noPPUpdatesNoScripts =
      Ledger.Generator.defaultConstants
        { Ledger.Generator.frequencyTxUpdates = 0
        , Ledger.Generator.frequencyTxWithMetadata = 0
        , Ledger.Generator.maxCertsPerTx = 0
        }

genSequenceOfValidTransactions :: Utxo -> Gen [CardanoTx]
genSequenceOfValidTransactions initialUtxo
  | initialUtxo == mempty = pure []
  | otherwise = do
    n <- getSize
    numTxs <- choose (1, n)
    genFixedSizeSequenceOfValidTransactions numTxs initialUtxo

genFixedSizeSequenceOfValidTransactions :: Int -> Utxo -> Gen [CardanoTx]
genFixedSizeSequenceOfValidTransactions numTxs initialUtxo
  | initialUtxo == mempty = pure []
  | otherwise =
    reverse . snd <$> foldM newTx (initialUtxo, []) [1 .. numTxs]
 where
  newTx (utxos, acc) _ = do
    tx <- genTx utxos
    case applyTransactions cardanoLedger utxos [tx] of
      Left err -> error $ show err
      Right newUtxos -> pure (newUtxos, tx : acc)

-- TODO: Enable arbitrary datum in generators
genOutput ::
  forall era ctx.
  (IsShelleyBasedEra era) =>
  VerificationKey PaymentKey ->
  Gen (TxOut ctx era)
genOutput vk = do
  assets <- fromLedgerValue <$> scale (* 8) arbitrary
  let value =
        either
          (`TxOutAdaOnly` selectLovelace assets)
          (`TxOutValue` assets)
          (multiAssetSupportedInEra (cardanoEra @era))
  pure $ TxOut (mkVkAddress (Testnet $ NetworkMagic 42) vk) value TxOutDatumNone

genUtxo :: Gen Utxo
genUtxo = do
  genesisTxId <- arbitrary
  utxo <- Ledger.Generator.genUtxo0 (Ledger.Generator.genEnv Proxy)
  pure $ fromLedgerUtxo . Ledger.UTxO . Map.mapKeys (setTxId genesisTxId) $ Ledger.unUTxO utxo
 where
  setTxId ::
    Ledger.TxId Ledger.StandardCrypto ->
    Ledger.TxIn Ledger.StandardCrypto ->
    Ledger.TxIn Ledger.StandardCrypto
  setTxId baseId (Ledger.TxIn _ti wo) = Ledger.TxIn baseId wo

-- | Generate utxos owned by the given cardano key.
genUtxoFor :: VerificationKey PaymentKey -> Gen Utxo
genUtxoFor vk = do
  n <- arbitrary `suchThat` (> 0)
  inputs <- vectorOf n arbitrary
  outputs <- vectorOf n (genOutput vk)
  pure $ Utxo $ Map.fromList $ zip (fromLedgerTxIn <$> inputs) outputs

-- | Generate a single UTXO owned by 'vk'.
genOneUtxoFor :: VerificationKey PaymentKey -> Gen Utxo
genOneUtxoFor vk = do
  input <- arbitrary
  -- NOTE(AB): calling this generator while running a property will yield larger and larger
  -- values (quikcheck increases the 'size' parameter upon success) up to the point they are
  -- too large to fit in a transaction and validation fails in the ledger
  output <- scale (const 1) $ genOutput vk
  pure $ Utxo $ Map.singleton (fromLedgerTxIn input) output

-- | Generate UTXO entries that do not contain any assets. Useful to test /
-- measure cases where
genAdaOnlyUtxo :: Gen Utxo
genAdaOnlyUtxo = do
  fmap adaOnly <$> arbitrary
 where
  adaOnly :: TxOut CtxUTxO AlonzoEra -> TxOut CtxUTxO AlonzoEra
  adaOnly = \case
    TxOut addr value datum ->
      TxOut addr (lovelaceToTxOutValue $ txOutValueToLovelace value) datum

shrinkUtxo :: Utxo -> [Utxo]
shrinkUtxo = shrinkMapBy (Utxo . fromList) utxoPairs (shrinkList shrinkOne)
 where
  shrinkOne :: (TxIn, TxOut CtxUTxO AlonzoEra) -> [(TxIn, TxOut CtxUTxO AlonzoEra)]
  shrinkOne (i, o) = case o of
    TxOut _ TxOutAdaOnly{} _ ->
      []
    TxOut addr (TxOutValue MultiAssetInAlonzoEra value) datum ->
      [ (i, TxOut addr (TxOutValue MultiAssetInAlonzoEra value') datum)
      | value' <- shrinkValue value
      ]

shrinkValue :: Value -> [Value]
shrinkValue =
  shrinkMapBy valueFromList valueToList shrinkListAggressively

--
-- Temporary / Quick-n-dirty
--

-- NOTE: The constructor for Hash isn't exposed in the cardano-api. Although
-- there's a 'CastHash' type-class, there are not instances for everything, so
-- we have to resort to binary serialisation/deserialisation to cast hashes.
unsafeCastHash ::
  (SerialiseAsCBOR (Hash a), SerialiseAsCBOR (Hash b), HasCallStack) =>
  Hash a ->
  Hash b
unsafeCastHash a =
  either
    (\e -> error $ "unsafeCastHash: incompatible hash: " <> show e)
    identity
    (deserialiseFromCBOR (proxyToAsType Proxy) (serialiseToCBOR a))

-- FIXME: Do not hard-code this, make it configurable / inferred from the
-- genesis configuration.
ledgerEnv :: Ledger.LedgerEnv LedgerEra
ledgerEnv =
  Ledger.LedgerEnv
    { Ledger.ledgerSlotNo = SlotNo 1
    , Ledger.ledgerIx = 0
    , Ledger.ledgerPp =
        def
          { Ledger.Alonzo._maxTxSize = 1024 * 1024
          , Ledger.Alonzo._maxValSize = 5000
          , Ledger.Alonzo._maxCollateralInputs = 10
          , Ledger.Alonzo._maxTxExUnits =
              Ledger.Alonzo.ExUnits
                { Ledger.Alonzo.exUnitsMem = 10_000_000
                , Ledger.Alonzo.exUnitsSteps = 10_000_000_000
                }
          , Ledger.Alonzo._maxBlockExUnits =
              Ledger.Alonzo.ExUnits
                { Ledger.Alonzo.exUnitsMem = 50_000_000
                , Ledger.Alonzo.exUnitsSteps = 40_000_000_000
                }
          , Ledger.Alonzo._costmdls =
              -- XXX(SN): This is a sledgehammer approach: The genTx would hit
              -- execution budgets with the defaultCostModel. There is a TODO in
              -- cardano-ledger's AlonzoEraGen.hs about not using freeCostModel
              Map.fromList $
                [ (lang, Ledger.Alonzo.freeCostModel)
                | lang <- [minBound .. maxBound]
                ]
          }
    , Ledger.ledgerAccount = error "ledgerEnv: ledgersAccount undefined"
    }

-- FIXME: Do not hard-code this, make it configurable / inferred from the
-- genesis configuration.
--
-- From: shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Utils.hs
globals :: Ledger.Globals
globals =
  Ledger.Globals
    { Ledger.epochInfoWithErr = Slotting.fixedEpochInfo (Ledger.EpochSize 100) (Slotting.mkSlotLength 1)
    , Ledger.slotsPerKESPeriod = 20
    , Ledger.stabilityWindow = 33
    , Ledger.randomnessStabilisationWindow = 33
    , Ledger.securityParameter = 10
    , Ledger.maxKESEvo = 10
    , Ledger.quorum = 5
    , Ledger.maxMajorPV = 1000
    , Ledger.maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , Ledger.activeSlotCoeff = Ledger.mkActiveSlotCoeff . unsafeBoundRational $ 0.9
    , Ledger.networkId = Ledger.Testnet
    , Ledger.systemStart = Slotting.SystemStart $ posixSecondsToUTCTime 0
    }
 where
  unsafeBoundRational r =
    fromMaybe (error $ "Could not convert from Rational: " <> show r) $ Ledger.boundRational r
