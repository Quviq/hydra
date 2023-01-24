{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Cardano.Api
import Hydra.Chain.Direct.Tx
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Ledger.Babbage.PParams (PParams)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Natural (wordToNatural)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Fixture (
  epochInfo,
  genForParty,
  pparams,
  systemStart,
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_)
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import qualified Hydra.Contract.Initial as Initial
import Hydra.Ledger.Cardano (
  adaOnly,
  genOneUTxOFor,
  genVerificationKey,
  renderTx,
 )
import Hydra.Ledger.Cardano.Evaluate (EvaluationReport, maxTxExecutionUnits)
import Hydra.Party (Party)
import Plutus.V2.Ledger.Api (toData)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (
  Property,
  choose,
  counterexample,
  elements,
  forAll,
  getPositive,
  label,
  property,
  vectorOf,
  withMaxSuccess,
 )
import Test.QuickCheck.Instances.Semigroup ()

spec :: Spec
spec =
  parallel $ do
    describe "collectComTx" $ do
      prop "cover fee correctly handles redeemers" $
        withMaxSuccess 60 $ \txIn cperiod (party :| parties) cardanoKeys walletUTxO ->
          forAll (genForParty genVerificationKey <$> elements (party : parties)) $ \signer ->
            forAll genScriptRegistry $ \scriptRegistry ->
              let params = HeadParameters cperiod (party : parties)
                  tx = initTx testNetworkId cardanoKeys params txIn
               in case observeInitTx testNetworkId cardanoKeys cperiod party tx of
                    Just InitObservation{initials, threadOutput} -> do
                      let InitialThreadOutput{initialThreadUTxO = (headInput, headOutput, headDatum)} = threadOutput
                          initials' = Map.fromList [(a, (b, c)) | (a, b, c) <- initials]
                          lookupUTxO =
                            mconcat
                              [ Map.fromList ((headInput, headOutput) : [(a, b) | (a, b, _) <- initials])
                              , UTxO.toMap (registryUTxO scriptRegistry)
                              ]
                              & Map.mapKeys toLedgerTxIn
                              & Map.map toLedgerTxOut
                       in case abortTx mempty scriptRegistry signer (headInput, headOutput, headDatum) (mkHeadTokenScript testSeedInput) initials' mempty of
                            Left err ->
                              property False & counterexample ("AbortTx construction failed: " <> show err)
                            Right (toLedgerTx -> txAbort) ->
                              case coverFee_ ledgerPParams systemStart epochInfo lookupUTxO walletUTxO txAbort of
                                Left err ->
                                  True
                                    & label
                                      ( case err of
                                          ErrNoFuelUTxOFound{} -> "No fuel UTxO found"
                                          ErrNotEnoughFunds{} -> "Not enough funds"
                                          ErrUnknownInput{} -> "Unknown input"
                                          ErrScriptExecutionFailed{} -> "Script(s) execution failed"
                                          ErrTranslationError{} -> "Transaction context translation error"
                                      )
                                Right (fromLedgerTx -> txAbortWithFees) ->
                                  let actualExecutionCost = totalExecutionCost ledgerPParams txAbortWithFees
                                      fee = txFee' txAbortWithFees
                                   in actualExecutionCost > Lovelace 0 && fee > actualExecutionCost
                                        & label "Ok"
                                        & counterexample ("Execution cost: " <> show actualExecutionCost)
                                        & counterexample ("Fee: " <> show fee)
                                        & counterexample ("Tx: " <> show txAbortWithFees)
                                        & counterexample ("Input utxo: " <> show (walletUTxO <> lookupUTxO))
                    _ ->
                      property False
                        & counterexample "Failed to construct and observe init tx."
                        & counterexample (renderTx tx)

      prop "Ignore InitTx with wrong contestation period" $
        withMaxSuccess 60 $ \txIn cperiod (party :| parties) cardanoKeys -> do
          i <- getPositive <$> arbitrary
          let params = HeadParameters cperiod (party : parties)
              (UnsafeContestationPeriod cp) = cperiod
              -- construct different/wrong CP
              wrongCPeriod = UnsafeContestationPeriod $ cp + wordToNatural i
              tx = initTx testNetworkId cardanoKeys params txIn
          pure $ case observeInitTx testNetworkId cardanoKeys wrongCPeriod party tx of
            Just InitObservation{} -> do
              property False
                & counterexample "Failed to ignore init tx with the wrong contestation period."
                & counterexample (renderTx tx)
            Nothing -> property True

ledgerPParams :: PParams LedgerEra
ledgerPParams = toLedgerPParams (shelleyBasedEra @Era) pparams

withinTxExecutionBudget :: EvaluationReport -> Property
withinTxExecutionBudget report =
  ( totalMem <= maxMem
      && totalCpu <= maxCpu
  )
    & counterexample
      ( "Ex. Cost Limits exceeded, mem: "
          <> show totalMem
          <> "/"
          <> show maxMem
          <> ", cpu: "
          <> show totalCpu
          <> "/"
          <> show maxCpu
      )
 where
  budgets = rights $ Map.elems report
  totalMem = sum $ executionMemory <$> budgets
  totalCpu = sum $ executionSteps <$> budgets
  ExecutionUnits
    { executionMemory = maxMem
    , executionSteps = maxCpu
    } = maxTxExecutionUnits

-- | Generate a UTXO representing /commit/ outputs for a given list of `Party`.
-- NOTE: Uses 'testPolicyId' for the datum.
-- NOTE: We don't generate empty commits and it is used only at one place so perhaps move it?
-- FIXME: This function is very complicated and it's hard to understand it after a while
generateCommitUTxOs :: [Party] -> Gen (Map.Map TxIn (TxOut CtxUTxO, ScriptData, UTxO))
generateCommitUTxOs parties = do
  txins <- vectorOf (length parties) (arbitrary @TxIn)
  let vks = (\p -> (genVerificationKey `genForParty` p, p)) <$> parties
  committedUTxO <-
    vectorOf (length parties) $ do
      singleUTxO <- fmap adaOnly <$> (genOneUTxOFor =<< arbitrary)
      pure $ head <$> nonEmpty (UTxO.pairs singleUTxO)
  let commitUTxO =
        zip txins $
          uncurry mkCommitUTxO <$> zip vks committedUTxO
  pure $ Map.fromList commitUTxO
 where
  mkCommitUTxO :: (VerificationKey PaymentKey, Party) -> Maybe (TxIn, TxOut CtxUTxO) -> (TxOut CtxUTxO, ScriptData, UTxO)
  mkCommitUTxO (vk, party) utxo =
    ( toUTxOContext $
        TxOut
          (mkScriptAddress @PlutusScriptV2 testNetworkId commitScript)
          commitValue
          (mkTxOutDatum commitDatum)
          ReferenceScriptNone
    , fromPlutusData (toData commitDatum)
    , maybe mempty (UTxO.fromPairs . pure) utxo
    )
   where
    commitValue =
      mconcat
        [ lovelaceToValue (Lovelace 2000000)
        , maybe mempty (txOutValue . snd) utxo
        , valueFromList
            [ (AssetId testPolicyId (assetNameFromVerificationKey vk), 1)
            ]
        ]

    commitScript = fromPlutusScript Commit.validatorScript

    commitDatum = mkCommitDatum party Head.validatorHash utxo (toPlutusCurrencySymbol testPolicyId)

prettyEvaluationReport :: EvaluationReport -> String
prettyEvaluationReport (Map.toList -> xs) =
  "Script Evaluation(s):\n" <> intercalate "\n" (prettyKeyValue <$> xs)
 where
  prettyKeyValue (ptr, result) =
    toString ("  - " <> show ptr <> ": " <> prettyResult result)
  prettyResult =
    either (T.replace "\n" " " . show) show

-- NOTE: Uses 'testPolicyId' for the datum.
genAbortableOutputs :: [Party] -> Gen ([UTxOWithScript], [(TxIn, TxOut CtxUTxO, ScriptData, UTxO)])
genAbortableOutputs parties =
  go
 where
  go = do
    (initParties, commitParties) <- (`splitAt` parties) <$> choose (0, length parties)
    initials <- mapM genInitial initParties
    commits <- fmap (\(a, (b, c, u)) -> (a, b, c, u)) . Map.toList <$> generateCommitUTxOs commitParties
    pure (initials, commits)

  genInitial p =
    mkInitial (genVerificationKey `genForParty` p) <$> arbitrary

  mkInitial ::
    VerificationKey PaymentKey ->
    TxIn ->
    UTxOWithScript
  mkInitial vk txin =
    ( txin
    , initialTxOut vk
    , fromPlutusData (toData initialDatum)
    )

  initialTxOut :: VerificationKey PaymentKey -> TxOut CtxUTxO
  initialTxOut vk =
    toUTxOContext $
      TxOut
        (mkScriptAddress @PlutusScriptV2 testNetworkId initialScript)
        ( headValue
            <> valueFromList
              [ (AssetId testPolicyId (assetNameFromVerificationKey vk), 1)
              ]
        )
        (mkTxOutDatum initialDatum)
        ReferenceScriptNone

  initialScript = fromPlutusScript Initial.validatorScript

  initialDatum = Initial.datum (toPlutusCurrencySymbol testPolicyId)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

third :: (a, b, c) -> c
third (_, _, c) = c

drop2nd :: (a, b, c) -> (a, c)
drop2nd (a, _, c) = (a, c)

drop3rd :: (a, b, c) -> (a, b)
drop3rd (a, b, _) = (a, b)

tripleToPair :: (a, b, c) -> (a, (b, c))
tripleToPair (a, b, c) = (a, (b, c))
