
module Hydra.ThreatModel where

import Prelude
import Data.Coerce

import Cardano.Api.UTxO as Hydra
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.Contract.Abort

import Test.QuickCheck
import Test.QuickCheck.ThreatModel
import Test.QuickCheck.ThreatModel.TxModifier ()

makeThreatModelEnv :: (Tx Hydra.Era, Hydra.UTxO) -> ThreatModelEnv
makeThreatModelEnv (tx, utxo) = ThreatModelEnv
  { currentTx    = tx
  , currentUTxOs = coerce utxo
  , pparams      = LedgerProtocolParameters Fixture.pparams
  }

someThreatModel :: ThreatModel ()
someThreatModel = do
  inp <- anyReferenceInput
  shouldValidate $ removeInput inp    -- Fails!
  pure ()

prop_something :: Property
prop_something = runThreatModel someThreatModel [makeThreatModelEnv healthyAbortTx]

