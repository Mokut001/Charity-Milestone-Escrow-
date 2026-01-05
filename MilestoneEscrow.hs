
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module MilestoneEscrow where

import           PlutusV2.Ledger.Api
import           PlutusV2.Ledger.Contexts
import           PlutusTx.Prelude
import qualified PlutusTx

-- | The problem: Donor accountability for Charities.
-- Funds are only released if the 'Auditor' signs off on the milestone.
-- If the deadline passes, the Donor can take the money back.

data EscrowDatum = EscrowDatum
    { donorPkh      :: PubKeyHash
    , charityPkh    :: PubKeyHash
    , auditorPkh    :: PubKeyHash
    , deadline      :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''EscrowDatum

{-# INLINEABLE mkEscrowValidator #-}
mkEscrowValidator :: EscrowDatum -> () -> ScriptContext -> Bool
mkEscrowValidator dat _ ctx = 
    (charityCanClaim && auditorApproved) || (donorCanReclaim && deadlinePassed)
  where
    info = scriptContextTxInfo ctx

    -- Condition 1: Charity claims with Auditor's signature
    charityCanClaim = txSignedBy info (charityPkh dat)
    auditorApproved = txSignedBy info (auditorPkh dat)

    -- Condition 2: Donor reclaims after deadline
    donorCanReclaim = txSignedBy info (donorPkh dat)
    deadlinePassed  = contains (from (deadline dat)) (txInfoValidRange info)

-- Compiled Plutus Script
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkEscrowValidator ||])
