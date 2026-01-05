
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

-- | Charity Milestone Escrow
-- Funds are released only if the 'Auditor' and 'Charity' both sign.
-- Donor can reclaim funds if the deadline has passed.

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

    charityCanClaim = txSignedBy info (charityPkh dat)
    auditorApproved = txSignedBy info (auditorPkh dat)

    donorCanReclaim = txSignedBy info (donorPkh dat)
    deadlinePassed  = contains (from (deadline dat)) (txInfoValidRange info)

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkEscrowValidator ||])
