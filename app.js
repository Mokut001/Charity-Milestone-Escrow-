const BLOCKFROST_KEY = "preprodIb3LJHd44Ilolee1XVbQPRLS1eIAh1xb";
// Placeholder script - in real life, you'd compile the Haskell above to get this Hex
const SCRIPT_HEX = "5856585401000032323232323222253330044a2224410000014a22244100000100414a222441000001";

let lucid;

async function init() {
    const status = document.getElementById("status");
    try {
        lucid = await Lucid.new(
            new Blockfrost("https://cardano-preprod.blockfrost.io/api/v0", BLOCKFROST_KEY),
            "Preprod"
        );
        const api = await window.cardano.eternl.enable();
        lucid.selectWallet(api);
        status.innerText = "Wallet Connected ✅";
    } catch (e) {
        status.innerText = "Connection Error: " + e.message;
    }
}

async function deposit() {
    const status = document.getElementById("status");
    try {
        if (!lucid) await init();
        
        const donorAddr = await lucid.wallet.address();
        const donorPkh = lucid.utils.getAddressDetails(donorAddr).paymentCredential.hash;
        const charityPkh = document.getElementById("charityPkh").value;
        const auditorPkh = document.getElementById("auditorPkh").value;
        const deadline = new Date(document.getElementById("deadline").value).getTime();
        const amount = document.getElementById("amt").value;

        // Datum: [Donor, Charity, Auditor, Deadline]
        const datum = Lucid.Data.to([donorPkh, charityPkh, auditorPkh, BigInt(deadline)]);
        const scriptAddr = lucid.utils.validatorToAddress({ type: "PlutusV2", script: SCRIPT_HEX });

        status.innerText = "Creating Escrow...";
        const tx = await lucid.newTx()
            .payToContract(scriptAddr, { inline: datum }, { lovelace: BigInt(amount * 1000000) })
            .complete();

        const signed = await tx.sign().complete();
        const hash = await signed.submit();
        status.innerText = "Funds Locked! Tx: " + hash;
    } catch (e) {
        status.innerText = "Error: " + e.message;
    }
}

async function release() {
    const status = document.getElementById("status");
    try {
        if (!lucid) await init();
        status.innerText = "Building Release Tx (Multi-sig)...";
        
        const script = { type: "PlutusV2", script: SCRIPT_HEX };
        const scriptAddr = lucid.utils.validatorToAddress(script);
        const utxos = await lucid.utxosAt(scriptAddr);

        if (utxos.length === 0) throw new Error("No active escrows found.");

        const tx = await lucid.newTx()
            .collectFrom(utxos, Lucid.Data.to(new Int8Array([])))
            .attachSpendingValidator(script)
            .addSigner(await lucid.wallet.address()) // Charity signs
            // In a real DApp, you'd send this to the Auditor to sign too
            .complete();

        const signed = await tx.sign().complete();
        const hash = await signed.submit();
        status.innerText = "Milestone Approved! Tx: " + hash;
    } catch (e) {
        status.innerText = "Error: " + e.message;
    }
}

window.addEventListener('load', init);