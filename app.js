const BLOCKFROST_KEY = "preprodIb3LJHd44Ilolee1XVbQPRLS1eIAh1xb";
// Placeholder Script Hex (Plutus V2)
const SCRIPT_HEX = "5856585401000032323232323222253330044a2224410000014a22244100000100414a222441000001";

let lucid;

async function init() {
    const status = document.getElementById("status");
    const walletInfo = document.getElementById("wallet-info");
    
    try {
        // Fix for "Lucid not defined" or bundle namespace issues
        if (typeof Lucid === 'undefined') {
            throw new Error("Lucid library failed to load. Check your internet connection.");
        }

        // The bundle usually attaches to window.Lucid
        const LucidLib = Lucid.Lucid ? Lucid.Lucid : Lucid;
        const BlockfrostLib = Lucid.Blockfrost ? Lucid.Blockfrost : Blockfrost;

        lucid = await LucidLib.new(
            new BlockfrostLib("https://cardano-preprod.blockfrost.io/api/v0", BLOCKFROST_KEY),
            "Preprod"
        );

        // Detect available wallet
        const walletName = window.cardano.eternl ? 'eternl' : 
                         window.cardano.nami ? 'nami' : 
                         window.cardano.flint ? 'flint' : 
                         window.cardano.lace ? 'lace' : null;

        if (!walletName) throw new Error("No Cardano wallet found (Eternl/Nami/Lace needed).");

        const api = await window.cardano[walletName].enable();
        lucid.selectWallet(api);

        const addr = await lucid.wallet.address();
        walletInfo.innerText = `Connected: ${addr.substring(0, 15)}...`;
        status.innerText = "System Ready ✅";
    } catch (e) {
        status.innerText = "Error: " + e.message;
        console.error(e);
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

        if (!charityPkh || !auditorPkh || isNaN(deadline)) throw new Error("Please fill all fields.");

        // Data structure: [Donor, Charity, Auditor, Deadline]
        const datum = Lucid.Data.to([donorPkh, charityPkh, auditorPkh, BigInt(deadline)]);
        const scriptAddr = lucid.utils.validatorToAddress({ type: "PlutusV2", script: SCRIPT_HEX });

        status.innerText = "Submitting to Blockchain...";
        const tx = await lucid.newTx()
            .payToContract(scriptAddr, { inline: datum }, { lovelace: BigInt(amount * 1000000) })
            .complete();

        const signed = await tx.sign().complete();
        const hash = await signed.submit();
        status.innerText = "Success! Tx ID: " + hash;
        alert("Locked! TX: " + hash);
    } catch (e) {
        status.innerText = "Error: " + e.message;
    }
}

async function release() {
    const status = document.getElementById("status");
    try {
        if (!lucid) await init();
        status.innerText = "Searching for locked funds...";
        
        const script = { type: "PlutusV2", script: SCRIPT_HEX };
        const scriptAddr = lucid.utils.validatorToAddress(script);
        const utxos = await lucid.utxosAt(scriptAddr);

        if (utxos.length === 0) throw new Error("No active donations found in this contract.");

        // In a real scenario, you'd filter UTXOs by your specific datum
        const tx = await lucid.newTx()
            .collectFrom(utxos, Lucid.Data.empty())
            .attachSpendingValidator(script)
            .addSigner(await lucid.wallet.address())
            .complete();

        const signed = await tx.sign().complete();
        const hash = await signed.submit();
        status.innerText = "Release Successful! Tx: " + hash;
    } catch (e) {
        status.innerText = "Error: " + e.message;
    }
}

// Start when page loads
window.addEventListener('load', init);