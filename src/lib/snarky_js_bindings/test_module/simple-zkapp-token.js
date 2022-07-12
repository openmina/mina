import {
  Circuit,
  Field,
  declareState,
  declareMethods,
  State,
  PrivateKey,
  SmartContract,
  isReady,
  shutdown,
  Mina,
  Permissions,
  Ledger,
  Party,
  UInt64,
  Bool,
  partiesToJson,
  Token,
} from "snarkyjs";

function sendTransaction(tx) {
  console.log("DEBUG -- TXN\n", JSON.stringify(partiesToJson(tx.transaction)));
  tx.send();
}

await isReady;

// declare the zkapp
class SimpleZkapp extends SmartContract {
  constructor(address) {
    super(address);
    this.x = State();
  }

  deploy(args) {
    super.deploy(args);
    this.setPermissions({
      ...Permissions.default(),
      editState: Permissions.proofOrSignature(),
    });
    this.balance.addInPlace(UInt64.fromNumber(initialBalance));
    this.x.set(initialState);
  }

  update(y) {
    let x = this.x.get();
    this.x.set(x.add(y));
  }

  initialize() {
    this.x.set(initialState);
  }

  mint(receiver) {
    let recieverAddress = receiver.toPublicKey();

    this.token.mint({
      address: recieverAddress,
      amount: 1_000_000_000,
    });

    console.log(`Minting ${1_000_000_000} to ${recieverAddress.toBase58()}`);
  }

  burn(receiver) {
    let recieverAddress = receiver.toPublicKey();

    this.token.burn({
      address: recieverAddress,
      amount: 100,
    });

    console.log(`Burning ${100} to ${recieverAddress.toBase58()}`);
  }

  send(sender, receiver) {
    let recieverAddress = receiver.toPublicKey();
    let senderAddress = sender.toPublicKey();

    this.token.transfer({
      from: senderAddress,
      to: recieverAddress,
      amount: 100,
    });

    console.log(`Sending ${100} to ${recieverAddress.toBase58()}`);
  }
}
// note: this is our non-typescript way of doing what our decorators do
declareState(SimpleZkapp, { x: Field });
declareMethods(SimpleZkapp, {
  initialize: [],
  update: [Field],
  send: [PrivateKey, PrivateKey],
  mint: [PrivateKey],
  burn: [PrivateKey],
});

let Local = Mina.LocalBlockchain();
Mina.setActiveInstance(Local);

// a test account that pays all the fees, and puts additional funds into the zkapp
let feePayer = Local.testAccounts[0].privateKey;

// the zkapp account
let zkappKey = PrivateKey.fromBase58(
  "EKEfEZpMctKoyon4nxhqFBiKyUsCyyZReF9fbs21nDrrTgGMTcok"
);
let zkappAddress = zkappKey.toPublicKey();

// a special account that is allowed to pull out half of the zkapp balance, once
let privilegedKey = Local.testAccounts[1].privateKey;
let privilegedAddress = privilegedKey.toPublicKey();

let privilegedKey1 = Local.testAccounts[2].privateKey;
let privilegedAddress1 = privilegedKey1.toPublicKey();

let initialBalance = 10_000_000_000;
let initialState = Field(1);
let zkapp = new SimpleZkapp(zkappAddress);

console.log("deploy");
let tx = await Local.transaction(feePayer, () => {
  Party.fundNewAccount(feePayer, { initialBalance });
  zkapp.deploy({ zkappKey });
});
tx.send();

console.log(`initial balance: ${zkapp.account.balance.get().div(1e9)} MINA`);

// Log custom token info
const customToken = new Token({ tokenOwner: zkappAddress });
console.log("---FEE PAYER", feePayer.toPublicKey().toBase58());
console.log("---TOKEN OWNER", zkappAddress.toBase58());
console.log("---CUSTOM TOKEN", customToken.id);
console.log("---TOKEN ACCOUNT1", privilegedAddress.toBase58());
console.log("---TOKEN ACCOUNT2", privilegedAddress1.toBase58());

console.log("----------token minting----------");
tx = await Local.transaction(feePayer, () => {
  Party.fundNewAccount(feePayer);
  zkapp.mint(privilegedKey);
  zkapp.sign(zkappKey);
});
sendTransaction(tx);

console.log(
  `token_account_1 balance: ${JSON.stringify(
    Local.getAccount(privilegedAddress, customToken.id).balance.value
  )} custom tokens`
);

console.log("----------token burning----------");
tx = await Local.transaction(feePayer, () => {
  zkapp.burn(privilegedKey);
  zkapp.sign(zkappKey);
});
tx = tx.sign([privilegedKey]);
sendTransaction(tx);

console.log(
  `token_account_1 balance: ${JSON.stringify(
    Local.getAccount(privilegedAddress, customToken.id).balance.value
  )} custom tokens`
);

console.log("----------token transfer----------");
tx = await Local.transaction(feePayer, () => {
  Party.fundNewAccount(feePayer);
  zkapp.send(privilegedKey, privilegedKey1);
  zkapp.sign(zkappKey);
});
tx = tx.sign([privilegedKey, privilegedKey1]);
sendTransaction(tx);

console.log(
  `token_account_1 balance: ${JSON.stringify(
    Local.getAccount(privilegedAddress, customToken.id).balance.value
  )} custom tokens`
);

console.log(
  `token_account_2 balance: ${JSON.stringify(
    Local.getAccount(privilegedAddress1, customToken.id).balance.value
  )} custom tokens`
);

shutdown();
