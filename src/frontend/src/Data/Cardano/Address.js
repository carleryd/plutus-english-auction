"use strict";

exports.fromBech32Impl =
  (cardanoWasm) => (bechStr) => (onError) => (onSuccess) => {
    console.log("[UPDATED 1] fromBech32Impl", bechStr);
    window.cardanoWasm1 = cardanoWasm;
    try {
      return onSuccess(cardanoWasm.Address.from_bech32(bechStr));
    } catch (e) {
      return onError(e);
    }
  };

exports.toBech32 = (address) => address.to_bech32();

exports.fromBytesImpl =
  (cardanoWasm) => (hexAddr) => (onError) => (onSuccess) => {
    console.log("[UPDATED 1] fromBytesImpl", hexAddr);
    window.cardanoWasm2 = cardanoWasm;
    window.hexAddr = hexAddr;
    window.Buffer = Buffer;
    try {
      return onSuccess(
        cardanoWasm.Address.from_bytes(Buffer.from(hexAddr, "hex"))
      );
    } catch (e) {
      return onError(e);
    }
  };
