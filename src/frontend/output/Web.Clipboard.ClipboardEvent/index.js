// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toEvent = Unsafe_Coerce.unsafeCoerce;
var fromEvent = Web_Internal_FFI.unsafeReadProtoTagged("ClipboardEvent");
var clipboardData = function ($0) {
    return Data_Nullable.toMaybe($foreign["_clipboardData"]($0));
};
module.exports = {
    fromEvent: fromEvent,
    toEvent: toEvent,
    clipboardData: clipboardData
};