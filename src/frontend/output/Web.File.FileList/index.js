// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var item = function (i) {
    var $1 = $foreign["_item"](i);
    return function ($2) {
        return Data_Nullable.toMaybe($1($2));
    };
};
var items = function (dictUnfoldable) {
    return function (fl) {
        return Data_Unfoldable.unfoldr(dictUnfoldable)(function (i) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Tuple.Tuple.create)(i + 1 | 0))(item(i)(fl));
        })(0);
    };
};
module.exports = {
    item: item,
    items: items,
    length: $foreign.length
};