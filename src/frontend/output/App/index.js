"use strict";
var Affjax = require("../Affjax/index.js");
var Affjax_ResponseFormat = require("../Affjax.ResponseFormat/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Data_Argonaut_Core = require("../Data.Argonaut.Core/index.js");
var Data_Argonaut_Decode_Class = require("../Data.Argonaut.Decode.Class/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HTTP_Method = require("../Data.HTTP.Method/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Aff_Class = require("../Effect.Aff.Class/index.js");
var Effect_Class_Console = require("../Effect.Class.Console/index.js");
var Halogen_Aff_Util = require("../Halogen.Aff.Util/index.js");
var Halogen_Component = require("../Halogen.Component/index.js");
var Halogen_HTML_Core = require("../Halogen.HTML.Core/index.js");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements/index.js");
var Halogen_HTML_Events = require("../Halogen.HTML.Events/index.js");
var Halogen_Query_HalogenM = require("../Halogen.Query.HalogenM/index.js");
var Halogen_VDom_Driver = require("../Halogen.VDom.Driver/index.js");
var Initialize = (function () {
    function Initialize() {

    };
    Initialize.value = new Initialize();
    return Initialize;
})();
var FetchBalances = (function () {
    function FetchBalances() {

    };
    FetchBalances.value = new FetchBalances();
    return FetchBalances;
})();

// TODO: Json for now, should be properly decoded datatype in future
var renderWallet = function (wallet) {
    return Halogen_HTML_Elements.p_([ Halogen_HTML_Core.text(Data_Argonaut_Core.stringify(wallet)) ]);
};
var render = function (state) {
    return Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.h1_([ Halogen_HTML_Core.text("Balances") ]), Data_Maybe.fromMaybe(Halogen_HTML_Core.text("No wallets found"))(Data_Functor.map(Data_Maybe.functorMaybe)(function (wb) {
        return Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.h2_([ Halogen_HTML_Core.text("Wallet 1") ]), renderWallet(wb.w1), Halogen_HTML_Elements.h2_([ Halogen_HTML_Core.text("Wallet 2") ]), renderWallet(wb.w2) ]);
    })(state)), Halogen_HTML_Elements.button([ Halogen_HTML_Events.onClick(function (v) {
        return FetchBalances.value;
    }) ])([ Halogen_HTML_Core.text("Refresh balances") ]) ]);
};
var jsonToWb = Data_Argonaut_Decode_Class.decodeJson(Data_Argonaut_Decode_Class.decodeRecord(Data_Argonaut_Decode_Class.gDecodeJsonCons(Data_Argonaut_Decode_Class.decodeFieldId(Data_Argonaut_Decode_Class.decodeJsonJson))(Data_Argonaut_Decode_Class.gDecodeJsonCons(Data_Argonaut_Decode_Class.decodeFieldId(Data_Argonaut_Decode_Class.decodeJsonJson))(Data_Argonaut_Decode_Class.gDecodeJsonNil)({
    reflectSymbol: function () {
        return "w2";
    }
})()())({
    reflectSymbol: function () {
        return "w1";
    }
})()())());
var initialState = function (v) {
    return Data_Maybe.Nothing.value;
};
var handleAction = function (dictMonadAff) {
    return function (v) {
        if (v instanceof Initialize) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(handleAction(dictMonadAff)(FetchBalances.value))(function () {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (balances) {
                    return Effect_Class_Console.log(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))("Balances: " + Data_Maybe.maybe("None")(function (wb) {
                        return Data_Argonaut_Core.stringify(wb.w1) + Data_Argonaut_Core.stringify(wb.w2);
                    })(balances));
                });
            });
        };
        if (v instanceof FetchBalances) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Aff_Class.liftAff(Halogen_Query_HalogenM.monadAffHalogenM(dictMonadAff))(Affjax.request({
                method: new Data_Either.Left(Data_HTTP_Method.GET.value),
                url: "/balances",
                headers: Affjax.defaultRequest.headers,
                content: Affjax.defaultRequest.content,
                username: Affjax.defaultRequest.username,
                password: Affjax.defaultRequest.password,
                withCredentials: Affjax.defaultRequest.withCredentials,
                responseFormat: Affjax_ResponseFormat.json,
                timeout: Affjax.defaultRequest.timeout
            })))(function (result) {
                if (result instanceof Data_Either.Left) {
                    return Effect_Class_Console.log(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))("GET /balances response failed to decode: " + Affjax.printError(result.value0));
                };
                if (result instanceof Data_Either.Right) {
                    return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(Data_Either.fromRight(Data_Maybe.Nothing.value)(Data_Functor.map(Data_Either.functorEither)(Data_Maybe.Just.create)(jsonToWb(result.value0.body))));
                };
                throw new Error("Failed pattern match at App (line 104, column 5 - line 107, column 68): " + [ result.constructor.name ]);
            });
        };
        throw new Error("Failed pattern match at App (line 83, column 16 - line 107, column 68): " + [ v.constructor.name ]);
    };
};
var component = function (dictMonadAff) {
    return Halogen_Component.mkComponent({
        initialState: initialState,
        render: render,
        "eval": Halogen_Component.mkEval({
            handleAction: handleAction(dictMonadAff),
            handleQuery: Halogen_Component.defaultEval.handleQuery,
            receive: Halogen_Component.defaultEval.receive,
            initialize: new Data_Maybe.Just(Initialize.value),
            finalize: Halogen_Component.defaultEval.finalize
        })
    });
};
var main = Halogen_Aff_Util.runHalogenAff(Control_Bind.bind(Effect_Aff.bindAff)(Halogen_Aff_Util.awaitBody)(function (body) {
    return Halogen_VDom_Driver.runUI(component(Effect_Aff_Class.monadAffAff))(Data_Unit.unit)(body);
}));
module.exports = {
    main: main,
    Initialize: Initialize,
    FetchBalances: FetchBalances,
    component: component,
    initialState: initialState,
    render: render,
    jsonToWb: jsonToWb,
    renderWallet: renderWallet,
    handleAction: handleAction
};
