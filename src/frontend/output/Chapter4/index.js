"use strict";
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Class_Console = require("../Effect.Class.Console/index.js");
var Effect_Random = require("../Effect.Random/index.js");
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
var Regenerate = (function () {
    function Regenerate() {

    };
    Regenerate.value = new Regenerate();
    return Regenerate;
})();
var Finalize = (function () {
    function Finalize() {

    };
    Finalize.value = new Finalize();
    return Finalize;
})();
var render = function (state) {
    var value = Data_Maybe.maybe("No number generated yet")(Data_Show.show(Data_Show.showNumber))(state);
    return Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.h1_([ Halogen_HTML_Core.text("Random number") ]), Halogen_HTML_Elements.p_([ Halogen_HTML_Core.text("Current value: " + value) ]), Halogen_HTML_Elements.button([ Halogen_HTML_Events.onClick(function (v) {
        return Regenerate.value;
    }) ])([ Halogen_HTML_Core.text("Generate new number") ]) ]);
};
var initialState = function (v) {
    return Data_Maybe.Nothing.value;
};
var handleAction = function (dictMonadEffect) {
    return function (v) {
        if (v instanceof Initialize) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(handleAction(dictMonadEffect)(Regenerate.value))(function () {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (newNumber) {
                    return Effect_Class_Console.log(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))("Initialized: " + Data_Show.show(Data_Maybe.showMaybe(Data_Show.showNumber))(newNumber));
                });
            });
        };
        if (v instanceof Regenerate) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))(Effect_Random.random))(function (newNumber) {
                return Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(new Data_Maybe.Just(newNumber));
            });
        };
        if (v instanceof Finalize) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (number) {
                return Effect_Class_Console.log(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadEffect))("Finalized! Last number was: " + Data_Show.show(Data_Maybe.showMaybe(Data_Show.showNumber))(number));
            });
        };
        throw new Error("Failed pattern match at Chapter4 (line 57, column 16 - line 69, column 56): " + [ v.constructor.name ]);
    };
};
var component = function (dictMonadEffect) {
    return Halogen_Component.mkComponent({
        initialState: initialState,
        render: render,
        "eval": Halogen_Component.mkEval({
            handleAction: handleAction(dictMonadEffect),
            handleQuery: Halogen_Component.defaultEval.handleQuery,
            receive: Halogen_Component.defaultEval.receive,
            initialize: new Data_Maybe.Just(Initialize.value),
            finalize: new Data_Maybe.Just(Finalize.value)
        })
    });
};
var main = Halogen_Aff_Util.runHalogenAff(Control_Bind.bind(Effect_Aff.bindAff)(Halogen_Aff_Util.awaitBody)(function (body) {
    return Halogen_VDom_Driver.runUI(component(Effect_Aff.monadEffectAff))(Data_Unit.unit)(body);
}));
module.exports = {
    main: main,
    Initialize: Initialize,
    Regenerate: Regenerate,
    Finalize: Finalize,
    component: component,
    initialState: initialState,
    render: render,
    handleAction: handleAction
};
