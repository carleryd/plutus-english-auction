"use strict";
var Affjax = require("../Affjax/index.js");
var Affjax_ResponseFormat = require("../Affjax.ResponseFormat/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var DOM_HTML_Indexed_ButtonType = require("../DOM.HTML.Indexed.ButtonType/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Aff_Class = require("../Effect.Aff.Class/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Halogen_Aff_Util = require("../Halogen.Aff.Util/index.js");
var Halogen_Component = require("../Halogen.Component/index.js");
var Halogen_HTML_Core = require("../Halogen.HTML.Core/index.js");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements/index.js");
var Halogen_HTML_Events = require("../Halogen.HTML.Events/index.js");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties/index.js");
var Halogen_Query_HalogenM = require("../Halogen.Query.HalogenM/index.js");
var Halogen_VDom_Driver = require("../Halogen.VDom.Driver/index.js");
var Web_Event_Event = require("../Web.Event.Event/index.js");
var SetUsername = (function () {
    function SetUsername(value0) {
        this.value0 = value0;
    };
    SetUsername.create = function (value0) {
        return new SetUsername(value0);
    };
    return SetUsername;
})();
var MakeRequest = (function () {
    function MakeRequest(value0) {
        this.value0 = value0;
    };
    MakeRequest.create = function (value0) {
        return new MakeRequest(value0);
    };
    return MakeRequest;
})();
var render = function (st) {
    return Halogen_HTML_Elements.form([ Halogen_HTML_Events.onSubmit(function (ev) {
        return new MakeRequest(ev);
    }) ])([ Halogen_HTML_Elements.h1_([ Halogen_HTML_Core.text("Look up GitHub user") ]), Halogen_HTML_Elements.label_([ Halogen_HTML_Elements.div_([ Halogen_HTML_Core.text("Enter username:") ]), Halogen_HTML_Elements.input([ Halogen_HTML_Properties.value(st.username), Halogen_HTML_Events.onValueInput(function (str) {
        return new SetUsername(str);
    }) ]) ]), Halogen_HTML_Elements.button([ Halogen_HTML_Properties.disabled(st.loading), Halogen_HTML_Properties.type_(Halogen_HTML_Core.isPropButtonType)(DOM_HTML_Indexed_ButtonType.ButtonSubmit.value) ])([ Halogen_HTML_Core.text("Fetch info") ]), Halogen_HTML_Elements.p_([ Halogen_HTML_Core.text((function () {
        if (st.loading) {
            return "Working...";
        };
        return "";
    })()) ]), Halogen_HTML_Elements.div_((function () {
        if (st.result instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        if (st.result instanceof Data_Maybe.Just) {
            return [ Halogen_HTML_Elements.h2_([ Halogen_HTML_Core.text("Response:") ]), Halogen_HTML_Elements.pre_([ Halogen_HTML_Elements.code_([ Halogen_HTML_Core.text(st.result.value0) ]) ]) ];
        };
        throw new Error("Failed pattern match at Chapter3 (line 66, column 9 - line 73, column 14): " + [ st.result.constructor.name ]);
    })()) ]);
};
var initialState = function (v) {
    return {
        loading: false,
        username: "",
        result: Data_Maybe.Nothing.value
    };
};
var handleAction = function (dictMonadAff) {
    return function (v) {
        if (v instanceof SetUsername) {
            return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                var $14 = {};
                for (var $15 in v1) {
                    if ({}.hasOwnProperty.call(v1, $15)) {
                        $14[$15] = v1[$15];
                    };
                };
                $14.username = v.value0;
                $14.result = Data_Maybe.Nothing.value;
                return $14;
            });
        };
        if (v instanceof MakeRequest) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Effect_Class.liftEffect(Halogen_Query_HalogenM.monadEffectHalogenM(dictMonadAff.MonadEffect0()))(Web_Event_Event.preventDefault(v.value0)))(function () {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.gets(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                    return v1.username;
                }))(function (username) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                        var $18 = {};
                        for (var $19 in v1) {
                            if ({}.hasOwnProperty.call(v1, $19)) {
                                $18[$19] = v1[$19];
                            };
                        };
                        $18.loading = true;
                        return $18;
                    }))(function () {
                        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Effect_Aff_Class.liftAff(Halogen_Query_HalogenM.monadAffHalogenM(dictMonadAff))(Affjax.get(Affjax_ResponseFormat.string)("https://api.github.com/users/" + username)))(function (response) {
                            return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (v1) {
                                var $21 = {};
                                for (var $22 in v1) {
                                    if ({}.hasOwnProperty.call(v1, $22)) {
                                        $21[$22] = v1[$22];
                                    };
                                };
                                $21.loading = false;
                                $21.result = Data_Functor.map(Data_Maybe.functorMaybe)(function (v2) {
                                    return v2.body;
                                })(Data_Either.hush(response));
                                return $21;
                            });
                        });
                    });
                });
            });
        };
        throw new Error("Failed pattern match at Chapter3 (line 77, column 16 - line 86, column 73): " + [ v.constructor.name ]);
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
            initialize: Halogen_Component.defaultEval.initialize,
            finalize: Halogen_Component.defaultEval.finalize
        })
    });
};
var main = Halogen_Aff_Util.runHalogenAff(Control_Bind.bind(Effect_Aff.bindAff)(Halogen_Aff_Util.awaitBody)(function (body) {
    return Halogen_VDom_Driver.runUI(component(Effect_Aff_Class.monadAffAff))(Data_Unit.unit)(body);
}));
module.exports = {
    main: main,
    SetUsername: SetUsername,
    MakeRequest: MakeRequest,
    component: component,
    initialState: initialState,
    render: render,
    handleAction: handleAction
};
