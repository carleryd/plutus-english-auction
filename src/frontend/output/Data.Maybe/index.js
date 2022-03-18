// Generated by purs version 0.14.5
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var Data_Generic_Rep = require("../Data.Generic.Rep/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Nothing = (function () {
    function Nothing() {

    };
    Nothing.value = new Nothing();
    return Nothing;
})();
var Just = (function () {
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    return Just;
})();
var showMaybe = function (dictShow) {
    return {
        show: function (v) {
            if (v instanceof Just) {
                return "(Just " + (Data_Show.show(dictShow)(v.value0) + ")");
            };
            if (v instanceof Nothing) {
                return "Nothing";
            };
            throw new Error("Failed pattern match at Data.Maybe (line 216, column 1 - line 218, column 28): " + [ v.constructor.name ]);
        }
    };
};
var semigroupMaybe = function (dictSemigroup) {
    return {
        append: function (v) {
            return function (v1) {
                if (v instanceof Nothing) {
                    return v1;
                };
                if (v1 instanceof Nothing) {
                    return v;
                };
                if (v instanceof Just && v1 instanceof Just) {
                    return new Just(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0));
                };
                throw new Error("Failed pattern match at Data.Maybe (line 185, column 1 - line 188, column 43): " + [ v.constructor.name, v1.constructor.name ]);
            };
        }
    };
};
var optional = function (dictAlt) {
    return function (dictApplicative) {
        return function (a) {
            return Control_Alt.alt(dictAlt)(Data_Functor.map(dictAlt.Functor0())(Just.create)(a))(Control_Applicative.pure(dictApplicative)(Nothing.value));
        };
    };
};
var monoidMaybe = function (dictSemigroup) {
    return {
        mempty: Nothing.value,
        Semigroup0: function () {
            return semigroupMaybe(dictSemigroup);
        }
    };
};
var maybe$prime = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v(Data_Unit.unit);
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe (line 243, column 1 - line 243, column 62): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var maybe = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2 instanceof Nothing) {
                return v;
            };
            if (v2 instanceof Just) {
                return v1(v2.value0);
            };
            throw new Error("Failed pattern match at Data.Maybe (line 230, column 1 - line 230, column 51): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
        };
    };
};
var isNothing = maybe(true)(Data_Function["const"](false));
var isJust = maybe(false)(Data_Function["const"](true));
var genericMaybe = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return Nothing.value;
        };
        if (x instanceof Data_Generic_Rep.Inr) {
            return new Just(x.value0);
        };
        throw new Error("Failed pattern match at Data.Maybe (line 220, column 1 - line 220, column 52): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof Nothing) {
            return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
        };
        if (x instanceof Just) {
            return new Data_Generic_Rep.Inr(x.value0);
        };
        throw new Error("Failed pattern match at Data.Maybe (line 220, column 1 - line 220, column 52): " + [ x.constructor.name ]);
    }
};
var functorMaybe = {
    map: function (v) {
        return function (v1) {
            if (v1 instanceof Just) {
                return new Just(v(v1.value0));
            };
            return Nothing.value;
        };
    }
};
var invariantMaybe = {
    imap: Data_Functor_Invariant.imapF(functorMaybe)
};
var fromMaybe$prime = function (a) {
    return maybe$prime(a)(Control_Category.identity(Control_Category.categoryFn));
};
var fromMaybe = function (a) {
    return maybe(a)(Control_Category.identity(Control_Category.categoryFn));
};
var fromJust = function (dictPartial) {
    return function (v) {
        if (v instanceof Just) {
            return v.value0;
        };
        throw new Error("Failed pattern match at Data.Maybe (line 281, column 1 - line 281, column 46): " + [ v.constructor.name ]);
    };
};
var extendMaybe = {
    extend: function (v) {
        return function (v1) {
            if (v1 instanceof Nothing) {
                return Nothing.value;
            };
            return new Just(v(v1));
        };
    },
    Functor0: function () {
        return functorMaybe;
    }
};
var eqMaybe = function (dictEq) {
    return {
        eq: function (x) {
            return function (y) {
                if (x instanceof Nothing && y instanceof Nothing) {
                    return true;
                };
                if (x instanceof Just && y instanceof Just) {
                    return Data_Eq.eq(dictEq)(x.value0)(y.value0);
                };
                return false;
            };
        }
    };
};
var ordMaybe = function (dictOrd) {
    return {
        compare: function (x) {
            return function (y) {
                if (x instanceof Nothing && y instanceof Nothing) {
                    return Data_Ordering.EQ.value;
                };
                if (x instanceof Nothing) {
                    return Data_Ordering.LT.value;
                };
                if (y instanceof Nothing) {
                    return Data_Ordering.GT.value;
                };
                if (x instanceof Just && y instanceof Just) {
                    return Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                };
                throw new Error("Failed pattern match at Data.Maybe (line 205, column 1 - line 205, column 51): " + [ x.constructor.name, y.constructor.name ]);
            };
        },
        Eq0: function () {
            return eqMaybe(dictOrd.Eq0());
        }
    };
};
var eq1Maybe = {
    eq1: function (dictEq) {
        return Data_Eq.eq(eqMaybe(dictEq));
    }
};
var ord1Maybe = {
    compare1: function (dictOrd) {
        return Data_Ord.compare(ordMaybe(dictOrd));
    },
    Eq10: function () {
        return eq1Maybe;
    }
};
var boundedMaybe = function (dictBounded) {
    return {
        top: new Just(Data_Bounded.top(dictBounded)),
        bottom: Nothing.value,
        Ord0: function () {
            return ordMaybe(dictBounded.Ord0());
        }
    };
};
var applyMaybe = {
    apply: function (v) {
        return function (v1) {
            if (v instanceof Just) {
                return Data_Functor.map(functorMaybe)(v.value0)(v1);
            };
            if (v instanceof Nothing) {
                return Nothing.value;
            };
            throw new Error("Failed pattern match at Data.Maybe (line 68, column 1 - line 70, column 30): " + [ v.constructor.name, v1.constructor.name ]);
        };
    },
    Functor0: function () {
        return functorMaybe;
    }
};
var bindMaybe = {
    bind: function (v) {
        return function (v1) {
            if (v instanceof Just) {
                return v1(v.value0);
            };
            if (v instanceof Nothing) {
                return Nothing.value;
            };
            throw new Error("Failed pattern match at Data.Maybe (line 126, column 1 - line 128, column 28): " + [ v.constructor.name, v1.constructor.name ]);
        };
    },
    Apply0: function () {
        return applyMaybe;
    }
};
var applicativeMaybe = {
    pure: Just.create,
    Apply0: function () {
        return applyMaybe;
    }
};
var monadMaybe = {
    Applicative0: function () {
        return applicativeMaybe;
    },
    Bind1: function () {
        return bindMaybe;
    }
};
var altMaybe = {
    alt: function (v) {
        return function (v1) {
            if (v instanceof Nothing) {
                return v1;
            };
            return v;
        };
    },
    Functor0: function () {
        return functorMaybe;
    }
};
var plusMaybe = {
    empty: Nothing.value,
    Alt0: function () {
        return altMaybe;
    }
};
var alternativeMaybe = {
    Applicative0: function () {
        return applicativeMaybe;
    },
    Plus1: function () {
        return plusMaybe;
    }
};
var monadZeroMaybe = {
    Monad0: function () {
        return monadMaybe;
    },
    Alternative1: function () {
        return alternativeMaybe;
    },
    MonadZeroIsDeprecated2: function () {
        return undefined;
    }
};
module.exports = {
    Nothing: Nothing,
    Just: Just,
    maybe: maybe,
    "maybe'": maybe$prime,
    fromMaybe: fromMaybe,
    "fromMaybe'": fromMaybe$prime,
    isJust: isJust,
    isNothing: isNothing,
    fromJust: fromJust,
    optional: optional,
    functorMaybe: functorMaybe,
    applyMaybe: applyMaybe,
    applicativeMaybe: applicativeMaybe,
    altMaybe: altMaybe,
    plusMaybe: plusMaybe,
    alternativeMaybe: alternativeMaybe,
    bindMaybe: bindMaybe,
    monadMaybe: monadMaybe,
    monadZeroMaybe: monadZeroMaybe,
    extendMaybe: extendMaybe,
    invariantMaybe: invariantMaybe,
    semigroupMaybe: semigroupMaybe,
    monoidMaybe: monoidMaybe,
    eqMaybe: eqMaybe,
    eq1Maybe: eq1Maybe,
    ordMaybe: ordMaybe,
    ord1Maybe: ord1Maybe,
    boundedMaybe: boundedMaybe,
    showMaybe: showMaybe,
    genericMaybe: genericMaybe
};