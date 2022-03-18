// Generated by purs version 0.14.5
"use strict";
var Control_Apply = require("../Control.Apply/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Enum = require("../Data.Enum/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Time_Component = require("../Data.Time.Component/index.js");
var Data_Time_Duration = require("../Data.Time.Duration/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var $$Math = require("../Math/index.js");
var Time = (function () {
    function Time(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    Time.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new Time(value0, value1, value2, value3);
                };
            };
        };
    };
    return Time;
})();
var showTime = {
    show: function (v) {
        return "(Time " + (Data_Show.show(Data_Time_Component.showHour)(v.value0) + (" " + (Data_Show.show(Data_Time_Component.showMinute)(v.value1) + (" " + (Data_Show.show(Data_Time_Component.showSecond)(v.value2) + (" " + (Data_Show.show(Data_Time_Component.showMillisecond)(v.value3) + ")")))))));
    }
};
var setSecond = function (s) {
    return function (v) {
        return new Time(v.value0, v.value1, s, v.value3);
    };
};
var setMinute = function (m) {
    return function (v) {
        return new Time(v.value0, m, v.value2, v.value3);
    };
};
var setMillisecond = function (ms) {
    return function (v) {
        return new Time(v.value0, v.value1, v.value2, ms);
    };
};
var setHour = function (h) {
    return function (v) {
        return new Time(h, v.value1, v.value2, v.value3);
    };
};
var second = function (v) {
    return v.value2;
};
var minute = function (v) {
    return v.value1;
};
var millisecond = function (v) {
    return v.value3;
};
var millisToTime = function (v) {
    var hours = $$Math.floor(v / 3600000.0);
    var minutes = $$Math.floor((v - hours * 3600000.0) / 60000.0);
    var seconds = $$Math.floor((v - (hours * 3600000.0 + minutes * 60000.0)) / 1000.0);
    var milliseconds = v - (hours * 3600000.0 + minutes * 60000.0 + seconds * 1000.0);
    return Data_Maybe.fromJust()(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Time.create)(Data_Enum.toEnum(Data_Time_Component.boundedEnumHour)(Data_Int.floor(hours))))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMinute)(Data_Int.floor(minutes))))(Data_Enum.toEnum(Data_Time_Component.boundedEnumSecond)(Data_Int.floor(seconds))))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMillisecond)(Data_Int.floor(milliseconds))));
};
var hour = function (v) {
    return v.value0;
};
var timeToMillis = function (t) {
    return Data_Time_Duration.Milliseconds(3600000.0 * Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumHour)(hour(t))) + 60000.0 * Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMinute)(minute(t))) + 1000.0 * Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumSecond)(second(t))) + Data_Int.toNumber(Data_Enum.fromEnum(Data_Time_Component.boundedEnumMillisecond)(millisecond(t))));
};
var eqTime = {
    eq: function (x) {
        return function (y) {
            return Data_Eq.eq(Data_Time_Component.eqHour)(x.value0)(y.value0) && Data_Eq.eq(Data_Time_Component.eqMinute)(x.value1)(y.value1) && Data_Eq.eq(Data_Time_Component.eqSecond)(x.value2)(y.value2) && Data_Eq.eq(Data_Time_Component.eqMillisecond)(x.value3)(y.value3);
        };
    }
};
var ordTime = {
    compare: function (x) {
        return function (y) {
            var v = Data_Ord.compare(Data_Time_Component.ordHour)(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            var v1 = Data_Ord.compare(Data_Time_Component.ordMinute)(x.value1)(y.value1);
            if (v1 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v1 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            var v2 = Data_Ord.compare(Data_Time_Component.ordSecond)(x.value2)(y.value2);
            if (v2 instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v2 instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(Data_Time_Component.ordMillisecond)(x.value3)(y.value3);
        };
    },
    Eq0: function () {
        return eqTime;
    }
};
var diff = function (dictDuration) {
    return function (t1) {
        return function (t2) {
            return Data_Time_Duration.toDuration(dictDuration)(Data_Semigroup.append(Data_Time_Duration.semigroupMilliseconds)(timeToMillis(t1))(Data_Time_Duration.negateDuration(Data_Time_Duration.durationMilliseconds)(timeToMillis(t2))));
        };
    };
};
var boundedTime = {
    bottom: new Time(Data_Bounded.bottom(Data_Time_Component.boundedHour), Data_Bounded.bottom(Data_Time_Component.boundedMinute), Data_Bounded.bottom(Data_Time_Component.boundedSecond), Data_Bounded.bottom(Data_Time_Component.boundedMillisecond)),
    top: new Time(Data_Bounded.top(Data_Time_Component.boundedHour), Data_Bounded.top(Data_Time_Component.boundedMinute), Data_Bounded.top(Data_Time_Component.boundedSecond), Data_Bounded.top(Data_Time_Component.boundedMillisecond)),
    Ord0: function () {
        return ordTime;
    }
};
var maxTime = timeToMillis(Data_Bounded.top(boundedTime));
var minTime = timeToMillis(Data_Bounded.bottom(boundedTime));
var adjust = function (dictDuration) {
    return function (d) {
        return function (t) {
            var tLength = timeToMillis(t);
            var d$prime = Data_Time_Duration.fromDuration(dictDuration)(d);
            var wholeDays = Data_Time_Duration.Days($$Math.floor(Data_Newtype.unwrap()(d$prime) / 8.64e7));
            var msAdjust = Data_Semigroup.append(Data_Time_Duration.semigroupMilliseconds)(d$prime)(Data_Time_Duration.negateDuration(Data_Time_Duration.durationMilliseconds)(Data_Time_Duration.fromDuration(Data_Time_Duration.durationDays)(wholeDays)));
            var msAdjusted = Data_Semigroup.append(Data_Time_Duration.semigroupMilliseconds)(tLength)(msAdjust);
            var wrap = (function () {
                var $112 = Data_Ord.greaterThan(Data_Time_Duration.ordMilliseconds)(msAdjusted)(maxTime);
                if ($112) {
                    return 1.0;
                };
                var $113 = Data_Ord.lessThan(Data_Time_Duration.ordMilliseconds)(msAdjusted)(minTime);
                if ($113) {
                    return -1.0;
                };
                return 0.0;
            })();
            return new Data_Tuple.Tuple(Data_Semigroup.append(Data_Time_Duration.semigroupDays)(wholeDays)(wrap), millisToTime(Data_Semigroup.append(Data_Time_Duration.semigroupMilliseconds)(msAdjusted)(8.64e7 * -wrap)));
        };
    };
};
module.exports = {
    Time: Time,
    hour: hour,
    setHour: setHour,
    minute: minute,
    setMinute: setMinute,
    second: second,
    setSecond: setSecond,
    millisecond: millisecond,
    setMillisecond: setMillisecond,
    adjust: adjust,
    diff: diff,
    eqTime: eqTime,
    ordTime: ordTime,
    boundedTime: boundedTime,
    showTime: showTime
};