// Generated by purs version 0.14.5
"use strict";
var KindSubtitles = (function () {
    function KindSubtitles() {

    };
    KindSubtitles.value = new KindSubtitles();
    return KindSubtitles;
})();
var KindCaptions = (function () {
    function KindCaptions() {

    };
    KindCaptions.value = new KindCaptions();
    return KindCaptions;
})();
var KindDescriptions = (function () {
    function KindDescriptions() {

    };
    KindDescriptions.value = new KindDescriptions();
    return KindDescriptions;
})();
var KindChapters = (function () {
    function KindChapters() {

    };
    KindChapters.value = new KindChapters();
    return KindChapters;
})();
var KindMetadata = (function () {
    function KindMetadata() {

    };
    KindMetadata.value = new KindMetadata();
    return KindMetadata;
})();
var renderKindValue = function (v) {
    if (v instanceof KindSubtitles) {
        return "subtitles";
    };
    if (v instanceof KindCaptions) {
        return "captions";
    };
    if (v instanceof KindDescriptions) {
        return "descriptions";
    };
    if (v instanceof KindChapters) {
        return "chapters";
    };
    if (v instanceof KindMetadata) {
        return "metadata";
    };
    throw new Error("Failed pattern match at DOM.HTML.Indexed.KindValue (line 11, column 19 - line 16, column 29): " + [ v.constructor.name ]);
};
module.exports = {
    KindSubtitles: KindSubtitles,
    KindCaptions: KindCaptions,
    KindDescriptions: KindDescriptions,
    KindChapters: KindChapters,
    KindMetadata: KindMetadata,
    renderKindValue: renderKindValue
};