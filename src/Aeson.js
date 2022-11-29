var JSONbig = require('json-bigint');
var bignumber = require("bignumber.js")

exports.stringifyJsonBigInt = json => JSONbig.stringify(json);
exports.parseJsonBigInt = maybeNothing => maybeJust => jsonStr => {
    try {
        return maybeJust(JSONbig.parse(jsonStr));
    } catch (_) {
        return maybeNothing;
    }
};

exports.bigNumberFromString = bignumber.BigNumber;

exports.caseJson =
    caseNull =>
    caseBoolean =>
    caseNumber =>
    caseString =>
    caseArray =>
    caseObject =>
    json => {

    if (json === null)
        return caseNull(json);
    if (typeof json === 'boolean')
        return caseBoolean(json);
    if (typeof json === 'string')
        return caseString(json);
    if (typeof json === 'number')
        return caseNumber(
            bignumber.BigNumber(json).toFixed()
        );
    if (bignumber.BigNumber.isBigNumber(json))
        return caseNumber(json.toFixed());
    if (Array.isArray(json))
        return caseArray(json);
    if (typeof json === 'object')
        return caseObject(json);

    throw "Imposible happened: JSON object is incorrect";
};