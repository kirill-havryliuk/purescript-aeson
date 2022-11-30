var JSONbig = require('json-bigint');
var bignumber = require("bignumber.js")

exports.stringifyJsonBigInt = json => JSONbig.stringify(traverseFormattingBigInt(json));
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

// Hack zone

class BigNumberFixed extends bignumber.BigNumber {
    constructor(bignum) {
        super(bignum)
    }
    toJSON() {
        return this.toFixed()
    }
}

function traverseFormattingBigInt(json){
    if (json === null)
        return json;
    if (typeof json === 'boolean')
        return json;
    if (typeof json === 'string')
        return json;
    if (typeof json === 'number')
        return new BigNumberFixed(new bignumber.BigNumber(json));
    if (bignumber.BigNumber.isBigNumber(json))
        return new BigNumberFixed(json);
    if (Array.isArray(json))
        return json.map(traverseFormattingBigInt);
    if (typeof json === 'object'){
        Object.keys(json).forEach(function(key, _index) {
            json[key] = traverseFormattingBigInt(json[key]);
        });
        return json;
    }
    throw "Imposible happened: JSON object is incorrect";
};
