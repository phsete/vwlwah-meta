var demoData = require("./demoData.json");
var resultHex = "2A000480";
var output = "";
var wordLength = 32;
var i = 0;

demoData[1].vwlwah[wordLength].forEach(row => {
    output = output.concat(row);
    console.log("HEX " + "00000000".concat(parseInt(row, 2).toString(16).toUpperCase()).substr(-8));
    console.log("RESULT HEX " + resultHex.substr(i*8, 8));
    i++;
})

console.log("BIN " + output);
for(let i = 0; i < resultHex.length/8; i++) {
    console.log("RESULT BIN " + "00000000000000000000000000000000".concat(parseInt(resultHex.substr(i*8, 8), 16).toString(2)).substr(-32));
}