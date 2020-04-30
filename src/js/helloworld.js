"use strict";

const fs = require("fs");

console.log ("Hello world!");

const textToWrite = "This is the text to write\nThe second line\n";
const outFileName = "test1.txt";

fs.writeFile(outFileName, textToWrite, function (err) {
    if (err) { console.log ("Write error") };
    console.log("wrote test1.txt");
})

             

             
