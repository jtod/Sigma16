// testmod2.mjs

import {testvar1, testmod1Fcn} from "./testmod1.mjs";
export const testvar2 = testvar1 + 1;
let foo = testmod1Fcn (testvar1);
let bar = testmod1Fcn (testvar2);

console.log (`testmod2: testvar1=${testvar1} testvar2=${testvar2}`);
console.log (`testmod2: foo=${foo} bar=${bar}`);
