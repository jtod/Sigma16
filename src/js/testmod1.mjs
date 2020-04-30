// testmod1.mjs
export const testvar1 = 42;
export function testmod1Fcn (x) {
    let y = x*2;
    console.log (`testmod1: testmod1Fcn (x=${x}) returning ${y}`);
    return y;
}
console.log (`testmod1 says testvar1=${testvar1}`);
