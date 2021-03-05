// Sigma16: arithmetic.mjs
// Copyright (C) 2020-2021 John T. O'Donnell
// email: john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. See Sigma16/README.md, LICENSE.txt

// This file is part of Sigma16.  Sigma16 is free software: you can
// redistribute it and/or modify it under the terms of the GNU General
// Public License as published by the Free Software Foundation, either
// version 3 of the License, or (at your option) any later version.
// Sigma16 is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.  You should have received
// a copy of the GNU General Public License along with Sigma16.  If
// not, see <https://www.gnu.org/licenses/>.

//-------------------------------------------------------------------------------
// arithmetic.mjs defines arithmetic for the architecture using
// JavaScript arithmetic.  This includes word representation, data
// conversions, and bit manipulation, operations on fields, and
// arithmetic as required by the instruction set architecture.
//------------------------------------------------------------------------------

import * as com from './common.mjs';
import * as smod from './s16module.mjs';
import * as arch from './architecture.mjs';


//------------------------------------------------------------------------------
// Bit manipulation - big endian
//------------------------------------------------------------------------------

// Bits are numbered from left to right, where the most significant
// (leftmost) bit has index 0 and the least significant (rightmost)
// bit has index 15.

// Return bit i in word w
export function getBitInWordBE (w,i) { return (w >>> (15-i)) & 0x0001 }

// Return bit i in nibble (4-bit word) w
function getBitInNibbleBE (w,i) { return (w >>> (3-i)) & 0x0001 }

// Return bit i in register r
export function getBitInRegBE (r,i) { return (r.get() >>> (15-i)) & 0x0001 }

// Generate mask to clear/set bit i in a word
export function maskToClearBitBE (i) { return ~(1<<(15-i)) & 0xffff }
export function maskToSetBitBE (i) { return (1 << (15-i)) & 0xffff }

// Clear/set bit i in register r
export function clearBitInRegBE (r,i) { r.put (r.get() & maskToClearBitBE(i)) }
export function setBitInRegBE   (r,i) { r.put (r.get() | maskToSetBitBE(i)) }

// Put bit b into word x in bit position i
export function putBitInWord (x,i,b) {
    return b==0 ? x & maskToClearBitBE(i) : x | maskToSetBitBE(i)
}

// Return a word where bit i is 1
function setBitBE(i) { return 1 << 15-i }
// old little endian version
//function setBit(i) { return 1 << i }

// return bit i in word x, where the leftmost bit has index 0
export function extractBitBE (x,i) {
    return (x >>> 15-i) & 0x00000001
}

// return bit i in word x, where the rightmost bit has index 0
export function extractBitLE (x,i) {
    return (x >>> i) & 0x00000001
}

// return Boolean from bit i in word x, where the rightmost bit has index 0
export function extractBool (x,i) {
    return extractBitBE (x,i) === 1;
}

// Check where this is used ??????????????

let intToBit = function (x) {
    if (x < 0 || x > 1)
    {com.mode.devlog('intToBit invalid int: ' + x);
     return('#');
    }
    return x===0 ? '0' : '1';
}

//------------------------------------------------------------------------------
// Bit manipulation - little endian
//------------------------------------------------------------------------------

// Bits are numbered from right to left, where the least significant
// bit has index 0 and the most significant (leftmost) bit has index
// 15.

// Return bit i in word w
function getBitInWordLE (w,i) { return (w >>> i) & 0x0001 }

// Return bit i in register r
function getBitInRegLE (r,i) { return (r.get() >>> i) & 0x0001 }

// Generate mask to clear/set bit i in a word
function maskToClearBitLE (i) { return ~(1<<i) & 0xffff }
function maskToSetBitLE (i) { return (1 << i) & 0xffff }

// Clear/set bit i in register r
function clearBitInRegLE (r,i) { r.put (r.get() & maskToClearBitLE(i)) }
function setBitInRegLE   (r,i) { r.put (r.get() | maskToSetBitLE(i)) }

//------------------------------------------------------------------------------
// Logic
//------------------------------------------------------------------------------

// Mnemonics for logic

/*
  x  y   f x y
 -------------
  0  0    p
  0  1    q
  1  0    r
  1  1    s

inv = 1100 = 12
and = 0001 =  1
or  = 0111 =  7
xor = 0110 =  6
*/

// Given the mnemonic for a logic function, return the truth table
// encoding needed for the general logic box circuit
export function logicFunction(mnemonic) {
    return mnemonic=="andnew" ? 1
        : mnemonic=="and"     ? 1
        : mnemonic=="ornew"   ? 7
        : mnemonic=="or"      ? 7
        : mnemonic=="xornew"  ? 6
        : mnemonic=="xor"     ? 6
        : mnemonic=="invnew"  ? 12
        : mnemonic=="inv"     ? 12
        : 0
}

// fcn is an encoded truth table, x and y are bit operands

export function applyLogicFcnBit (fcn, x, y) {
    let result = x==0
        ? (y==0 ? getBitInNibbleBE (fcn,0) : getBitInNibbleBE (fcn,1))
        : (y==0 ? getBitInNibbleBE (fcn,2) : getBitInNibbleBE (fcn,3))
    com.mode.devlog (`applyLogicFcn fcn=${fcn} x=${x} y=${y} result=${result}`);
    return result
}

function lut (p,q,r,s,x,y) {
    return x==0 ? (y==0 ? p : q) : (y==0 ? r : s)
}

export function applyLogicFcnWord (fcn, x, y) {
    com.mode.devlog (`applyLogicFcnWord fcn=${fcn} x=${wordToHex4(x)} y=${wordToHex4(y)}`);
    let p = getBitInNibbleBE (fcn,0);
    let q = getBitInNibbleBE (fcn,1);
    let r = getBitInNibbleBE (fcn,2);
    let s = getBitInNibbleBE (fcn,3);
    let result = 0;
    for (let i=0; i<16; i++) {
        let z = lut (p,q,r,s, getBitInWordBE(x,i), getBitInWordBE(y,i));
        if (z==1) { result = result | maskToSetBitBE(i) }
    }
    com.mode.devlog (`applyLogicFcnWord result=${wordToHex4(result)}`);
    return result
}

function applyLogicFcnHelper (p,q,r,s, x, y) {
    let result = x==0 ? (y==0 ? p : q) : (y==0 ? r : s);
    com.mode.devlog (`applyLogicFcn fcn=${fcn} x=${x} y=${y} result=${result}`);
    return result
}


//------------------------------------------------------------------------------
// Words, binary numbers, and two's complement integers
//------------------------------------------------------------------------------

// Sigma16 uses 16-bit words.  A word is represented as a JavaScript
// integer whose value is the binary interpretation of the word.  Thus
// a word w must satisfy 0 <= w < 2^16.

const const8000  = 32768;    // 2^15
const constffff  = 65535     // 2^16 - 1
const const10000 = 65536;    // 2^16

// A JavaScript variable has a large range, but a JavaScript variable
// used to represent a Sigma16 binary word or two's complement integer
// must be an integer (no fractional part like 3.5) and within a
// restricted range.

// Binary number          0 <= x < 2^16          0, 1, ...,          65535
// Two's complemen t  -2^15 <= x < 2^15 - 1    -32768, ...,  0, ..., 32767

const minBin = 0;
const maxBin = 65535;
const minTC  = -32768;
const maxTC  = 32767;

const wordTrue = 1;
const wordFalse = 0;

export function boolToWord (x) {
    return x ? wordTrue : wordFalse;
}

export function wordToBool (x) {
    return ! (x === 0);
}

// return bit i from word w, result is number (0 or 1)
//function extractBit (w,i) {
//    let foo = 1 << i;
//    let bar = foo & w;
//    com.mode.devlog (`foo = ${foo}`);
//    return bar===0 ? 0 : 1;
//}

// Determine whether a JavaScript number is a valid Sigma16 word
// (which is represented using binary).  If not, print an error
// message and treat the number as 0.

function validateWord (x) {
    if (x < minBin || x > maxBin) {
	com.mode.devlog (`validateWord: ${x} is not a valid word (out of range)`);
	return 0;
    } else {
	return x;
    }
}

// Restrict word to the 16 bit integer part; no error if there are extra 1 bits

export function truncateWord (x) {
    const r = (x < 0) ? 0 : (x & 0xffff);
    com.mode.devlog (`truncateWord x${wordToHex4(x)} r=${wordToHex4(r)}`);
    return r;
}

// Determine whether a JavaScript number is a valid Sigma16 integer
// (which is represented using two's complement).  If not, print an
// error message and treat the number as 0.

function validateInt (x) {
    //    if (x < minTC || x > maxTC) {
    if (x < minTC || x > maxBin) {
	com.mode.devlog (`validateInt: ${x} is not a valid int (out of range)`);
	return 0;
    } else {
	return x;
    }
}

//------------------------------------------------------------------------------
// Converting between binary words and two's complement integers
//------------------------------------------------------------------------------

// Natural numbers are represented in binary.  The binary value of a
// word w is just s (i.e. the value of the JavaScript int).

// Integers are represented in two's complement.  The two's complement
// value of a word w is returned by wordToInt (w)

// Invariants
//   intToWord (wordToInt (x)) = x
//   wordToInt (intToWord (x)) = x

export function wordToInt (w) {
    let x = validateWord (w);
    return x < const8000 ? x : x - const10000;
}

// An Int is represented using two's complement.  return [w, ovfl]
// where w is a word representing the two's complement int, and ovfl
// is a boolean.  If ovfl is true then x is not representable in a 16
// bit word

export function intToWord (x) {
    const y = validateInt (x);
    const result = y < 0 ? y + const10000 : y;
    com.mode.devlog (`intToWord ${x} (${y}) returning ${result}`);
    return result;
}

// Show a word in hex, binary, and two's complement

function showWord (w) {
    return 0 <= w && w <= maxBin
	? `${wordToHex4(w)} bin=${w} tc=${wordToInt(w)}`
        : `word ${w} is invalid: out of range`;
}

//------------------------------------------------------------------------------
//  Operating on fields of a word
//------------------------------------------------------------------------------

// Return the 4-bit fields of a word

export function splitWord (x) {
    let y = validateWord (x);
    let s = y & 0x000f;
    y = y >>> 4;
    let r = y & 0x000f;
    y = y >>> 4;
    let q = y & 0x000f;
    y = y >>> 4;
    let p = y & 0x000f;
    return [p,q,r,s];
}


//------------------------------------------------------------------------------
// Hexadecimal notation
//------------------------------------------------------------------------------

// The assembly language allows hex numbers to be written with either
// lower case a-f or upper case A-F.  When it outputs hex numbers, the
// system always uses the lower case form.  There are conversion
// functions between hex strings (which must be four characters
// limited to valid hex digits) and words.

// Invariants.  For valid x and str:
//   hex4ToWord (wordToHex4 (x)) = x       e.g. hex4ToWord (wordToHex4 (52309))
//   wordToHex4 (hex4ToWord (str)) = str   e.g. wordToHex4 (hex4ToWord ("5c7e"))

// Character code constants for useful hex characters
const charCode0 = '0'.charCodeAt(0);
const charCode9 = '9'.charCodeAt(0);
const charCodea = 'a'.charCodeAt(0);
const charCodef = 'f'.charCodeAt(0);
const charCodeA = 'A'.charCodeAt(0);
const charCodeF = 'F'.charCodeAt(0);
const hexDigit =
      ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'];

// Return a string giving the hex representation of a word

export function wordToHex4 (x) {
    let [p,q,r,s] = splitWord (x);
    return hexDigit[p] + hexDigit[q] + hexDigit[r] + hexDigit[s];
}

function showBit (b) {
    return b==0 ? '0' : 1
}

// Convert a string with four hex digits to a word.  If the string
// isn't a valid hex number, thhe result is NaN.  To be valid, the
// string must contain exactly four characters, each a valid hex
// digit.  There is no prefix syntax: 3a8b is ok but not $3a8b or
// 0x3a8b.  The assembly language uses a $ to indicate that a number
// is hex, but the $ is not part of the hex number.

export function hex4ToWord (h) {
//    com.mode.devlog("hex4ToWord " + h.length);
    if (h.length != 4) return NaN;
    return (4096 * hexCharToInt(h[0])
  	    +  256  * hexCharToInt(h[1])
    	    +   16  * hexCharToInt(h[2])
       	    +         hexCharToInt(h[3]));
}

// Convert a hex character to an integer, or NaN if it isn't a valid
// hex character.  The valid hex characters are 0-9, a-f, and A-F.

function hexCharToInt (cx) {
    let c = cx.charCodeAt(0);
    let y = charCode0 <= c && c <= charCode9
	? c - charCode0
	: (charCodea <= c && c <= charCodef
	   ? 10 + c - charCodea
	   : (charCodeA <= c && c <= charCodeF
	      ? 10 + c - charCodeA
	      : NaN));
//    com.mode.devlog("hexCharToInt " + cx + " c=" + c + " y=" + y)
    return y;
}

//------------------------------------------------------------------------------
// Bitwise logic on words
//------------------------------------------------------------------------------

// Invert each bit in a word

function wordInvert (x) {
    return x ^ 0x0000ffff
}


//------------------------------------------------------------------------------
// Testing arithmetic operations
//------------------------------------------------------------------------------

// Example: test_rrdc ("op_add", op_add, 49, -7)

// The operation generators (e.g. rrdc) are defined in emulator.js.
// This function tests an operation such as op_add.

// opn = string describing the operation, e.g. "op_add"
// f = operations, e.g. op_add
// x = first operand, typically ra
// y = second operand, typically rb

// opn = name of op (e.g. "add")
// op  = operation (e.g. add)
// a = reg[ir_sa]
// b = reg[ir_sb]
// expect = string giving expected result

function test_op (g,opn,op,c,a,b,expect) {
    com.mode.devlog (`test_op ${opn} ${c} ${a} ${b}`);
    com.mode.devlog (`  c = ${showWord(c)} [${showCC(c)}]`);
    com.mode.devlog (`  a = ${showWord(a)}`);
    com.mode.devlog (`  b = ${showWord(b)}`);
    let [primary,secondary] = g (op,c,a,b);
    com.mode.devlog (`  primary = ${showWord(primary)}`);
    com.mode.devlog (`  secondary = ${showWord(secondary)} [${showCC(secondary)}]`);
    com.mode.devlog (`  expecting ${expect}`);
}

const test_r   = (opn,op,a,expect)     => test_op (g_r,  opn,op,0,a,0,expect);
const test_rr  = (opn,op,a,b,expect)   => test_op (g_rr, opn,op,0,a,b,expect);
const test_crr = (opn,op,c,a,b,expect) => test_op (g_crr,opn,op,c,a,b,expect);

const g_r   = (op,c,a,b) => op (a);
const g_rr  = (op,c,a,b) => op (a,b);
const g_crr = (op,c,a,b) => op (c,a,b);


//------------------------------------------------------------------------------
// Basic addition and subtraction
//------------------------------------------------------------------------------

// Add two binary numbers.  If there is a carry out, that is
// discarded: instead of overflow, the result wraps around.  For
// example, binAdd (65535, 7) returns 6.  This is used to calculate
// effective addresses and to increment the pc register.

export function binAdd (x, y) {
    let r = validateWord (x) + validateWord (y);
    return r & 0x0000ffff;
}

//------------------------------------------------------------------------------
// Operations for the instructions
//------------------------------------------------------------------------------

// Shift a by k bits.  k>0 means shift left, k<0 means shift right.
// Thhis calculates a * 2^k where a is a natural number (binary).

export function op_shift (a,k) {
    const i = wordToInt (k);
    const primary = (i > 0) ? shiftL(a,i) : shiftR(a,-i);
    const secondary = 0;
    return [primary, secondary];
}

// The primitive shifting functions are shiftL and shiftR.  These are
// used by the instructions shiftl and shiftr, as well as shift.

export function shiftL (x,k) {
    return truncateWord (x << k);
}

export function shiftR (x,k) {
    return truncateWord (x >>> k);
}

// Set the condition code after performing addition-like arithmetic:
// add, addc, sub.  This is calculated by examining the most
// significant bit of the result, and the carry output.

export function additionCC (a,b,primary,sum) {
    const msba = extractBitLE (a, 15)
    const msbb = extractBitLE (b, 15)
    const msbsum = extractBitLE (sum, 15)
    const carryOut = extractBitLE (sum,16)
    const binOverflow = carryOut === 1
    const tcOverflow = (msba===0 && msbb===0 && msbsum===1 )
          || (msba===1 && msbb===1 && msbsum===0)
    const is0 = primary === 0
    const binPos = sum != 0
    const tcPos = !tcOverflow && msbsum === 0
    const tcNeg = !tcOverflow && msbsum === 1
    const secondary = (binOverflow ? ccV : 0)
 	  | (binOverflow ? ccV : 0)
          | (binOverflow ? ccC : 0)
 	  | (tcOverflow ? ccv : 0)
          | (is0 ? ccE : 0)
          | (binPos ? ccG : 0)
          | (tcNeg ? ccl : 0)
          | (tcPos ? ccg : 0)
    return secondary
    //     if (tcOverflow) { setBitInRegBE (req,overflowBit) } ??????????????? req
/*
    console.log (`additionCC sum = ${wordToHex4(sum)}`)
    console.log (`  addCC msb = ${msb} carryOut = ${carryOut}`)
    console.log (`  addCC binOverflow = ${binOverflow} tcOverflow = ${tcOverflow}`)
    console.log (`  addCC secondary = ${wordToHex4(secondary)}`)
*/
}

// Arithmetic for the add instruction (rrdc).  There is no carry input
// bit, so the condition code is not needed as an argument.  The
// primary result is the two's complement sum, and the secondary
// result is a condition code containing the carry out and indications
// of binary overflow and integer overflow.

export function op_add (a,b) {
    const sum = a + b
    const primary = sum & 0x0000ffff
    const secondary = additionCC (a,b,primary,sum)
// com.mode.devlog (`*** op_add a=${wordToHex4(a)} b=${wordToHex4(b)} p=${wordToHex4(primary)} s=${wordToHex4(secondary)}`)
    return [primary, secondary]
}

export function op_addc (c,a,b) {
    let sum = a + b + extractBitBE(c,arch.bit_ccC);
    let primary = sum & 0x0000ffff;
    const secondary = additionCC (a,b,primary,sum)
    return [primary, secondary]
}

export function op_sub (a,b) {
    let sum = a + wordInvert (b) + 1;
    let primary = sum & 0x0000ffff;
    const secondary = additionCC (a,b,primary,sum)
    return [primary, secondary]
}


function representableAsTc (x) {
    return minTc <= x && x <= maxTc
}
    
export function op_mul (a,b) {
    let aint = wordToInt (a);
    let bint = wordToInt (b);
    let p = a * b;
    let primary = p & 0x0000ffff;
    let tcOverflow = ! (minTC <= p && p <= maxTC)
    let secondary = (tcOverflow ? ccv : 0);
//    if (tcOverflow) { setBitInRegBE (req,overflowBit) } req ???????????????
    return [primary, secondary];
}

export function op_muln (c,a,b) {
    const k16 = 2**16
    const x = c * 2**16 + a;
    const product = x * b;
    const primary  = product % k16;
    const secondary = Math.floor (product / k16);
    com.mode.devlog (`op_muln c=${c} a=${a} b=${b} prim=${primary} sec=${secondary}`);
    return [primary, secondary];
}


export function op_divn (a,b) {
    const k16 = 2**16
    const x = c * 2**16 + a;
    const q = Math.floor (x/b);
    const r = x % b;

    const primary  = product % k16;
    const secondary = Math.floor (product / k16);
    com.mode.devlog (`op_muln c=${c} a=${a} b=${b} prim=${primary} sec=${secondary}`);
    return [primary, secondary];

}

function test_mul () {
    test_rr ("mul", op_mul, 2, 3, "6 []");
    test_rr ("mul", op_mul, 5, intToWord(-7), "-35 []");
    test_rr ("mul", op_mul, intToWord(-3), intToWord(-10), "30 []");
}

export function op_div (a,b) {
    let aint = wordToInt (a);
    let bint = wordToInt (b);
    let primary = intToWord (Math.floor (aint / bint));  // Knuth quotient
    let secondary = intToWord (a - b * Math.floor(a/b)); // Knuth mod
//    if (bint==0) { setBitInRegBE (req,zDivBit) }; req ??????????????????
    return [primary, secondary];
}

function test_div () {
    test_rr ("div", op_div, 35, 7, "5 0");
    test_rr ("div", op_div, intToWord(-9), 3, "-3 0");
    test_rr ("div", op_div, 49, intToWord(-8), "6 1");
}

export function op_cmp (a,b) {
    let aint = wordToInt (a);
    let bint = wordToInt (b);
    let ltBin = a < b;
    let gtBin = a > b;
    let eq    = a===b;
    let ltTc  = aint < bint;
    let gtTc  = aint > bint;
    let cc = ( a > b ? ccG : 0)
 	| (aint > bint ? ccg : 0)
 	| (a === b ? ccE : 0)
 	| (aint < bint ? ccl : 0)
        | ( a < b ? ccL : 0) ;
    com.mode.devlog (`op_cmp a=${a} b=${b} aint=${aint} bint=${bint}`);
    com.mode.devlog (`op_cmp ltBin=${ltBin} gtBin${gtBin}`);
    com.mode.devlog (`op_cmp ltTc=${ltTc} gtTc${gtTc}`);
    com.mode.devlog (`op_cmp eq=${eq}`);
    com.mode.devlog (`op_cmp cc=${cc} showCC(cc)`);
    return cc;
}

export function op_cmplt (a,b) {
    let aint = wordToInt (a);
    let bint = wordToInt (b);
    let primary = boolToWord (aint < bint);
    return primary;
}

export function op_cmpeq (a,b) {
    let primary = boolToWord (a === b)
    return primary;
}

export function op_cmpgt (a,b) {
    let aint = wordToInt (a);
    let bint = wordToInt (b);
    let primary = boolToWord (aint > bint)
    return primary;
}

export function op_inv (a) {
    let primary = wordInvert (a);
    return primary;
}

export function op_and (a,b) {
    let primary = a & b;
    return primary;
}

export function op_or (a,b) {
    let primary = a | b;
    return primary;
}

export function op_xor (a,b) {
    let primary = a ^ b;
    return primary;
}

//------------------------------------------------------------------------------
// Accessing condition codes
//------------------------------------------------------------------------------

// These definitions give a mask with 1 in specified bit position
const ccG = setBitBE(arch.bit_ccG);
const ccg = setBitBE(arch.bit_ccg);
const ccE = setBitBE(arch.bit_ccE);
const ccL = setBitBE(arch.bit_ccL);
const ccl = setBitBE(arch.bit_ccl);
const ccV = setBitBE(arch.bit_ccV);
const ccv = setBitBE(arch.bit_ccv);
const ccC = setBitBE(arch.bit_ccC);

const ccSO = setBitBE (arch.bit_ccStackOverflow);
const ccSU = setBitBE (arch.bit_ccStackUnderflow);

export function showCC (c) {
    com.mode.devlog (`showCC ${c}`);
    return (extractBool (c,arch.bit_ccC) ? 'c' : '')
	+ (extractBool (c,arch.bit_ccv) ? 'v' : '')
	+ (extractBool (c,arch.bit_ccV) ? 'V' : '')
	+ (extractBool (c,arch.bit_ccL) ? 'L' : '')
	+ (extractBool (c,arch.bit_ccl) ? '&lt;' : '')
	+ (extractBool (c,arch.bit_ccE) ? '=' : '')
	+ (extractBool (c,arch.bit_ccg) ? '>' : '')
	+ (extractBool (c,arch.bit_ccG) ? 'G' : '') ;
}

// These constants provide a faster way to set or clear the flags

const clearIntEnable = maskToClearBitBE (arch.intEnableBit);
const setSystemState = maskToClearBitBE (arch.userStateBit);

export function calculateExtract (wsize, fsize, x, xi, y, yi) {
    // p = source field (surrounded by 0) shifted to destination position
    const p = (((x << xi) & 0xffff) >>> (wsize-fsize)) << (wsize-yi-fsize);
    // destination mask has 1 bits where field will be inserted
    const dmask = (0xffff >>> (wsize-fsize)) << (wsize-yi-fsize);
    const dmaski = (~dmask) & 0xffff;
    const z = (y & dmaski) | p;
    com.mode.devlog (`calculateExtract wsize=${wsize} fsize=${fsize}`
                     +  ` xi=${xi} yi=${yi}`
                     + `  x=${wordToHex4(x)}`
                     + `  dmask=${wordToHex4(dmask)}`
                     + `  p=${wordToHex4(p)}`
                     + `  z=${wordToHex4(z)}`);
    return z;
}

export function calculateExtracti (wsize, fsize, x, xi, y, yi) {
    // p = source field (surrounded by 0) shifted to destination position
    const xx = (~x) & 0xff;
    const p = (((xx << xi) & 0xffff) >>> (wsize-fsize)) << (wsize-yi-fsize);
    // destination mask has 1 bits where field will be inserted
    const dmask = (0xffff >>> (wsize-fsize)) << (wsize-yi-fsize);
    const dmaski = (~dmask) & 0xffff;
    const z = (y & dmaski) | p;
    com.mode.devlog (`calculateExtract wsize=${wsize} fsize=${fsize}`
                     +  ` xi=${xi} yi=${yi}`
                     + `  x=${wordToHex4(x)}`
                     + `  dmask=${wordToHex4(dmask)}`
                     + `  pp=${wordToHex4(pp)}`
                     + `  z=${wordToHex4(z)}`);
    return z;
}


export function testCalculateExport () {
    console.log ("testCalculateExport");
    calculateExtract (16,4, 0xffff, 8, 0, 0);
    calculateExtract (16,4, 0xffff, 8, 0, 4);
    calculateExtract (16,4, 0xffff, 8, 0, 8);
    calculateExtract (16,4, 0xffff, 8, 0, 10);
    calculateExtract (16,4, 0xffff, 8, 0, 12);
    calculateExtract (16, 5, 0xffff, 3, 7, 9); // 007c = 0000 0000 0111 11000
}

function test_add () {
    test_rr ("add", op_add, 2, 3, "5 []");
    test_rr ("add", op_add, 49, intToWord(-7), "42 []");
    test_rr ("add", op_add, intToWord(-3), intToWord(-10), "-17 []");
    test_rr ("add", op_add, 20000, 30000, "bin 50000 [v]");
}

/*
    let msb = extractBitBE (sum, 15);
    let carryOut = extractBitBE (sum,16);
    let binOverflow = carryOut === 1;
    let tcOverflow = ! (extractBitBE(sum,14) === extractBitBE(sum,15));
    let secondary = (binOverflow ? ccV : 0)
 	| (tcOverflow ? ccv : 0)
        | (carryOut ? ccC : 0);
    return [primary, secondary];
*/

/*
function test_sub () {
    test_rr ("sub", op_sub, 24, 7, "17 []");
    test_rr ("sub", op_sub, 2, 3, "-1 []");
    test_rr ("sub", op_sub, 49, intToWord(-7), "56 []");
    test_rr ("sub", op_sub, intToWord(-3), intToWord(-10), "7 []");
    test_rr ("sub", op_sub, 20000, 30000, "tc -10000 [v]");
}

*/
/* addc    
    let msb = extractBitBE (sum, 15);
    //    let carryOut = extractBitBE (sum,16);
    let carryOut = sum > 65535
    let binOverflow = carryOut
    let tcOverflow = ! (extractBitBE(sum,14) === extractBitBE(sum,15));
    let secondary = (binOverflow ? ccV : 0)
 	| (tcOverflow ? ccv : 0)
        | (carryOut ? ccC : 0);
    com.mode.devlog (`************ op_addc co=${carryOut}`)
    return [primary, secondary];
*/
