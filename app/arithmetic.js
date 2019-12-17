//------------------------------------------------------------------------------
// Bit manipulation
//------------------------------------------------------------------------------

// Bits are numbered from right to left, where the least significant
// bit has index 0 and the most significant (leftmost) bit has index
// 15.

// Return bit i in word w
function getBitInWord (w,i) { return (w >>> i) & 0x0001 }

// Return bit i in register r
function getBitInReg (r,i) { return (r.get() >>> i) & 0x0001 }

// Generate mask to clear/set bit i in a word
function maskToClearBit (i) { return ~(1<<i) & 0xffff }
function maskToSetBit (i) { return (1 << i) & 0xffff }

// Clear/set bit i in register r
function clearBitInReg (r,i) { r.put (r.get() & maskToClearBit(i)) }
function setBitInReg   (r,i) { r.put (r.get() | maskToSetBit(i)) }

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

function boolToWord (x) {
    return x ? wordTrue : wordFalse;
}

function wordToBool (x) {
    return ! (x === 0);
}

// return bit i from word w, result is number (0 or 1)
function extractBit (w,i) {
    let foo = 1 << i;
    let bar = foo & w;
    console.log (`foo = ${foo}`);
    return bar===0 ? 0 : 1;
}

// Determine whether a JavaScript number is a valid Sigma16 word
// (which is represented using binary).  If not, print an error
// message and treat the number as 0.

function validateWord (x) {
    if (x < minBin || x > maxBin) {
	console.log (`validateWord: ${x} is not a valid word (out of range)`);
	return 0;
    } else {
	return x;
    }
}

// Determine whether a JavaScript number is a valid Sigma16 integer
// (which is represented using two's complement).  If not, print an
// error message and treat the number as 0.

function validateInt (x) {
    if (x < minTC || x > maxTC) {
	console.log (`validateInt: ${x} is not a valid int (out of range)`);
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

function wordToInt (w) {
    let x = validateWord (w);
    return x < const8000 ? x : x - const10000;
}

// An Int is represented using two's complement.  return [w, ovfl]
// where w is a word representing the two's complement int, and ovfl
// is a boolean.  If ovfl is true then x is not representable in a 16
// bit word

function intToWord (x) {
    let y = validateInt (x);
    return y < 0 ? y + const10000 : y;
}

// ??????????? deprecated
/*
function intToTc (x) {                  now use wordToInt
    if (0 <= x && x < 32768) {
	return x
    } else if (-32768 <= x & x < 0) {
	return wordInvert(x) + 1
    } else { return NAN }
}
*/

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

function splitWord (x) {
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
// Operating on individual bits in a word
//------------------------------------------------------------------------------

// Return a word where bit i is 1
function setBit(i) { return 1 << i }

// return bit i in word x, where the rightmost bit has index 0
function extractBit (x,i) {
    return (x >>> i) & 0x00000001;
}

// return Boolean from bit i in word x, where the rightmost bit has index 0
function extractBool (x,i) {
    return extractBit (x,i) === 1;
}

// Check where this is used ??????????????

var intToBit = function (x) {
    if (x < 0 || x > 1)
    {console.log('intToBit invalid int: ' + x);
     return('#');
    }
    return x===0 ? '0' : '1';
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

var wordToHex4 = function (x) {
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

function hex4ToWord (h) {
//    console.log("hex4ToWord " + h.length);
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
//    console.log("hexCharToInt " + cx + " c=" + c + " y=" + y)
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
    console.log (`test_op ${opn} ${c} ${a} ${b}`);
    console.log (`  c = ${showWord(c)} [${showCC(c)}]`);
    console.log (`  a = ${showWord(a)}`);
    console.log (`  b = ${showWord(b)}`);
    let [primary,secondary] = g (op,c,a,b);
    console.log (`  primary = ${showWord(primary)}`);
    console.log (`  secondary = ${showWord(secondary)} [${showCC(secondary)}]`);
    console.log (`  expecting ${expect}`);
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

function binAdd (x, y) {
    let r = validateWord (x) + validateWord (y);
    return r & 0x0000ffff;
}

//------------------------------------------------------------------------------
// Operations for the instructions
//------------------------------------------------------------------------------

// Arithmetic for the add instruction (rrdc).  There is no carry input
// bit, so the condition code is not needed as an argument.  The
// primary result is the two's complement sum, and the secondary
// result is a condition code containing the carry out and indications
// of binary overflow and integer overflow.

function op_add (a,b) {
    let sum = a + b;
    let primary = sum & 0x0000ffff;
    let msb = extractBit (sum, 15);
    let carryOut = extractBit (sum,16);
    let binOverflow = carryOut === 1;
    let tcOverflow = ! (extractBit(sum,14) === extractBit(sum,15));
    let secondary = (binOverflow ? ccV : 0)
 	| (tcOverflow ? ccv : 0)
        | (carryOut ? ccC : 0);
    if (tcOverflow) { setBitInReg (ireq,overflowBit) }
    return [primary, secondary];
}

function test_add () {
    test_rr ("add", op_add, 2, 3, "5 []");
    test_rr ("add", op_add, 49, intToWord(-7), "42 []");
    test_rr ("add", op_add, intToWord(-3), intToWord(-10), "-17 []");
    test_rr ("add", op_add, 20000, 30000, "bin 50000 [v]");
}

function op_sub (a,b) {
    let sum = a + wordInvert (b) + 1;
    let primary = sum & 0x0000ffff;
    let msb = extractBit (sum, 15);
    let carryOut = extractBit (sum,16);
    let binOverflow = carryOut === 1;
    let tcOverflow = ! (extractBit(sum,14) === extractBit(sum,15));
    let secondary = (binOverflow ? ccV : 0)
 	| (tcOverflow ? ccv : 0)
        | (carryOut ? ccC : 0);
    return [primary, secondary];
}

function test_sub () {
    test_rr ("sub", op_sub, 24, 7, "17 []");
    test_rr ("sub", op_sub, 2, 3, "-1 []");
    test_rr ("sub", op_sub, 49, intToWord(-7), "56 []");
    test_rr ("sub", op_sub, intToWord(-3), intToWord(-10), "7 []");
    test_rr ("sub", op_sub, 20000, 30000, "tc -10000 [v]");
}

function representableAsTc (x) {
    return minTc <= x && x <= maxTc
}
    
function op_mul (a,b) {
    let aint = wordToInt (a);
    let bint = wordToInt (b);
    let p = a * b;
    let primary = p & 0x0000ffff;
    let tcOverflow = ! (minTC <= p && p <= maxTC)
    let secondary = (tcOverflow ? ccv : 0);
    if (tcOverflow) { setBitInReg (ireq,overflowBit) }
    return [primary, secondary];
}

function test_mul () {
    test_rr ("mul", op_mul, 2, 3, "6 []");
    test_rr ("mul", op_mul, 5, intToWord(-7), "-35 []");
    test_rr ("mul", op_mul, intToWord(-3), intToWord(-10), "30 []");
}

function op_div (a,b) {
    let aint = wordToInt (a);
    let bint = wordToInt (b);
    let primary = intToWord (Math.trunc (aint / bint));
    let secondary = intToWord (aint % bint);
    if (bint==0) { setBitInReg (ireq,zDivBit) };
    return [primary, secondary];
}

function test_div () {
    test_rr ("div", op_div, 35, 7, "5 0");
    test_rr ("div", op_div, intToWord(-9), 3, "-3 0");
    test_rr ("div", op_div, 49, intToWord(-8), "6 1");
}

function op_cmp (a,b) {
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
    console.log (`op_cmp a=${a} b=${b} aint=${aint} bint=${bint}`);
    console.log (`op_cmp ltBin=${ltBin} gtBin${gtBin}`);
    console.log (`op_cmp ltTc=${ltTc} gtTc${gtTc}`);
    console.log (`op_cmp eq=${eq}`);
    console.log (`op_cmp cc=${cc} showCC(cc)`);
    return cc;
}

function op_cmplt (a,b) {
    let aint = wordToInt (a);
    let bint = wordToInt (b);
    let primary = boolToWord (aint < bint);
    return primary;
}

function op_cmpeq (a,b) {
    let primary = boolToWord (a === b)
    return primary;
}

function op_cmpgt (a,b) {
    let aint = wordToInt (a);
    let bint = wordToInt (b);
    let primary = boolToWord (aint > bint)
    return primary;
}

function op_inv (a) {
    let primary = wordInvert (a);
    return primary;
}

function op_and (a,b) {
    let primary = a & b;
    return primary;
}

function op_or (a,b) {
    let primary = a | b;
    return primary;
}

function op_xor (a,b) {
    let primary = a ^ b;
    return primary;
}

function op_addc (c,a,b) {
    let sum = a + b + extractBit(c,bit_ccc);
    let primary = sum & 0x0000ffff;
    let msb = extractBit (sum, 15);
    let carryOut = extractBit (sum,16);
    let binOverflow = carryOut === 1;
    let tcOverflow = ! (extractBit(sum,14) === extractBit(sum,15));
    let secondary = (binOverflow ? ccV : 0)
 	| (tcOverflow ? ccv : 0)
        | (carryOut ? ccC : 0);
    return [primary, secondary];
}

//------------------------------------------------------------------------------
// Condition codes
//------------------------------------------------------------------------------

// Bits are numbered from right to left, starting with 0.  Thus the
// least significant bit has index 0, and the most significant bit has
// index 15.

// Define a word for each condition that is representable in the
// condition code.  The arithmetic operations may or several of these
// together to produce the final condition code.

const bit_ccG = 0;   //    G   >          binary
const bit_ccg = 1;   //    >   >          two's complement
const bit_ccE = 2;   //    =   =          all types
const bit_ccl = 3;   //    <   <          two's complement
const bit_ccL = 4;   //    L   <          binary
const bit_ccV = 5;   //    V   overflow   binary
const bit_ccv = 6;   //    v   overflow   two's complement
const bit_ccc = 7;   //    c   carry      binary

const ccG = setBit(bit_ccG);
const ccg = setBit(bit_ccg);
const ccE = setBit(bit_ccE);
const ccl = setBit(bit_ccl);
const ccL = setBit(bit_ccL);
const ccV = setBit(bit_ccV);
const ccv = setBit(bit_ccv);
const ccC = setBit(bit_ccc);

function showCC (c) {
    console.log (`showCC ${c}`);
    return (extractBool (c,bit_ccc) ? 'c' : '')
	+ (extractBool (c,bit_ccv) ? 'v' : '')
	+ (extractBool (c,bit_ccV) ? 'V' : '')
	+ (extractBool (c,bit_ccL) ? 'L' : '')
	+ (extractBool (c,bit_ccl) ? '&lt;' : '')
	+ (extractBool (c,bit_ccE) ? '=' : '')
	+ (extractBool (c,bit_ccg) ? '>' : '')
	+ (extractBool (c,bit_ccG) ? 'G' : '') ;
}

//------------------------------------------------------------------------------
// Experiments.... deprecated

// experiment, try a function defined in main.  The idea is that a
// function might be defined in main for the electron standalone
// version, but there could be a fall back definition in the browser
// version

// result of clicking Editor: try def in main
//   in browser: tryMainFcn NO
//   in electron standalone app:  

function tryMainFcn () {
    if (typeof testDefInMain === 'function') {
	document.getElementById('EditorTextArea').value =
	    "tryMainFcn: testDefInMain exists " + testDefInMain (41);
    } else {
	document.getElementById('EditorTextArea').value = "tryMainFcn NO";
    }
}
