asmtest.mjs

//-----------------------------------------------------------------------------
// Running and testing
//-----------------------------------------------------------------------------

// Run the current test case, called in Initialization
function run () {
    console.log('run()');
//    assembler();
}


// For testing
function showSymbol (s) {
    return (s.symbol + ' val=' + s.val + ' def ' + s.defLine);
}

// parse displacement separately, for RX just use ([a-zA-Z0-9\$]+)
// ((?:[a-zA-Z][a-zA-Z0-9]*)|(?:\$[0-9a-f]{4})|(?:-?[0-9]+))

// const constParser = /^(dec number) | (hex const) $/;

// attempt at rxParser allowing underscore, but this is the wrong approach
// /^R([0-9a-f]|(?:1[0-5])),(-?[a-zA-Z][a-zA-Z0-9_]\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
// const nameParser = /^[a-zA-Z][a-zA-Z0-9]*$/;

// Use data parser instead...
// const expParser = /^\$([0-9a-f]{4})$/;  // temp: just allow hex const ?????

// CRX asm format (control reg name, register): putctl mask,R3
// const crxParser =
//    /^([a-zA-Z][a-zA-Z0-9]*),R([0-9a-f]|(?:1[0-5]))$/;

// testParser (parseString, '"abc123"')
// testParser (parseString, '"ab\\"c123"')

//-----------------------------------------------------------------------------
//  Testing the regular expressions
//-----------------------------------------------------------------------------

// Test a parser p on a string s and print the fields that are extracted.
// Example: testParser (rxParser, "R7,$2d3[R5]")


// White space is matched by (\s+)

// experiment.

// const splitParser =
//    /(^[^\s;]*)((?:\s+)?)((?:[^\s;]+)?)((?:\s+)?)((?:[^\s;]+)?)(.*$)/;

// const parseSplitFields =
//     /^ ((?:\w+)?) ((?:\s+)?) ((?:\w+)?) ((?:\s+)?) (.*) $/;

// const regexpSplitFields =
//      '^((?:\\w+)?)((?:\\s+)?)((?:\\w+)?)((?:\\s+)?)((?:\\w+)?)((?:\\s+)?)(.*)$';

     // '((?:\\w+)?)';  field
// const regExpComment = '((?:(?:(;(?:.*))|(?:.*))?)';

// Field is    (  (" (\" | ^")*  ")   |  (^ ";) ) +
// Field is    (  ("((\\")|[^"])*")   |  [^\\s";]) ) +

const regExpString = '"((\")|[^"])*"';

function testParseString () {
    let p = new RegExp(regExpString);
    testParser (p, '"abc"');
    testParser (p, '"abc def"');
    testParser (p, '"abc\\"hithere"');

}

//                             " (    \"   | ^")*")

// ........

function runTestFieldNoStringLit () {
    let p = new RegExp(regexpFieldSimpleNoStringLit);
    testParser (p, 'abc');
    testParser (p, 'abc,def');
    testParser (p, 'abc def');
    testParser (p, 'abc;def');
    testParser (p, 'abc"def');
}

function runTestParser () {
    testParser (parseSplitFields, 'abc');
    testParser (parseSplitFields, 'abc def');
    testParser (parseSplitFields, 'abc def ghi');
    testParser (parseSplitFields, 'abc def ghi jkl');

    testParser (parseSplitFields, '  abc');
    testParser (parseSplitFields, '  abc def');
    testParser (parseSplitFields, '  abc def ghi');
    testParser (parseSplitFields, '  abc def ghi jkl');

    testParser (parseSplitFields, ';  abc');
    testParser (parseSplitFields, '  abc ; def');
    testParser (parseSplitFields, '  abc def ; ghi');
    testParser (parseSplitFields, '  abc def ghi ; jkl');

    testParser (parseSplitFields, 'loop');
    testParser (parseSplitFields, '     load   R1,x[R2]  R1 := x');
    testParser (parseSplitFields, 'lbl  load   R1,x[R2]  R1 := x');
    testParser (parseSplitFields, 'lbl  load   R1,x[R2]  ; R1 := x');

    testParser (parseSplitFields, 'arr  data  3,9,-12,$03bf,42  initial data');
    testParser (parseSplitFields, '  data  "hello world"');
    testParser (parseSplitFields, 'lbl  lea  R3,","[R0]   R3 := comma ');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0]   R3 := space ');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0]');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0] foo');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0] foo ","');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0] foo ",');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0] foo " "');
    testParser (parseSplitFields, 'lbl  lea  R3," "[R0]   ; R3 := " " ');
    testParser (parseSplitFields, 'lbl  lea  R3," "[R0]   R3 := " " ');

    testParser (parseField, 'abc');
    testParser (parseField, 'abc def');
    testParser (parseField, 'abc" "def');
    testParser (parseSplitFields, 'abc" "def"  "ghi  123 456');
    testParser (parseSplitFields, 'abc" "def"  "ghi  123 456  R1 := " " ha');
    testParser (parseSplitFields, 'abc" "def"  "ghi  123 456  R1 := "," ha');
}


// const parseSplitFields =
//       /^((?:[^;\s]|(?:"(?:(?:\\")|[^"])*"))*)((?:\s+)?)(.*)$/;

// testParser (parseSplitFields, 'loop_23')
// testParser (parseSplitFields, 'loop_23 add')
// testParser (parseSplitFields, '*,3x)2r')
// testParser (parseSplitFields, 'label load R1,x[R0] hello')
// testParser (parseSplitFields, '12"abc123"xy')
// testParser (parseSplitFields, 'ab;comment')
// testParser (parseSplitFields, 'ab"cd;ef"gh')


// testParser (parseSplitFields, '   load')


//    / (^[^\s;]*) ((?:\s+)?) ((?:[^\s;]+)?) ((?:\s+)?) ((?:[^\s;]+)?) (.*$)/;

function testParser (p,s) {
    console.log (`testParser ${p} on ${s}`);
    let result = p.exec(s);
    console.log (`testParser result[0] = /${result[0]}/`);
    console.log (`testParser result[1] = /${result[1]}/`);
    console.log (`testParser result[2] = /${result[2]}/`);
    console.log (`testParser result[3] = /${result[3]}/`);
    console.log (`testParser result[4] = /${result[4]}/`);
    console.log (`testParser result[5] = /${result[5]}/`);
    console.log (`testParser result[6] = /${result[6]}/`);
    console.log (`testParser result[7] = /${result[7]}/`);
    console.log (`testParser result[8] = /${result[8]}/`);
    console.log (`testParser result[9] = /${result[9]}/`);
    console.log (`testParser result[10] = /${result[10]}/`);
    console.log (`testParser result[11] = /${result[11]}/`);
    console.log (`testParser result[12] = /${result[12]}/`);
}


//-----------------------------------------------------------------------------
//  Testing
//-----------------------------------------------------------------------------

// Test the parser on sample assembly source data

function testParse () {
    testAsmLine ("");
    testAsmLine ("; full line comment");
    testAsmLine ("   ; spaces and full line comment");
    testAsmLine ("  loop1  cmplt  R6,R2,R2  ; spaces before label");
    testAsmLine ("loop1");
    testAsmLine ("loop1  ");
    testAsmLine ("loop1;  ");
    testAsmLine ("loop1  ;  ");
    testAsmLine ("loo;p1  ;  ");
    testAsmLine ("loop1  cmplt  ; start of loop");
    testAsmLine ("loop1  add  R6,R2,R2  ; start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2  ; start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2  ; start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2  ; start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2  ; start of loop");
    testAsmLine ("lo;op1  cmplt  R6,R2,R2   start of loop");
    testAsmLine ("loop1;  cmplt  R6,R2,R2   start of loop");
    testAsmLine ("loop1 ; cmplt  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cm;plt  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt;  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt ;  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt  R6;,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2;   start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2 ;  start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2   start of loop");
}

function testOperands () {
    console.log('testOperands');
    testOperand('R2,R4,R5');       // RRR
    testOperand('R2,R4,R15');      // RRR
    testOperand('R5,arr[R4]');     // RX
    testOperand('123');            // decimal number
    testOperand('-1');             // signed decimal number
    testOperand('$ab3e');          // hex number
    testOperand('xyz');            // label
    testOperand('');               // empty
    testOperand('R2,R4,R45');      // invalid RRR
    testOperand('R2,R4,R16');      // invalid RRR
    testOperand('R16,arr[R4]');    // invalid RX
    testOperand('12a3');           // invalid decimal number
    testOperand('$abgh');          // invalid hex number
}

// Try to parse string x as an operand and print the result

function testOperand (m,x) {
    let ma = m.asmInfo;
    showOperand(parseOperand(m,x));
}

