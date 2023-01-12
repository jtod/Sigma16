// Sigma16: sexp.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3 or later
// See Sigma16/README, LICENSE, and https://jtod.github.io/home/Sigma16

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

//------------------------------------------------------------------------
// S-expressions
//------------------------------------------------------------------------

import * as com from './common.mjs';
import * as arith from './arithmetic.mjs';

//----------------------------------------------------------------------
// Primitive data structures
//----------------------------------------------------------------------

// An s-expression is either an Atom or a Cons cell.  It has a tag
// that indicates wiether it is a Cons, or its data type if it is an
// atom.

const TagCons   = Symbol ('Cons')
const TagNil    = Symbol ('Nil')
const TagInt16  = Symbol ('Int16')
const TagBool   = Symbol ('Bool')
const TagString = Symbol ('String')
const TagSymbol = Symbol ('Symbol')

class Atom {
    constructor (tag, v) {
        this.tag = tag
        this.val = v
    }
    show () {
        return `${this.val}`
    }
}

class Cons {
    constructor (car, cdr) {
        this.tag = TagCons
        this.car = car
        this.cdr = cdr
    }
    show () {
        return '(' + this.car.show() + ' . ' + this.cdr.show() + ')'
    }
}

//----------------------------------------------------------------------
// The heap
//----------------------------------------------------------------------

// The Cons constructor uses the JavaScript virtual machine to
// allocate a cell.  Currently the cons function uses that, and the
// program relies on the JavaScript virtual machine's garbage
// collector.  An alternative to consider is to initialzie the system
// by building an explicit avail list, and to use its own garbage
// collector.  That would be more complex, and might be slower (or
// conceivable faster), but it would make it easier to limit the
// amount of memory allocation and could give more graceful failure if
// a program allocates too much.  This is a point to examine in more
// depth later.

//----------------------------------------------------------------------
// Primitive functions
//----------------------------------------------------------------------

// The program normally doesn't use the class constructors directly;
// instead it uses primitive functions that manipluate s-expressions. 
// Functions to create an atom and cons cell.  Atom strings are not
// currently interned; consider that for later.  Currently, each atom
// has a separate copy of its string.

// Make atoms
function mkInt16 (x) { return new Atom (TagInt16, x) }
function mkBool (x)  { return new Atom (TagBool, x) }
function mkSymbol (x) { return new Atom (TagSymbol, x) }
function mkString (x) { return new Atom (TagString, x) }
function cons (x,y) { return new Cons (x,y) }

// Global constant atoms
let nil = mkSymbol ('nil')
let err = mkSymbol ('Error')
let eof = mkSymbol ('EOF')

// Check type of atom
function isAtom (x) { return x.tag !== TagCons }
function isInt16 (x) { return x.tag == TagInt16 }

function testAtom () {
    console.log ('\ntestAtom start')
    let x = mkSymbol ('abc')
    console.log (`x => ${x.show()}`)
    let t = mkSymbol ('true')
    console.log (`t = ${t.show()}`)
    let f = mkSymbol ('false')
    console.log (`f = ${f.show()}`)
    let n = mkInt16 ('345')
    console.log (`n = ${n.show()}`)
    console.log ('testAtom end\n')
}




function nullp (x) { return x === nil }

// Global names for basic atoms.


// Since atoms are not interned, eq needs to compare the strings.

function eq (x,y) {   // ???????????????????
    return (x == y)
//        || (x.atomic && y.atomic && x.atomstring == y.atomstring)
}

function car (x) {
    if (isAtom(x)) {
        console.log (`error: car applied to atom ${x.show}`)
        return err
    } else {
        return x.car
    }
}

function rplaca (x,y) {
    if (isAtom(x)) {
        console.log (`error: rplaca applied to atom ${x.show}`)
        return err
    } else {
        x.car = y
        return x
    }
}

function cdr (x) {
    if (isAtom(x)) {
        console.log (`error: cdr applied to atom ${x.show}`)
        return err
    } else {
        return x.cdr
    }
}

function rplacd (x,y) {
    if (isAtom(x)) {
        console.log (`error: rplacd applied to atom ${x.show}`)
        return err
    } else {
        x.cdr = y
        return x
    }
}

//----------------------------------------------------------------------
// lexer
//----------------------------------------------------------------------

function getToken () {
    if (inputTokens.length == 0) {
        return eof
    } else {
        return inputTokens.shift()
    }
}

function lookToken () {
    if (inputTokens.length == 0) {
        return eof
    } else {
        return inputTokens[0]
    }
}

// Determine what kind of atom a token represents
function isSymbol (tok) {
    let a = tok[0]
    return a != '(' && a != ')' && a != '.'
}
function isInt (tok) {
    return /^\d+$/.test(tok)
}
function isString (tok) {
    let a = tok[0]
    return a == '"'
}

function testPrim () {
    console.log ('\ntestPrim start')
    console.log (`${nullp(nil)}`)
    console.log (`${nullp (mkSymbol("abc"))}`)
    console.log (`${nullp (cons(nil,nil))}`)
    console.log ('testPrim end\n')
}

function testGetToken () {
    console.log ('\ntestGetToken start')
    inputTokens =
    [ "abc",
      "pqr",
      "2345",
      '"this is a string"',
      "(",
      ")",
      ".",
      "end" ]
    console.log (lookToken())  // abc
    console.log (lookToken())  // abc
    console.log (getToken())   // abc
    console.log (lookToken())  // pqr  
    console.log (getToken())   // pqr
    console.log (getToken())   // 2345
    console.log (getToken())   // "this is a string"
    console.log (getToken())   // (
    console.log (getToken())   // )
    console.log (getToken())   // .

    console.log (isSymbol ('abc'))
    console.log (isSymbol ('('))
    console.log (isSymbol (')'))
    console.log (isSymbol ('.'))
    console.log (isSymbol (';'))
    console.log (`isInt('35') = ${isInt('35')}`)
    console.log (`isInt('ab35') = ${isInt('ab35')}`)
    console.log ('testGetToken end\n')
}

//----------------------------------------------------------------------
// parser
//----------------------------------------------------------------------


function error(msg) {
    console.log (`error: ${msg}`)
    return nil
}

function readsexp () {
    let t = getToken ()
    let result = nil
//    console.log (`readsexp: t = ${t}`)
//    if (isSymbol(t)) {
//        result = symbol(t)
//    }
    if (t == "true" || t == "false") {
        result = mkBool(t)
    } else if (t == "(") {
//        console.log ("readsexp: handling left paren")
        let head = cons (nil,nil)
        let last = head
        let x = nil
        let newCell = nil
        while (lookToken() != ")") {
            x = readsexp()
//            console.log (`readsexp: x=${x.show()}`)
            newCell = cons (x,nil)
            rplacd (last,newCell)
            last = newCell
        }
        let rp = getToken()
        if (rp == ")") {
            result = cdr(head)
        } else {
            error ("list doesn't end in right parenthesis")
        }
    } else if (t == ")") {
        error ("readsexp: unexpected right paren")
    } else if (t == ".") {
        error ("readsexp: unexpected dot")
    } else {
        result = mkSymbol(t)
//        error ("readsexp: unexpected token ${t}")
    }
//    console.log (`reasexp: returning ${result.show()}`)
    return result
}

function printSexp (x) {
    let str = ""
    if (isAtom(x)) {
        str = x.show()
    } else {
        str = "("
        while (!nullp(x)) {
            str = str + printSexp (car(x)) + " "
            if (isAtom(cdr(x)) && !nullp(cdr(x))) {
                str = str + " . " + cdr(x).show()
                x = nil
            } else {
                x = cdr(x)
            }
        }
        str = str + ")"
    }
    return str
}

//----------------------------------------------------------------------
// Examples and unit testing
//----------------------------------------------------------------------

function unitTest () {
    console.log ("\nunitTest start")

    testGetToken()
    
    // atoms
    let x = new Atom("abc")
    let y = new Atom ("def")
    console.log (x.show())
    console.log (y.show())
    console.log (eq (x,x))
    console.log (eq (x,y))

    // z and w are equal but not eq
    let z = new Cons (x,y)
    let w = new Cons (x,y)
    console.log (z.show())
    console.log (w.show())
    console.log (eq (z,z))
    console.log (eq (z,w))

    // ztwo is eq to z
    let ztwo = z
    console.log ('ztwo is eq to z')
    console.log (ztwo.show())

    console.log (w.show())
    console.log (eq(ztwo,w))
    
    // but ztwo is not eq to w
    console.log ('ztwo is not eq to w')
    console.log (ztwo.show())
    console.log (z.show())
    console.log (eq(ztwo,z))

    // dotted pair
    let dp = cons (mkSymbol('a1'), mkSymbol('a2'))
    console.log (dp.show())

    // list of atoms
    let lat = cons (mkSymbol ('a'),
                    cons (mkSymbol('b'),
                          cons (mkSymbol('c'), nil)))
    console.log (lat.show())
    console.log ("unitTest end\n")
}


function readShow () {
    let x = readsexp ()
    console.log (`readShow: ${x.show()}`)
    console.log (`${printSexp(x)}\n`)
}

function testRead () {
    console.log ('\ntestRead start')
    inputTokens =
        [ "abc",
          "true",
          "false",
          "2345",
          '"this is a string"',
          "pqr",
          "hello",
          "(", "a", "b", "c", ")",
          "(", "w", "x", "(", "p", "q", ")", "y", "z", ")",
          "nil",
          ".",
          ")",
          "end" ]
    readShow()
    readShow()
    readShow()
    readShow()
    readShow()
    readShow()
    readShow()
    readShow()
    readShow()
    readShow()
    readShow()
    readShow()
    readShow()
    console.log ('testRead end\n')
}

// Main program: Run test cases

let inputTokens = []

inputTokens =
    [ "abc",
      "true",
      "false",
      "2345",
      '"this is a string"',
      "pqr",
      ".",
      ")",
      "hello",
      "(", "a", "b", "c", ")",
      "(", "w", "x", "(", "p", "q", ")", "y", "z", ")",
      "nil",
      "end" ]


// testGetToken ()
testAtom ()
// testPrim()
// unitTest()
testRead()
