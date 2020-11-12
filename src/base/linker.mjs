// Sigma16: linker.mjs
// Copyright (C) 2020 John T. O'Donnell
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

//-----------------------------------------------------------------------------
// linker.mjs manipulates object code, including the functions of a
// linker and loader.  Services include combining a collection of
// object modules to form an executable module; performing address
// relocation; and loading an object module into memory.
//-----------------------------------------------------------------------------

import * as com from './common.mjs';
import * as smod from './s16module.mjs';
import * as arch from './architecture.mjs';
import * as arith from './arithmetic.mjs';
import * as st from './state.mjs';
import * as asm from './assembler.mjs';

// refactor
let curAsmap = [];

// Main linker function is linker (exe, mods)
// Interface to CLI: read object files, call linker, write exe file
// Interface to GUI: button id and function
//   LP_Read_Object     getLinkerModulesGui
//   LP_Link            linkerGUI
//   LP_Show_Object     linkShowExeObject
//   LP_Show_Metadata   linkShowExeMetadata
//   LP_Boot            em.boot (st_emulatorState)

//-------------------------------------------------------------------------------
// Linker state
//-------------------------------------------------------------------------------

// The linker state class encapsulates the linker's variables,
// avoiding a group of global variables.  Normally there will be only
// one object in the class, which can be a global variable or passed
// as an argument to user interface functions.  When a linker state is
// created it is given the file basename of the executable to be
// created, and a list of text strings comprising the object code of
// the modules to be linked.

export class LinkerState  {
    constructor (exeName, oms) {
        this.exeName = exeName;
        this.oms = oms;
        this.modMap = new Map ();
        this.locationCounter = 0;
        this.linkErrors = []; // error messages
    }
}

export function showLS (ls) {
    let xs = "Linker state:\n"
    xs += `Executable: ${ls.exeName}\n`
    xs += `Location counter = ${arith.wordToHex4(ls.locationCounter)}\n`;
    xs += `${ls.linkErrors.length} Error messages: ${ls.linkErrors}\n`;
    xs += "Modules:\n";
    for (const om of ls.oms) {
        xs += showObjectModule (om);
//        xs += om.modName;
//        xs += "\n";
    }
    return xs;
}
    
//-------------------------------------------------------------------------------
// Object module
//-------------------------------------------------------------------------------

// The ObjectModule class collects information about each module being
// linked, as well as the executable.  Constructor arguments: modname
// is string giving base name of the module; objText is a string
// giving the object code text, and objMd is a string giving the
// metadata text (null if there is no metadata).

export class ObjectModule {
    constructor(modName, objText, objMd) {
        this.modName = modName;
        this.objText = objText;
        this.objectLines = objText.split("\n");
        this.objMd = objMd;
        this.mdLines = objMd.split("\n");
        this.mdAsMap = [];
        this.mdAsmListingPlain = [];
        this.mdAsmListingDec = [];
        this.dataBlocks = [new ObjectBlock ()];
        this.imports = [];
        this.exports = [];
        this.origin = 0;
        this.size = 0;
        this.startAddress = 0;
        this.endAddress = 0;
    }
}

export class ObjectBlock {
    constructor (blockStart) {
        this.blockStart = blockStart;
        this.blockSize = 0;
        this.xs = [];
    }
    showBlock () {
        return `Block ${this.blockSize} words at `
            + `${arith.wordToHex4(this.blockStart)}`
            + `${this.xs}`;
    }
    insertWord (x) {
        this.xs.push(x);
        this.blockSize++;
    }
}

function showObjectModule (om) {
    const xs = `${om.modName}\n`
          + `  lines of code = ${om.objectLines.length}\n`
          + `  start address = ${om.startAddress}\n`
          + `  end address = ${om.endAddress}\n`
          + `  imports = ${om.imports}\n`
          + `  exports = ${om.exports}\n`
          + `  AS map = ${om.mdAsMap}`
          + showBlocks (om.dataBlocks)
          + "\n";
    return xs;
}

function showBlocks (bs) {
    console.log ("this is showBlocks");
    let xs = "Blocks\n";
    for (const b of bs) {
        xs += b.showBlock();
        console.log (b.xs);
    }
    return xs;
}

//-------------------------------------------------------------------------------
// Linker main interface
//-------------------------------------------------------------------------------

// The linker takes two arguments: the name of the executable and a
// list of object modules.  Each object module should have two or
// three fields defined: the module name, object text, and metadata
// (or null if there is no metadata).  The linker creates and returns
// an executable module.  If there are linker errors, the messages are
// placed in the result module; if there are no linker errors, the
// result module can be booted.

export function linker (exeName, ms) {
    console.log (`Linking executable ${exeName} from ${ms.length} object modules`);
    let ls = new LinkerState (exeName, ms);
    console.log ("Initial linker state:\n" + showLS (ls));
    console.log ("----------------------");
/*
    let b = new ObjectBlock (500);
    console.log (`asdfasdf ${b.blockStart}`);
    console.log (b.showBlock());
    b.insertWord(12);
    console.log (b.showBlock());
    b.insertWord(2);
    b.insertWord(35);
    b.insertWord(19);
    console.log (b.showBlock());
    return ;
*/
    for (const om of ls.oms) {
        parseObject (ls, om);
    }
    console.log ("Final linker state:\n" + showLS (ls));
    console.log ("Linker returning");
}

//-------------------------------------------------------------------------------
// GUI interface to linker
//-------------------------------------------------------------------------------

// Obtain the modules to link: the editor pane should contain a list
// consisting of the executable base name, followed by basenames of
// the modules to be linked.  These should be given on separate lines,
// with no white space.

// Finding object/metadata text for a given module basename mbn
//   Module name: if there is a filename, the extension asm/obj
//   Search the list of modules for one that matches mbn M
//   If not found then error
//     else (m matches basename)
//        If m.asmInfo exists, obtain obj and md there (error if not found)
//        

export function linkerGui () {
    console.log ("linkerGui");
    let {exeMod, objMod} = getLinkerModulesGui ();
//    console.log (`exemod = ${exeMod.dclmodname}`);
}

// getLinkerModulesGui takes modlist, a list of base names.  The modlist
// must have one basename per line; the first entry is the executable
// and the rest are the modules to link.  It searches the modules
// list, looks for anything tthat matches one of the modlist, and
// returns a corresponding array of ObjectModules.

export function getLinkerModulesGui () {
    console.log ('getLinkerModulesGui');
    let modlist = document.getElementById('EditorTextArea').value.split('\n');
    let exeBaseName = modlist[0];
    let objMod = []; // obj info for the modules in modlist
    let ys = ""; // output describing object modules
    if (modlist.length < 3) {
        let xs = `Error: executable and at least two modules must be specified`;
        document.getElementById('LinkerBody').innerHTML = xs;
        return;
    }
    let exeMod = null;
    for (let x of smod.s16modules.values()) { // does exe module exist?
        if (x.file && x.file.name===`${exeBaseName}.obj.txt`) {
            exeMod = x;
        }
    }
    if (exeMod===null) {
        exeMod = new ObjectModule(`${exeBaseName}.ext.txt`);
    }
    exeMod.modName = `${exeBaseName}.obj.txt`; // redundant
    for (let i = 1; i < modlist.length; i++) {
        let mbn = modlist[i];
        console.log (`getLinkerModulesGui i=${i} mbn=${mbn}\n`);
        if (mbn==="") break; // skip blank lines
        ys += "\n---------------------------------\n";
        ys += `Moduleaaa ${mbn}\n`;
        let om = new ObjectModule (mbn);
        objMod.push (om);
        for (let x of smod.s16modules.values()) { // does this module match?
            if (x.file && x.file.name===`${mbn}.asm.txt`) {
                console.log (`Checking ${mbn}.asm.txt`);
                if (x.asmInfo && x.asmInfo.nAsmErrors===0) {
                    let xs = `\nObject from assembler ${mbn}.asm.txt\n`;
                    console.log (xs);
                    ys += xs;
                    om.objText = x.asmInfo.objectText;
                    ys += "\nObject text:\n" + om.objText + "\n";
                    om.mdText = x.asmInfo.metadataText;
                    console.log (om.mdText);
                    ys += "\nMetadata text:\n" + om.mdText + "\n";
                } else {
                    let xs = `\ngetLinkerModulesGui: asm error in ${mbn}\n`;
                    console.log (xs);
                    ys += xs;
                }
            } else if (x.file && x.file.name===`${mbn}.obj.txt`) {
                om.objText = x.text;
                console.log ("4444444444444444444444444444");
                console.log (om.objText);
                console.log ("55555555555555555555555555555");
                let xs = `\nObject from ${mbn}.obj.txt\n` + om.objText;
                console.log (xs);
                ys += xs;
            } else if (x.file && x.file.name===`${mbn}.md.txt`) {
                om.mdText = x.text;
                let xs = `\nMetadata from ${mbn}.md.txt\n` + om.mdText;
                console.log (xs);
                ys += xs;
            }
        }
    }
    let zs = "<pre class='HighlightedTextAsHtml'>"
        + "<h3>Summary</h3>"
        + modlist.join("<br>")
        + "<h3>Modules</h3>";
    for (let om of objMod) {
        zs += "<hr>";
        zs += `${om.modname}\n`;
        zs += "Object code:\n";
        zs += om.objText;
        zs += "\n";
        zs += "Metadata:\n";
//        zs += om.mdText;  // skipping md for now
        zs += "\n";
    }
    zs += "</pre>";
    document.getElementById('LinkerBody').innerHTML = zs;
    console.log (modlist);
    return {exeMod, objMod};
}

// Linker object button
export function linkShowExeObject () {
    console.log ("linkShowExeObject");
    let xs = "<pre class='HighlightedTextAsHtml'>"
        + "no object"
        + "</pre>";
    document.getElementById('LinkerBody').innerHTML = xs;
}

// Linker metadata button
export function linkShowExeMetadata () {
    console.log ("linkShowExeMetadata");
    let xs = "<pre class='HighlightedTextAsHtml'>"
        + "<h3>no metadata</h3>\nNone available<br>yet"
        + "</pre>";
    document.getElementById('LinkerBody').innerHTML = xs;
}

//-------------------------------------------------------------------------------
// Linker
//-------------------------------------------------------------------------------

// Parse the text lines of an object module and record the information



function parseObject (ls, om) {
    com.mode.trace = true;
    console.log (`parseObject ${om.modName}`);
    om.origin = ls.locationCounter;
    ls.modMap.set (om.modName, om);
    for (let x of om.objectLines) {
        console.log (`Object line <${x}>`);
        let fields = parseObjLine (x);
        com.mode.devlog (`-- op=${fields.operation} args=${fields.operands}`);
        if (x == "") {
            console.log ("skipping blank line");
        } else if(fields.operation == "module") {
            om.dclmodname = fields.operands[0];
            com.mode.devlog (`  Module name: ${om.dclmodname}`);
        } else if (fields.operation == "data") {
            com.mode.devlog ("-- data");
            for (let j = 0; j < fields.operands.length; j++) {
                let val = arith.hex4ToWord(fields.operands[j]);
                let safeval = val ? val : 0;
                com.mode.devlog (`  ${arith.wordToHex4(ls.locationCounter)} `
                                 + `${arith.wordToHex4(safeval)}`); 
                ls.locationCounter++;
            }
        } else if (fields.operation == "import") {
            com.mode.devlog (`  Import (${fields.operands})`)
            ls.isExecutable = false;
        } else if (fields.operation == "export") {
            com.mode.devlog (`  Export (${fields.operands})`)
        } else if (fields.operation == "relocate") {
            com.mode.devlog (`  Relocate (${fields.operands})`)
        } else {
            com.mode.devlog (`>>> Syntax error (${fields.operation})`)
            ls.isExecutable = false;
        }
    }
}

//-------------------------------------------------------------------------------
// Parser
//-------------------------------------------------------------------------------

// Given source lines for the metadata, build the metadata object for emulator
function parseObjMetadata (mdLines) {
    console.log ("parseObjMetadata");
    console.log (mdLines);
    let mdAsMap = [];
    let mdPlainLines = [];
    let mdDecLines = [];
    let i = 0;
    while (i < mdLines.length & mdLines[i] != "Source") {
        console.log (`asmap entry ${mdLines[i]}`);
        i++
    }
    console.log (`source starts at line ${i} --${mdLines[i]--}`);
    let nSrcLines = mdLines[i];
    console.log (`nSrcLines = ${nSrcLines}`);
    return nSrcLines;
//    return {mdLines, mdAsMap, mdPlainLines, mdDecLines}
}

// A line of object code contains a required operation code, white
// space, and a required operand which is a comma-separated list of
// fields that may contain letters, digits, and commas.

export function parseObjLine (xs) {
    const objLineParser = /^([a-z]+)\s+([\w,]+)$/;
    const blankLineParser = /^\w*$/;
    let blankLine = blankLineParser.exec (xs);
    let splitLine = objLineParser.exec (xs);
    let operation = "";
    let operands = [];
    if (splitLine) {
        operation = splitLine[1];
        operands = splitLine[2].split(',');
    } else if (blankLine) {
        console.log (`parseObjLine: found blank line <${xs}>`);
    } else {
        console.log ('linker error: object line has invalid format: ' + xs);
    }
        return {operation, operands}
}

function mkObjStmt (i,srcLine,operation,operands) {
    return {
        objLineNo : i,
        objSrcLine : objSrcLine,
        objOperation : operation,
        objOperands : operands,
        objLocation : 0,
        objSize : 0,
        objOperandNames : [],
        objLine : []
    }
}

// Combine object modules and record commands
function combine (oms) {
    let xs = "";
    for (om of oms) {
        xs += "\n------------------------\n";
        xs += `om.modname\n`;
        xs += om.objText;
        xs += om.mdText.slice(0,100);
    }
}
