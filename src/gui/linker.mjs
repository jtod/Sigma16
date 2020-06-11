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
import * as ed from './editor.mjs';
import * as asm from './assembler.mjs';

// refactor
let curAsmap = [];

// Interface to GUI: button id and function
//   LP_Read_Object     getLinkerModules
//   LP_Link            linkGUI
//   LP_Show_Object     linkShowExeObject
//   LP_Show_Metadata   linkShowExeMetadata
//   LP_Boot            em.boot (st_emulatorState)

//-------------------------------------------------------------------------------
// Object module
//-------------------------------------------------------------------------------

// The linker collects information about each module being linked in
// an ObjectModule.  getObjectModules builds a list of all of these,
// objmod.

export class ObjectModule {
    constructor(modname) {
        this.modname = modname;
        this.objText = "";
        this.mdText = "";
        this.objectLines = [];
        this.metadataLines = [];
        this.mdAsMap = [];
        this.mdAsmListingPlain = [];
        this.mdAsmListingDec = [];
        this.imports = [];
        this.exports = [];
        this.origin = 0;
        this.size = 0;
        this.last = 0;
    }
}



//-------------------------------------------------------------------------------
// Gui interface to linker
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

// getLinkerModules takes modlist, a list of base names.  The modlist
// must have one basename per line; the first entry is the executable
// and the rest are the modules to link.  It searches the modules
// list, looks for anything tthat matches one of the modlist, and
// returns a corresponding array of ObjectModules.

export function getLinkerModules () {
    console.log ('getLinkerModules');
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
        console.log (`getLinkerModules i=${i} mbn=${mbn}\n`);
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
                    let xs = `\ngetLinkerModules: asm error in ${mbn}\n`;
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
        zs += om.mdText;
        zs += "\n";
    }
    zs += "</pre>";
    document.getElementById('LinkerBody').innerHTML = zs;
    console.log (modlist);
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

function linkGUI () {
    console.log ("link");
    let exeName = "exe"; // ???
    let ms = []; // ???
    linker (exeName, ms);
}

//-------------------------------------------------------------------------------
// Linker
//-------------------------------------------------------------------------------

// Given a list objs of object modules; return an executable module
// with given name.  This can be called by linkGUI or linkCLI.

export function linker (exeName, ms) {
    console.log (`link.linker ${exeName} from ${ms.length} object modules`);
    for (let i = 0; i < ms.length; i++) {
        let om = ms[i];
        console.log ("-------------------------------------------");
        console.log (`Module ${i} ${om.modname}`);
        console.log ("\nObject code\n");
        console.log (om.objText);
        console.log ("\nMetadata\n");
        console.log (om.mdText);
//        console.log (`${m.objectLines.length} lines of object code`);
//        console.log (`${m.metadataLines.length} lines of metadata`);
//        console.log (m.objectLines.join("\n"));
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
    let splitLine = objLineParser.exec (xs);
    let operation = "";
    let operands = [];
    if (splitLine) {
        operation = splitLine[1];
        operands = splitLine[2].split(',');
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


// ------------------------------------------------------------------------
// Deprecated from linker

// A Block is a sequence of words in adjacent memory locations.  The
// functional argument f emits the next word of object code.
/*
class Block {
    constructor(f) {
        this.startAddr = 0;
        this.current = 0;
        this.words = [];
        this.f = f;
    }
    insert(w) {
        this.words.push(w);
    }
    relocate(x) {
        this.startAddr = x;
    }
    emit () {
        if (this.current < this.words.length) {
            this.f (this.current+this.startAddr, this.words[this.current]);
            this.current++;
        } else {
            console.log (`emit: out of data`);
        }
        return (this.current >= this.words.length);
    }
}
*/

/* Usage
function fcn (a,w) { console.log (`emit ${a} ${w}`); }
function testBlock() {
    let t1 = new Block (fcn);
    t1.insert(12);
    t1.insert(34);
    t1.insert(56);
    console.log (t1.emit());
    console.log (t1.emit());
    console.log (t1.emit());
    console.log (t1.emit());
}
*/
