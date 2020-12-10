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
// let curAsmap = []; deprecated

// Main linker function is linker (exe, mods)
// Interface to CLI: read object files, call linker, write exe file
// Interface to GUI: button id and function
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
// created it is given the file baseName of the executable to be
// created, and a list of text strings comprising the object code of
// the modules to be linked.

// exeMod is an S16Module in which an executable will be built in
// linkerInfo, and oms is a list of S15Modules that contain objInfo

export class LinkerState  {
    //    constructor (exeMod, objMods) {
    constructor (obMdTexts) { // obMdTexts is an ObjMd object with text for obj, md
        this.obMdTexts = obMdTexts;
        this.modMap = new Map ();
        this.oiList = [];
        this.locationCounter = 0;
        this.linkErrors = []; // error messages
        this.exeCode = "";
        this.exeMetadata = "";
        this.exeObjMd = null; // result of link
    }
}

export function showLS (ls) {
    let xs = "Linker state:\n"
    xs += `Executable: ${ls.parent.baseName}\n`
    xs += `Location counter = ${arith.wordToHex4(ls.locationCounter)}\n`;
    xs += `${ls.linkErrors.length} Error messages: ${ls.linkErrors}\n`;
    xs += "Modules:\n";
    for (const om of ls.oms) {
        xs += showObjectModule (om);
//        xs += om.omName;
//        xs += "\n";
    }
    return xs;
}
    
// Constructor arguments: module that exports a name, the name, the
// address/field where the imported value will be inserted.

class AsmImport {
    constructor (mod, name, addr, field) {
        this.mod = mod;
        this.name = name;
        this.addr = addr;
        this.field = field;
    }
    show () {
        return `AsmImport mod=${this.mod} name=${this.name} `
        + `addr=${arith.wordToHex4(this.addr)} field=${this.field}\n`;
    }
}

// name (string) is the identifier being exported, val (number) is the
// value of the identifier, status (string) is either "relocatable" or
// "fixed".

class AsmExport {
    constructor (name,val,status) {
        this.name = name;
        this.val = val;
        this.status = status;
    }
    show () {
        return `Export name=${this.name} val=${arith.wordToHex4(this.val)}`
            + ` status=${this.status}`;
    }
}

//-------------------------------------------------------------------------------
// Object Info
//-------------------------------------------------------------------------------

// The ObjectInfo class collects information about each module being
// linked, as well as the executable.  Constructor arguments: modname
// is string giving base name of the module; omText is a string
// giving the object code text, and omMd is a string giving the
// metadata text (null if there is no metadata).

// Return just the first few lines of a (possibly long) text
function takePrefix (xs) {
    return xs ? xs.split("\n").slice(0,3).join("\n") : xs
}

export class ObjectInfo {
    //    constructor(parent) { // omModule is the parent container
    constructor (i, obmdtext) {
//        this.parent = parent; // ???
        //        this.omText = "";
        this.index = i;
        this.obmdtext = obmdtext;
        this.baseName = this.obmdtext.baseName;
        this.objectText = this.obmdtext.objText;
        this.mdText = this.obmdtext.mdText;
        this.omObjectLines = [];
        this.omMdText = "";
        this.omMdLines = [];
        this.omMetadata = null;
        this.omDataBlocks = [new ObjectBlock (0)];
        this.omRelocations = [];
        this.omAsMap = [];
        this.omMdAsmListingPlain = [];
        this.omMdAsmListingDec = [];
        this.omAsmImports = [];
        this.omAsmExportMap = new Map ();
        this.omAsmExports = [];
        this.omSize = 0;
        this.omStartAddress = 0;
        this.omEndAddress = 0;
    }
}

export class ObjectBlock {
    constructor (blockStart) {
        this.blockStart = blockStart;
        this.blockSize = 0;
        this.xs = [];
    }
    showBlock () {
        return `Block of ${this.blockSize} words from `
            + `${arith.wordToHex4(this.blockStart)}: `
            + `${this.xs.map(arith.wordToHex4)}`;
    }
    insertWord (x) {
        this.xs.push(x);
        this.blockSize++;
    }
}

function showObjectInfo (om) {
    const xs = `${om.omName}\n`
          + `  lines of code = ${om.omObjectLines.length}\n`
          + `  start address = ${om.omStartAddress}\n`
          + `  end address = ${om.omEndAddress}\n`
          + `  omRelocations = ${om.omRelocations}\n`
          + showAsmImports (om.omAsmImports)
          + showAsmExports (om.omAsmExports)
          + showAsmExportMap (om.omAsmExportMap)
          + `  AS map = ${om.omAsMap}\n`
          + showBlocks (om.omDataBlocks)
          + "\n";
    return xs;
}

function showAsmImports (xs) {
    let r = "AsmImports...\n";
    for (const x of xs) { r += x.show() }
    return r;
}

function showModMap (m) {
    let r = "Module map...\n";
    for (const k of m.keys()) {
        r += `key ${k} -> ${m.get(k).omObjectLines.length}\n`;
    }
    return r;

}
function showAsmExportMap (m) {
    //    let r = "AsmExport map...\n";
    let r = "";
    for (const k of m.keys()) {
        r += `key ${k} -> ${m.get(k).show()}`;
    }
    return r;
}


function showAsmExports (xs) {
    let r = "AsmExports...\n";
    for (const x of xs) { r += x.show() }
    return r;
}

function showBlocks (bs) {
    let xs = "";
    for (const b of bs) {
        xs += b.showBlock();
        console.log (b.xs);
    }
    return xs;
}

// adjust an object code word, for either import or relocation.
// context is ls (linker state) and om (object module).
// addr (number) is address of object code word to change.
// f is function to calculate new value of object code word.

function adjust (ls, om, addr, f) {
    let found = false;
    let i = 0;
    while (!found && i < om.omDataBlocks.length) {
        let b = om.omDataBlocks[i];
        if (b.blockStart <= addr && addr < b.blockStart+b.blockSize) {
            let x = om.omDataBlocks[i].xs[addr-b.blockStart];
            let y = f (x);
            console.log (`    Adjusting block ${i}`
                         + ` start=${arith.wordToHex4(b.blockStart)}`
                         + ` size=${b.blockSize}`
                         + ` addr=${arith.wordToHex4(addr)}`
                         + ` old=${arith.wordToHex4(x)}`
                         + ` new=${arith.wordToHex4(y)}`);
            om.omDataBlocks[i].xs[addr-b.blockStart] = y;
            found = true;
        }
        i++;
    }
    if (!found) {
        console.log (`Linker error: address ${arith.wordToHex4(addr)} not defined`);
    }
}

//-------------------------------------------------------------------------------
// GUI interface to linker
//-------------------------------------------------------------------------------

/* From gui.mjs
// Linker pane (LP)
prepareButton ('LP_Link',          link.linkerGUI);
prepareButton ('LP_Read_Object',   link.getLinkerModules);
prepareButton ('LP_Show_Object',   link.linkShowExeObject);
prepareButton ('LP_Show_Metadata', link.linkShowExeMetadata);
prepareButton ('LP_Boot',          () => em.boot(st.emulatorState));  // No!
*/

// Linker command: Obtain the modules to link: the editor pane should
// contain a list consisting of the executable base name, followed by
// baseNames of the modules to be linked.  These should be given on
// separate lines, with no white space.

// Finding object/metadata text for a given module baseName mbn
//   Module name: if there is a filename, the extension asm/obj
//   Search the list of modules for one that matches mbn M
//   If not found then error
//     else (m matches baseName)
//        If m.asmInfo exists, obtain obj and md there (error if not found)
//        

// Link the open modules, and use selected module as main program

// linkerGUI is invoked when the Link button on the Linker tab is
// clicked.  It gathers all the modules that are loaded and calls the
// linker; the selected module is the main program and the executable
// is stored in the selected module.

export function linkerGUI () {
    console.log ("linkerGUI");
    //    const selm = st.env.showSelectedModuleName();
//    console.log (`linkerGUI main program is ${selm}`);
    const selm = st.env.getSelectedModule ();
    const selOMD = selm.objMd;
    let objs = [selOMD];
    for (const m of st.env.modules.values ()) {
        const isSel = selm.baseName === m.baseName;
        if (!isSel) { objs.push(m.objMd) }
        console.log (`linkerGUI ${isSel} ${m.baseName}`);
    }
    for (let foo of objs) {
        console.log (`Object module ${foo.baseName}`);
    }
//    let exe = new st.ObjMd ();
    //    linker (exe, mods);
    linker (objs);
    let ls = st.env.linkerState;
    let xs = "<pre class='HighlightedTextAsHtml'>"
        + ls.exeCode
        + "</pre>";
    document.getElementById('LinkerBody').innerHTML = xs;

    let xm = st.env.getSelectedModule ();
    let exe = new st.ObjMd (xm.baseName, ls.exeCode, ls.exeMetadata);
    xm.executable = exe;
}

// getLinkerModulesGui looks at the selected module, which should be a
// linker command file.  It builds and returns a list of object
// modules; if any object module isn't present it provides an error
// message and returns null.

/* deprecated
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
        exeMod = new smod.ObjectModule(`${exeBaseName}.ext.txt`);
    }
    exeMod.omName = `${exeBaseName}.obj.txt`; // redundant
    for (let i = 1; i < modlist.length; i++) {
        let mbn = modlist[i];
        console.log (`getLinkerModulesGui i=${i} mbn=${mbn}\n`);
        if (mbn==="") break; // skip blank lines
        ys += "\n---------------------------------\n";
        ys += `Moduleaaa ${mbn}\n`;
        let om = new smod.ObjectModule (mbn);
        objMod.push (om);
        for (let x of smod.s16modules.values()) { // does this module match?
            if (x.file && x.file.name===`${mbn}.asm.txt`) {
                console.log (`Checking ${mbn}.asm.txt`);
                if (x.asmInfo && x.asmInfo.nAsmErrors===0) {
                    let xs = `\nObject from assembler ${mbn}.asm.txt\n`;
                    console.log (xs);
                    ys += xs;
                    om.omText = x.asmInfo.objectText;
                    ys += "\nObject text:\n" + om.omText + "\n";
                    om.mdText = x.asmInfo.metadataText;
                    console.log (om.mdText);
                    ys += "\nMetadata text:\n" + om.mdText + "\n";
                } else {
                    let xs = `\ngetLinkerModulesGui: asm error in ${mbn}\n`;
                    console.log (xs);
                    ys += xs;
                }
            } else if (x.file && x.file.name===`${mbn}.obj.txt`) {
                om.omText = x.text;
                console.log ("4444444444444444444444444444");
                console.log (om.omText);
                console.log ("55555555555555555555555555555");
                let xs = `\nObject from ${mbn}.obj.txt\n` + om.omText;
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
        zs += om.omText;
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
*/

// Display executable on the linker pane; invoked when "Show object"
// button is clicked

export function linkShowExecutable () {
    console.log ("linkShowExeObject");
    let ls = st.env.linkerState;
    let code = ls.exeCode;
    let xs = "<pre class='HighlightedTextAsHtml'>"
        + code
        + "</pre>";
    document.getElementById('LinkerBody').innerHTML = xs;
}

// Display the executable metadata; invokded when "Show metadata"
// button is clicked

export function linkShowMetadata () {
    console.log ("linkShowExeMetadata");
    console.log ("linkShowExeObject");
    let ls = st.env.linkerState;
    let md = "No metadata"
    let xs = "<pre class='HighlightedTextAsHtml'>"
        + md
        + "</pre>";
    document.getElementById('LinkerBody').innerHTML = xs;
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

// The argument is a list of ObjMd records; each contains an objText
// which is a string, and an mdText which is either a string
// representing the metadata, or null.  The linker returns a
// LinkerState object which contains an ObjMd that holds the
// executable code and optionally metadata for it.

export function linker (obMdTexts) {
    const ls = new LinkerState (obMdTexts);
    st.env.linkerState = ls;
    console.log ("-------------- entering linker...");
    let mcount = 0; // number of object modules being linked
    let oiList = []; // an ObjectInfo for each module being linked
    for (const obtext of obMdTexts) {
        console.log (`--- Module ${mcount}`);
        console.log (obtext.showShort());
        let oi = new ObjectInfo (mcount, obtext); // info about this module
        ls.modMap.set (obtext.baseName, oi); // lookup for imports
        ls.oiList.push (oi); // list keeps the modules in fixed order
        mcount++;
    }
    console.log ("Traverse the keys...");
    for (let temp of ls.modMap.keys()) {
        console.log (`Module key = ${temp}`);
        let temp2 = ls.modMap.get(temp).baseName;
        console.log (`baseName:\n ${temp2}`);
        let temp3 = ls.modMap.get(temp).objectText;
        console.log (`objText:\n ${takePrefix(temp3)}`);
        let temp4 = ls.modMap.get(temp).mdText;
        console.log (`mdText:\n ${takePrefix(temp4)}`);
    }
    console.log ("Traverse the oiList...");
    for (let i = 0; i < ls.oiList.length; i++) {
        console.log (`i=${i} baseName=${ls.oiList[i].baseName}`);
    }
    console.log ("-------------- linker starting ...");

    pass1 (ls); // parse object and record directives
//    console.log ("\n-------------------------------------------------");
//    console.log ("Linker state after pass 1:");
//    console.log (showLS (ls))
    pass2 (ls); // process imports and relocations
//    console.log ("\n-------------------------------------------------");
//    console.log ("Final linker state:");
//    console.log (showLS (ls))
    const objectCode = emitCode (ls);
    console.log ("linker, Here is the objectcode");
    console.log (objectCode);
    ls.exeCode = objectCode;
    const linkerListing = "Linker listing\n";
    console.log ("linker, Here is the linker listing");
    console.log (linkerListing);
    const result = {exeCode: objectCode, lnkTxt: linkerListing};
    return result;
}

//-------------------------------------------------------------------------------
// Linker pass 1
//-------------------------------------------------------------------------------

// Pass 1 traverses the text lines of an object module, parses the
// syntax, builds data blocks, and records the directives

function pass1 (ls) {
    console.log ("*** Linker pass 1");
    for (let i = 0; i < ls.oiList.length; i++) {
        let oi = ls.oiList[i];
        console.log (`--- Object info ${i} (${oi.baseName})`);
        oi.omObjectLines = oi.objectText.split("\n");
        oi.omMdLines = oi.mdText.split("\n");
        parseObject (ls, oi);
    }

}
/* old version deprecated
    const objMods = ls.objMods;
    for (const om of objMods) {
        const objInfo = om.objInfo;
        oi.omObjectLines = objInfo.omText.split("\n");
        objInfo.omMdLines = objInfo.omMd.split("\n");
        parseObject (ls, om);
    }
*/


// Parse object module om with linker state ls

function parseObject (ls, obj) {
    com.mode.trace = true;
    //    const obj = om.objInfo;
//    let obj = oi;
    obj.omStartAddress = ls.locationCounter;
    ls.modMap.set (obj.omName, obj);
    obj.omAsmExportMap = new Map ();
    const relK = obj.omStartAddress; // relocation constant for the object module
    for (let x of obj.omObjectLines) {
        console.log (`Object line <${x}>`);
        let fields = parseObjLine (x);
        com.mode.devlog (`-- op=${fields.operation} args=${fields.operands}`);
        if (x == "") {
            console.log ("skipping blank line");
        } else if(fields.operation == "module") {
            obj.dclmodname = fields.operands[0];
            com.mode.devlog (`  Module name: ${obj.dclmodname}`);
        } else if (fields.operation == "data") {
            com.mode.devlog ("-- data");
            for (let j = 0; j < fields.operands.length; j++) {
                let val = arith.hex4ToWord(fields.operands[j]);
                let safeval = val ? val : 0;
                com.mode.devlog (`  ${arith.wordToHex4(ls.locationCounter)} `
                                 + `${arith.wordToHex4(safeval)}`);
                obj.omDataBlocks[obj.omDataBlocks.length-1].insertWord(safeval);
                ls.locationCounter++;
            }
        } else if (fields.operation == "import") {
            obj.omAsmImports.push(new AsmImport (...fields.operands));
        } else if (fields.operation == "export") {
            const [name,val,status] = [...fields.operands];
            const valNum = arith.hex4ToWord(val);
            const valExp = status == "relocatable" ? valNum + relK : valNum;
            console.log ("Building export pass 1 export:");
            console.log (`name=${name} ${typeof name}`);
            console.log (`val=${val} ${typeof val}`);
            console.log (`valNum=${arith.wordToHex4(valNum)} ${typeof valNum}`);
            console.log (`valExp=${arith.wordToHex4(valExp)} ${typeof valExp}`);
            console.log (`status=${status} ${typeof status}`);
            const x = new AsmExport (name, valExp, status);
            obj.omAsmExportMap.set(fields.operands[0], x);
        } else if (fields.operation == "relocate") {
            obj.omRelocations.push(...fields.operands);
        } else {
            com.mode.devlog (`>>> Syntax error (${fields.operation})`)
        }
    }
}

//-------------------------------------------------------------------------------
// Linker pass 2
//-------------------------------------------------------------------------------

// Pass 2 revisits the imports and relocations that were recorded
// during Pass 1, and makes the necessary adjustments to the object
// code.

function pass2 (ls) {
    console.log ("Pass 2");
    for (let i = 0; i < ls.oiList.length; i++) {
        let oi = ls.oiList[i];
        console.log (`--- pass 2 oi ${i} (${oi.baseName})`);
        resolveImports (ls, oi);
        resolveRelocations (ls, oi);
    }
}
/*
function pass2 (ls) {
    console.log ("Pass 2");
    for (const om of ls.objMods) {
        resolveImports (ls, om.objInfo);
        resolveRelocations (ls, om.objInfo);
    }
}
*/


// om is an objInfo object

function resolveImports (ls, om) {
    //    console.log (`Resolving imports for ${om.omName}`);
    console.log (`Resolving imports for ${om.baseName}`);
    for (const x of om.omAsmImports) {
        console.log (`  Importing ${x.name} from ${x.mod}`);
        if (ls.modMap.has(x.mod)) { // Does module we're importing from exist?
            const exporter = ls.modMap.get(x.mod);
            if (exporter.omAsmExportMap.has(x.name)) { // Is the name exported?
                const v = exporter.omAsmExportMap.get(x.name);
                console.log (`LOOK v status = ${v.status}`);
                const addrNum = arith.hex4ToWord(x.addr);
                const valNum = v.val;
                console.log (`look at resolveImport ${valNum} ${typeof valNum}`);
                console.log (`    Set ${x.addr}.${x.field} := ${v.val}`);
                adjust (ls, om, addrNum, (y) => valNum);
            } else {
                console.log (`Linker error: ${x.name} not exported by ${x.mod}`);
            }
        } else {
            console.log (`Linker error: ${x.mod} not found`);
        }
    }
}

function resolveRelocations (ls, om) {
    const relK = om.omStartAddress; // relocation constant for the object module
    console.log (`Resolving relocations for ${om.baseBame}`
                 + ` relocation=${arith.wordToHex4(relK)}`);
    for (const a of om.omRelocations) {
        console.log (`  relocate ${arith.wordToHex4(a)}`);
        const addressNum = arith.hex4ToWord(a);
        adjust (ls, om, addressNum, (y) => y + relK);
    }
}
    //    console.log (`Resolving relocations for ${om.omName}`


//-------------------------------------------------------------------------------
// Emit object code
//-------------------------------------------------------------------------------

function emitCode (ls) {
    console.log ("Emit object code");
    let exeCode = "";
    if (ls.linkErrors.length > 0) {
        console.log ("Link errors, cannot emit code");
    } else {
        //        for (const om of ls.objMods) {
        for (let i = 0; i < ls.oiList.length; i++) {
            let oi = ls.oiList[i];
            console.log (`Emitting code for ${oi.baseName}`);
            exeCode += `module ${oi.baseName}\n`;
            exeCode += `org ${arith.wordToHex4(oi.omStartAddress)}\n`;
            for (const b of oi.omDataBlocks) {
                exeCode += emitObjectWords (b.xs);
            }
        }
        console.log ("Executable code:");
        console.log (exeCode);
    }
    return exeCode;
}

//            console.log (`Emitting code for ${om.omName}`);
//            exeCode += `module ${om.baseName}\n`;
//            exeCode += `org ${arith.wordToHex4(om.omStartAddress)}\n`;
//            for (const b of om.objInfo.omDataBlocks) {
//                let ys = b.xs.map(arith.wordToHex4);
//                let zs = ys.join(",");
//                console.log (`emit ys=${ys}\n zs=${zs}`);
//                exeCode += `data ${zs}\n`;


// ws is a list of words to be emitted as a sequence of data
// statements, with a limited number of words per data statement in
// order to keep the lines to a reasonable length.

const objBufferLimit = 8;

function emitObjectWords (ws) {
    let xs, ys, zs;
    let code = "";
    while (ws.length > 0) {
        xs = ws.splice (0, objBufferLimit);
        ys = xs.map( (w) => arith.wordToHex4(w));
        zs = 'data ' + ys.join(',') + "\n";
        code += zs;
    }
    return code;
}

//-------------------------------------------------------------------------------
// Parser
//-------------------------------------------------------------------------------

// Given source lines for the metadata, build the metadata object for emulator
function parseObjMetadata (omMdLines) {
    console.log ("parseObjMetadata");
    console.log (omMdLines);
    let omAsMap = [];
    let mdPlainLines = [];
    let mdDecLines = [];
    let i = 0;
    while (i < omMdLines.length & omMdLines[i] != "Source") {
        console.log (`asmap entry ${omMdLines[i]}`);
        i++
    }
    console.log (`source starts at line ${i} --${omMdLines[i]--}`);
    let nSrcLines = omMdLines[i];
    console.log (`nSrcLines = ${nSrcLines}`);
    return nSrcLines;
//    return {omMdLines, omAsMap, mdPlainLines, mdDecLines}
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
        objOmSize : 0,
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
        xs += om.omText;
        xs += om.mdText.slice(0,100);
    }
}
