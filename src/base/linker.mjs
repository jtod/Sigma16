// Sigma16: linker.mjs
// Copyright (c) 2023 John T. O'Donnell.  License: GNU GPL Version 3
// See Sigma16/README, LICENSE, and https://github.com/jtod/Sigma16

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

//-------------------------------------------------------------------------
// linker.mjs manipulates object code, including the functions of a
// linker and loader.  Services include combining a collection of
// object modules to form an executable module; performing address
// relocation; and loading an object module into memory.
//-------------------------------------------------------------------------

import * as com from './common.mjs';
import * as smod from './s16module.mjs';
import * as arch from './architecture.mjs';
import * as arith from './arithmetic.mjs';
import * as st from './state.mjs';
import * as asm from './assembler.mjs';

// Interface to CLI: read object files, call linker, write exe file
// Interface to GUI: button id and function
//   LP_Link            linkerGUI
//   LP_Show_Object     linkShowExeObject
//   LP_Show_Metadata   linkShowExeMetadata

// Modify an object code word, for either import or relocation.  The
// context is ls (linker state) and om (object module).  addr (number)
// is address of object code word to change.  f is a function that
// calculates the new value of the object code word.

function adjust (ls, om, addr, f) {
    let found = false;
    let i = 0;
    while (!found && i < om.dataBlocks.length) {
        let b = om.dataBlocks[i];
        if (b.blockStart <= addr && addr < b.blockStart+b.blockSize) {
            let x = om.dataBlocks[i].xs[addr-b.blockStart];
            let y = f (x);
            console.log (`    Adjusting block ${i}`
                         + ` start=${arith.wordToHex4(b.blockStart)}`
                         + ` size=${b.blockSize}`
                         + ` addr=${arith.wordToHex4(addr)}`
                         + ` old=${arith.wordToHex4(x)}`
                         + ` new=${arith.wordToHex4(y)}`);
            om.dataBlocks[i].xs[addr-b.blockStart] = y;
            found = true;
        }
        i++;
    }
    if (!found) {
        console.log (`Linker error: address `
                     + `${arith.wordToHex4(addr)} not defined`);
    }
}

//------------------------------------------------------------------------
// GUI interface to linker
//------------------------------------------------------------------------

// linkerGUI is an interface for the GUI to use; the primary work of
// linkingis performed by linker.  linkerGUI is invoked when the Link
// butto on the Linker tab is clicked.  It gathers all the modules
// that are loaded and calls the linker.  The selected module is the
// main program and the executable is stored in the selected module.

export function linkerGUI () {
    console.log ("linkerGUI");
    const selm = st.env.moduleSet.getSelectedModule ();
    const selOMD = selm.objMd
    let objMds = [selOMD]; // objMds :: [ObjMd], selected module first
    for (const m of st.env.moduleSet.modules) {
        console.log (`Linker checking ${m.moduleName} key=${m.modKey}`)
        const isSel = m.modKey === selm.modKey
        if (!isSel) {
            objMds.push (m.objMd);
        }
        console.log (`linkerGUI ${isSel} ${m.moduleName}`);
    }
    console.log ("Objects to be linked:")
    //    for (const m of objMds) {
    for (const om of objMds) {
        console.log (`   ${om.modName}`)
    }
    console.log (`Calling linker with ${objMds.length} ObjMds`)
    //    let ls = linker (selm.baseName, objs);
    let ls = linker (selm.moduleName, objMds);
    console.log (ls.show())
    let exeObjMd = ls.exeObjMd;
    selm.linkMainObjMd = exeObjMd;
    console.log (`linkerGUI: null? linkMainObjMd = ${null==exeObjMd}`)
    let xs = "<pre class='HighlightedTextAsHtml'>"
//        + objectText
//        + listing
        + "\n\nMetadata\n"
//        + mdText
        + "</pre>";
    document.getElementById('LP_Body').innerHTML = xs;
    st.env.linkerState = ls;
}

export function linkShowObject () {
    console.log ("linkShowObject");
    let ls = st.env.linkerState;
    let code = ls.exeCodeText;
    let xs = "<pre class='HighlightedTextAsHtml'>"
        + "Object code\n"
        + code
        + "</pre>";
    document.getElementById('LP_Body').innerHTML = xs;
}

// Display executable on the linker pane; invoked when "Show
// executable" button is clicked

export function linkShowExecutable () {
    console.log ("linkShowExecutable");
    let ls = st.env.linkerState;
    console.log (`${ls ? "ls ok" : "ls bad"}`);
    let xs;
    if (ls.linkErrors.length > 0) {
        xs = "<pre class='HighlightedTextAsHtml'>"
            + "Link errors, program is not executable\n"
            + ls.linkErrors.join("\n")
            + "</pre>";
    } else {
        let code = ls.exeCodeText;
        xs = "<pre class='HighlightedTextAsHtml'>"
            + "Link successful, executable is:\n"
            + code
            + "</pre>";
    }
    document.getElementById('LP_Body').innerHTML = xs;
}

// Display the executable metadata; invokded when "Show metadata"
// button is clicked

export function linkShowMetadata () {
//    testMetadata ()      // testing
    let ls = st.env.linkerState;
    let md = "No metadata"
    let om = ls.exeObjMd
    if (om) {
        console.log ('linkShowMetadata have om')
        md = om.mdText
    }
    let xs = "<pre class='HighlightedTextAsHtml'>"
        + md
        + "</pre>";
    document.getElementById('LP_Body').innerHTML = xs;
}

export function testMetadata () {
    console.log ("*** testMetadata");
    let x = new st.Metadata ()
    x.addMapping (5,6)
    x.addMapping (23,24)
    x.addMapping (100,101)
    x.addMapping (200,201)
    x.addMapping (300,301)
    x.addMapping (400,401)
    x.addMapping (500,501)
    x.addSrcLines (["a", "b", "c", "d", "e", "f", "g",
                    "h", "i", "j", "k", "l" ])
    console.log (`getSrcLines = ${x.getSrcLines()}`)
    console.log (`5 maps to ${x.mapArr[5]}`)
    console.log (`200 maps to ${x.mapArr[200]}`)
    console.log (` mapToTexts() = ${x.mapToTexts()}`)
    console.log ("----------------- this is x to text -----------")
    const xtext = x.toText()
    console.log (`xtext = ${xtext}`)

    let y = new st.Metadata ()
    console.log ("----------------- defining y from xtext -----------")
    y.fromText (xtext)
    console.log ("----------------- this is y to text -----------")
    const ytext = y.toText()
    console.log (`ytext = ${ytext}`)
    return
}

//-------------------------------------------------------------------------
// Linker main interface
//-------------------------------------------------------------------------

// The linker takes a list of ObjMd records; each contains an objText
// which is a string representing the object code, and an mdText which
// is either a string representing the metadata, or null.  The linker
// returns a LinkerState object which contains an ObjMd that holds the
// executable code and optionally metadata for it.  If there are
// linker errors, the messages are placed in the result module; if
// there are no linker errors, the result module can be booted.

export function linker (mainName, objMds) {
    console.log (`Entering linker, main module = ${mainName}`);
    const ls = new st.LinkerState (mainName, objMds);
    st.env.linkerState = ls; // record linker state in global environment
    pass1 (ls); // parse object and record directives
    pass2 (ls); // process imports and relocations
    ls.exeCodeText = emitCode (ls);
    ls.exeMdText = ls.metadata.toText ();
    ls.exeObjMd = ls.linkErrors.length > 0
        ? null
        : new st.ObjMd ("executable", ls.exeCodeText, ls.exeMdText);
    console.log (`Number of linker errors = ${ls.linkErrors.length}`)
    console.log (`Linker errors = ${ls.linkErrors}`)
//    console.log (ls.exeObjMd.showShort());
    return ls;
}

//-------------------------------------------------------------------------
// Linker pass 1
//-------------------------------------------------------------------------

// Pass 1 traverses the text lines of an object module, parses the
// syntax, builds data blocks, and records the directives

function pass1 (ls) {
    console.log ("*** Linker pass 1");
    ls.mcount = 0; // number of object modules being linked
    ls.oiList = []; // an ObjectInfo for each module being linked
    for (const objMd of ls.objMds) {
        console.log (`Linker pass 1: i=${ls.mcount}`
                     + ` examining ${objMd.modName}`)
        let oi = new st.ObjectInfo (ls.mcount, objMd.modName, objMd);
        ls.modMap.set (oi.modName, oi); // support lookup for imports
        console.log (`Linker pass1 modMap set ${oi.modName}`)
        ls.oiList.push (oi); // list keeps the modules in fixed order
        oi.objectLines = oi.objText.split("\n");
        oi.metadata = new st.Metadata ();
        oi.metadata.fromText (oi.mdText);
        oi.startAddress = ls.locationCounter
        oi.srcLineOrigin = ls.metadata.getPlainLines().length;
        ls.metadata.addSrcLines (oi.metadata.getSrcLines ());
        parseObject (ls, oi);
//        ls.metadata.addSrcLines (oi.metadata.getSrcLines ());
        oi.metadata.translateMap (oi.startAddress, oi.srcLineOrigin)
        ls.metadata.addPairs (oi.metadata.pairs)
        ls.mcount++;
    }
    console.log ("Linker pass1 finished");
    ls.showModMap ();
}
// oi contains relevant info about this module
//    ls.modMap.set (objMd.baseName, oi); // support lookup for imports
//    ls.modMap.set (objMd.moduleName, oi); // support lookup for imports

// Parse object module om with linker state ls

function parseObject (ls, obj) {
    com.mode.trace = false;
    obj.startAddress = ls.locationCounter;
    ls.modMap.set (obj.omName, obj);
        console.log (`Lnk-parseObject modMap set ${obtext.moduleName}`)
    obj.asmExportMap = new Map ();
    const relK = obj.startAddress;
    // relocation constant for the object module
    for (let x of obj.objectLines) {
//        console.log (`Object line <${x}>`);
        let fields = st.parseObjLine (x);
        com.mode.devlog (`--op=${fields.operation} args=${fields.operands}`)
        if (x == "") {
//            console.log ("skipping blank line");
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
                obj.dataBlocks[obj.dataBlocks.length-1].insertWord(safeval);
                ls.locationCounter++;
            }
        } else if (fields.operation == "import") {
            obj.asmImports.push(new st.AsmImport (...fields.operands));
        } else if (fields.operation == "export") {
            const [name,val,status] = [...fields.operands];
            const valNum = arith.hex4ToWord(val);
            const valExp = status == "relocatable" ? valNum + relK : valNum;
            console.log ("Building export pass 1 export:");
            console.log (`name=${name} ${typeof name}`);
            console.log (`val=${val} ${typeof val}`);
            console.log (`valNum=${arith.wordToHex4(valNum)}`
                         + ` ${typeof valNum}`);
            console.log (`valExp=${arith.wordToHex4(valExp)}`
                         + ` ${typeof valExp}`);
            console.log (`status=${status} ${typeof status}`);
            const x = new st.AsmExport (name, valExp, status);
            obj.asmExportMap.set(fields.operands[0], x);
        } else if (fields.operation == "relocate") {
            obj.relocations.push(...fields.operands);
        } else {
            com.mode.devlog (`>>> Syntax error (${fields.operation})`)
        }
    }
}

//-------------------------------------------------------------------------
// Linker pass 2
//-------------------------------------------------------------------------

// Pass 2 revisits the imports and relocations that were recorded
// during Pass 1, and makes the necessary adjustments to the object
// code.

function pass2 (ls) {
    console.log ("Pass 2");
    for (let i = 0; i < ls.oiList.length; i++) {
        let oi = ls.oiList[i];
        //        console.log (`--- pass 2 oi ${i} (${oi.baseName})`);
        console.log (`--- pass 2 oi ${i} (${oi.moduleName})`);
        resolveImports (ls, oi);
        resolveRelocations (ls, oi);
    }
}

// Resolve the imports for objInfo object

function resolveImports (ls, om) {
    //    console.log (`Resolving imports for ${om.baseName}`);
    console.log (`Resolving imports for ${om.moduleName}`);
    for (const x of om.asmImports) {
        console.log (`  Importing ${x.name} from ${x.mod}`);
        if (ls.modMap.has(x.mod)) { // does import module exist?
            const exporter = ls.modMap.get(x.mod);
            if (exporter.asmExportMap.has(x.name)) { // is name exported?
                const v = exporter.asmExportMap.get(x.name);
                console.log (`LOOK v status = ${v.status}`);
                const addrNum = arith.hex4ToWord(x.addr);
                const valNum = v.val;
                console.log (`look at resolveImport ${valNum}`
                             + ` ${typeof valNum}`);
                console.log (`    Set ${x.addr}.${x.field} := ${v.val}`);
                adjust (ls, om, addrNum, (y) => valNum);
            } else {
                console.log (`Linker error: ${x.name}`
                             + ` not exported by ${x.mod}`);
            }
        } else {
            console.log (`Linker error: ${x.mod} not found`);
        }
    }
}

function resolveRelocations (ls, om) {
    const relK = om.startAddress;
    // relocation constant for the object module
    console.log (`Resolving relocations for ${om.baseBame}`
                 + ` relocation=${arith.wordToHex4(relK)}`);
    for (const a of om.relocations) {
        console.log (`  relocate ${arith.wordToHex4(a)}`);
        const addressNum = arith.hex4ToWord(a);
        adjust (ls, om, addressNum, (y) => y + relK);
    }
}

//-------------------------------------------------------------------------
// Emit object code
//-------------------------------------------------------------------------

function emitCode (ls) {
    console.log ("Emit object code");
    let exeCode = "";
    if (ls.linkErrors.length > 0) {
        console.log ("Link errors, cannot emit code");
    } else {
        for (let i = 0; i < ls.oiList.length; i++) {
            let oi = ls.oiList[i];
            console.log (`Emitting code for ${oi.moduleName}`);
            exeCode += `module ${oi.moduleName}\n`;
//            console.log (`Emitting code for ${oi.baseName}`);
//            exeCode += `module ${oi.baseName}\n`;
            exeCode += `org ${arith.wordToHex4(oi.startAddress)}\n`;
            for (const b of oi.dataBlocks) {
                exeCode += emitObjectWords (b.xs);
            }
        }
        console.log ("Executable code:");
        console.log (exeCode);
    }
    return exeCode;
}

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

// deprecated

//    const result = {exe: ls.exeObjMd, listing: ls.listing};
//    return result;

//        console.log (`pass 1 ${oi.baseName} origin=${oi.srcLineOrigin}`);
//        let foo = oi.metadata.getSrcLines ();
//        ls.metadata.addSrcLines (foo)
//        let foo = oi.metadata.getSrcLines ();
/*
        ls.listing +=
            `Object module ${oi.baseName}\n`
            + `Start address = ${oi.startAddress}`
            + ` Source origin = ${oi.SrcLineOrigin}\n`
            + "Module object code\n"
            + `${oi.objText}`
            + "Module metadata\n"
            + oi.metadata.toText ()
            + "---- pairs:\n"
            + oi.metadata.pairs.toString()
            + "\n";
*/
//        console.log ("--------- pass1 pairs length .***************..")
//        console.log (`ls.metadata.pairs.length = ${ls.metadata.pairs.length}`)
        //        oi.srcLineOrigin = ls.metadata.getSrcLines().length;
//        console.log (`----------------- BEFORE TRANSLATE pass1 ${oi.baseName} ${oi.metadata.mapToTexts().join(" ")} ------------  `)
//        console.log (`----------------- AFTER TRANSLATE pass1 ${oi.baseName} ${oi.metadata.mapToTexts().join(" ")} ------------  `)

// linkerGUI
        //        if (!isSel) { objs.push(m.objMd) }
//    const selObj = selm.objText
//    const selMd = selm.mdText
//    console.log (`selm obj = ${selObj} md=${selMd}`)
//    let objs = [selObj]
    //    let mds = [selMd]
//            objs.push (m.objText)
//            mds.push (m.mdText)
//    let result = linker (selm.baseName, objs);
// for (const m of st.env.modules.values ()) {
// const isSel = selm.baseName === m.baseName;
//    const selOMD = selm.objMd;
    //    let objs = [selOMD]; // put selected object module first

// Show each object module ???

// linkShowObject
//    let code = 'Object code modules...'
//    document.getElementById('LinkerBody').innerHTML = xs;

/*    
    if (ls.linkErrors.length > 0) {
        st.env.haveExecutable = false;
        st.env.executable = "";
    } else {
        st.env.haveExecutable = true;
        st.env.executableCode = ls.exeCodeText;
        st.env.executableMD = ls.exeMdText;
        }
    console.log ("-------------------------- linker executable code -----")
    console.log (ls.exeCodeText);
    console.log ("-------------------------- linker exe metadata -----")
    console.log (ls.exeMdText);
    console.log ("-------------------------- end of exe metadata -----")
*/

//    console.log ("--------------------------");
//    console.log ("linkerGUI exeObjMd");
//    console.log (exeObjMd);
//    console.log ("--------------------------");


/*    showObject should iterate over modules?
    console.log ("linkShowExecutable");
    let ls = st.env.linkerState;
    let code = 'No executable'
    //    let code = ls.exeCode;
    let om = ls.exeObjMd
    if (om) {
        console.log ('linkShowExecutable have om')
        let {exeObjMd, exeListing} = om
        //        code = om.objText
        code = exeObjMd.objText
    }
    let xs = "<pre class='HighlightedTextAsHtml'>"
        + code
        + "</pre>";
    //    document.getElementById('LinkerBody').innerHTML = xs;
    document.getElementById('LP_Body').innerHTML = xs;
    }
*/

//    let objectText = exeObjMd.objText;
//    let mdText = exeObjMd.mdText;
//    let listing = ""; // result.listing;
//    let xm = st.env.moduleSet.getSelectedModule ();
//    xm.executable = st.exeObjMd;

//    const result = new ObjMd ("exe", ls.exeCodeText, ls.exeMdText);
//    ls.exeObjMd = result;
//    ls.exeObjMd = new st.ObjMd (exeName, ls.exeCodeText, ls.exeMdText);

//        console.log (m.showShort())
//            console.log (`lnk adding ${m.objText}`)
        //        console.log (`linkerGUI ${isSel} ${m.baseName}`);
