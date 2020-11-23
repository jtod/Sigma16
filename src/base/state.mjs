// Sigma16: state.mjs
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
// state.mjs defines global state for the system, IDE, modules, and
// emulator
//-----------------------------------------------------------------------------

// The main components of the program avoid using global variables;
// instead the necessary state is organized into records and passed as
// needed to the functions.  This module defines those records.  The
// gui operations, defined in Sigma16.html, call the emulator's
// interface functios with the state, for example boot(emulatorState).

import * as com from './common.mjs';
import * as arch from './architecture.mjs';

//-----------------------------------------------------------------------------
// Global system module environment
//-----------------------------------------------------------------------------

// The system state is held in a global variable, moduleEnvironment.
// This is a map from a base name foo to an S16Module object.  This,
// in turn, contains optional information about foo.asm.txt,
// foo.obj.txt, and foo.lnk.txt.

export class SystemState {
    constructor () {
        this.clearModules ();
        this.emState = null;
    }
    clearModules () {
        this.modules = new Map ();
        this.anonymousCount = 0;
        this.selectedModule = null;
    }
    mkSelectModule (mname) {
        console.log (`>>>>> mkSelectModule ${mname}`);
        if (this.modules.has (mname)) {
            this.selectedModule = mname;
        } else if (mname) {
            this.modules.set (mname, new S16Module (mname));
        } else {
            this.anonymousCount++;
            const xs = `anonymous${this.anonymousCount}`;
            this.modules.set (xs, new S16Module (xs));
            this.selectedModule = xs;
        }
        //        return this.selectedModule;
        return this.modules.get(this.selectedModule);
    }
    closeModule (mname) {
        this.modules.delete (mname);
    }
    getSelectedModule () {
        if (env.modules.size == 0) { // no modules
            return mkModule ();
        } else if (!this.selectedModule) { // nothing selected
            this.selectedModule = [...this.modules.keys()][0];
            return this.modules.get (this.selectedModule);
        } else if (this.modules.get(this.selectedModule)) { // it exists
            return this.modules.get (this.selectedModule);
        } else  { // it doesn't exist
            return this.mkModule (this.selectedModule);
        }
    }
    
}

//-----------------------------------------------------------------------------
// Global state
//-----------------------------------------------------------------------------

// The environment is a global variable that contains all the system
// state.

export const env = new SystemState ();

//-----------------------------------------------------------------------------
// S16Module
//-----------------------------------------------------------------------------

// An S16Module is a container for all the files and objects that
// share the same basename.

export class S16Module {
    constructor (baseName) {
        console.log (`>>> new S16Module ${baseName}`);
        this.baseName = baseName;
        env.modules.set (baseName, this);
        this.selectId = `select_${baseName}`;
        this.closeId = `close_${baseName}`;
        this.asmFile = null;
        this.objFile = null;
        this.exeFile = null;
        this.linkFile = null;
        this.asmInfo = null;
        this.objInfo = null;
        this.linkInfo = null;
        this.exeInfo = null;
    }
    showShort () {
        let xs = `Module ${this.baseName}\n`;
        xs += this.asmInfo ? "Has assembly language code\n" : "";
        xs += this.objInfo ? "Has object code\n" : "";
        xs += this.linkInfo ? "Has linked code\n" : "";
        return xs;
    }
    showHtml () {
        let xs = "<ul>\n";
        xs += `<li> <b> ${this.baseName} </b>`;
        xs += (env.selectedModule == this.baseName ? "Selected" : "Not selected" );
        xs += `<button id='${this.selectId}'>Select</button>`;
        xs += `<button id='${this.closeId}'>Close</button>`;
        xs += "<br>";
        xs += this.asmFile ? this.asmFile.text : "No asm file";
        xs += this.asmInfo ? this.asmInfo.showHtml() : "No assembly code<br>";
        xs += this.objInfo ? this.objInfo.showHtml() : "No object code<br>";
        xs += this.linkInfo ? this.linkInfo.showHtml() : "No linker code<br>";
        xs += `</li>\n`;
        xs += "</ul>\n";
        return xs;
    }
}

 // m.selectButton;

//    for (const bn of moduleEnvironment.keys()) {

/*
function showAsmSubmodule (m) {
    let xs = "Asm submodule"
    const info = m.asmInfo;
    if (info == null) {
        xs += `Module ${m.baseName} has no assembly data`;
    } else {
        xs += info.show;
    }
    return xs;
}

function showObjSubmodule (m) {
    let xs = "Obj submodule"
    return xs;
}

function showLinkSubmodule (m) {
    let xs = "Link submodule"
    return xs;
}
*/


// Deprecated


/*
export function showS16Module (m) {
    let xs = "<ul>\n";
        xs += `<li> <b> ${bn} </b>`;
        xs += m.selectButton;
        xs += m.closeButton;
        xs += "<br>";
        xs += m.asmInfo ? m.asmInfo.show() : "No assembly code<br>";
        xs += m.objInfo ? m.objInfo.show() : "No object code<br>";
        xs += m.linkInfo ? m.linkInfo.show() : "No linker code<br>";
        xs += `</li>\n`;
    }
    xs += "</ul>\n";
    return xs;
}
*/

/*
export class s16module {
    constructor () {
        const s = Symbol (`module_#${moduleGenSym}`);   // the key to this module in s16modules
        moduleGenSym++;
        s16modules.set (s, this);   // record this module in the set
        selectedModule = s;         // select the module as it's created
        com.mode.devlog (`NEW S16MODULE SETTING selectedModule ${selectedModule.description}`);
        this.sym = s;                  // make the key available, given module
        this.text = "init mod text";                // raw source text
        this.modName = null;
        this.file = null;
        this.fileReader = null;
        this.fileReadComplete = true;
        this.asmInfo = null;           // to be filled in by assembler
        this.objInfo = null;           // to be filled in by linker
        this.selectId = `select_${this.idNumber}`;
        this.closeId = `close_${this.idNumber}`;
        this.selectButton = `<button id='${this.selectId}'>Select</button>`;
        this.closeButton = `<button id='${this.closeId}'>Close</button>`;
    }
    show () {
        return "module"
//                + showModType (this.type)
                + this.text
    }
}

function modSelect (m) {
    com.mode.devlog ("modSelect");
    selectedModule = m.sym;
    refreshModulesList ();
    refreshEditorBuffer ();
    com.mode.devlog ("modSelect returning");
}


function modClose (m) {
    com.mode.devlog ("close");
    let s = m.sym;
    let closedSelected = s === selectedModule;
    s16modules.delete (s);
    if (s16modules.size === 0) {
        com.mode.devlog ("closed last module, reinitializing");
        initModules ();
    }
    if (closedSelected) {
        //        selectedModule = [...s16modules.keys()][0];
        com.mode.devlog ("closedSelected... FIX THIS ??????????");
    }
    refreshModulesList ();
}
*/
