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

export function envSummary () {
    console.log ("Begin envSummary");
    for (const x of env.modules.keys()) {
        console.log (`key = ${x}`);
        const m = env.modules.get(x);
        console.log (`  bn=${m.baseName}`);
    }
    console.log ("End envSummary");
}

export class SystemState {
    constructor () {
        this.modules = new Map ();
        this.selectedModule = null;
        this.anonymousCount = 0;
        this.emState = null;
    }
    clearModules () {
        this.modules = new Map ();
        this.anonymousCount = 0;
        this.selectedModule = null;
    }
    mkSelectModule (mname) {
        if (mname && this.modules.has (mname)) {
            this.selectedModule = mname;
        } else if (mname) {
            this.selectedModule = mname;
            this.modules.set (mname, new S16Module (mname));
        } else {
            this.anonymousCount++;
            const xs = `anonymous${this.anonymousCount}`;
            this.modules.set (xs, new S16Module (xs));
            this.selectedModule = xs;
        }
        const m = this.modules.get(this.selectedModule);
        return m;
    }
    closeModule (mname) {
        this.modules.delete (mname);
    }
    getSelectedModule () {
        if (env.modules.size == 0) { // no modules
            this.anonymousCount++;
            const xs = `anonymous${this.anonymousCount}`;
            this.modules.set (xs, new S16Module (xs));
            this.selectedModule = xs;
        } else if (!this.selectedModule) { // nothing selected
            this.selectedModule = [...this.modules.keys()][0].baseName;
        } else if (this.modules.get(this.selectedModule)) { // it exists
        } else  { // it doesn't exist
            return this.mkModule (this.selectedModule);
        }
        return this.modules.get (this.selectedModule);

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
        xs += `<li> <b>`;
        xs += ((env.selectedModule == this.baseName)
               ? `<span class='SELECTEDFILE'>${this.baseName}</span>`
               : `${this.baseName}`);
        xs += `</b>\n`;
        xs += (env.selectedModule == this.baseName ? "Selected" : "" );
        xs += `<button id='${this.selectId}'>Select</button>`;
        xs += `<button id='${this.closeId}'>Close</button>`;
        xs += "<br>";
        xs += showFilePrefix (this.asmFile, "Assembly code:");
        //        xs += this.asmFile ? this.asmFile.text : "No asm file";
//        xs += this.asmFile
//            ? "<br>Assembly language:<br>" + textPrefix(this.asmFile.text)
//            : "No asm file";
//        xs += this.asmInfo ? this.asmInfo.showHtml() : "No assembly code<br>";
//        xs += this.objInfo ? this.objInfo.showHtml() : "No object code<br>";
//        xs += this.linkInfo ? this.linkInfo.showHtml() : "No linker code<br>";
        xs += `</li>\n`;
        xs += "</ul>\n";
        return xs;
    }
}

export const TextPrefixLength = 6;

function showFilePrefix (fr, label) {
    if (fr) {
        return "<div class='HighlightedTextAsHtml'>"
            + fr.text.split("\n").slice(0,TextPrefixLength).join("<br>")
            + "<div>\n";
    } else {
        return "";
    }
}
    
    

function textPrefix (xs) {
    return "<div class='HighlightedTextAsHtml'>"
        + xs.split("\n").slice(0,4).join("<br>");
        + "<div>\n";
}
