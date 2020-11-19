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

export const moduleEnvironment = new Map ();

// An S16Module is a container for all the files and objects that
// share the same basename

export class S16Module {
    constructor (baseName) {
        this.baseName = baseName;
        moduleEnvironment.set (baseName, this);
        //        this.asmModule = null; // ??? rename to asmInfo
        this.asmInfo = null;
        this.objInfo = null;
        this.linkerInfo = null;
        this.selectId = `select_${baseName}`;
        this.closeId = `close_${baseName}`;
        this.selectButton = `<button id='${this.selectId}'>Select</button>`;
        this.closeButton = `<button id='${this.closeId}'>Close</button>`;
    }
}

export function showS16Module () {
    console.log ("showS16Module");
    let xs = "<ul>\n";
    for (const bn of moduleEnvironment.keys()) {
        const m = moduleEnvironment.get(bn);
        xs += `<li> <b> ${bn} </b>`;
        xs += m.selectButton;
        xs += m.closeButton;
        xs += showAsmSubmodule (bn);
        xs += showObjSubmodule (bn);
        xs += showLinkSubmodule (bn);
        xs += "info about it"
        xs += `</li>\n`;
    }
    xs += "</ul>\n";
    return xs;
}

function showAsmSubmodule (m) {
    let xs = "Asm submodule"
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

