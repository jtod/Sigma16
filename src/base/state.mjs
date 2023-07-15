// Sigma16: state.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3
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

//-------------------------------------------------------------------------
// state.mjs defines global state for the system, IDE, modules, and
// emulator
//-------------------------------------------------------------------------

// The main components of the program avoid using global variables;
// instead the necessary state is organized into records and passed as
// needed to the functions.  This module defines those records.  The
// gui operations, defined in Sigma16.html, call the emulator's
// interface functios with the state, for example boot(emulatorState).

import * as com from './common.mjs'
import * as arch from './architecture.mjs'
import * as ab from './arrbuf.mjs'

//-------------------------------------------------------------------------
// Stages
//-------------------------------------------------------------------------

export const StageAsm = Symbol ("Asm");   // .asm.   assembly language source
export const StageObj = Symbol ("Obj");   // .obj.   object code
export const StageOmd = Symbol ("Omd");   // .omd.   metadata for obj
export const StageLnk = Symbol ("Lnk");   // .lnk.   link command
export const StageExe = Symbol ("Exe");   //.xmd.    metadata for exe
export const StageXmd = Symbol ("Xmd");   //.xmd.    metadata for exe

// Convert a string from filename into symbolic stage

export function getStageSym (xs) {
    return xs == "asm" ? StageAsm
        : xs == "obj" ? StageObj
        : xs == "omd" ? StageOmd
        : xs == "lnk" ? StageLnk
        : xs == "exe" ? StageExe
        : xs == "xmd" ? StageXmd
        : null
}

//-------------------------------------------------------------------------
// System state
//-------------------------------------------------------------------------

// The system state contains separate components for the ModuleSet,
// the emulator state, and the linker state.

export class SystemState {
    constructor () {
        this.moduleSet = null
        this.emulatorState = null;
        this.linkerState = null;
    }
}
//        this.modules = new Map ();
//        this.selectedModule = null;
//        this.anonymousCount = 0;

//-------------------------------------------------------------------------
// Global state variable
//-------------------------------------------------------------------------

// The environment is a global variable that contains all the system
// state.

export const env = new SystemState ();
// export const gst = env;  // refactor, use one or the other of gst, env
