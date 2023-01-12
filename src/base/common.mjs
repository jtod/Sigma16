// Sigma16: common.js
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

//----------------------------------------------------------------------
// common.js
//----------------------------------------------------------------------

export const S16HOMEPAGEURL = 'https://jtod.github.io/home/Sigma16'

// ES_thread_host indicates which thread this emulator instance is
// running in.  This is represented with an unsigned int, not a
// symbol, so it can be stored in the system state vector.

export const ES_gui_thread      = 0
export const ES_worker_thread   = 1

export function showThread (x) {
    return x==0 ? "main"
        : x==1 ? "worker"
        : "?"
}

export function stacktrace () { console.trace () }

/*
export function showThread (x) {
    return x==0 ? ES_gui_thread_sym
        : x==1 ? ES_worker_thread_sym
        : null
}
*/

export let mode = {
    trace: false,
    showErr: true,
    setTrace () {
        this.trace = true
    },
    clearTrace () {
        this.trace = false
    },
    showMode () {
        console.log (`trace=${trace}`);
    },
    devlog (xs) {
        if (this.trace) { console.log (xs) }
    },
    errlog (xs) {
        if (this.showErr) { console.log (xs) }
    }
}

//----------------------------------------------------------------------
// Logging error message
//----------------------------------------------------------------------

export function indicateError (xs) {
    console.log (`%c${xs}`, 'color:red; font-weight:bold')
    console.trace ()
}

//----------------------------------------------------------------------
// Dialogues with the user
//----------------------------------------------------------------------

export function modalWarning (msg) {
    alert (msg);
}

// The innerHTML
// string is <pre ...>text of example</pre>.  The pre and pre tags
// need to be removed: they would confuse the assembler.

export const openingPreTag = /^<[^>]*>/;   // <pre...> at beginning
export const closingPreTag = /<[^>]*>$/;   // ...</pre> at end

// Clear the display of the object code in the linker pane

export function clearObjectCode () {
    let listing = "<pre class='HighlightedTextAsHtml'>"
        + "</pre>"
//    document.getElementById('LinkerText').innerHTML = listing;
    document.getElementById('LP_Body').innerHTML = listing;
//    console.log ("clearObjectCode skipping clear LinkerText");
}

// Similar to highlightListingLine in emulator

export function highlightField (xs,highlight) {
    return "<span class='" + highlight + "'>" + xs + "</span>";
}


// scrolling doesn't work if it just uses <pre> but not <code>

export let editorBufferTextArea; /* set when window.onload */
export let textFile = null; /* for save download */
export let create;  /* for save download */
export let textbox; /* for save download */

// Text

export function highlightText (txt,tag) {
    return "<span class='" + tag + "'>" + txt + "</span>";
}
