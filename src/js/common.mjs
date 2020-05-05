// Sigma16: common.js
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

//-------------------------------------------------------------------------------
// common.js
//-------------------------------------------------------------------------------

// The innerHTML
// string is <pre ...>text of example</pre>.  The pre and pre tags
// need to be removed: they would confuse the assembler.

export const openingPreTag = /^<[^>]*>/;   // <pre...> at beginning
export const closingPreTag = /<[^>]*>$/;   // ...</pre> at end

// Clear the display of the object code in the linker pane
export function clearObjectCode () {
    let listing = "<pre class='HighlightedTextAsHtml'>"
        + "</pre>"
    document.getElementById('LinkerText').innerHTML = listing;
}



// similar to highlightListingLine in emulator

export function highlightField (xs,highlight) {
    return "<span class='" + highlight + "'>" + xs + "</span>";
}

export function highlightListingLine (es,i,highlight) {
    es.asmListingCurrent[i] =
        "<span class='" + highlight + "'>" + es.asmListingPlain[i] + "</span>";
}

// "<span class='" + highlight + "'>" + es.asmListingPlain[i] + "</span>";
// "<div class='" + highlight + "'>" + es.asmListingPlain[i] + "</span>";
// scrolling doesn't work if it just uses <pre> but not <code>

export let editorBufferTextArea; /* set when window.onload */
export let textFile = null; /* for save download */
export let create;  /* for save download */
export let textbox; /* for save download */

