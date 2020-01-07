// Sigma16: linker.js
// Copyright (c) 2019 John T. O'Donnell.  john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. Sigma16/ LICENSE.txt NOTICE.txt

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
// linker.js combines a collection of object modules to form an
// executable module, performing address relocation as needed.
//-------------------------------------------------------------------------------

var exMod;           // the module that is executing
var curAsmap = [];

// Clear the display of the object code in the linker pane

function clearObjectCode () {
    let listing = "<pre class='HighlightedTextAsHtml'>"
        + "</pre>"
    document.getElementById('LinkerText').innerHTML = listing;
}

function setCurrentObjectCode () {
    let objHeader = "Module " + selectedModule + " object code"
    let objText =
	"<pre class='HighlightedTextAsHtml'><span class='ListingHeader'>"
	+ objHeader + "</span>\n"
	+ s16modules[selectedModule].objectCode.join('\n')
	+ "</pre>";
    document.getElementById('LinkerText').innerHTML	= objText;

}

function showLinkerStatus () {
    console.log ('showLinkerStatus');
    let listing = "<pre class='HighlightedTextAsHtml'>"
        + "no modules selected"
        + "</pre>"
    document.getElementById('LinkerText').innerHTML = listing;
}

function setLinkerModules () {
    console.log ('setLinkerModules');
}

function link () {
    console.log ('link');
}
