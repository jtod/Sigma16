// Sigma16: gui.js
// Copyright (c) 2019 John T. O'Donnell.  john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. Sigma16/ LICENSE.txt COPYRIGHT.txt

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
// gui.js is the main program.  It's launched by Sigma16.html and is
// the last JavaScript file to be loaded

//-------------------------------------------------------------------------------
// Parameters
//-------------------------------------------------------------------------------

// Calculate the value of pxPerChar, which is needed to control the
// scrolling to make the current line visible.  The calculated value
// overrides the initialized value.  The method is to measure the
// height of the listing in pixels and divide by the number of lines
// in the listing.  Some other geometric parameters are also obtained,
// but aren't currently used.

function getListingDims (es) {
    let e = document.getElementById('ProcAsmListing');
    let x = e.getBoundingClientRect(); // dimensions of visible listing area
    let w = e.scrollWidth; // width of the content, not used
    let h = e.scrollHeight; // height of content (not of the window)
    es.asmListingHeight = h; // save in emulator state
    console.log (`h=${h} w=${w}`);
    let n = es.asmListingPlain.length;
    console.log(`getListingDims: n=${n}`);
    pxPerChar = h / n; // update this global variable, used for scrolling
    console.log (`getListingDims: pxPerChar = ${pxPerChar}`);
}

// asmScrollOffsetAbove specifies the preferred number of lines that
// should appear above the scroll target in the processor assembly
// listing

const asmScrollOffsetAbove = 8;

// pxPerChar is the height of characters used in the processor
// assembly listing.  This is needed to scroll the listing to keep the
// current line visible.  There doesn't appear to be a good way to
// measure this; the value is found by trial and error.  Measuring it
// or extracting it from font metadata would be far better.

let pxPerChar = 13.05;

//-------------------------------------------------------------------------------
// Debug and test tools
//-------------------------------------------------------------------------------

// Each field controls debug/test output for one aspect of the
// program.  The output will be produced iff the field is set to true.
// This can be done interactively in the browser console, which is
// toggled by shift-control-I.  For example, to give more information
// in the Modules list, enter developer.files = true in the console.

let developer = {
    files : null,   // give full file information in Modules list
    assembler : null
}


//-------------------------------------------------------------------------------
// Global variables
//-------------------------------------------------------------------------------

// Define the global variables; some are set by function initialize
// which is executed when window.onload occurs

var globalObject = this; // to enable script in userguide to define glob var
var myglobalvar = 9876; // can script in userguide see this?

var fileContents = "file not read yet"
var editorBufferTextArea; /* set when window.onload */
var textFile = null; /* for save download */
var create;  /* for save download */
var textbox; /* for save download */

function makeTextFile (text) {
    var data = new Blob([text], {type: 'text/plain'});

    // If we are replacing a previously generated file we need to
    // manually revoke the object URL to avoid memory leaks.
    if (textFile !== null) {
      window.URL.revokeObjectURL(textFile);
    }

    textFile = window.URL.createObjectURL(data);

    return textFile;
}

var ioLogBuffer = "";

function refreshIOlogBuffer() {
    console.log (`refreshIOlogBugfer ${ioLogBuffer}`);
    let elt = document.getElementById("IOlog");
    elt.innerHTML = "<pre>" + ioLogBuffer + "</pre>";
    elt.scrollTop = elt.scrollHeight;
}

// global variables for emulator
var procAsmListingElt;

//-------------------------------------------------------------------------------
// Experiments and testing
//-------------------------------------------------------------------------------

function jumpToAnchorInGuide () {
    console.log ("jumpToAnchorInGuide");
    let anchor = "#how-to-run-the-program";
    let elt = document.getElementById("UserGuideIframeId");
    let elthtml = elt.contentWindow.document.body.innerHTML;
    console.log (`anchor = ${anchor} elt=${elthtml}`);
    elthtml.location.hash = anchor;
}


function tryfoobar () {
/* Try to make button go to a point in the user guide */
    console.log ('trySearchUserguide');    
/*
    var midmainright = document.getElementById('MidMainRight');
    console.log ('midmainright = ' + midmainright);
    usrguidecontent = document.getElementById("WelcomeHtml").innerHTML;
    console.log ('usrguidecontent = ' + usrguidecontent);
 */
    let e = document.getElementById("THISISIT");
    console.log('e = ' + e);
}

function jumpToAnchor (target){
    console.log( 'jumpToAnchor ' + target);
    console.log('about to do it');
    let elt = document.getElementById('WelcomeHtml');
    console.log('did it');
    console.log('elt = ' + elt);
    let loc = elt.location;
    console.log('loc = ' + loc);
    elt.location.hash=target;
}

/* onclick="jumpToAnchor('itemAttributes');jumpToAnchor('x')"> */

// find out how slow it is to refresh a register
// is it worthwhile avoiding refreshing a register if it will also be highlighted?
// For n=10000 the time is about 88ms, fine for interactive use

// measure time for n register put operations
function measureRegPut (n) {
    var tstart = performance.now();
    for (var i = 0; i<n; i++) {
	pc.put(i);
    }
    var tend = performance.now();
    console.log('measureRegRefresh (' + n + ') took '
		+ (tend - tstart) + ' ms');
}

// function springen(anker) { 
//    var childWindow =  document.getElementById("UserGuideIframeId").contentWindow;
//     childWindow.scrollTo(0,childWindow.document.getElementById(anker).offsetTop);
// }

// Scroll user guide to an anchor
function jumpToGuideSection (anchor) {
    let elt = document.getElementById("UserGuideIframeId").contentWindow;
    elt.scrollTo(0,elt.document.getElementById(anchor).offsetTop);
}

// Scroll user guide to top
function jumpToGuideTop () {
    let elt = document.getElementById("UserGuideIframeId").contentWindow;
    elt.scrollTo(0,0);
}

// Want to make Editor button 1 go to an anchor in the User Guide
// Doesn't work yet
// I put this manually into the user guide: <a href="HREFTESTING">dummy href</a>
function editorButton1() {
    console.log("Editor button 1 clicked");
    // Try to visit <a  href="file:Readme"> in the user guide
    let userGuideElt = document.getElementById("MidMainRight");
    console.log("UserGuideElt = " + userGuideElt);
    window.location.hash = "#HREFTESTING";
	
//    var loc = userGuideElt.location;
//    console.log ("ed button 1, loc = " + loc);
//    loc.href = "#HREFTESTING";
    
}

//-------------------------------------------------------------------------------
// Examples pane
//-------------------------------------------------------------------------------

// Errors in displaying the directories: GET 404:
// www.dcs.gla.ac.uk/icons/blank.gif
// www.dcs.gla.ac.uk/:9uk/icons/text.gif

function examplesHome() {
    console.log ("examplesHome");
    document.getElementById("ExamplesIframeId").src =
	"../examples/index.html";
}

// Copy the example text to the editor.  The example is shown as a web
// page and its content is obtained using innerHTML.  The innerHTML
// string is <pre ...>text of example</pre>.  The pre and pre tags
// need to be removed: they would confuse the assembler.

const openingPreTag = /^<[^>]*>/;   // <pre...> at beginning
const closingPreTag = /<[^>]*>$/;   // ...</pre> at end


// This does not work.  Perhaps because it's an iframe, not an input?
// Copy text of example buffer to clipboard
function copyExampleToClipboard () {
    console.log ('Copy example to clipboard');
    let exElt = document.getElementById('ExamplesIframeId');
    exElt.select();
    exElt.setSelectionRange(0,5);
    document.execCommand('copy');
}


// var myIFrame = document.getElementById("myIframe");
// var content = myIFrame.contentWindow.document.body.innerHTML;

//-------------------------------------------------------------------------------
// Editor pane
//-------------------------------------------------------------------------------

function editorClear () {
    document.getElementById('EditorTextArea').value = "";
}

//-------------------------------------------------------------------------------
// Window sizing: adjust relative size of system and user guide
//-------------------------------------------------------------------------------

// For the window resizing: relative size of system and user guide
// sections.  All the code implementing feature appears here, apart
// from a one line call to the initialization function within the
// onload event handler

// Commented out the div class="MiddleSectionResizeHandle" in
// Sigma16.html.  Any mentions of this in the css file should be
// ignorable.  When this was commented out, the system and doc
// sections run up right against each other, but should be possible
// later to get some space between them.

// windowWidthb = 498.5
// full frame width = 997.2
// middle section width = 965.2
// mid main left width = 571.85
// mid main rigth width = 393.35
//   left + right width = 965.2

// Persistent variables given values by initialize_mid_main_resizing ()

var windowWidth;     // inner width of entire browser window
var middleSection;  // the middle section of the window; set in onload
var midMainLeft;     // mid-main-left; set in onload
var midMainRight;     // mid-main-right; set in onload, not used anywhere
var midLRratio = 0.6;  // width of midMainLeft / midMainRight; set in onLoad
var midSecExtraWidth = 15;  // width of borders in px

// Initialize the variables (middleSection, midMainLeft, midMainRight)
// in the onload event, because the DOI elements must exist before the
// variables are assigned.

function initialize_mid_main_resizing () {
    console.log ('initializing mid-main resizing')
    middleSection = document.getElementById("MiddleSection");
    midMainLeft = document.getElementById("MidMainLeft");
    midMainRight = document.getElementById("MidMainRight");
    windowWidth =  window.innerWidth;
}

// Update the saved ratio
function setMidMainLRratio (r) {
//    console.log ('setMidMainLRratio:  midLRratio = ' + r)
    midLRratio = r;
}

// Readjust the widths of left and right sections to match ratio r
function adjustToMidMainLRratio () {
    console.log ('adjustToMidMainLRratio:  midLRratio = ' + midLRratio)
    let ww =  window.innerWidth - midSecExtraWidth;
    let x = midLRratio * ww;
//    console.log ('  windowWidth = ' + windowWidth);
//    console.log ('  setting left width = ' + x);
//    console.log ('  about to call set left width');
    setMidMainLeftWidth (x);
//    console.log ('  back from calling set left width');
}

// grow/shrink the left section to w pixels
function setMidMainLeftWidth (newxl) {
    console.log ('setMidMainLeftWidth ' + newxl);

    let ww =  window.innerWidth - midSecExtraWidth;
    let oldxl = midMainLeft.style.width;
    let oldratio = midLRratio;
    console.log ('  old dimensions: ww = ' + ww +
		 ' oldxl=' + oldxl + ' oldratio=' + oldratio);

    let newxr = ww - newxl;
    let newxlp = newxl + "px";
    let newratio = newxl / (newxl + newxr);
    console.log ('  new dimensions: ww = ' + ww +
		 ' newxl=' + newxl + ' newxr=' + newxr + ' newratio=' + newratio);

    setMidMainLRratio (newratio);

    console.log ('  setting left = ' + newxl + '  right = ' + newxr);
    midMainLeft.style.width = newxlp;
    midMainLeft.style.flexGrow = 0; // without this they don't grow/shrink together

    console.log ('  left width:   old=' + oldxl + ' new=' + newxl);
    console.log ('  ratio:  old=' + oldratio + '  new=' + newratio);
    console.log ('setMidMainLeftWidth finished');

    /*
    midMainLeft.style.width = xl;
    midMainRight.style.width = xl;
    midMainLeft.style.flexGrow = 0; // without this they don't grow/shrink together

    midMainLeft.style.flexGrow = xlp
    midMainRight.style.flexGrow = xrp;

    midMainLeft.style.flexBasis = xlp
    midMainRight.style.flexBasis = xrp;
    */
    

}

function expLRflex (xl) {
    console.log ('expLRflex');
    let ww =  window.innerWidth - midSecExtraWidth;
    let xr = ww - xl;
    let xlp = xl + 'px';
    let xrp = xr + 'px';
    midMainLeft.style.flexBasis = xlp;
    midMainLeft.style.flexGrow = '0px';
    midMainRight.style.flexBasis = xrp;
    midMainRight.style.flexGrow = '0px';
}

function showSizeParameters () {
//    console.log ('showSizeParameters');
    let ww =  window.innerWidth - midSecExtraWidth;
    let y = midMainLeft.style.width;
//    console.log ('  windowWidth = ' + ww);
//    console.log ('  midMainLeftWidth = ' + y);
//    console.log ('  midLRratio = ' + midLRratio);
}

// Resize the system (midMainLeft) and user guide (midMainRight)
// sections.  When the - or + button is clicked in the GUI,
// user_guide_resize (x) is called: x>0 means expand the user guide by
// x px; x<0 means shrink it.

function user_guide_resize(x) {
    console.log ('user_guide_resize ' + x);
//    showSizeParameters ();
    let old_width = midMainLeft.style.width;
    console.log ('  old width = ' + old_width);
    let w = parseInt(midMainLeft.style.width,10);
    console.log ('  old width number = ' + w);
    let new_width = w+x;
    console.log ('  new_width = ' + new_width)
    setMidMainLeftWidth (new_width);
//    let z = (w + x) + "px";
//    console.log (' mml z = ' + z);
//    midMainLeft.style.width = z;
//    midMainLeft.style.flexGrow = 0; // without this they don't grow/shrink together
    showSizeParameters ();
}

//    let containerOffsetLeft = middleSection.offsetLeft;
//		+ ' containerOffsetLeft=' + containerOffsetLeft

//    document.getElementById("EditorTextArea").style.width= z + 'px';

// Diagnostics

function checkTestBody () {
//    console.log ('checkTestBody width = ' + testPaneBodyElt.style.width);
}

// deprecated, delete
/* initialize sizes of left and right parts of middle section */
//    let y = (w+ 10*x) + "px";
//    document.getElementById("AssemblerText").style.width = z + 'px';
// var testPaneBodyElt;
//    testPaneBodyElt = document.getElementById('TestPaneBody');



//-------------------------------------------------------------------------------
// Top level tab buttons control which pane is visible and active
//-------------------------------------------------------------------------------

let breakDialogueVisible = false;

function procBreakpoint () {
    console.log ("procBreakpoint");
    document.getElementById("BreakDialogue").style.display
	= breakDialogueVisible ? "none" : "block";
    breakDialogueVisible = !breakDialogueVisible;
}

function hideBreakDialogue () {
    document.getElementById("BreakDialogue").style.display = "none";
    breakDialogueVisible = false;
}

function hideTabbedPane(paneId) {
//    console.log("hiding tabbed pane " + paneId);
    document.getElementById(paneId).style.display = "none";
}

function hideAllTabbedPanes() {
    hideTabbedPane("WelcomePane");
    hideTabbedPane("ExamplesPane");
    hideTabbedPane("ModulesPane");
    leaveEditor(); // will also hide editor pane
    hideTabbedPane("AssemblerPane");
    hideTabbedPane("LinkerPane");
    hideTabbedPane("ProcessorPane");
    hideTabbedPane("TestPane");
}

function showTabbedPane(paneId) {
//    console.log("showing tabbed pane " + paneId);
    hideAllTabbedPanes();
    document.getElementById(paneId).style.display = "block";
    console.log("Now on tabbed pane " + paneId);
}

function welcome_pane_button() {
//    console.log("welcome_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("WelcomePane");
}

function examples_pane_button() {
    hideAllTabbedPanes();
    showTabbedPane("ExamplesPane");
}

function modules_pane_button() {
    console.log("modules_pane_button clicked")
    hideAllTabbedPanes();
    refreshModulesList();
    showTabbedPane("ModulesPane");
}

function editor_pane_button() {
//    console.log("editor_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("EditorPane");
}

function assembler_pane_button() {
//    console.log("assembler_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("AssemblerPane");
}

function linker_pane_button() {
//    console.log("linker_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("LinkerPane");
}

function processor_pane_button() {
//    console.log("processor_pane button clicked")
    hideAllTabbedPanes();
    showTabbedPane("ProcessorPane");
}

function test_pane_button() {
//    console.log("test_pane button clicked")
    hideAllTabbedPanes();
    showTabbedPane("TestPane");
}

function userman_pane_button() {
    console.log("userman_pane_button clicked")
}


//-------------------------------------------------------------------------------
// Example programs
//-------------------------------------------------------------------------------

function insert_example(exampleText) {
    console.log('Inserting example add into editor text');
    document.getElementById('EditorTextArea').value = exampleText;
};

const example_hello_world =
`; Program Hello, world!
; A simple starter program for Sigma16

; Calculate result := 6 * x, where x = 7

     lea    R1,6[R0]       ; R1 := 6
     load   R2,x[R0]       ; R2 := x (variable initialized to 7)
     mul    R3,R1,R2       ; R3 := 6 * x = 42 (hex 002a)
     store  R3,result[R0]  ; result := 6 * x
     trap   R0,R0,R0       ; halt

; How to run the program:
;   (1) Translate to machine language: Assembler tab, click Assemble
;   (2) Run it: Processor tab, Boot, click Step for each instruction

; When the program halts, we should see the following:
;   R1 contains  6 (0006)
;   R2 contains  7 (0007)
;   R3 contains 42 (002a)
;   result contains 42 (002a)
;   result is in memory, and the assembly listing shows its address

; Variables are defined  after the program
x         data   7         ; initial value of x = 7
result    data   0         ; initial value of result = 0
`;

//-------------------------------------------------------------------------------
//  Handle window events
//-------------------------------------------------------------------------------

// This doesn't seem to work.  Want to ask user to confirm if they click back
// when it would abort the session

//window.addEventListener('beforeunload', function () {
//    console.log ('Really???');
  // Cancel the event
//  e.preventDefault();
//    console.log ('Really????????????');
  // Chrome requires returnValue to be set
//    e.returnValue = '';
//    return 'you hit back button do you mean it?';
// });

window.onbeforeunload = function(event) {
    event.returnValue = "Write something clever here..";
};

// Warning before leaving the page (back button, or outgoinglink)
//window.onbeforeunload = function() {
//   return "Do you really want to leave our brilliant application?";
   //if we return nothing here (just calling return;) then there will be no pop-up question at all
   //return;
//};

window.onresize = function () {
    console.log ('window.onresize');
//    showSizeParameters ();
    //    setMidMainLRratio (midLRratio);  // preserve ratio as window is resized
    adjustToMidMainLRratio ();
    console.log ('window.onresize finished');
}



// https://stackoverflow.com/questions/31048215/how-to-create-txt-file-using-javascript-html5

let myblob = new Blob(["Hello, world!"], {type: "text/plain;charset=utf-8"});

function foobarSaveAs () {
    saveAs(blob, "hello-world-file.txt");
}



/*
https://www.codeproject.com/Questions/896991/create-file-by-javascript
So, we just need to call the script on window onload like.
Hide   Expand    Copy Code
window.onload = function(){
            var textFile = null,

            makeTextFile = function (text) {
                                var data = new Blob([text], {type: 'text/plain'});

                                // If we are replacing a previously generated file we need to
                                // manually revoke the object URL to avoid memory leaks.
                                if (textFile !== null) {
                                  window.URL.revokeObjectURL(textFile);
                                }

                                textFile = window.URL.createObjectURL(data);

                                return textFile;
                            };

            var create = document.getElementById('create'),
            textbox = document.getElementById('textbox');

            create.addEventListener('click', function () {
                                                var link = document.getElementById('downloadlink');
                                                link.href = makeTextFile(textbox.value);
                                                link.style.display = 'block';
                                            }, false);
        };

*/

//-------------------------------------------------------------------------------
// Complete initialization when onload occurs
//-------------------------------------------------------------------------------

window.onload = function () {
    
    console.log("window.onload activated");

//    s16modules = [];    // All the modules in the system
//    nModules = 1;
//    selectedModule = 0;
    initModules();

    showTabbedPane("WelcomePane");
    initializeProcessorElements ();  // so far, it's just instr decode
    clearInstrDecode (emulatorState);
    
    hideBreakDialogue ();

// Initialize the modules
    initModules ();
    document.getElementById('LinkerText').innerHTML = "";    

// Initialize the registers

    // Register file
    // R0 is built specially as it is constant; all others are built with mkReg

    nRegisters = 0;
    for (var i = 0; i<16; i++) {
	let regname = 'R' + i; // also the id for element name
//	console.log('xx' + regname + 'xx');
	thisReg = (i==0) ?
	    mkReg0 (regname, regname, wordToHex4)
	    : mkReg (regname, regname, wordToHex4);
	thisReg.regIdx = i;
	regFile[i] = thisReg;
	register[i] = thisReg;
    }

// Instruction control registers
    pc       = mkReg ('pc',       'pcElt',       wordToHex4);
    ir       = mkReg ('ir',       'irElt',       wordToHex4);
    adr      = mkReg ('adr',      'adrElt',      wordToHex4);
    dat      = mkReg ('dat',      'datElt',      wordToHex4);
    //    sysStat  = mkReg ('sysStat',  'sysStatElt',  showSysStat);
    // bit 0 (lsb) :  0 = User state, 1 = System state
    // bit 1       :  0 = interrupts disabled, 1 = interrupts enabled
    // bit 2       :  0 = segmentation disabled, 1 = segmentation enabled

    // Interrupt control registers
    ctlRegIndexOffset = nRegisters;
    statusreg   = mkReg ('statusreg',  'statusElt',  wordToHex4);
//    ienable  = mkReg ('ienable',  'enableElt',  showBit);
    mask     = mkReg ('mask',     'maskElt',    wordToHex4);
    req      = mkReg ('req',      'reqElt',     wordToHex4);
    // mask and request
    // bit 0 (lsb)  overflow
    // bit 1        divide by 0
    // bit 2        trap 3
    // bit 3        
    istat    = mkReg ('istat',    'istatElt',      wordToHex4);
    ipc      = mkReg ('ipc',      'ipcElt',      wordToHex4);
    vect   = mkReg ('vect',   'vectElt', wordToHex4);

// Segment control registers
//    sEnable  = mkReg ('sEnable',  'sEnableElt',  showBit);
    bpseg = mkReg ('bpseg',    'bpsegElt',    wordToHex4);
    epseg = mkReg ('epseg',    'epsegElt',    wordToHex4);
    bdseg = mkReg ('bdseg',    'bdsegElt',    wordToHex4);
    edseg = mkReg ('edseg',    'edsegElt',    wordToHex4);

// Record the control registers    
    nRegisters = 16;  // Start after the first 16 (the regfile)
    controlRegisters =
	[pc, ir, adr, dat,   // not accessible to getctl/putctl instructions
	 // the following can be used for getctl/getctl, indexing from 0
	 statusreg,
	 mask, req, istat, ipc, vect,
         bpseg, epseg, bdseg, edseg
	];
    controlRegisters.forEach (function (r) {
	console.log('making reg ' + nRegisters + ' = ' + r.regName);
	register[nRegisters] = r;
	r.regIdx = nRegisters;
	nRegisters++;
        });


// Initialize the memory
    memInitialize();
    procAsmListingElt = document.getElementById('ProcAsmListing');
    
    editorBufferTextArea = document.getElementById("EditorTextArea");
    
/* for save download */
//    create = document.getElementById('CreateFileForDownload'),
//    textbox = document.getElementById('DownloadFileTextBox');
//  create.addEven
//    var link = document.getElementById('downloadlink');
//    link.href = makeTextFile(textbox.value);
//    link.style.display = 'block';
    //  }, false);


    resetRegisters();
    initialize_mid_main_resizing ();
    setMidMainLRratio(0.65);  // useful for dev to keep mem display visible
    showSizeParameters();
    adjustToMidMainLRratio();
    initializeSubsystems ();

    
    console.log("Initialization complete");
}


/*
https://stackoverflow.com/questions/31048215/how-to-create-txt-file-using-javascript-html5

(function () {
var textFile = null,
  makeTextFile = function (text) {
    var data = new Blob([text], {type: 'text/plain'});

    // If we are replacing a previously generated file we need to
    // manually revoke the object URL to avoid memory leaks.
    if (textFile !== null) {
      window.URL.revokeObjectURL(textFile);
    }

    textFile = window.URL.createObjectURL(data);

    return textFile;
  };


  var create = document.getElementById('create'),
    textbox = document.getElementById('textbox');

  create.addEventListener('click', function () {
    var link = document.getElementById('downloadlink');
    link.href = makeTextFile(textbox.value);
    link.style.display = 'block';
  }, false);
})();

*/
