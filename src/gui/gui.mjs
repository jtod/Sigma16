// Sigma16: gui.mjs
// Copyright (C) 2021 John T. O'Donnell
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
// gui.mjs is the main program for the browser version.  It's launched
// by Sigma16.html and is the last JavaScript file to be loaded
//-----------------------------------------------------------------------------

import * as ver   from '../base/version.mjs';
import * as com   from '../base/common.mjs';
import * as smod  from '../base/s16module.mjs';
import * as arch  from '../base/architecture.mjs';
import * as arith from '../base/arithmetic.mjs';
import * as st    from '../base/state.mjs';
import * as ed    from './editor.mjs';
import * as asm   from '../base/assembler.mjs';
import * as link  from '../base/linker.mjs';
import * as em    from '../base/emulator.mjs';
import * as gt    from "./guitools.mjs";

export function modalWarning (msg) {
    alert (msg);
}

//-------------------------------------------------------------------------------
// Parameters and global variables
//-------------------------------------------------------------------------------

// probably won't need this
let globalObject = this; // to enable script in userguide to define glob var

let fileContents = "file not read yet"

// Persistent variables given values by initialize_mid_main_resizing ()

let windowWidth;     // inner width of entire browser window
let middleSection;  // the middle section of the window; set in onload
let midMainLeft;     // mid-main-left; set in onload
let midMainRight;     // mid-main-right; set in onload, not used anywhere
let midLRratio = 0.6;  // width of midMainLeft / midMainRight; set in onLoad
let midSecExtraWidth = 15;  // width of borders in px

//------------------------------------------------------------------------------
// Tabbed panes
//------------------------------------------------------------------------------

// Symbols identify the panes that can be displayed

export const WelcomePane   = Symbol ("WelcomePane");
export const ExamplesPane  = Symbol ("ExamplesPane");
export const ModulesPane   = Symbol ("ModulesPane");
export const EditorPane    = Symbol ("EditorPane");
export const AssemblerPane = Symbol ("AssemblerPane");
export const LinkerPane    = Symbol ("LinkerPane");
export const ProcessorPane = Symbol ("ProcessorPane");
export const DevToolsPane  = Symbol ("DevToolsPane");

let currentPane = WelcomePane; // The current pane is displayed; others are hidden

// Return the string Id for a Pane symbol; needed for getElementById
function paneIdString (p) { return p.description }

// When the program starts, show the Welcome page and hide the others

function initializePane () {
    currentPane = WelcomePane;
    let f = (p,x) => document.getElementById(paneIdString(p)).style.display = x;
    f (WelcomePane, "block");
    f (ExamplesPane, "none");
    f (ModulesPane, "none");
    f (EditorPane, "none");
    f (AssemblerPane, "none");
    f (LinkerPane, "none");
    f (ProcessorPane, "none");
    f (DevToolsPane, "none");
}

// Leave the current pane and switch to p; run showInitializer if the
// pane has one.  Provide for possible hooks.

export function showPane (p) {
    com.mode.devlog (`showPane ${p.description}`);
    finalizeLeaveCurrentPane ();
    currentPane = p;
    switch (currentPane) {
    case WelcomePane:
        break;
    case ExamplesPane: ;
        break;
    case ModulesPane: ;
        smod.refreshModulesList ();
        break;
    case EditorPane:
        ed.enterEditor ();
        break;
    case AssemblerPane:
        asm.enterAssembler ();
        break;
    case LinkerPane:
        break;
    case ProcessorPane:
        break;
    case DevToolsPane:
        break;
    }
    document.getElementById(paneIdString(p)).style.display = "block";
    com.mode.devlog(`Show pane ${p.description}`);
}

// Provide a finalizer to save state when pane is hidden

export function finalizeLeaveCurrentPane () {
    com.mode.devlog (`Leave pane ${currentPane.description}`);
    switch (currentPane) {
    case WelcomePane:
        break;
    case ExamplesPane: ;
        break;
    case ModulesPane: ;
        break;
    case EditorPane:
        ed.leaveEditor ();
        break;
    case AssemblerPane:
        break;
    case LinkerPane:
        break;
    case ProcessorPane:
        break;
    case DevToolsPane:
        break;
    }
    document.getElementById(paneIdString(currentPane)).style.display = "none";
}

//-------------------------------------------------------------------------------
// Debug, testing, and experiments
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

function makeTextFile (text) {
    let data = new Blob([text], {type: 'text/plain'});

    // If we are replacing a previously generated file we need to
    // manually revoke the object URL to avoid memory leaks.
    if (textFile !== null) {
      window.URL.revokeObjectURL(textFile);
    }

    textFile = window.URL.createObjectURL(data);

    return textFile;
}

function jumpToAnchorInGuide () {
    com.mode.devlog ("jumpToAnchorInGuide");
    let anchor = "#how-to-run-the-program";
    let elt = document.getElementById("UserGuideIframeId");
    let elthtml = elt.contentWindow.document.body.innerHTML;
    com.mode.devlog (`anchor = ${anchor} elt=${elthtml}`);
    elthtml.location.hash = anchor;
}

function tryfoobar () {
/* Try to make button go to a point in the user guide */
    console.log ('trySearchUserguide');    
/*
    let midmainright = document.getElementById('MidMainRight');
    console.log ('midmainright = ' + midmainright);
    usrguidecontent = document.getElementById("WelcomeHtml").innerHTML;
    console.log ('usrguidecontent = ' + usrguidecontent);
 */
    let e = document.getElementById("THISISIT");
    console.log('e = ' + e);
}

function jumpToAnchor (target){
    com.mode.devlog( 'jumpToAnchor ' + target);
    com.mode.devlog('about to do it');
    let elt = document.getElementById('WelcomeHtml');
    com.mode.devlog('did it');
    com.mode.devlog('elt = ' + elt);
    let loc = elt.location;
    com.mode.devlog('loc = ' + loc);
    elt.location.hash=target;
}

/* onclick="jumpToAnchor('itemAttributes');jumpToAnchor('x')"> */

// find out how slow it is to refresh a register
// is it worthwhile avoiding refreshing a register if it will also be highlighted?
// For n=10000 the time is about 88ms, fine for interactive use

// measure time for n register put operations
function measureRegPut (n) {
    let tstart = performance.now();
    for (let i = 0; i<n; i++) {
	pc.put(i);
    }
    let tend = performance.now();
    com.mode.devlog('measureRegRefresh (' + n + ') took '
		+ (tend - tstart) + ' ms');
}

// function springen(anker) { 
//    let childWindow =  document.getElementById("UserGuideIframeId").contentWindow;
//     childWindow.scrollTo(0,childWindow.document.getElementById(anker).offsetTop);
// }

// Scroll user guide to an anchor
function showGuideSection (anchor) {
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
    com.mode.devlog("Editor button 1 clicked");
    // Try to visit <a  href="file:Readme"> in the user guide
    let userGuideElt = document.getElementById("MidMainRight");
    com.mode.devlog("UserGuideElt = " + userGuideElt);
    window.location.hash = "#HREFTESTING";
	
//    let loc = userGuideElt.location;
//    console.log ("ed button 1, loc = " + loc);
//    loc.href = "#HREFTESTING";
    
}

//------------------------------------------------------------------------------
// Define actions for buttons
//------------------------------------------------------------------------------

// Connect a button in the html with its corresponding function

function prepareButton (bid,fcn) {
    com.mode.devlog (`prepare button ${bid}`);
    document.getElementById(bid)
        .addEventListener('click', event => {fcn()});
}

// Pane buttons

prepareButton ('Welcome_Pane_Button',   () => showPane(WelcomePane));
prepareButton ('Examples_Pane_Button',  () => showPane(ExamplesPane));
prepareButton ('Modules_Pane_Button',   () => showPane(ModulesPane));
prepareButton ('Editor_Pane_Button',    () => showPane(EditorPane));
prepareButton ('Assembler_Pane_Button', () => showPane(AssemblerPane));
prepareButton ('Linker_Pane_Button',    () => showPane(LinkerPane));
prepareButton ('Processor_Pane_Button', () => showPane(ProcessorPane));
prepareButton ('DevTools_Pane_Button', () => showPane(DevToolsPane));
prepareButton ('About_Button',         () => showGuideSection('about-sigma16'));  

// User guide resize (UGR) buttons
// UGR Distance (px) to move boundary between gui and userguide on resize
const UGRSMALL = 1;
const UGRLARGE = 20;
prepareButton ('UG_Resize_Right_Large_Button', () => user_guide_resize(UGRLARGE));
prepareButton ('UG_Resize_Right_Small_Button', () => user_guide_resize(UGRSMALL));
prepareButton ('UG_Resize_Left_Small_Button', () => user_guide_resize(-UGRSMALL));
prepareButton ('UG_Resize_Left_Large_Button', () => user_guide_resize(-UGRLARGE));

// Welcome pane (WP)
prepareButton ('WP_Guide_Top', jumpToGuideTop);
prepareButton ('WP_Tutorials', () => showGuideSection('tutorial'));
prepareButton ('WP_Architecture', () => showGuideSection('architecture'));
prepareButton ('WP_ISA', () => showGuideSection('instruction-set'));
prepareButton ('WP_Assembly_Language', () => showGuideSection('assembly-language'));
prepareButton ('WP_Linker', () => showGuideSection('linker'));
prepareButton ('WP_Programming', () => showGuideSection('programming'));

// Examples pane (EXP)
prepareButton ('EXP_Examples_Home',    examplesHome);
prepareButton ('EXP_Select_Example',    smod.selectExample);

// Modules pane (MP)
// prepareButton ('MP_New',    smod.newModule);
prepareButton ('MP_Refresh',    smod.refreshModulesList);
prepareButton ('MP_New',        smod.newMod);
prepareButton ('MP_Hello_world', () => insert_example(example_hello_world));

// Editor pane (EDP)
prepareButton ('EDP_Selected',    ed.edSelectedButton);
prepareButton ('EDP_Clear',       ed.edClear);
prepareButton ('EDP_New',         ed.edNew);
prepareButton ('EDP_Revert',      ed.edRevert);
prepareButton ('EDP_Asm',         ed.edAsm);
prepareButton ('EDP_Obj',         ed.edObj);
prepareButton ('EDP_Exe',         ed.edExe);
prepareButton ('EDP_Link',        ed.edLink);
prepareButton ('EDP_Hello_world', () => insert_example(example_hello_world));
prepareButton ('EDP_Save',        ed.edDownload);

// Assembler pane (AP)
prepareButton ('AP_Assemble',        asm.assemblerGUI);
prepareButton ('AP_Show_Source',     asm.displayAsmSource);
prepareButton ('AP_Show_Object',     asm.setObjectListing);
prepareButton ('AP_Show_Listing',    asm.setAsmListing);
prepareButton ('AP_Show_Metadata',   asm.setMetadata);

// Linker pane (LP)
prepareButton ('LP_Link',            link.linkerGUI);
prepareButton ('LP_Read_Object',     link.getLinkerModules);
prepareButton ('LP_Show_Executable', link.linkShowExecutable);
prepareButton ('LP_Show_Metadata',   link.linkShowMetadata);

// Processor pane (PP)
prepareButton ('PP_Reset',        () => em.procReset(em.emulatorState));
prepareButton ('PP_Boot',         () => em.boot(em.emulatorState));
prepareButton ('PP_Step',         () => em.procStep(em.emulatorState));
prepareButton ('PP_Run',          () => em.procRun(em.emulatorState));
prepareButton ('PP_Pause',        () => em.procPause(em.emulatorState));
prepareButton ('PP_Interrupt',    () => em.procInterrupt(em.emulatorState));
prepareButton ('PP_Breakpoint',   () => em.procBreakpoint(em.emulatorState));
prepareButton ('PP_Timer_Interrupt',  em.timerInterrupt);
prepareButton ('PP_Toggle_Display',  em.toggleFullDisplay);

// Breakpoint popup dialogue
/*
prepareButton ("BreakRefresh", em.breakRefresh(em.emulatorState));
prepareButton ("BreakEnable", em.breakEnable(em.emulatorState));
prepareButton ("BreakDisable", em.breakDisable(em.emulatorState));
prepareButton ("BreakClose", em.breakClose());
*/

// DevTools
prepareButton ('DevTools100',    devTools100);
prepareButton ('DevTools101',    devTools101);
prepareButton ('DevTools102',    devTools102);
prepareButton ('DevTools103',    devTools103);
prepareButton ('DevTools104',    devTools104);
prepareButton ('DevTools105',    devTools105);
prepareButton ('DevTools106',    devTools106);
prepareButton ('DisableDevTools', disableDevTools);

//------------------------------------------------------------------------------
// Examples pane
//------------------------------------------------------------------------------

// This file is Sigma16/src/gui/gui.mjs
// The examples directory is Sigma16/examples
// The index for the examples directory is ../../examples/index.html

function examplesHome() {
    com.mode.devlog ("examplesHome");
    document.getElementById("ExamplesIframeId").src =
	"../../examples/index.html";
}

// Copy the example text to the editor.  The example is shown as a web
// page and its content is obtained using innerHTML.

// This does not work.  Perhaps because it's an iframe, not an input?
// Copy text of example buffer to clipboard
function copyExampleToClipboard () {
    com.mode.devlog ('Copy example to clipboard');
    let exElt = document.getElementById('ExamplesIframeId');
    exElt.select();
    exElt.setSelectionRange(0,5);
    document.execCommand('copy');
}

// let myIFrame = document.getElementById("myIframe");
// let content = myIFrame.contentWindow.document.body.innerHTML;

//-------------------------------------------------------------------------------
// Editor pane
//-------------------------------------------------------------------------------

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


// Initialize the variables (middleSection, midMainLeft, midMainRight)
// in the onload event, because the DOI elements must exist before the
// variables are assigned.

function initialize_mid_main_resizing () {
    com.mode.devlog ('initializing mid-main resizing')
    middleSection = document.getElementById("MiddleSection");
    midMainLeft = document.getElementById("MidMainLeft");
    midMainRight = document.getElementById("MidMainRight");
    windowWidth =  window.innerWidth;
}

// Update the saved ratio
function setMidMainLRratio (r) {
//    com.mode.devlog ('setMidMainLRratio:  midLRratio = ' + r)
    midLRratio = r;
}

// Readjust the widths of left and right sections to match ratio r
function adjustToMidMainLRratio () {
    com.mode.devlog ('adjustToMidMainLRratio:  midLRratio = ' + midLRratio)
    let ww =  window.innerWidth - midSecExtraWidth;
    let x = midLRratio * ww;
//    com.mode.devlog ('  windowWidth = ' + windowWidth);
//    com.mode.devlog ('  setting left width = ' + x);
//    com.mode.devlog ('  about to call set left width');
    setMidMainLeftWidth (x);
//    com.mode.devlog ('  back from calling set left width');
}

// grow/shrink the left section to w pixels
function setMidMainLeftWidth (newxl) {
    com.mode.devlog ('setMidMainLeftWidth ' + newxl);

    let ww =  window.innerWidth - midSecExtraWidth;
    let oldxl = midMainLeft.style.width;
    let oldratio = midLRratio;
    com.mode.devlog ('  old dimensions: ww = ' + ww +
		 ' oldxl=' + oldxl + ' oldratio=' + oldratio);

    let newxr = ww - newxl;
    let newxlp = newxl + "px";
    let newratio = newxl / (newxl + newxr);
    com.mode.devlog ('  new dimensions: ww = ' + ww +
		 ' newxl=' + newxl + ' newxr=' + newxr + ' newratio=' + newratio);

    setMidMainLRratio (newratio);

    com.mode.devlog ('  setting left = ' + newxl + '  right = ' + newxr);
    midMainLeft.style.width = newxlp;
    midMainLeft.style.flexGrow = 0; // without this they don't grow/shrink together

    com.mode.devlog ('  left width:   old=' + oldxl + ' new=' + newxl);
    com.mode.devlog ('  ratio:  old=' + oldratio + '  new=' + newratio);
    com.mode.devlog ('setMidMainLeftWidth finished');

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
    com.mode.devlog ('expLRflex');
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
//    com.mode.devlog ('showSizeParameters');
    let ww =  window.innerWidth - midSecExtraWidth;
    let y = midMainLeft.style.width;
//    com.mode.devlog ('  windowWidth = ' + ww);
//    com.mode.devlog ('  midMainLeftWidth = ' + y);
//    com.mode.devlog ('  midLRratio = ' + midLRratio);
}

// Resize the system (midMainLeft) and user guide (midMainRight)
// sections.  When the - or + button is clicked in the GUI,
// user_guide_resize (x) is called: x>0 means expand the user guide by
// x px; x<0 means shrink it.

function user_guide_resize(x) {
    com.mode.devlog ('user_guide_resize ' + x);
//    showSizeParameters ();
    let old_width = midMainLeft.style.width;
    com.mode.devlog ('  old width = ' + old_width);
    let w = parseInt(midMainLeft.style.width,10);
    com.mode.devlog ('  old width number = ' + w);
    let new_width = w+x;
    com.mode.devlog ('  new_width = ' + new_width)
    setMidMainLeftWidth (new_width);
//    let z = (w + x) + "px";
//    com.mode.devlog (' mml z = ' + z);
//    midMainLeft.style.width = z;
//    midMainLeft.style.flexGrow = 0; // without this they don't grow/shrink together
    showSizeParameters ();
}

//    let containerOffsetLeft = middleSection.offsetLeft;
//		+ ' containerOffsetLeft=' + containerOffsetLeft

//    document.getElementById("EditorTextArea").style.width= z + 'px';

// Diagnostics

function checkTestBody () {
//    com.mode.devlog ('checkTestBody width = ' + testPaneBodyElt.style.width);
}

//-------------------------------------------------------------------------------
// Example programs
//-------------------------------------------------------------------------------

function insert_example(exampleText) {
    com.mode.devlog('Inserting example add into editor text');
    let m = st.env.mkSelectModule ("HelloWorld");
    m.asmEdText = exampleText;
    smod.refreshModulesList ();
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
//    com.mode.devlog ('Really???');
  // Cancel the event
//  e.preventDefault();
//    com.mode.devlog ('Really????????????');
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
    com.mode.devlog ('window.onresize');
//    showSizeParameters ();
    //    setMidMainLRratio (midLRratio);  // preserve ratio as window is resized
    adjustToMidMainLRratio ();
    com.mode.devlog ('window.onresize finished');
}

//-------------------------------------------------------------------------------
// DevTools
//-------------------------------------------------------------------------------

// These are experimental tools that are normally disabled.  They
// aren't documented, aren't intended for users, and aren't stable.
// To use these functions, press F12 to display the browser console,
// then enter enableDevTools()

function showDevToolsButton () {
    console.log ("showDevToolsButton");
    document.getElementById("DevTools_Pane_Button").style.visibility = "visible";
}

function hideDevToolsButton () {
    com.mode.devlog ("hideDevToolsButton");
    showPane (WelcomePane);
    document.getElementById("DevTools_Pane_Button").style.visibility = "hidden";
}

function disableDevTools () {
    com.mode.devlog ("disableDevTools");
    hideDevToolsButton ();
}

function enableDevTools () {
    com.mode.devlog ("enableDevTools called");
    showDevToolsButton ();
}

// Attach the enableDevTools function to the window to put it into
// scope at the top level

window.enableDevTools = enableDevTools;

function devTools100 () {
    console.log ("DevTools100 clicked");
    action100 ()
}

function devTools101 () {
    console.log ("DevTools101 clicked");
    action101 ()
}

function devTools102 () {
    console.log ("DevTools102 clicked");
    action102 ()
}

function devTools103 () {
    console.log ("DevTools103 clicked");
    action103 ()
}

function devTools104 () {
    console.log ("DevTools104 clicked");
    action104 ()
}
function devTools105 () {
    console.log ("DevTools105 clicked");
    action105 ()
}
function devTools106 () {
    console.log ("DevTools106 clicked");
    action106 ()
}

//-------------------------------------------------------------------------------
// Emulator thread
//-------------------------------------------------------------------------------

// Define worker thread

const emthread = new Worker("../base/emthread.mjs");

function initThreads () {
    console.log ("main initThreads")
    let msg = {code: 100, payload: st.sysStateArr}
    emthread.postMessage (msg)
}

// A message is an object of the form {code: ..., payload: ...}.  The
// gui main thread uses codes 100, 101, ... and the emulator thread
// uses codes 200, 201, ...

// Actions

function action101 () { // send foo
    console.log (`action 101 foo=${st.foo}`)
    let msg = {code: 101, payload: st.foo}
    console.log (`main sending: <${msg}>`)
    emthread.postMessage (msg)    
}

function action102 () { // send foo, emt replies with emtCount, main202 prints
    console.log (`main action 102 send foo=${st.foo}`)
    let msg = {code: 102, payload: st.foo}
    emthread.postMessage (msg)    
}

function action103 () { // send foo
    console.log (`main action 103 foo=${st.foo}`)
    let msg = {code: 103, payload: st.foo}
    console.log (`main sending: <${msg}>`)
    emthread.postMessage (msg)    
}

function action104 () { // Tell emt to increment arr[23]
    console.log ("main action 104")
    let msg = {code: 104, payload: null}
    emthread.postMessage (msg)    
}

function action105 () { // Tell emt to print regfile
//    console.log (`main action 105 arr[23] = ${st.sysStateArr[23]}`)
    let msg = {code: 105, payload: null}
    emthread.postMessage (msg)    
}


function action106 () {
    console.log ("main 106")
}

// Handle incoming messages from emt

emthread.addEventListener ("message", e => {
    console.log ("main has received a message")
    if (e.data) {
        console.log ("main has received event data")
        let mval = e.data.payload
        switch (e.data.code) {
        case 200:
            console.log (`main rec 200 received val=${mval}`)
            break
        case 202:
            console.log (`main 202 rec ${mval} st.foobar=${st.foobar}`)
            break
        default:
            console.log (`main received bad code = ${e.data.code}`)
        }
        console.log (`main event handler returning`)

    }
})

// Check whether the browser supports workers.  Print a message on the
// console and return a Boolen: true if workers are supported

function checkBrowserWorkerSupport () {
    com.mode.devlog ("checkBrowserWorkerSupport")
    let workersSupported = false
    if (window.Worker) {
        com.mode.devlog ("Browser supports concurrent worker threads");
        workersSupported = true
    } else {
        com.mode.devlog ("Browser does not support concurrent worker threads");
    }
    return workersSupported
}

//-------------------------------------------------------------------------------
// Run the initializers when onload event occurs
//-------------------------------------------------------------------------------

let browserSupportsWorkers = false

window.onload = function () {
    com.mode.devlog("window.onload activated: starting initializers");
    em.hideBreakDialogue ();
    em.initializeMachineState ();
    em.initializeSubsystems ();
    document.getElementById('LinkerText').innerHTML = "";    
    smod.prepareChooseFiles ();
    initialize_mid_main_resizing ();
    setMidMainLRratio(0.65);  // useful for dev to keep mem display visible
    showSizeParameters();
    adjustToMidMainLRratio();
    initializePane ();
    smod.initModules ();
    window.mode = com.mode;
    hideDevToolsButton ();
    browserSupportsWorkers = checkBrowserWorkerSupport ()
    if (browserSupportsWorkers) {
        initThreads ()
    }
    enableDevTools () // if commented out, this can be entered manually in console
    com.mode.devlog("Initialization complete");
}
