// Sigma16: gui.mjs
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
// gui.mjs is the main program for the browser version.  It's launched
// by Sigma16.html and is the last JavaScript file to be loaded
//-----------------------------------------------------------------------------

import * as ver from './version.mjs';
import * as com from './common.mjs';
import * as smod from './s16module.mjs';
import * as arch from './architecture.mjs';
import * as arith from './arithmetic.mjs';
import * as st from './state.mjs';
import * as ed from './editor.mjs';
import * as asm from './assembler.mjs';
import * as link from './linker.mjs';
import * as em from './emulator.mjs';

//-------------------------------------------------------------------------------
// Global variables for gui.js
//-------------------------------------------------------------------------------

let globalObject = this; // to enable script in userguide to define glob var
let myglobalvar = 9876; // can script in userguide see this?

let fileContents = "file not read yet"

// Persistent variables given values by initialize_mid_main_resizing ()
let windowWidth;     // inner width of entire browser window
let middleSection;  // the middle section of the window; set in onload
let midMainLeft;     // mid-main-left; set in onload
let midMainRight;     // mid-main-right; set in onload, not used anywhere
let midLRratio = 0.6;  // width of midMainLeft / midMainRight; set in onLoad
let midSecExtraWidth = 15;  // width of borders in px

//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------

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
    let midmainright = document.getElementById('MidMainRight');
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
    let tstart = performance.now();
    for (let i = 0; i<n; i++) {
	pc.put(i);
    }
    let tend = performance.now();
    console.log('measureRegRefresh (' + n + ') took '
		+ (tend - tstart) + ' ms');
}

// function springen(anker) { 
//    let childWindow =  document.getElementById("UserGuideIframeId").contentWindow;
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
	
//    let loc = userGuideElt.location;
//    console.log ("ed button 1, loc = " + loc);
//    loc.href = "#HREFTESTING";
    
}

//------------------------------------------------------------------------------
// Define actions for buttons
//------------------------------------------------------------------------------

// Connect a button in the html with its corresponding function
function prepareButton (bid,fcn) {
    console.log (`prepare button ${bid}`);
    document.getElementById(bid)
        .addEventListener('click', event => {fcn()});
}


// Pane buttons
prepareButton ('Welcome_Pane_Button',   f_welcome_pane_button);
prepareButton ('Examples_Pane_Button',  f_examples_pane_button);
prepareButton ('Modules_Pane_Button',   f_modules_pane_button);
prepareButton ('Editor_Pane_Button',    f_editor_pane_button);
prepareButton ('Assembler_Pane_Button', f_assembler_pane_button);
prepareButton ('Linker_Pane_Button',    f_linker_pane_button);
prepareButton ('Processor_Pane_Button', f_processor_pane_button);
prepareButton ('About_Button',         () => jumpToGuideSection('about-sigma16'));  


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
prepareButton ('WP_Tutorials', () => jumpToGuideSection('tutorials'));
prepareButton ('WP_Architecture', () => jumpToGuideSection('architecture'));
prepareButton ('WP_ISA', () => jumpToGuideSection('instruction-set'));
prepareButton ('WP_Assembly_Language', () => jumpToGuideSection('assembly-language'));
prepareButton ('WP_Linker', () => jumpToGuideSection('linker'));
console.log ("wp almost done");
prepareButton ('WP_Programming', () => jumpToGuideSection('programming'));
console.log ("wp done");

// Examples pane (EXP)
prepareButton ('EXP_Examples_Home',    examplesHome);
prepareButton ('EXP_Select_Example',    smod.selectExample);

// Modules pane (MP)
prepareButton ('MP_New',    smod.newModule);
prepareButton ('MP_Refresh',    smod.refreshModulesList);

// Editor pane (EDP)
prepareButton ('EDP_Clear',    ed.editorClear);
prepareButton ('EDP_New',    smod.newModule);
prepareButton ('EDP_Hello_World',  () => insert_example(example_hello_world));
prepareButton ('EDP_Save',  ed.editorDownload);

// Assembler pane (AP)
prepareButton ('AP_Assemble',  asm.assembler);
prepareButton ('AP_Show_Object',  asm.setObjectListing);
prepareButton ('AP_Show_Listing',  asm.setAsmListing);
prepareButton ('AP_Show_Metadata',  asm.setMetadata);

// Linker pane (LP)
prepareButton ('LP_Show_Object',  link.linkShowSelectedObj);
prepareButton ('LP_Read_Object',  link.readObjectFromEditor);
prepareButton ('LP_Read_Modules', link.setLinkerModules);
prepareButton ('LP_Link',         link.link);
prepareButton ('LP_Boot',         () => em.boot(st.emulatorState));
prepareButton ('LP_Show_Metadata', link.linkShowMetadata);

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

//                          id="FullDisplayToggleButton"


// Breakpoint popup dialogue
/*
prepareButton ("BreakRefresh", em.breakRefresh(em.emulatorState));
prepareButton ("BreakEnable", em.breakEnable(em.emulatorState));
prepareButton ("BreakDisable", em.breakDisable(em.emulatorState));
prepareButton ("BreakClose", em.breakClose());
*/


//------------------------------------------------------------------------------
// Dialogues with the user
//------------------------------------------------------------------------------

function modalWarning (msg) {
    alert (msg);
}

//------------------------------------------------------------------------------
// Tabbed panes
//------------------------------------------------------------------------------

//-------------------------------------------------------------------------------
// Top level tab buttons control which pane is visible and active
//-------------------------------------------------------------------------------

export function hideTabbedPane(paneId) {
//    console.log("hiding tabbed pane " + paneId);
    document.getElementById(paneId).style.display = "none";
}

export function hideAllTabbedPanes() {
    hideTabbedPane("WelcomePane");
    hideTabbedPane("ExamplesPane");
    hideTabbedPane("ModulesPane");
    hideTabbedPane("EditorPane");
    hideTabbedPane("AssemblerPane");
    hideTabbedPane("LinkerPane");
    hideTabbedPane("ProcessorPane");
    hideTabbedPane("TestPane");
}

//    ed.leaveEditor(); // will also hide editor pane  kills asm/lnk

export function showTabbedPane(paneId) {
    console.log("showing tabbed pane " + paneId);
    hideAllTabbedPanes();
    document.getElementById(paneId).style.display = "block";
    console.log("Now on tabbed pane " + paneId);
}

export function f_welcome_pane_button() {
    console.log("welcome_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("WelcomePane");
}


function f_examples_pane_button() {
    console.log("examples_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("ExamplesPane");
}
export function f_modules_pane_button() {
    console.log("modules_pane_button clicked")
    hideAllTabbedPanes();
    smod.refreshModulesList();
    showTabbedPane("ModulesPane");
}

export function f_editor_pane_button() {
//    console.log("editor_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("EditorPane");
}

export function f_assembler_pane_button() {
    console.log("assembler_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("AssemblerPane");
    console.log("f assembler_pane_button returning")
}

export function f_linker_pane_button() {
//    console.log("linker_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("LinkerPane");
}

export function f_processor_pane_button() {
//    console.log("processor_pane button clicked")
    hideAllTabbedPanes();
    showTabbedPane("ProcessorPane");
}


export function f_userman_pane_button() {
    console.log("userman_pane_button clicked")
}









//------------------------------------------------------------------------------
// Examples pane
//------------------------------------------------------------------------------

// Errors in displaying the directories: GET 404:
// www.dcs.gla.ac.uk/icons/blank.gif
// www.dcs.gla.ac.uk/:9uk/icons/text.gif

function examplesHome() {
    console.log ("examplesHome");
    document.getElementById("ExamplesIframeId").src =
	"../examples/index.html";
}

// Copy the example text to the editor.  The example is shown as a web
// page and its content is obtained using innerHTML.


// This does not work.  Perhaps because it's an iframe, not an input?
// Copy text of example buffer to clipboard
function copyExampleToClipboard () {
    console.log ('Copy example to clipboard');
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

//-------------------------------------------------------------------------------
// Complete initialization when onload occurs
//-------------------------------------------------------------------------------

window.onload = function () {
    console.log("window.onload activated");
    smod.initModules ();
    em.hideBreakDialogue ();
    em.initializeMachineState ();
    em.initializeSubsystems ();
    document.getElementById('LinkerText').innerHTML = "";    
    smod.prepareChooseFiles ();
    initialize_mid_main_resizing ();
    setMidMainLRratio(0.65);  // useful for dev to keep mem display visible
    showSizeParameters();
    adjustToMidMainLRratio();


    //    showTabbedPane("WelcomePane");
        showTabbedPane("ProcessorPane");
    console.log("Initialization complete (new version)");
}

/*
export function f_test_pane_button() {
//    console.log("test_pane button clicked")
    hideAllTabbedPanes();
    showTabbedPane("TestPane");
}
*/
/*
let run_examples_pane_button = event => {
    console.log ("run examples pane button");
    examples_pane_button();
}
*/

// onclick="user_guide_resize(1)"
// prepareButton ('UG_Resize_Right_Large_Button',    f_ug_resize_right_large_button);
/*
function f_ug_resize_right_large_button () {
    console.log ("resize right large");
    user_guide_resize(20);
}
*/

/*
// prepareButton ('About_Button',          f_about_button);
function f_about_button () {
    	jumpToGuideSection('about-sigma16');
}
*/

/*


 */
