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
import * as cn    from './config.mjs';
import * as com   from '../base/common.mjs';
import * as smod  from '../base/s16module.mjs';
import * as arch  from '../base/architecture.mjs';
import * as arith from '../base/arithmetic.mjs';
import * as st    from '../base/state.mjs';
import * as ed    from './editor.mjs';
import * as asm   from '../base/assembler.mjs';
import * as link  from '../base/linker.mjs';
import * as em    from '../base/emulator.mjs';

export const procAsmListingElt = document.getElementById('ProcAsmListing');

export function modalWarning (msg) {
    alert (msg);
}

//-----------------------------------------------------------------------------
// Emulator thread
//-----------------------------------------------------------------------------

function logShmStatus (es) {
    let status = st.showSCBstatus (es)
    let n = st.readSCB (es, st.SCB_nInstrExecuted)
    let cur = st.readSCB (es, st.SCB_cur_instr_addr)
    let next = st.readSCB (es, st.SCB_next_instr_addr)
    let mode = st.readSCB (es, st.SCB_emwt_run_mode)
    let trap =  st.readSCB (es, st.SCB_emwt_trap)
    let pause = st.readSCB (es, st.SCB_pause_request)
    let xs = `Shm flags:\n`
        + ` status = ${status}\n`
        + ` n = ${n}\n`
        + ` cur = ${cur}\n`
        + ` next = ${next}\n`
        + ` mode = ${mode}\n`
        + ` trap = ${trap}\n`
        + ` pause = ${pause}\n`
    return xs
}

//----------------------------------------
// Check capabilities of the platform
//----------------------------------------

// Check whether the browser supports workers.  Print a message on the
// console and return a Boolen: true if workers are supported

function checkBrowserWorkerSupport () {
    console.log ("checkBrowserWorkerSupport")
    console.log ("checkBrowserWorkerSupport")
    let workersSupported = false
    if (window.Worker) {
        console.log ("Browser supports concurrent worker threads");
        workersSupported = true
    } else {
        console.log ("Browser does not support concurrent worker threads");
    }
    return workersSupported
}

//----------------------------------------
// Communications protocol
//----------------------------------------

// The main gui thread and emulator thread communicate through two
// mechanisms: message passing and shared memory.  A consistent
// protocol is used for the message passing.

// Messages are oranized into pairs: a request sent by the main gui to
// the emulator thread, and a response sent by the emulator thread
// back to the main gui.  A message is an object of the form {code:
// ..., payload: ...}.  The gui main thread uses codes 100, 101,
// ... and the emulator thread uses codes 200, 201, ...  If a request
// has code x, the response has code x+100.  The codes are:

//   100 initialize: emwt receives shared memory and builds emulator state
//   101 step: emwt executes one instruction
//   102 run: emwt executes instructions until a stopping condition
//   103 print state: emwt prints key registers and part of memory to console
//   104 emwt test 1 - for testing and development
//   105 emwt test 2 - for testing and development

//----------------------------------------
// emwt 100: initialize
//----------------------------------------

// The main process sends the shared system state vector to the
// emulator worker thread, which saves it in a local object.  The
// worker also creates an emulator state which points to the shared
// system state vector, and initializes the emulator state.

// This action is essential and it's performed automatically in the
// window.onload event handler.

function emwtInit (es) { // called by onload initializer, request 100
    console.log ("main gui: emwtInit")
    let msg = {code: 100, payload: es.shm}
    emwThread.postMessage (msg)
    console.log ("main gui: posted init message 100 to emwt")
}
    //    let msg = {code: 100, payload: st.sysStateVec}
    //    let msg = {code: 100, payload: guiEmulatorState.shm}

function handleEmwtInitResponse (p) {
    console.log (`main gui: received response to emwt init ${p}`)
}

//----------------------------------------
// emwt 101: step
//----------------------------------------

function emwtStep () {
    console.log ("main: emwt step");
    let msg = {code: 101, payload: 0}
    emwThread.postMessage (msg)
}

function handleEmwtStepResponse (p) {
    console.log (`main: handle emwt step response ${p}`)
    //    em.refresh (guiEmulatorState)
    em.execInstrPostDisplay (guiEmulatorState)
    let newstatus = st.readSCB (guiEmulatorState, st.SCB_status)
    console.log (`main handle emwt step response: status=${newstatus}`)
    if (newstatus === st.SCB_relinquish) {
        console.log (`***** main gui: handle worker step relinquish`)
    }
}

//----------------------------------------
// emwt 102: run
//----------------------------------------

const guiRefreshInterval = 1000 // period of display refresh during run (ms)

function periodicRefresher () {
//    console.log ("periodicRefresher")
    updateClock (guiEmulatorState)
    refreshRFdisplay (guiEmulatorState)
}

// Show the current values of the register file without highlighting

function refreshRFdisplay (es) {
    for (let i = 0; i < es.nRegisters; i++) {
        let j = st.EmRegBlockOffset + i
        let x = i === 0 ? 0 : es.shm[j]
        let e = es.register[i].elt
        e.innerHTML = arith.wordToHex4 (x)
    }
}

const ClockWidth = 4

export function updateClock (es) {
    const now = new Date ()
    const elapsed = now.getTime () - es.startTime
    const xs = elapsed < 1000
          ? `${elapsed.toFixed(0)} ms`
          : `${(elapsed/1000).toPrecision(ClockWidth)} s`
    document.getElementById("PP_time").innerHTML = xs
}

function startClock (es) {
    clearTime (es)
    updateClock (es)
    es.eventTimer = setInterval (periodicRefresher, guiRefreshInterval)
}

function stopClock (es) {
    clearInterval (es.eventTimer)
    console.log ("stopClock")
    updateClock (es)
    es.eventTimer = null
}

export function test1 (es) {
    updateClock (es)
    refreshRFdisplay (es)
}


// (es should be guiEmulatorState)

function emwtRun (es) { // run until stopping condition; relinquish on trap
    console.log ("main: emwt run");
    let instrLimit = 0 // disabled; stop after this many instructions
    st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_running_emwt)
    let msg = {code: 102, payload: instrLimit}
    emwThread.postMessage (msg)
    clearTime (guiEmulatorState)
    startClock (guiEmulatorState)
    console.log ("main: emwt run posted start message");
}

function handleEmwtRunResponse (p) { // run when emwt sends 202
    let status = st.readSCB (guiEmulatorState, st.SCB_status)
    let  msg = {code: 0, payload: 0}
    console.log (`main: handle emwt run response: p=${p} status=${status}`)
    switch (status) {
    case st.SCB_halted:
        console.log (`*** main: handle emwt halt`)
        stopClock (guiEmulatorState)
        em.refresh (guiEmulatorState)
        st.showSCBstatus (guiEmulatorState)
        break
    case st.SCB_paused:
        console.log (`*** main: handle emwt pause`)
        stopClock (guiEmulatorState)
        em.refresh (guiEmulatorState)
        st.showSCBstatus (guiEmulatorState)
        st.writeSCB (guiEmulatorState, st.SCB_pause_request, 0)
        st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_ready)
        st.showSCBstatus (guiEmulatorState)
        console.log (`*** main: finished handle emwt pause`)
        break
    case st.SCB_break:
        console.log (`*** main: handle emwt break`)
        stopClock (guiEmulatorState)
        em.refresh (guiEmulatorState)
        st.showSCBstatus (guiEmulatorState)
        st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_ready)
        st.showSCBstatus (guiEmulatorState)
        console.log (`*** main: finished handle emwt break`)
        break
    case st.SCB_blocked:
        console.log (`*** main: handle emwt blocked`)
        break
    case st.SCB_relinquish: // emwt halt signals halt, not relinquish
        console.log (`*** main: handle emwt relinquish`)
        st.showSCBstatus (guiEmulatorState)
        st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_running_gui)
        em.executeInstruction (guiEmulatorState)
        if (st.readSCB (guiEmulatorState, st.SCB_status) === st.SCB_halted) {
            stopClock (guiEmulatorState)
            em.refresh (guiEmulatorState)
            console.log (`*** main: handle emwt relinquish then halted`)
        } else {
            console.log (`*** main: handle emwt relinquish resuming`)
            st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_running_emwt)
            msg = {code: 102, payload: 0}
            emwThread.postMessage (msg)
        }
        console.log (`*** main: finished handle emwt relinquish`)
        break
    case st.SCB_reset:
    case st.SCB_ready:
    case st.SCB_running_gui:
    case st.SCB_running_emwt:
    default:
        console.log (`main:handleEmwtRunResponse unknown status = ${status}`)
    }
    console.log ("main: handleEmwtRunResponse finished")
}

/*        
//    if (newstatus === st.SCB_relinquish) {
        console.log (`***** main gui: handle worker run relinquish`)
        console.log (`SCB status = ${st.readSCB (guiEmulatorState, st.SCB_status)}`)
        console.log (`handle WT Run response, run one instruction in main thread`)

        st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_running_gui)
        em.executeInstruction (guiEmulatorState)
        if (st.readSCB (guiEmulatorState, st.SCB_status) != st.SCB_halted) {
            st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_ready)
        }

        console.log (`main relinquish after instruction, SCB status =`
                     + ` ${st.readSCB (guiEmulatorState, st.SCB_status)}`)
        let newerStatus = st.readSCB (guiEmulatorState, st.SCB_status)
        console.log (`main relinquixh after instruction, status=${newerStatus}`)
        switch (newerStatus) {
        case st.SCB_halted:
            console.log (`main handle run relinquish, halted`)
            em.refresh (guiEmulatorState)
            break
        case st.SCB_ready:
            console.log (`main handle run relinquish, resuming`)
            emwtRun ()
            break
        default: console.log (`main handle relinquish, status=${newerStatus}`)
        }
    }
*/

//----------------------------------------
// emwt 103: print state on console
//----------------------------------------

function emwtShow () {
    console.log ("main: emwtShowRegs")
    let msg = {code: 103, payload: 0}
    emwThread.postMessage (msg)
}

function handleEmwtShowResponse (p) {
    console.log (`main: handle emwt show response ${p}`)
}

//----------------------------------------
// emwt 104: emwt test 1
//----------------------------------------

function emwtTest1 () {
    console.log ("main: emwt test 1")
    console.log (logShmStatus (guiEmulatorState))
}
//    let msg = {code: 104, payload: 73} // arbitrary payload
//    emwthread.postMessage (msg)

function handleEmwtTest1Response (p) {
    console.log (`main: handle emwt test 1 response ${p}`)
}

//----------------------------------------
// emwt 105: emwt test 2
//----------------------------------------

function emwtTest2 () {
    console.log ("main: emwt test 2")
    let msg = {code: 105, payload: 78} // arbitrary payload
    emwThread.postMessage (msg)
}

function handleEmwtTest2Response (p) { // 
    console.log (`main: handle emwt test 2 response ${p}`)
}

//----------------------------------------
// Handle responses from emwt
//----------------------------------------

function initializeEmwtProtocol (es) {
    emwThread.addEventListener ("message", e => {
        console.log ("main has received a message")
        if (e.data) {
            console.log ("main has received data from message")
            let p = e.data.payload
            switch (e.data.code) {
            case 200: // initialize
                console.log (`main: received 200 init response`)
                handleEmwtInitResponse (p)
                break
            case 201: // emwt step
                console.log (`main: received 201 step response`)
                handleEmwtStepResponse (p)
                break
            case 202: // emwt run
                console.log (`main rec 202 run response`)
                handleEmwtRunResponse (p)
                break
            case 203: // emwt show
                console.log (`main: rec 203 emwt show response`)
                handleEmwtShowResponse (p)
                break
            case 204: // emwt test 1
                console.log (`main: rec 204 emwt test 1 response`)
                handleEmwtTest1Response (p)
                break
            case 205: // emwt test 2
                console.log (`main: rec 205 emwt test 2 response`)
                handleEmwtTest2Response (p)
                break
            default:
                console.log (`main: received unknown code = ${e.data.code}`)
            }
            console.log (`main event handler returning`)
        }
    })
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
export const OptionsPane   = Symbol ("OptionsPane");
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
    f (OptionsPane, "none");
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
    case OptionsPane:
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
    case OptionsPane:
        break;
    case DevToolsPane:
        break;
    }
    document.getElementById(paneIdString(currentPane)).style.display = "none";
}

//------------------------------------------------------------------------------
// Processor pane
//------------------------------------------------------------------------------


// Run instructions until stopping condition on selected emulator thread

function procRun (es) {
    console.log (`procRun, thread = ${es.emRunThread}`)
    switch (es.emRunThread) {
    case em.ES_gui_thread:
        em.procRunMainThread (es)
        break
    case em.ES_worker_thread:
        emwtRun (es)
        break
    default:
        console.log (`Error procRun ${es.emRunThread}`)
    }
    console.log (`procRun finished`)
}

// Main interface function to step one instruction; runs in main gui
// thread

export function procStep (es) {
    if (es.thread_host != em.ES_gui_thread) {
        console.log (`procStep: host=${es.thread_host}, skipping`)
        return
    }
    let q = st.readSCB (es, st.SCB_status)
    switch (q) {
    case st.SCB_ready:
    case st.SCB_paused:
    case st.SCB_break:
    case st.SCB_relinquish:
        console.log ("procStep: main thread executing instruction...")
        st.writeSCB (es, st.SCB_status, st.SCB_running_gui)
        em.executeInstruction (es)
        if (st.readSCB (es, st.SCB_status) != st.SCB_halted) {
            st.writeSCB (es, st.SCB_status, st.SCB_ready)
        }
        em.execInstrPostDisplay (es)
        em.guiDisplayNinstr (es)
        break
    case st.SCB_reset:
    case st.SCB_running_gui:
    case st.SCB_running_emwt:
    case st.SCB_halted:
    case st.SCB_blocked:
        console.log ("procStep skipping instruction...")
        break
    default: console.log (`error: procStep unknown SCB_tatus= ${q}`)
    }
}

// Separate clearing state from refreshing display
export function procReset (es) {
    console.log ("em reset");
    com.mode.devlog ("reset the processor");
    st.resetSCB (es)
    em.resetRegisters (es);
    em.memClear (es);
    clearTime (es)
    refreshDisplay (es)
}

export function refreshDisplay (es) {
    em.refreshRegisters (es);
    em.memDisplay (es);
    document.getElementById('ProcAsmListing').innerHTML = "";
    em.clearInstrDecode (es);
    em.refreshInstrDecode (es);
    em.guiDisplayNinstr (es)
    es.ioLogBuffer = ""
    em.refreshIOlogBuffer (es)
    st.showSCBstatus (es)
//    memClearAccesses ();
}



// Time

export function clearTime (es) {
    const now = new Date ()
    es.startTime = now.getTime ()
    document.getElementById("PP_time").innerHTML = `0ms`
}


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

     lea    R1,6       ; R1 := 6
     load   R2,x       ; R2 := x (variable initialized to 7)
     mul    R3,R1,R2   ; R3 := 6 * x = 42 (hex 002a)
     store  R3,result  ; result := 6 * x
     trap   R0,R0,R0   ; halt

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
// To use them, press F12 to display the browser console, then enter
// enableDevTools() which will make the tools' buttons visible; to
// hide them again enter disableDevTools().

// DevElts is list of ids of elements to be shown or hidden.  To
// ensure they are hidden by default, they should have class="Hidden"
// specified in their html element: the css for .Hidden specifies
// visibility: "hidden".

const DevElts = [ "DevTools_Pane_Button", "PP_RunGui", "PP_Test1", "PP_Test2"]

// Define functions that show/hide the DevElts and attach the
// functions to the window to put them into scope at the top level

function disableDevTools () { DevElts.map (hideElement) }
function enableDevTools ()  { DevElts.map (showElement) }
window.enableDevTools = enableDevTools;
window.disableDevTools = disableDevTools;

function showElement (eltName) {
    document.getElementById(eltName).style.visibility = "visible";
}

function hideElement (eltName) {
    document.getElementById(eltName).style.visibility = "hidden";
}

function devTools100 () {
    console.log ("DevTools100 clicked");
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

function configureBrowser (es) {
    let supportWorker = cn.checkBrowserWorkerSupport ()
    cn.output (`Browser supports web workers = ${supportWorker}`)
    let supportSharedMem = cn.checkSharedMemorySupport ()
    cn.output (`Browser supports shared array buffers = ${supportSharedMem}`)
    es.emRunCapability = supportWorker &&  supportSharedMem
        ? em.ES_worker_thread
        : em.ES_gui_thread
    es.emRunThread = es.emRunCapability // default: run according to capability
    cn.output (`Emulator run capability = ${es.emRunCapability}`)
}

// System state vector

export let sysStateBuf = null
export let sysStateVec = null
let emwThread = null

// Memory is allocated in the main thread and sent to the worker
// thread, if there is a worker

function allocateStateVector (es) {
    switch (es.emRunCapability) {
    case em.ES_worker_thread:
        sysStateBuf = new SharedArrayBuffer (st.EmStateSizeByte)
        sysStateVec = new Uint16Array (sysStateBuf)
        es.shm = sysStateVec
        // Start the emulator thread and initialize it
        console.log ("gui.mjs starting emwt")
        emwThread = new Worker("../base/emwt.mjs", {type:"module"});
        initializeEmwtProtocol (es)
        emwtInit (es)
        console.log ("gui.mjs has started emwt")
        break
    case em.ES_gui_thread:
        sysStateVec = new Uint16Array (st.EmStateSizeWord)
        es.shm = sysStateVec
        break
    default:
        cn.output (`allocateStateVector: bad emRunCapability = `
                   + `${es.emRunCapability}`)
    }
    es.shm = sysStateVec
    cn.output (`EmStateSizeWord = ${st.EmStateSizeWord}`)
    cn.output (`EmStateSizeByte = ${st.EmStateSizeByte}`)
    cn.output (`sysStateVec contains ${es.shm.length} elements`)
}

function testSysStateVec (es) {
    cn.output (`Testing emulator memory: ${es.thread_host.description}...`)
    let xs = ""
    let n = 5
    for (let i = 0; i < n; i++) es.shm[i] = i
    for (let i = 0; i < n; i++) es.shm[i] += 100
    for (let i = 0;  i < n; i++) xs += ` ${i}->${es.shm[i]}`
    cn.output (`${es.thread_host.description} ${xs} ... finished`)
}
//    for (let i = 0; i < n; i++) st.sysStateVec[i] = i
//    for (let i = 0; i < n; i++) st.sysStateVec[i] += 100
//    for (let i = 0;  i < n; i++) xs += ` ${i}->${st.sysStateVec[i]}`

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

// Pane buttons; initialization must occur after emulator state is defined

function initializeButtons () {
prepareButton ('Welcome_Pane_Button',   () => showPane(WelcomePane));
prepareButton ('Examples_Pane_Button',  () => showPane(ExamplesPane));
prepareButton ('Modules_Pane_Button',   () => showPane(ModulesPane));
prepareButton ('Editor_Pane_Button',    () => showPane(EditorPane));
prepareButton ('Assembler_Pane_Button', () => showPane(AssemblerPane));
prepareButton ('Linker_Pane_Button',    () => showPane(LinkerPane));
prepareButton ('Processor_Pane_Button', () => showPane(ProcessorPane));
prepareButton ('Options_Pane_Button'  , () => showPane(OptionsPane));
prepareButton ('DevTools_Pane_Button', () => showPane(DevToolsPane));
prepareButton ('About_Button',
               () => showGuideSection('sec-about-sigma16'));  

// User guide resize (UGR) buttons
// UGR Distance (px) to move boundary between gui and userguide on resize
const UGRSMALL = 1;
const UGRLARGE = 20;
prepareButton ('UG_Resize_Right_Large_Button', () => user_guide_resize(UGRLARGE));
prepareButton ('UG_Resize_Right_Small_Button', () => user_guide_resize(UGRSMALL));
prepareButton ('UG_Resize_Left_Small_Button', () => user_guide_resize(-UGRSMALL));
prepareButton ('UG_Resize_Left_Large_Button', () => user_guide_resize(-UGRLARGE));

// Welcome pane (WP)
// prepareButton ('WP_Guide_Top', jumpToGuideTop);
prepareButton ('WP_TOC', () => showGuideSection('table-of-contents'));
prepareButton ('WP_Tutorials', () => showGuideSection('sec-tutorial'));
prepareButton ('WP_Architecture', () => showGuideSection('sec-architecture'));
prepareButton ('WP_ISA', () => showGuideSection('sec-instruction-set'));
prepareButton ('WP_Assembly_Language',
               () => showGuideSection('sec-assembly-language'));
prepareButton ('WP_Linker', () => showGuideSection('sec-linker'));
prepareButton ('WP_Programming', () => showGuideSection('sec-programming'));

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
prepareButton ('EDP_Revert',      ed.edRevert);
prepareButton ('EDP_New',         ed.edNew);
prepareButton ('EDP_Save',        ed.edDownload);
prepareButton ('EDP_Hello_world', () => insert_example(example_hello_world));
// prepareButton ('EDP_Asm',         ed.edAsm);
// prepareButton ('EDP_Obj',         ed.edObj);
// prepareButton ('EDP_Exe',         ed.edExe);
// prepareButton ('EDP_Link',        ed.edLink);

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
prepareButton ('PP_Boot',         () => em.boot (guiEmulatorState));
prepareButton ('PP_Step',         () => procStep (guiEmulatorState));
prepareButton ('PP_Run',          () => procRun (guiEmulatorState));
prepareButton ('PP_RunWorker',    () => emwtRun (guiEmulatorState));
prepareButton ('PP_Pause',        () => em.procPause (guiEmulatorState));
prepareButton ('PP_Interrupt',    () => em.procInterrupt (guiEmulatorState));
prepareButton ('PP_Breakpoint',   () => em.procBreakpoint (guiEmulatorState));
prepareButton ('PP_Refresh',      () => refresh (guiEmulatorState));
prepareButton ('PP_Reset',        () => procReset (guiEmulatorState));
prepareButton ('PP_RunMain',      () => em.procRunMainThread (guiEmulatorState));
prepareButton ('PP_RunWorker',    () => emwtRun (guiEmulatorState));
prepareButton ('PP_Test1',        () => test1 (guiEmulatorState))
prepareButton ('PP_Test2',        emwtTest2);

prepareButton ('PP_Timer_Interrupt',  () => em.timerInterrupt (guiEmulatorState));
// prepareButton ('PP_Toggle_Display',  em.toggleFullDisplay);

// Breakpoint popup dialogue
/*
prepareButton ("BreakRefresh", em.breakRefresh(em.emulatorState));
prepareButton ("BreakEnable", em.breakEnable(em.emulatorState));
prepareButton ("BreakDisable", em.breakDisable(em.emulatorState));
prepareButton ("BreakClose", em.breakClose());
*/

// DevTools
prepareButton ('DevTools102',    devTools102);
prepareButton ('DevTools103',    devTools103);
prepareButton ('DevTools104',    devTools104);
prepareButton ('DevTools105',    devTools105);
prepareButton ('DevTools106',    devTools106);
prepareButton ('DisableDevTools', disableDevTools);
}

//-------------------------------------------------------------------------------
// Run the initializers when onload event occurs
//-------------------------------------------------------------------------------

let guiEmulatorState // declare here, define at onload event 
let browserSupportsWorkers = false

// The onload function runs in the main gui thread but not in worker thread

window.onload = function () {
    com.mode.devlog("window.onload activated: starting initializers");
    em.hideBreakDialogue ();
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
    guiEmulatorState = new em.EmulatorState (em.ES_gui_thread)
    configureBrowser (guiEmulatorState)
    allocateStateVector (guiEmulatorState)
    testSysStateVec (guiEmulatorState)
    em.initializeMachineState (guiEmulatorState)
    initializeButtons ()
    procReset (guiEmulatorState)
    clearTime (guiEmulatorState)
    com.mode.trace = true
    com.mode.devlog (`Thread ${guiEmulatorState.mode} initialization complete`)
    com.mode.trace = false
}
