// Sigma16: gui.mjs
// Copyright (C) 2019-2021 John T. O'Donnell
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
let latestVersion = 'unknown'

//-----------------------------------------------------------------------------
// Dev tools
//-----------------------------------------------------------------------------

// Needed only for console debugging; otherwise can be deleted or
// commented out

window.test_op_add = (x,y) => {
    let [p,s] = arith.op_add (x,y)
    return (`primary: ${arith.wordToHex4(p)} = ${p}`
            + ` cc: ${arith.wordToHex4(s)} ccflags=${arith.showCC(s)}`)
}

//-----------------------------------------------------------------------------
// Misc
//-----------------------------------------------------------------------------

export function modalWarning (msg) {
    alert (msg);
}

function updateWhileRunning (es) {
    em.updateClock (es)
    em.refreshRFdisplay (es)
}

//-----------------------------------------------------------------------------
// Gui state
//-----------------------------------------------------------------------------

class GuiState {
    constructor () {
        this.supportLocalStorage = false
        this.supportWorker = false
        this.supportSharedMem = false
        this.runCapability = null
        this.currentKeyMap = defaultKeyMap
        this.currentPaneButton = "Welcome_Pane_Button"
        console.log ("****** set currentPaneButton")
        this.emRunThread = st.ES_gui_thread
        this.mainSliceSize = 1
    }
}


//-----------------------------------------------------------------------------
// Configuration
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Keystrokes
//-----------------------------------------------------------------------------

const defaultKeyMap = new Map ([
    ["KeyH",  toggleDefaultHelp],
])

// let currentKeyMap = defaultKeyMap

const modulesKeyMap = new Map ([
    ["KeyH",  toggleModulesHelp],
    ["KeyR",  smod.refreshModulesList],
    ["KeyW",  () => insert_example(example_hello_world)],
])

const examplesKeyMap = new Map ([
    ["KeyH",  toggleExamplesHelp],
])

const editorKeyMap = new Map ([
    ["KeyH",  toggleEditorHelp],
    ["KeyC",  ed.edClear],
    ["KeyN",  ed.edNew],
    ["KeyS",  ed.edDownload],
    ["KeyW",  () => insert_example(example_hello_world)],
])

const asmKeyMap = new Map ([
    ["KeyH",  toggleAssemblerHelp],
    ["KeyA",  asm.assemblerGUI],
    ["KeyS",  asm.displayAsmSource],
    ["KeyO",  asm.setObjectListing],
    ["KeyL",  asm.setAsmListing],
    ["KeyM",  asm.setMetadata],
])

const linkerKeyMap = new Map ([
    ["KeyH",  toggleLinkerHelp],
    ["KeyL",  link.linkerGUI],
    ["KeyE",  link.showExecutable],
    ["KeyM",  link.showMetadata],
])

const procKeyMap = new Map ([
    ["KeyH",  toggleProcHelp],
    ["KeyB",  () => em.boot (guiEmulatorState)],
    ["KeyS",  () => procStep (guiEmulatorState)],
    ["KeyR",  () => procRun (guiEmulatorState)],
    ["KeyP",  () => procPause (guiEmulatorState)],
    ["KeyI",  () => procInterrupt (guiEmulatorState)],
])

let defaultHelpDialogueVisible = false
export function toggleDefaultHelp () {
    document.getElementById("DefaultHelpDialogue").style.display
	= defaultHelpDialogueVisible ? "none" : "block";
    defaultHelpDialogueVisible = !defaultHelpDialogueVisible;
}

let modulesHelpDialogueVisible = false
export function toggleModulesHelp () {
    document.getElementById("ModulesHelpDialogue").style.display
	= modulesHelpDialogueVisible ? "none" : "block";
    modulesHelpDialogueVisible = !modulesHelpDialogueVisible;
}

let examplesHelpDialogueVisible = false
export function toggleExamplesHelp () {
    document.getElementById("ExamplesHelpDialogue").style.display
	= examplesHelpDialogueVisible ? "none" : "block";
    examplesHelpDialogueVisible = !examplesHelpDialogueVisible;
}

let editorHelpDialogueVisible = false
export function toggleEditorHelp () {
    document.getElementById("EditorHelpDialogue").style.display
	= editorHelpDialogueVisible ? "none" : "block";
    editorHelpDialogueVisible = !editorHelpDialogueVisible;
}

let assemblerHelpDialogueVisible = false
export function toggleAssemblerHelp () {
    document.getElementById("AssemblerHelpDialogue").style.display
	= assemblerHelpDialogueVisible ? "none" : "block";
    assemblerHelpDialogueVisible = !assemblerHelpDialogueVisible;
}

let linkerHelpDialogueVisible = false
export function toggleLinkerHelp () {
    document.getElementById("LinkerHelpDialogue").style.display
	= linkerHelpDialogueVisible ? "none" : "block";
    linkerHelpDialogueVisible = !linkerHelpDialogueVisible;
}

let procHelpDialogueVisible = false
export function toggleProcHelp () {
    com.mode.devlog ("toggleProcHelp");
    document.getElementById("ProcHelpDialogue").style.display
	= procHelpDialogueVisible ? "none" : "block";
    procHelpDialogueVisible = !procHelpDialogueVisible;
}

function handleKeyDown (e) {
    console.log (`handleKeyDown code=${e.code} keyCode=${e.keyCode}`)
    let action = gst.currentKeyMap.get (e.code)
    if (action) {
        e.handled = true
        console.log (`=== do action for key code=${e.code} keyCode=${e.keyCode}`)
        action ()
        console.log (`finished action for key code=${e.code} keyCode=${e.keyCode}`)
    } else {
        console.log (`no action for key code=${e.code} keyCode=${e.keyCode}`)
    }
}


function handleTextBufferKeyDown (e) {
    console.log (`handleTextBbufferKeyDown code=${e.code} keyCode=${e.keyCode}`)
    e.stopPropagation () // inhibit using key as keyboard shortcut command
}

// Enable keyboard shortcuts
document.addEventListener ("keydown", handleKeyDown)

// Disable keyboard shortcuts for text entry buffers
document.getElementById("IOinputBuffer")
    .addEventListener ("keydown", handleTextBufferKeyDown)
document.getElementById("BreakTextArea")
    .addEventListener ("keydown", handleTextBufferKeyDown)
document.getElementById("EditorTextArea")
    .addEventListener ("keydown", handleTextBufferKeyDown)

//-----------------------------------------------------------------------------
// Interface to emulator
//-----------------------------------------------------------------------------

// These functions are passed to the emulator, which calls them

// Perform any operations on the gui display to prepare for a run
function initRun (es) {
    em.startClock (es)
}

function finishRun (es) {
    em.stopClock (es)
    em.execInstrPostDisplay (es)
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
    com.mode.devlog ("checkBrowserWorkerSupport")
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
    com.mode.devlog ("main gui: emwtInit")
    //    let msg = {code: 100, payload: es.shm}
    //    let msg = {code: 100, payload: sysStateBuf}
    let msg = {code: 100, payload: es.vecbuf}
    emwThread.postMessage (msg)
    com.mode.devlog ("main gui: posted init message 100 to emwt")
}
    //    let msg = {code: 100, payload: st.sysStateVec}
    //    let msg = {code: 100, payload: guiEmulatorState.shm}

function handleEmwtInitResponse (p) {
    com.mode.devlog (`main gui: received response to emwt init ${p}`)
}

//----------------------------------------
// emwt 101: step
//----------------------------------------

function emwtStep () {
    com.mode.devlog ("main: emwt step");
    let msg = {code: 101, payload: 0}
    emwThread.postMessage (msg)
}

function handleEmwtStepResponse (p) {
    com.mode.devlog (`main: handle emwt step response ${p}`)
    //    em.refresh (guiEmulatorState)
    em.execInstrPostDisplay (guiEmulatorState)
    let newstatus = st.readSCB (guiEmulatorState, st.SCB_status)
    com.mode.devlog (`main handle emwt step response: status=${newstatus}`)
    if (newstatus === st.SCB_relinquish) {
        com.mode.devlog (`***** main gui: handle worker step relinquish`)
    }
}

//----------------------------------------
// emwt 102: run
//----------------------------------------



// Initiate a run using the worker thread.  This function is the main
// gui's interface to the worker, so es should be guiEmulatorState.
// It will run instructions until a stopping condition, but will
// relinquish control to the main thread on a trap.

function emwtRun (es) {
    com.mode.devlog ("main: emwt run");
    let instrLimit = 0 // disabled; stop after this many instructions
    let msg = {code: 102, payload: instrLimit}
    emwThread.postMessage (msg)
    com.mode.devlog ("main: emwt run posted start message");
}

function handleEmwtRunResponse (p) { // run when emwt sends 202
    let status = st.readSCB (guiEmulatorState, st.SCB_status)
    let  msg = {code: 0, payload: 0}
    com.mode.devlog (`main: handle emwt run response: p=${p} status=${status}`)
    switch (status) {
    case st.SCB_halted:
        com.mode.devlog (`*** main: handle emwt halt`)
        em.refresh (guiEmulatorState)
//        st.showSCBstatus (guiEmulatorState)
        //        stopClock (guiEmulatorState)
        finishRun (guiEmulatorState)
        break
    case st.SCB_paused:
        com.mode.devlog (`*** main: handle emwt pause`)
        em.stopClock (guiEmulatorState)
        em.refresh (guiEmulatorState)
        st.showSCBstatus (guiEmulatorState)
        st.writeSCB (guiEmulatorState, st.SCB_pause_request, 0)
        st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_ready)
        st.showSCBstatus (guiEmulatorState)
        com.mode.devlog (`*** main: finished handle emwt pause`)
        break
    case st.SCB_break:
        com.mode.devlog (`*** main: handle emwt break`)
        em.stopClock (guiEmulatorState)
        em.refresh (guiEmulatorState)
        st.showSCBstatus (guiEmulatorState)
        st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_ready)
        st.showSCBstatus (guiEmulatorState)
        com.mode.devlog (`*** main: finished handle emwt break`)
        break
    case st.SCB_blocked:
        com.mode.devlog (`*** main: handle emwt blocked`)
        break
    case st.SCB_relinquish: // emwt halt signals halt, not relinquish
        com.mode.devlog (`*** main: handle emwt relinquish`)
        st.showSCBstatus (guiEmulatorState)
        st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_running_gui)
        em.executeInstruction (guiEmulatorState)
        st.decrInstrCount (guiEmulatorState) // instruction was counted twice
        if (st.readSCB (guiEmulatorState, st.SCB_status) === st.SCB_halted) {
            console.log ("main: handle emwt relinquish: halted")
//            em.refresh (guiEmulatorState)
//            stopClock (guiEmulatorState)
            finishRun (guiEmulatorState)
        } else {
            console.log (`main: handle emwt relinquish: resuming`)
            st.writeSCB (guiEmulatorState, st.SCB_status, st.SCB_running_emwt)
            msg = {code: 102, payload: 0}
            emwThread.postMessage (msg)
        }
        com.mode.devlog (`*** main: finished handle emwt relinquish`)
        break
    case st.SCB_reset:
    case st.SCB_ready:
    case st.SCB_running_gui:
    case st.SCB_running_emwt:
    default:
        com.mode.devlog (`main:handleEmwtRunResponse unknown status = ${status}`)
    }
    com.mode.devlog ("main: handleEmwtRunResponse finished")
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
    com.mode.devlog ("main: emwtShowRegs")
    let msg = {code: 103, payload: 0}
    emwThread.postMessage (msg)
}

function handleEmwtShowResponse (p) {
    com.mode.devlog (`main: handle emwt show response ${p}`)
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
        com.mode.devlog ("main has received a message")
        if (e.data) {
            com.mode.devlog ("main has received data from message")
            let p = e.data.payload
            switch (e.data.code) {
            case 200: // initialize
                com.mode.devlog (`main: received 200 init response`)
                handleEmwtInitResponse (p)
                break
            case 201: // emwt step
                com.mode.devlog (`main: received 201 step response`)
                handleEmwtStepResponse (p)
                break
            case 202: // emwt run
                com.mode.devlog (`main rec 202 run response`)
                handleEmwtRunResponse (p)
                break
            case 203: // emwt show
                com.mode.devlog (`main: rec 203 emwt show response`)
                handleEmwtShowResponse (p)
                break
            case 204: // emwt test 1
                com.mode.devlog (`main: rec 204 emwt test 1 response`)
                handleEmwtTest1Response (p)
                break
            case 205: // emwt test 2
                com.mode.devlog (`main: rec 205 emwt test 2 response`)
                handleEmwtTest2Response (p)
                break
            default:
                com.mode.devlog (`main: received unknown code = ${e.data.code}`)
            }
            com.mode.devlog (`main event handler returning`)
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

//export function showPane (p) {

const showPane = (gst) => (p) => {
    com.mode.devlog (`showPane ${p.description}`);
    finalizeLeaveCurrentPane ();
    currentPane = p;
    switch (currentPane) {
    case WelcomePane:
        gst.currentKeyMap = defaultKeyMap
        highlightPaneButton (gst, "Welcome_Pane_Button")
        break;
    case ExamplesPane: ;
        gst.currentKeyMap = examplesKeyMap
        highlightPaneButton (gst, "Examples_Pane_Button")
        break;
    case ModulesPane:
        gst.currentKeyMap = modulesKeyMap
        highlightPaneButton (gst, "Modules_Pane_Button")
        smod.refreshModulesList ();
        break;
    case EditorPane:
        gst.currentKeyMap = editorKeyMap
        highlightPaneButton (gst, "Editor_Pane_Button")
        ed.enterEditor ();
        break;
    case AssemblerPane:
        gst.currentKeyMap = asmKeyMap
        highlightPaneButton (gst, "Assembler_Pane_Button")
        asm.enterAssembler ();
        break;
    case LinkerPane:
        gst.currentKeyMap = linkerKeyMap
        highlightPaneButton (gst, "Linker_Pane_Button")
        break;
    case ProcessorPane:
        gst.currentKeyMap = procKeyMap
        highlightPaneButton (gst, "Processor_Pane_Button")
        break;
    case OptionsPane:
        gst.currentKeyMap = defaultKeyMap
        highlightPaneButton (gst, "Options_Pane_Button")
        break;
    case DevToolsPane:
        gst.currentKeyMap = defaultKeyMap
        break;
    }
    document.getElementById(paneIdString(p)).style.display = "block";
    com.mode.devlog(`Show pane ${p.description}`);
}

const DefaultPaneButtonBackground = "#f8f8f8"
const HighlightedPaneButtonBackground = "#e0fde0"

function highlightPaneButton (gst, bid) {
    console.log (`highlightPaneButton ${bid}`)
    document.getElementById(bid).style.background = HighlightedPaneButtonBackground
    gst.currentPaneButton = bid
}

function unhighlightPaneButton (gst) {
    console.log (`unhighlightPaneButton ${gst.currentPaneButton}`)
    const b = gst.currentPaneButton
    let oldbackground = document.getElementById(b).style.background
    console.log (`oldbackground = ${oldbackground}`)
    document.getElementById(b).style.background = DefaultPaneButtonBackground
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
    unhighlightPaneButton (gst)
    gst.currentPaneButton = "Welcome_Pane_Button"
    gst.currentKeyMap = defaultKeyMap
    document.getElementById(paneIdString(currentPane)).style.display = "none";
}

//------------------------------------------------------------------------------
// Processor pane
//------------------------------------------------------------------------------

// The runMain and runWorker functions set the preferred thread and
// then call procRun, so they set the thread choice persistently.
// runGeneric checks the emRunThread field in the gui state to decide
// which to use.

function runGeneric (gst) {
    console.log (`runGeneric emRunThread = ${gst.emRunThread}`)
    switch (gst.emRunThread) {
    case com.ES_gui_thread:
        console.log ("runGeneric: use main thread")
        runMain (guiEmulatorState)
        break
    case com.ES_worker_thread:
        console.log ("runGeneric: use worker thread")
        runWorker (guiEmulatorState)
        break
    default:
        console.log ("runGeneric: invalid emRunThread")
    }
}

function runMain (es) {
    es.emRunThread = com.ES_gui_thread
    procRun (es)
}

function runWorker (es) {
    es.emRunThread = com.ES_worker_thread
    procRun (es)
}

// Run instructions until stopping condition is reached.  This will be
// performed using either the main gui thread or on the worker thread.
// First decide whether to go aheead with the run; if so, decide which
// thread to run it in.

function procRun (es) {
    com.mode.devlog (`gui.procRun, thread = ${es.emRunThread}`)
    let q = st.readSCB (es, st.SCB_status)
    switch (q) {
    case st.SCB_ready:
    case st.SCB_paused:
    case st.SCB_blocked:
        switch (es.emRunThread) {
        case com.ES_gui_thread:
            console.log ("procRun: starting in main gui thread")
            st.writeSCB (es, st.SCB_status, st.SCB_running_gui)
            es.initRunDisplay (es)
            em.mainThreadLooper (es)
            break
        case com.ES_worker_thread:
            console.log ("procRun: starting in worker thread")
            es.initRunDisplay (es)
            st.writeSCB (es, st.SCB_status, st.SCB_running_emwt)
            emwtRun (es)
            break
        default:
            com.mode.devlog (`Error procRun ${es.emRunThread}`)
        }
        break
    default: // State is not appropriate for run, so don't do it
        com.mode.devlog (`procRun skipping because SCB_status=${q}`)
    }
}

// Main interface function to step one instruction; runs in main gui
// thread

export function procStep (es) {
    console.log ("procStep")
    if (es.thread_host != com.ES_gui_thread) {
        com.mode.devlog (`procStep: host=${es.thread_host}, skipping`)
        return
    }
    st.writeSCB (es, st.SCB_pause_request, 0)
    let q = st.readSCB (es, st.SCB_status)
    switch (q) {
    case st.SCB_ready:
    case st.SCB_paused:
    case st.SCB_break:
    case st.SCB_relinquish:
        com.mode.devlog ("procStep: main thread executing instruction...")
        em.executeInstruction (es)
        let qnew = st.readSCB (es, st.SCB_status)
        if (qnew != st.SCB_halted) st.writeSCB (es, st.SCB_status, st.SCB_ready)
        em.execInstrPostDisplay (es)
        em.guiDisplayNinstr (es)
        break
    case st.SCB_reset:
    case st.SCB_running_gui:
    case st.SCB_running_emwt:
    case st.SCB_halted:
    case st.SCB_blocked:
        com.mode.devlog ("procStep skipping instruction...")
        break
    default: com.mode.devlog (`error: procStep unknown SCB_status= ${q}`)
    }
    console.log ("procStep finished")
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

function examplesBack () {
    console.log (`examplesBack`)
}


// Copy the example text to the editor.  The example is shown as a web
// page and its content is obtained using innerHTML.

// This does not work.  Perhaps because it's an iframe, not an input?
// Copy text of example buffer to clipboard
/*
function copyExampleToClipboard () {
    com.mode.devlog ('Copy example to clipboard');
    let exElt = document.getElementById('ExamplesIframeId');
    exElt.select();
    exElt.setSelectionRange(0,5);
    document.execCommand('copy');
}
*/

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
    com.mode.devlog (`setMidMainLRratio:  old=${midLRratio} new=${r}`)
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
    console.log (`setMidMainLeftWidth old ratio = ${midLRratio} `
                 + `new ratio = ${newratio}`)
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


let showingUserGuide = true
let toggleGuideSaveRatio = midLRratio

function rememberCurrentMidMainLeftWidth () {
    currentMidMainLeftWidth = midMainLeft.style.width
}

// let currentMidMainWidth = midMainLeft.style.width

function toggleUserGuide () {
    let xs
    if (showingUserGuide) {
        xs = "Show User Guide"
        toggleGuideSaveRatio = midLRratio
        hideUserGuide ()
    } else {
        xs = "Hide User Guide"
        midLRratio = toggleGuideSaveRatio
        showUserGuide ()
    }
    showingUserGuide = !showingUserGuide
    document.getElementById("Toggle_UserGuide").textContent = xs
}

function showUserGuide () {
    console.log (`showUserGuide midLRratio=${midLRratio}`)
//    setMidMainLRratio(0.65);  // useful for dev to keep mem display visible
    showSizeParameters();
    adjustToMidMainLRratio();
}
//    setMidMainLeftWidth (currentMidMainWidth)
//    adjustToMidMainLRratio ()

function hideUserGuide () {
    setMidMainLeftWidth (window.innerWidth)
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

export function prepareExampleText () {
    console.log ("prepareExmapleText")
    document.getElementById("ExamplesIframeId")
        .addEventListener("load", event => checkExample ())
}

function checkExample () {
    const htmlDetector = /\s*<pre\sstyle=/
    const elt = document.getElementById("ExamplesIframeId")
    const xs = elt.contentWindow.document.body.innerHTML;
    const y = xs.split("\n")[0]
    const q = htmlDetector.exec (y)
    if (q) {
        console.log (`checkExample: looks like example text <${y}>`)
        selectExample ()
    } else {
        console.log (`checkExample: looks like html <${y}>`)
    }
//    console.log (`checkExample <${xs}>\n<${ys}>`)
}


// Make new module, copy example text into it, and select it

function selectExample() {
    let exElt = document.getElementById('ExamplesIframeId');
    let xs = exElt.contentWindow.document.body.innerHTML;
    com.mode.devlog (`selectExample raw xs = ${xs}`);
    let skipPreOpen = xs.replace(com.openingPreTag,"");
    let skipPreClose = skipPreOpen.replace(com.closingPreTag,"");
    com.mode.devlog (`skipPreOpen = ${skipPreOpen}`);
    let ys = skipPreClose;
    //    let m = new st.S16Module ("Example");
    let m = new st.S16Module (ed.findModName (ys))
    m.asmEdText = ys;
    smod.refreshEditorBuffer();
    smod.refreshModulesList();
}


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

/*
function configureBrowser (gst) {
    console.log ("configureBrowser")
    cn.runConfig (gst)
}

function configureBrowser (es) {
    const supportLocalStorage = checkBrowserStorageSupport ()
    cn.output (`Browser supports local storage = ${supportLocalStorage}`)
    const supportWorker = cn.checkBrowserWorkerSupport ()
    cn.output (`Browser supports web workers = ${supportWorker}`)
    const supportSharedMem = cn.checkSharedMemorySupport ()
    cn.output (`Browser supports shared array buffers = ${supportSharedMem}`)
    es.emRunCapability = supportWorker &&  supportSharedMem
        ? em.ES_worker_thread
        : em.ES_gui_thread
    es.emRunThread = es.emRunCapability // default: run according to capability
    cn.output (`Emulator run capability = ${es.emRunCapability}`)
}
*/

// System state vector

// export let sysStateBuf = null
// export let sysStateVec = null
let emwThread = null

// Memory is allocated in the main thread and sent to the worker
// thread, if there is a worker

function allocateStateVector (es) {
    console.log (`allocateStateVector es.emRunCapability = ${es.emRunCapability}`)
    com.mode.trace = true
    switch (es.emRunCapability) {
    case com.ES_worker_thread:
        console.log ("allocateStateVector, run capability: Worker supported")
        es.vecbuf = new SharedArrayBuffer (st.EmStateSizeByte)
        es.vec16 = new Uint16Array (es.vecbuf)
        es.vec32 = new Uint32Array (es.vecbuf)
        es.shm = es.vec16
        es.vec32[0] = 456
        es.vec32[1] = 2 * es.vec32[0]
        console.log (`***blarg**** ${es.vec32[0]} ${es.vec32[1]}`)
        // Start the emulator thread and initialize it
        com.mode.devlog ("gui.mjs starting emwt")
        emwThread = new Worker("../base/emwt.mjs", {type:"module"});
        initializeEmwtProtocol (es)
        emwtInit (es)
        com.mode.devlog ("gui.mjs has started emwt")
        com.mode.trace = false
        break
    case com.ES_gui_thread:
        console.log ("allocateStateVector, run capability: Worker not supported")
        es.vecbuf = new ArrayBuffer (st.EmStateSizeByte)
        es.vec16 = new Uint16Array (es.vecbuf)
        es.vec32 = new Uint32Array (es.vecbuf)
        es.shm = es.vec16
        break
    default:
        cn.output (`allocateStateVector: bad emRunCapability = `
                   + `${es.emRunCapability}`)
    }
    cn.output (`EmStateSizeWord = ${st.EmStateSizeWord}`)
    cn.output (`EmStateSizeByte = ${st.EmStateSizeByte}`)
    cn.output (`vec16 contains ${es.vec16.length} elements`)
}


/*      
//    es.shm = sysStateVec
        sysStateVec = new Uint16Array (st.EmStateSizeWord)
        es.shm = sysStateVec
        break
        sysStateBuf = new SharedArrayBuffer (st.EmStateSizeByte)
        sysStateVec = new Uint16Array (sysStateBuf)
        es.shm = sysStateVec
        // Start the emulator thread and initialize it
        com.mode.devlog ("gui.mjs starting emwt")
        emwThread = new Worker("../base/emwt.mjs", {type:"module"});
        initializeEmwtProtocol (es)
        emwtInit (es)
*/

function testSysStateVec (es) {
    cn.output (`Testing emulator memory: ${es.thread_host}`)
    let xs = ""
    let n = 3
    for (let i = 0; i < n; i++) es.shm[i] = i
    for (let i = 0; i < n; i++) es.shm[i] += 100
    for (let i = 0;  i < n; i++) xs += ` ${i}->${es.shm[i]}`
    cn.output (`thread host ${es.thread_host}: ${xs} finished`)
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
//    com.mode.devlog (`prepare button ${bid}`);
    document.getElementById(bid)
        .addEventListener('click', event => {fcn()});
}

// Pane buttons; initialization must occur after emulator state is defined

function initializeButtons () {
    prepareButton ('Welcome_Pane_Button',   () => showPane (gst) (WelcomePane));
    prepareButton ('Examples_Pane_Button',  () => showPane (gst) (ExamplesPane));
    prepareButton ('Modules_Pane_Button',   () => showPane (gst) (ModulesPane));
    prepareButton ('Editor_Pane_Button',    () => showPane (gst) (EditorPane));
    prepareButton ('Assembler_Pane_Button', () => showPane (gst) (AssemblerPane));
    prepareButton ('Linker_Pane_Button',    () => showPane (gst) (LinkerPane));
    prepareButton ('Processor_Pane_Button', () => showPane (gst) (ProcessorPane));
    prepareButton ('Options_Pane_Button'  , () => showPane (gst) (OptionsPane));
    prepareButton ('DevTools_Pane_Button', () => showPane (gst) (DevToolsPane));
    prepareButton ('About_Button',
                   () => showGuideSection('sec-about-sigma16'));  
    prepareButton ('Toggle_UserGuide', toggleUserGuide)

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
    prepareButton ('EXP_Help',       () => toggleExamplesHelp ());
    prepareButton ('ExamplesHelpClose',       () => toggleExamplesHelp ());
    prepareButton ('EXP_Examples_Home',    examplesHome);
    prepareButton ('EXP_Back',    examplesBack);

    // Modules pane (MP)
    // prepareButton ('MP_New',    smod.newModule)
    prepareButton ('MP_Help',       () => toggleModulesHelp ());
    prepareButton ('ModulesHelpClose',  () => toggleModulesHelp ());
    prepareButton ('MP_Refresh',    smod.refreshModulesList)
    prepareButton ('MP_New',        smod.newMod)
    prepareButton ('MP_Hello_world', () => insert_example(example_hello_world))
    prepareButton ('MP_Test1',        smod.test1)
    prepareButton ('MP_Test2',        smod.test2)
    prepareButton ('MP_Test3',        smod.test3)

    // Editor pane (EDP)
    prepareButton ('EDP_Help',       () => toggleEditorHelp ());
    prepareButton ('EditorHelpClose',       () => toggleEditorHelp ());
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
    prepareButton ('AP_Help',       () => toggleAssemblerHelp ());
    prepareButton ('AssemblerHelpClose',       () => toggleAssemblerHelp ());
    prepareButton ('AP_Assemble',        asm.assemblerGUI);
    prepareButton ('AP_Show_Source',     asm.displayAsmSource);
    prepareButton ('AP_Show_Object',     asm.setObjectListing);
    prepareButton ('AP_Show_Listing',    asm.setAsmListing);
    prepareButton ('AP_Show_Metadata',   asm.setMetadata);
    
    // Linker pane (LP)
    prepareButton ('LP_Help',       () => toggleLinkerHelp ());
    prepareButton ('LinkerHelpClose',       () => toggleLinkerHelp ());
    prepareButton ('LP_Link',            link.linkerGUI);
    prepareButton ('LP_Read_Object',     link.getLinkerModules);
    prepareButton ('LP_Show_Executable', link.linkShowExecutable);
    prepareButton ('LP_Show_Metadata',   link.linkShowMetadata);

    // Processor pane (PP)
    prepareButton ('PP_Help',       () => toggleProcHelp ());
    prepareButton ('ProcHelpClose',       () => toggleProcHelp ());
    prepareButton ('PP_Boot',       () => em.boot (guiEmulatorState));
    prepareButton ('PP_Step',       () => procStep (guiEmulatorState));
    prepareButton ('PP_Run',        () => runGeneric (gst))
    prepareButton ('PP_Pause',      () => em.procPause (guiEmulatorState));
    prepareButton ('PP_Interrupt',  () => em.procInterrupt (guiEmulatorState));
    prepareButton ('PP_Breakpoint', () => em.procBreakpoint (guiEmulatorState));
    prepareButton ('PP_Refresh',    () => em.refresh (guiEmulatorState));
    prepareButton ('PP_Reset',      () => em.procReset (guiEmulatorState));
    prepareButton ('PP_RunMain',    () => runMain (guiEmulatorState))
    prepareButton ('PP_RunWorker',  () => runWorker (guiEmulatorState))
    prepareButton ('PP_Test1',      () => test1 (guiEmulatorState))
    prepareButton ('PP_Test2',      emwtTest2);


    prepareButton ('PP_Timer_Interrupt',  () => em.timerInterrupt (guiEmulatorState));
    // prepareButton ('PP_Toggle_Display',  em.toggleFullDisplay);

    // Breakpoint popup dialogue
    /*
      prepareButton ("BreakRefresh", em.breakRefresh(em.emulatorState));
      prepareButton ("BreakEnable", em.breakEnable(em.emulatorState));
      prepareButton ("BreakDisable", em.breakDisable(em.emulatorState));
      prepareButton ("BreakClose", em.breakClose());
    */

    // Options
    
    // DevTools
    prepareButton ('DevTools102',    devTools102);
    prepareButton ('DevTools103',    devTools103);
    prepareButton ('DevTools104',    devTools104);
    prepareButton ('DevTools105',    devTools105);
    prepareButton ('DevTools106',    devTools106);
    prepareButton ('DisableDevTools', disableDevTools);
}

//-----------------------------------------------------------------------------
// Query SigServer for latest version
//-----------------------------------------------------------------------------

// Find version number of currently running program and set it in the gui

function findThisVersion () {
    const v = ver.s16version
    document.getElementById('ThisVersion').innerHTML = v
}

// Query Sigma16 home page on github pages for the SigServer location,
// then query server for the latest version number

function findLatestVersion () {
    console.log ("*** findLatestVersion starting")
    const serverAddressLoc = `${com.S16HOMEPAGEURL}/admin/SIGSERVERURL.txt`
    fetch (serverAddressLoc)
        .then (repositoryResponse => {
            return repositoryResponse.text()
        }).then (serverURL => {
            const latestURL = `${serverURL}/status/latest/${ver.s16version}`
            console.log (`*** findLatestVersion server= ${serverURL}`)
            console.log (`*** findLatestVersion latestURL= ${latestURL}`)
            return fetch (latestURL)
        }).then (serverResponse => {
            return serverResponse.text()
        }).then (latest => {
            console.log (`*** findLatestVersion latest= ${latest}`)
            latestVersion = latest
            document.getElementById('LatestVersion').innerHTML = latest
        })
        .catch (error => {
            console.log (`findLatestVersion error ${error}`)
        })
    console.log ("*** findLatestVersion started actions, now returning")
}

//-------------------------------------------------------------------------------
// Run the initializers when onload event occurs
//-------------------------------------------------------------------------------

let guiEmulatorState // declare here, define at onload event 
let browserSupportsWorkers = false

// The onload function runs in the main gui thread but not in worker thread

let gst // global GUI state, set during initialization

window.onload = function () {
    com.mode.devlog("window.onload activated: starting initializers");
    em.hideBreakDialogue ();
    gst = new GuiState ()
    em.initializeSubsystems ();
    document.getElementById('LinkerText').innerHTML = "";    
    smod.prepareChooseFiles ();
    initialize_mid_main_resizing ();
    setMidMainLRratio(0.65);  // useful for dev to keep mem display visible
    toggleGuideSaveRatio = midLRratio
    adjustToMidMainLRratio();
    //    showSizeParameters();
    initializePane ();
    smod.initModules ();
    window.mode = com.mode;
    guiEmulatorState = new em.EmulatorState (
        com.ES_gui_thread,
        () => initRun (guiEmulatorState),
        () => updateWhileRunning (guiEmulatorState),
        () => finishRun (guiEmulatorState) )
    console.log ("allocate state vector")
    cn.configureOptions (gst, guiEmulatorState)
    allocateStateVector (guiEmulatorState)
    console.log ("allocate state vector done")
    testSysStateVec (guiEmulatorState)
    em.initializeMachineState (guiEmulatorState)
    initializeButtons ()
    prepareExampleText ()
    em.clearClock (guiEmulatorState)
    guiEmulatorState.emRunThread = com.ES_gui_thread // default run mode
    em.procReset (guiEmulatorState)
    findThisVersion ()
    findLatestVersion ()
    com.mode.trace = true
    com.mode.devlog (`Thread ${guiEmulatorState.mode} initialization complete`)
    com.mode.trace = false
}
