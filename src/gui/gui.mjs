// Sigma16: gui.mjs
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

//-------------------------------------------------------------------------
// gui.mjs is the main program for the browser interface.  It's
// launched by Sigma16.html and is the last JavaScript file to be
// loaded
//-------------------------------------------------------------------------

import * as ver   from '../base/version.mjs';
import * as com   from '../base/common.mjs';
import * as smod  from '../base/s16module.mjs';
import * as arch  from '../base/architecture.mjs';
import * as arith from '../base/arithmetic.mjs';
import * as ab    from '../base/arrbuf.mjs';
import * as st    from '../base/state.mjs';
import * as ed    from './editor.mjs';
import * as asm   from '../base/assembler.mjs';
import * as link  from '../base/linker.mjs';
import * as em    from '../base/emulator.mjs';

// Normally runs in user mode, which is the default.  For development
//and experimentation, set Dev mode.  In console, enter setModeDev()
//or setModeUser().

//-------------------------------------------------------------------------
//Constant parameters
//-------------------------------------------------------------------------

// Window layout
const InitMidLRratio = 0.6  // initial width ratio midMainLeft/midMainRight
const UGRSMALL = 1  // small number of pixels to move for guide resizing
const UGRLARGE = 20 // large number of pixels to move for guide resizing

// Colors
const DefaultPaneButtonBackground = "#f8f8f8"
const HighlightedPaneButtonBackground = "#e0fde0"

// Initial options
const InitialMainSliceSize = 500 // user can override on options page
const InitialWorkerRefreshInterval = 250 // period (ms) worker disp refresh
const InitialMDslidingSize = 4096
const MemDispMinSize = 20 // always display at least this many locations
const InitialMemorySizeStr = '64KW'
const InitialMemorySize = 65536

//-------------------------------------------------------------------------
// Initialization
//-------------------------------------------------------------------------

// The program runs in a main thread and optionally a worker thread.
// The program starts in the main thread; a worker thread is created
// if and only if this is requested by teh options settings.  If the
// platform doesn't support shared memory, a worker thread is not
// created and the emulator runs only in the main thread.

// Each thread has its own private instance of class EmulatorState
// which contains variables that are visible only in that thread.

// The system starts up by initializing the state and optional worker
// thread.  The emulator state is not created until the user enters
// the processor pane; this gives the user a chance to change the
// settings in the options page.

// The user can examine and change settings in the Options page.  The
// defaults settings are not fixed; they are calculated based on the
// platform's capabilities in order to give a good environment.  This
// means users don't need to understand the internal workings of the
// program in order to use it.

// When the user clicks on the Processor tab, initializeProcessor is
// called, which performs the following actions:

// 1. When the window.onload event fires, it calls initializeSystem
//    which performs the following actions:

//    1.a initializeSystem creates a new GuiState object and saves it
//        in the global variable gst.  This is (or should be) the only
//        global variable.  The GuiState contains the overall system
//        state, including the state of the gui layout (e.g. the
//        window sizing)

//    1.b initializeSystem calls functions that set up the gui layout
//        and the event listeners.

//    1.c initialize system calls refreshOptionsDisplay, which queries
//        the platform and sets the initial options according to the
//        platform's capabilities.  The user can change the settings,
//        as long as they are compatible with the capabilities.

//    1.d initializeSystem calls findLatestRelease, which makes an
//        http request to query the SigServer (running on heroku) for
//        the number of the latest release.  Later, when (and if) the
//        response to this query is received, the display on the
//        Options page is updated to show the latest release number.

// 2. When the user selects the Processor tab, the main thread calls
//    enterProcessor.  The first time this is invoked it calls
//    initializeProcessor.  This is where the emulator state is
//    created and initialized, as follows:

//    2.a initializeProcessor calls mkMainEmulatorState to an instance
//        of the EmulatorState class for the main thread, an object
//        that contains information required by the emulator.  This is
//        accessible only in the main thread.  The emulator state
//        contains a field for the system state vector, as well as
//        several typed arrays which are views onto the system state
//        buffer.  Those fields are all initialized to be null.

//    2.b initializeProcessor calls allocateStateVector, which creates
//        either a typed array buffer, a shared array buffer, or a Web
//        Assemly memory, depending on the options settings.

//    2.c If the options specify using a worker thread,
//        initializeProcessor performs the following steps:

//        2.c.1 initializeProcessor creates a new worker thread and
//              stores it in gst.emwThread

//        2.c.2 It then calls initializeEmwtProtocol (gst.es), which
//              defines an event listener for messages from the
//              worker.

//        2.c.2 initiizeProcessor calls emwtInit (gst.es), which posts
//              a message to the worker thread.  The emulator worker
//              thread (emwt) responds to the message as follows:

//              2.c.2.a A new EmulatorState object is created and
//                      saved in emwt.es.

//              2.c.2.b The payload of the initialization message
//                      contains a reference to the system state
//                      vector, which is stored in emwt.es.vecbuf.
//                      This must be a shared array buffer and is
//                      created by the main thread.  Typed array views
//                      onto the buffer are defined and saved in
//                      emwt.es.

//              2.c.2.c emwt calls initializeMachineState (emwt.es),
//                      which creates the register objects.  The
//                      actual register values are kept in the shared
//                      array buffer, making them accessible to both
//                      the main thread and the worker thread.

//-------------------------------------------------------------------------
// Global state variable
//-------------------------------------------------------------------------

// The gui state is an instance of the GuiState class.  It's retained
// in a global variable gst, which is defined here to get it into
// scope, and initialized later when the onload event occurs.  The
// onload function runs in the main gui thread but not in a worker
// thread.  The gui state contains an emulator state for the main
// thread.  If there is a worker, it has an emulator state but not a
// GuiState.

let gst = null

//-------------------------------------------------------------------------
// Gui state class
//-------------------------------------------------------------------------

// The global gui state gst is an instance of GuiState

class GuiState {
    constructor () {
        // Configuration parameters
        this.options = new Options ()
        this.guiMode = 'User'
        
        // Keyboard
        this.currentKeyMap = defaultKeyMap

        // Window layout
        this.windowWidth = null
        this.middleSection = null
        this.midMainLeft = null
        this.midMainRight = null
        this.midLRratio = InitMidLRratio
        this.midSecExtraWidth = 15
        this.showingUserGuide = true
        this.toggleGuideSaveRatio = InitMidLRratio

        // Emulator state
        this.es = null   // emulator state for main thread (worker has its own)
        this.currentPaneButton = "Welcome_Pane_Button"
        //        this.emRunThread = com.ES_gui_thread  part of es
        //        this.emRunThread = com.ES_worker_thread

        // Worker thread
        this.emwThread = null // may be set by initializeWorkerThread
        
        // Processor display
        this.MemDispMode = ModeMemDisplayHBA
        this.memDispArg = 30
        this.memString = []
        this.instrCodeElt = null
        this.instrFmtElt = null
        this.instrOpElt = null
        this.instrArgsElt = null
        this.instrEAElt =  null
        this.instrCCElt =  null
        this.instrEffect1Elt = null
        this.instrEffect2Elt = null
        this.memElt1 = null
        this.memElt2 = null
        this.metadata           = null
	this.asmListingCurrent  = [] // listing displayed in emulator pane
        this.asmListingHeight   = 0   // height in pixels of the listing
	this.curInstrLineNo     = -1  // -1 if no line has been highlighted
	this.nextInstrLineNo    = -1
	this.saveCurSrcLine     = ""
	this.saveNextSrcLine    - ""
        this.mainSliceSize = 1

        // Memory display
        this.highBootAddress = 0

        // Clock
        this.startTime = null
        this.eventTimer = null
    }
}

//-----------------------------------------------------------------------------
// Configuration options
//-----------------------------------------------------------------------------

class Options {
    constructor () {
        com.mode.devlog ('Creating options record')

        // Version
        this.thisVersion = ver.s16version
        this.latestRelease = 'see Sigma16 Home Page' // update after server query
        
        // Platform capabilities
        this.supportLocalStorage = !!window.localStorage
        this.supportWorker = !!window.Worker
        this.supportSharedMem = !!window.SharedArrayBuffer
        this.crossOriginIsolated = !!window.crossOriginIsolated
//        this.crossOriginIsolated = true // temp test ????????????

        // Memory settings
        this.memoryIsAllocated = false
        this.bufferType = ArrayBufferLocal
        this.memorySize = InitialMemorySize
        this.memDisplayMode = ModeMemDisplayHBA
        this.currentMDslidingSize = InitialMDslidingSize

        // Emulator settings
        this.processorIsUninitialized = true
        this.currentThreadSelection = com.ES_gui_thread
        this.mainSliceSize = InitialMainSliceSize
        this.workerRefreshInterval = InitialWorkerRefreshInterval
    }
}

function adjustInitialOptions () {
    com.mode.devlog ('new Options, adjusting initial settings')
    const opt = gst.options
    if (isSetBufferSharedOk (false)) {
        setArrayBufferShared (false)
    } else {
        setArrayBufferLocal (false)
    }
    if (isSetThreadWorkerOk (false)) {
        setThreadWorker (false)
    } else {
        setThreadMain (false)
    }
}


const warningChangeAfterAllocation =
      'This memory option cannot be changed after memory has been allocated.'
      + ' This happens when you first enter the Processor page.  To adjust'
      + ' the setting, restart the app and change the options before entering'
      + ' the Processor page.'

const warningRequireSupportWorkerShared =
      'This option cannot be selected because the platform does not support'
      + ' both Workers and Shared Array Buffers.'

const warningRequireCrossOriginIsolation =
      'This option cannot be selected because the server does not provide'
      + ' cross origin isolation.'

const warningRequireSharedMemory =
      'This option cannot be selected because it requires shared memory.'
      + ' If memory is not yet allocated, you can select shared memory'
      + ' and try again.'

// Are requirements satisfied for selecting local typed array buffer?

function isSetBufferLocalOk (warn) {
    const opt = gst.options
    let ok = true
    if (opt.memoryIsAllocated) {
        ok = false
        maybeWarn (warn, warningChangeAfterAllocation)
    }
//    console.log (`isSetBufferLocalOk = ${ok}`)
    return ok
}

// Are requirements satisfied for selecting shared array buffer?

function isSetBufferSharedOk (warn) {
    const opt = gst.options
    let ok = true
    if (opt.memoryIsAllocated) {
        ok = false
        maybeWarn (warn, warningChangeAfterAllocation)
    } else if (! (opt.supportWorker && opt.supportSharedMem)) {
        ok = false
        maybeWarn (warn, warningRequireSupportWorkerShared)
    } else if (!opt.crossOriginIsolated) {
        ok = false
        maybeWarn (warn, warningRequireCrossOriginIsolation)
    }
//    console.log (`isSetBufferSharedOk = ${ok}`)
    return ok
}

// Are requirements satisfied for selecting worker thread?

function isSetThreadWorkerOk (warn) {
    const opt = gst.options
    let ok = true
    if (opt.bufferType === ArrayBufferLocal) {
        ok = false
        maybeWarn (warn, warningRequireSharedMemory)
    }
//    console.log (`isSetThreadWorkerOk = ${ok}`)
    return ok
}

function setArrayBufferLocal (warn) {
    com.mode.devlog ('setArrayBufferLocal')
    const opt = gst.options
    if (isSetBufferLocalOk (warn)) {
        gst.options.bufferType = ArrayBufferLocal
        document.getElementById('ArrayBufferLocal').checked = true
        setThreadMain (gst)
        refreshOptionsDisplay ()
    }}

function setArrayBufferShared (warn) {
    com.mode.devlog ('setArrayBufferShared')
    const opt = gst.options
    if (isSetBufferSharedOk (warn)) {
        opt.bufferType = ArrayBufferShared
        document.getElementById('ArrayBufferShared').checked = true
        refreshOptionsDisplay ()
    }
}
//    com.mode.devlog ('setArrayBufferShared - revert to wasm mem')
//    setArrayBufferWebAssembly (warn)

function setArrayBufferWebAssembly (warn) {
    com.mode.devlog ('setArrayBufferWebAssembly')
    setArrayBufferShared (warn)
}
/*
com.mode.devlog ('setArrayBufferWebAssembly')
    const opt = gst.options
    if (isSetBufferSharedOk (warn)) {
        opt.bufferType = ArrayBufferWebAssembly
        document.getElementById('ArrayBufferShared').checked = true
        refreshOptionsDisplay ()
    }
*/

function setThreadMain (warn) {
//    com.mode.devlog ('setThreadMain')
    const opt = gst.options
    opt.currentThreadSelection = com.ES_gui_thread
    document.getElementById('RTmain').checked = true
    refreshOptionsDisplay ()
}

function setThreadWorker (warn) {
//    com.mode.devlog ('setThreadWorker')
    const opt = gst.options
    if (isSetThreadWorkerOk (warn)) {
        opt.currentThreadSelection = com.ES_worker_thread
        document.getElementById('RTworker').checked = true
        refreshOptionsDisplay ()
    } else {
        setThreadMain (warn)
    }
}

// Set memory display mode according to radio buttons
//have ids MDhba, MDsliding, MDfull

const setMDhba = (gst) => (e) => {
    com.mode.devlog ('setMDhba')
    gst.options.memDispMode = ModeMemDisplayHBA
}

const setMDsliding = (gst) => (e) => {
    com.mode.devlog ('setMDsliding')
    gst.options.memDispMode = ModeMemDisplaySliding
}

const setMDfull = (gst) => (e) => {
    com.mode.devlog ('setMDfull')
    gst.options.memDispMode = ModeMemDisplayFull
}

// User can update numerical parameters in options page

function updateMemSize () {
//    com.mode.devlog ('updateMemSize')
    const opt = gst.options
    if (opt.memoryIsAllocated) {
        maybeWarn (true, warningChangeAfterAllocation)
    } else {
        let xs = document.getElementById('EnterMemSize').value
//        com.mode.devlog (`updateMemSize ${xs}`)
        let x = parseInt (xs)
        if (!isNaN (x)) {
            opt.memorySize = x
            refreshOptionsDisplay ()
        }
    }
}

function updateMDslidingSize () {
    com.mode.devlog ("updateMDslidingSize")
    let xs = document.getElementById("EnterMDslidingSize").value
    let x = parseInt (xs)
    com.mode.devlog (`update MemDispSize <${xs}> = ${x}`)
    if (!isNaN(x)) {
        document.getElementById('MDslidingSize').innerHTML = x
        gst.options.currentMDslidingSize = x
        com.mode.devlog (`Updated memory display sliding size := ${x}`)
    }
}

function updateMainSliceSize () {
//    com.mode.devlog ("updateMainSliceSize")
//    e.stopPropagation ()
    let xs = document.getElementById("EnterMainSliceSize").value
    let x = parseInt (xs)
    if (!isNaN(x)) {
        document.getElementById("MainSliceSize").innerHTML = x
        gst.options.mainSliceSize = x
        refreshOptionsDisplay ()
    }
}

function updateWorkerRefreshInterval () {
//    com.mode.devlog ("updateWorkerRefreshInterval")
//    e.stopPropagation ()
    let xs = document.getElementById("EnterWorkerRefreshInterval").value
    let x = parseInt (xs)
    if (!isNaN(x)) {
        document.getElementById("WorkerRefreshInterval").innerHTML = x
        gst.options.workerRefreshInterval = x
        refreshOptionsDisplay ()
    }
}

// Display current options in gui

function refreshOptionsDisplay () {
//    com.mode.devlog ('displayOptions')
    const opt = gst.options
//    com.mode.devlog (`LR = ${opt.latestRelease}`)
    setHtml ('ThisVersion', opt.thisVersion)
    setHtml ('LatestRelease', opt.latestRelease)
    setHtml ("SupportLocalStorage", opt.supportLocalStorage)
    setHtml ("SupportWorker", opt.supportWorker)
    setHtml ("SupportSharedMem", opt.supportSharedMem)
    setHtml ("CrossOriginIsolated", opt.crossOriginIsolated)
    setHtml ('CrossOriginIsolated', opt.crossOriginIsolated)
    setHtml ('MemoryIsAllocated', opt.memoryIsAllocated)
    setHtml ('OptBufferType', opt.bufferType.description)
    setHtml ('MemorySize', opt.memorySize)
    setHtml ('MDslidingSize', opt.currentMDslidingSize)
    setHtml ('CurrentThreadSelection', com.showThread(opt.currentThreadSelection))
    setHtml ('MainSliceSize', opt.mainSliceSize)
    setHtml ('WorkerRefreshInterval', opt.workerRefreshInterval)
}

// Memory options

const ArrayBufferWebAssembly = Symbol ('Buffer WebAssembly')
const ArrayBufferShared = Symbol ('Buffer Shared')
const ArrayBufferLocal = Symbol ('Buffer Local')

const ModeMemDisplayHBA = Symbol ("MD HBA")          // 0 to highest booted address
const ModeMemDisplaySliding = Symbol ("MD sliding")  // sliding window
const ModeMemDisplayFull = Symbol ("MD full")         // full memory

function showMemDisplayOptions () {
    const s = gst
    switch (s.memDispMode) {
    case ModeMemDisplayHBA:
        console.log (`memory display mode = HBA (${s.highBootAddress})`)
        break
    case ModeMemDisplaySliding:
        console.log (`memory display mode = sliding (${s.currentMDslidingSize})`)
        break
    case ModeMemDisplayFull:
        console.log (`memory display mode = full`)
        break
    default:
        console.log (`Invalid memory display mode`)
    }
}

//-----------------------------------------------------------------------------
// Query SigServer for latest version
//-----------------------------------------------------------------------------
// Query Sigma16 home page on github pages for the SigServer location,
// then query server for the latest version number

function findLatestRelease (gst) {
    console.log ('Looking up the latest release')
    const serverAddressLoc = `${com.S16HOMEPAGEURL}/admin/SIGSERVERURL.txt`
    fetch (serverAddressLoc)
        .then (repositoryResponse => {
            return repositoryResponse.text()
        }).then (serverURL => {
            const latestURL = `${serverURL}/status/latest/${ver.s16version}`
            com.mode.devlog (`*** findLatestRelease server= ${serverURL}`)
            com.mode.devlog (`*** findLatestRelease latestURL= ${latestURL}`)
            return fetch (latestURL)
        }).then (serverResponse => {
            return serverResponse.text()
        }).then (latest => {
            console.log (`Latest release is ${latest}`)
            gst.options.latestRelease = latest
            refreshOptionsDisplay ()
//          document.getElementById('LatestRelease').innerHTML = latest
        })
        .catch (error => {
            com.mode.devlog (`findLatestRelease error ${error}`)
        })
    com.mode.devlog ("*** findLatestRelease started actions, now returning")
}

//-----------------------------------------------------------------------------
// Utilities
//-----------------------------------------------------------------------------

// If warn then display msg in an alert

function maybeWarn (warn, msg) {
    if (warn) modalWarning (msg)
}

// Write the html text h into the DOM element with id i

function setHtml (i,h) {
    document.getElementById(i).innerHTML = h
}

export function modalWarning (msg) {
    alert (msg);
}

function updateWhileRunning (gst) {
    updateClock (gst)
    refreshRFdisplay (gst)
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

// Initialize the variables (middleSection, midMainLeft, midMainRight)
// in the onload event, because the DOI elements must exist before the
// variables are assigned.

function initialize_mid_main_resizing (gst) {
    com.mode.devlog ('initializing mid-main resizing')
    gst.middleSection = document.getElementById("MiddleSection");
    gst.midMainLeft = document.getElementById("MidMainLeft");
    gst.midMainRight = document.getElementById("MidMainRight");
    gst.windowWidth =  window.innerWidth;
}

// Update the saved ratio
function setMidMainLRratio (r) {
    com.mode.devlog (`setMidMainLRratio:  old=${gst.midLRratio} new=${r}`)
    gst.midLRratio = r;
}

// Readjust the widths of left and right sections to match ratio r
function adjustToMidMainLRratio () {
    com.mode.devlog ('adjustToMidMainLRratio:  midLRratio = ' + gst.midLRratio)
    let ww =  window.innerWidth - gst.midSecExtraWidth;
    let x = gst.midLRratio * ww;
//    com.mode.devlog ('  windowWidth = ' + windowWidth);
//    com.mode.devlog ('  setting left width = ' + x);
//    com.mode.devlog ('  about to call set left width');
    setMidMainLeftWidth (x);
//    com.mode.devlog ('  back from calling set left width');
}

// Grow/shrink the left section to w pixels

function setMidMainLeftWidth (newxl) {
    com.mode.devlog ('setMidMainLeftWidth ' + newxl);

    let ww =  window.innerWidth - gst.midSecExtraWidth;
    let oldxl = gst.midMainLeft.style.width;
    let oldratio = gst.midLRratio;
    com.mode.devlog ('  old dimensions: ww = ' + ww +
		 ' oldxl=' + oldxl + ' oldratio=' + oldratio);

    let newxr = ww - newxl;
    let newxlp = newxl + "px";
    let newratio = newxl / (newxl + newxr);
    com.mode.devlog (`setMidMainLeftWidth old ratio = ${gst.midLRratio} `
                 + `new ratio = ${newratio}`)
    com.mode.devlog ('  new dimensions: ww = ' + ww +
		 ' newxl=' + newxl + ' newxr=' + newxr + ' newratio=' + newratio);

    setMidMainLRratio (newratio);

    com.mode.devlog ('  setting left = ' + newxl + '  right = ' + newxr);
    gst.midMainLeft.style.width = newxlp;
    gst.midMainLeft.style.flexGrow = 0;  // make them grow/shrink together

    com.mode.devlog ('  left width:   old=' + oldxl + ' new=' + newxl);
    com.mode.devlog ('  ratio:  old=' + oldratio + '  new=' + newratio);
    com.mode.devlog ('setMidMainLeftWidth finished');
}

function expLRflex (xl) {
    com.mode.devlog ('expLRflex');
    let ww =  window.innerWidth - gst.midSecExtraWidth;
    let xr = ww - xl;
    let xlp = xl + 'px';
    let xrp = xr + 'px';
    gst.midMainLeft.style.flexBasis = xlp;
    gst.midMainLeft.style.flexGrow = '0px';
    gst.midMainRight.style.flexBasis = xrp;
    gst.midMainRight.style.flexGrow = '0px';
}

function showSizeParameters () {
    com.mode.devlog ('showSizeParameters');
    let ww =  window.innerWidth - gst.midSecExtraWidth;
    let y = gst.midMainLeft.style.width;
    com.mode.devlog ('  windowWidth = ' + ww);
    com.mode.devlog ('  midMainLeftWidth = ' + y);
    com.mode.devlog ('  midLRratio = ' + gst.midLRratio);
}

// Resize the system (midMainLeft) and user guide (midMainRight)
// sections.  When the - or + button is clicked in the GUI,
// user_guide_resize (x) is called: x>0 means expand the user guide by
// x px; x<0 means shrink it.

function user_guide_resize(x) {
    com.mode.devlog ('user_guide_resize ' + x);
//    showSizeParameters ();
    let old_width = gst.midMainLeft.style.width;
    com.mode.devlog ('  old width = ' + old_width);
    let w = parseInt(gst.midMainLeft.style.width,10);
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

function rememberCurrentMidMainLeftWidth () {
    currentMidMainLeftWidth = gst.midMainLeft.style.width
}

// let currentMidMainWidth = midMainLeft.style.width

//-------------------------------------------------------------------------------
//  Handle window events
//-------------------------------------------------------------------------------

window.onbeforeunload = function(event) {
    event.returnValue = "Write something clever here..";
};

window.onresize = function () {
    com.mode.devlog ('window.onresize');
//    showSizeParameters ();
    //    setMidMainLRratio (midLRratio);  // preserve ratio as window is resized
    adjustToMidMainLRratio ();
    com.mode.devlog ('window.onresize finished');
}

//-------------------------------------------------------------------------------
// User guide
//-------------------------------------------------------------------------------

function toggleUserGuide () {
    let xs
    if (gst.showingUserGuide) {
        xs = "Show User Guide"
        gst.toggleGuideSaveRatio = gst.midLRratio
        hideUserGuide ()
    } else {
        xs = "Hide User Guide"
        gst.midLRratio = gst.toggleGuideSaveRatio
        showUserGuide ()
    }
    gst.showingUserGuide = !gst.showingUserGuide
    document.getElementById("Toggle_UserGuide").textContent = xs
}

function showUserGuide () {
    com.mode.devlog (`showUserGuide midLRratio=${gst.midLRratio}`)
    showSizeParameters();
    adjustToMidMainLRratio();
}

function hideUserGuide () {
    setMidMainLeftWidth (window.innerWidth)
}

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
        enterProcessor ()
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

function highlightPaneButton (gst, bid) {
    com.mode.devlog (`highlightPaneButton ${bid}`)
    document.getElementById(bid).style.background = HighlightedPaneButtonBackground
    gst.currentPaneButton = bid
}

function unhighlightPaneButton (gst) {
    com.mode.devlog (`unhighlightPaneButton ${gst.currentPaneButton}`)
    const b = gst.currentPaneButton
    let oldbackground = document.getElementById(b).style.background
    com.mode.devlog (`oldbackground = ${oldbackground}`)
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
// Define actions for buttons
//------------------------------------------------------------------------------

// Connect a button in the html with its corresponding function

function prepareButton (bid,fcn) {
    document.getElementById(bid)
        .addEventListener('click', event => {fcn()});
}

// Pane buttons; initialization must occur after emulator state is defined

function initializeButtons () {
    prepareButton ('Arch16button', setArch16)
    prepareButton ('Arch32button', setArch32)
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

    // User guide resize (UGR) buttons.  UGR Distance (px) to move
    // boundary between gui and userguide on resize

    prepareButton ('UG_Resize_Right_Large_Button',
                   () => user_guide_resize(UGRLARGE));
    prepareButton ('UG_Resize_Right_Small_Button',
                   () => user_guide_resize(UGRSMALL));
    prepareButton ('UG_Resize_Left_Small_Button',
                   () => user_guide_resize(-UGRSMALL));
    prepareButton ('UG_Resize_Left_Large_Button',
                   () => user_guide_resize(-UGRLARGE));

    // Help boxes
    prepareButton ('WP_Help',              () => toggleWelcomeHelp ());
    prepareButton ('WelcomeHelpClose',     () => toggleWelcomeHelp ());
    prepareButton ('EXP_Help',             () => toggleExamplesHelp ());
    prepareButton ('ExamplesHelpClose',    () => toggleExamplesHelp ());
    prepareButton ('MP_Help',              () => toggleModulesHelp ());
    prepareButton ('ModulesHelpClose',     () => toggleModulesHelp ());
    prepareButton ('EDP_Help',             () => toggleEditorHelp ());
    prepareButton ('EditorHelpClose',      () => toggleEditorHelp ());
    prepareButton ('AP_Help',              () => toggleAssemblerHelp ());
    prepareButton ('AssemblerHelpClose',   () => toggleAssemblerHelp ());
    prepareButton ('LP_Help',              () => toggleLinkerHelp ());
    prepareButton ('LinkerHelpClose',      () => toggleLinkerHelp ());
    prepareButton ('PP_Help',              () => toggleProcHelp ())
    prepareButton ('ProcHelpClose',        () => toggleProcHelp ())
    
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
    prepareButton ('EXP_Back',    examplesBack);

    // Modules pane (MP)
    // prepareButton ('MP_New',    smod.newModule)
    prepareButton ('MP_Refresh',    smod.refreshModulesList)
    prepareButton ('MP_New',        smod.newMod)
    prepareButton ('MP_Hello_world', () => insert_example(example_hello_world))
    prepareButton ('MP_Test1',        smod.test1)
    prepareButton ('MP_Test2',        smod.test2)
    prepareButton ('MP_Test3',        smod.test3)

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
    prepareButton ('LP_Show_Object',     link.linkShowObject);
    prepareButton ('LP_Show_Executable', link.linkShowExecutable);
    prepareButton ('LP_Show_Metadata',   link.linkShowMetadata);
    prepareButton ('LP_Read_Object',     link.getLinkerModules);

    // Processor pane (PP)
    prepareButton ('PP_Boot',       () => procBoot (gst))
    prepareButton ('PP_Step',       () => procStep (gst))
    prepareButton ('PP_Run',        () => runGeneric (gst))
    prepareButton ('PP_Pause',      () => procPause (gst))
    prepareButton ('PP_Interrupt',  () => procInterrupt (gst))
    prepareButton ('PP_Breakpoint', () => procBreakpoint (gst))
    prepareButton ('PP_Refresh',    () => procRefresh (gst))
    prepareButton ('PP_Reset',      () => procReset (gst))
    prepareButton ('PP_T1',         () => test_t1 ())
    prepareButton ('PP_T2',         () => test_t2 ())
    prepareButton ('PP_Timer_Interrupt', () => timerInterrupt (gst));
//    prepareButton ('PP_RunMain',    () => runMain (gst))
//    prepareButton ('PP_RunWorker',  () => runWorker (gst))
//    prepareButton ('PP_Test1',      () => test1 (gst))
//    prepareButton ('PP_Test2',      emwtTest2);

    // Breakpoint popup dialogue
    prepareButton ("BreakRefresh", () => breakRefresh(gst));
    prepareButton ("BreakEnable",  () => breakEnable(gst));
    prepareButton ("BreakDisable", () => breakDisable(gst));
    prepareButton ("BreakClose",   () => breakClose(gst));

    // Options pane
    prepareButton ('UpdateMemSize',         () => updateMemSize (gst))
    prepareButton ('UpdateMDslidingSize',   () => updateMDslidingSize (gst))
    prepareButton ('UpdateMainSliceSize',   () => updateMainSliceSize (gst))
    prepareButton ('UpdateWorkerRefreshInterval',
                   () => updateWorkerRefreshInterval (gst))

    document.getElementById("MDhba")
        .addEventListener ("change", setMDhba (gst))
    document.getElementById("MDsliding")
        .addEventListener ("change", setMDsliding (gst))
    document.getElementById("MDfull")
        .addEventListener ("change", setMDfull (gst))

    document.getElementById("RTmain")
        .addEventListener ("change", (e) => setThreadMain (true))
    document.getElementById("RTworker")
        .addEventListener ("change", (e) => setThreadWorker (true))

    document.getElementById("ArrayBufferLocal")
        .addEventListener ("change", (e) => setArrayBufferLocal (true))
    document.getElementById("ArrayBufferShared")
        .addEventListener ("change", (e) => setArrayBufferShared (true))

    document.getElementById("EnterMainSliceSize")
        .addEventListener ("keydown", (e) => { e.stopPropagation () })
    document.getElementById("EnterWorkerRefreshInterval")
        .addEventListener ("keydown", (e) => { e.stopPropagation () })
    document.getElementById("EnterMDslidingSize")
        .addEventListener ("keydown", (e) => { e.stopPropagation () })

    // DevTools
    prepareButton ('DevTools102',    devTools102);
    prepareButton ('DevTools103',    devTools103);
    prepareButton ('DevTools104',    devTools104);
    prepareButton ('DevTools105',    devTools105);
    prepareButton ('DevTools106',    devTools106);
//    prepareButton ('DisableDev',     setModeUser);
}

//-------------------------------------------------------------------------
// Key maps
//-------------------------------------------------------------------------

const defaultKeyMap = new Map ([
    ["KeyH",  toggleWelcomeHelp],
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
    ["KeyB",  () => procBoot (gst)],
    ["KeyS",  () => procStep (gst.es)],
    ["KeyR",  () => procRun (gst.es)],
    ["KeyP",  () => procPause (gst)],
    ["KeyI",  () => procInterrupt (gst)],
])

//-----------------------------------------------------------------------------
// Help popups
//-----------------------------------------------------------------------------

// You can open or close a help box by pressing h

// Most tabs have their own specialized help.  For tabs where
// specialized help isn't useful, the generic help will be
// displayed.

let genericHelpDialogueVisible = false
export function toggleWelcomeHelp () {
    document.getElementById("WelcomeHelpDialogue").style.display
	= genericHelpDialogueVisible ? "none" : "block";
    genericHelpDialogueVisible = !genericHelpDialogueVisible;
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

//-----------------------------------------------------------------------------
// Keyboard
//-----------------------------------------------------------------------------

function handleKeyDown (e) {
    com.mode.devlog (`handleKeyDown code=${e.code} keyCode=${e.keyCode}`)
    let action = gst.currentKeyMap.get (e.code)
    if (action) {
        e.handled = true
        com.mode.devlog (`=== do action for key code=${e.code} keyCode=${e.keyCode}`)
        action ()
        com.mode.devlog (`finished action for key code=${e.code} keyCode=${e.keyCode}`)
    } else {
        com.mode.devlog (`no action for key code=${e.code} keyCode=${e.keyCode}`)
    }
}

// Enable keyboard shortcuts, requires gst to be defined

function enableKeyboardShortcuts () {
    document.addEventListener ("keydown", handleKeyDown)
    // Disable keyboard shortcuts for text entry buffers
    document.getElementById("IOinputBuffer")
        .addEventListener ("keydown", handleTextBufferKeyDown)
    document.getElementById("BreakTextArea")
        .addEventListener ("keydown", handleTextBufferKeyDown)
    document.getElementById("EditorTextArea")
        .addEventListener ("keydown", handleTextBufferKeyDown)
}

// Stop key down event propagation in text entry areas

function handleTextBufferKeyDown (e) {
    com.mode.devlog (`handleTextBbufferKeyDown code=${e.code} keyCode=${e.keyCode}`)
    e.stopPropagation () // inhibit using key as keyboard shortcut command
}

//-------------------------------------------------------------------------------
// Example programs
//-------------------------------------------------------------------------------

// This file is Sigma16/src/gui/gui.mjs
// The examples directory is Sigma16/examples
// The index for the examples directory is ../../examples/index.html

function examplesHome() {
    com.mode.devlog ("examplesHome");
    document.getElementById("ExamplesIframeId").src =
	"./examples/index.html";
}

function examplesBack () {
    com.mode.devlog (`examplesBack`)
}

export function prepareExampleText () {
    com.mode.devlog ("prepareExmapleText")
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
        com.mode.devlog (`checkExample: looks like example text <${y}>`)
        selectExample ()
    } else {
        com.mode.devlog (`checkExample: looks like html <${y}>`)
    }
//    com.mode.devlog (`checkExample <${xs}>\n<${ys}>`)
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
// Editor pane
//-------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Processor
//------------------------------------------------------------------------------

// Processor elements: html elements for displaying instruction decode

export function initializeProcessorElements (gst) {
    com.mode.devlog ('initializeProcessorElements');
    gst.instrCodeElt = document.getElementById("InstrCode");
    gst.instrFmtElt  = document.getElementById("InstrFmt");
    gst.instrOpElt   = document.getElementById("InstrOp");
    gst.instrArgsElt = document.getElementById("InstrArgs");
    gst.instrEAElt   = document.getElementById("InstrEA");
    gst.instrCCElt   = document.getElementById("InstrCC");
    gst.instrEffect1Elt = document.getElementById("InstrEffect1");
    gst.instrEffect2Elt = document.getElementById("InstrEffect2");
}

function enterProcessor () {
    console.log ('enterProcessor')
    if (gst.options.processorIsUninitialized) {
        initializeProcessor ()
1    }
}

function initializeProcessor () {
    console.log ('Initializing processor')
    mkMainEmulatorState ()
    allocateStateVector ()
    refreshOptionsDisplay ()
    if (gst.options.bufferType === ArrayBufferShared
       || gst.options.bufferType === ArrayBufferWebAssembly) {
        console.log ('initializeProcessor: worker for emwt')
        gst.emwThread = new Worker ("./emwt.mjs", {type: "module"});
        console.log (`gst.emwThread = ${gst.emwThread}`)
        console.log ("call initializeEmwtProtocol")
        initializeEmwtProtocol (gst.es)
        emwtInit (gst.es)
        com.mode.devlog ("gui.mjs has started emwt")
    }
    gst.options.processorIsUninitialized = false
}
//        gst.emwThread = new Worker ("./dummyworker.mjs", {type: "module"});

function procReset (gst) {
    com.mode.devlog ('gui.procReset')
    em.procReset (gst.es)
    em.clearMemLogging (gst.es)
    em.clearRegLogging (gst.es)
    newUpdateRegisters (gst)
    memDisplay (gst)
    refreshProcStatusDisplay (gst)
    guiDisplayNinstr (gst)

}

//------------------------------------------------------------------------------
// Processor display
//------------------------------------------------------------------------------

// The mode determines the quantity and destination of output

export const Mode_GuiDisplay = 100
export const Mode_Console    = 200
export const Mode_Quiet      = 300

// Number of header lines in the listing before the source lines begin
const listingLineInitialOffset = 1;

// export function refreshDisplay (es) {
export function refreshDisplay (gst) {
    newUpdateRegisters (gst)
    memDisplay (gst);
    document.getElementById('ProcAsmListing').innerHTML = "";
    refreshInstrDecode (gst);
    guiDisplayNinstr (gst)
    es.ioLogBuffer = ""
    em.refreshIOlogBuffer (gst.es)
    st.showSCBstatus (gst.es)
}

// These functions display information on the gui; they abstract the
// document DOM out of the emulator

export function guiDisplayNinstr (gst) {
        let n = gst.es.vec32[0]
        document.getElementById("nInstrExecuted").innerHTML = n
}

export function guiDisplayMem (gst, elt, xs) {
    if (gst.es.thread_host === com.ES_gui_thread) elt.innerHTML = xs
}

// Displaying processor

// For single stepping, we want to keep display of registers and
// memory up to date and show access by highlighting the fetched and
// updated locations.  For Run mode, we want to avoid updating the
// memory continuosly, although it may be useful to keep the register
// displays updated.

// The strategy is: (1) for stepping, there is a function to prepare
// before executing an instruction, and another to update the displays
// after execution, with the expectation that the user will spend some
// time looking at the displays before steppign again.  (2) For Run,

// (for running) Prepare the displays before running sequence of
// instructions (the Run button).

// After this, either updateRegisters or refreshRegisters

export function execInstrPostDisplay (gst) {
//    console.log (`execInstrPostDisplay: ${em.showEsInfo (gst.es)}`)
    const es = gst.es
    com.mode.devlog ("main: execInstrPostDisplay, proceeding")
    newUpdateRegisters (gst)
    memDisplay (gst)
    highlightListingAfterInstr (gst)
    updateInstrDecode (gst)
    guiDisplayNinstr (gst)
    document.getElementById("procStatus").innerHTML = ab.showSCBstatus (es)
}

function procRefresh (gst) {
//    com.mode.devlog ("procRefresh")
    com.mode.devlog ("procRefresh")
    newUpdateRegisters (gst)
    memDisplayFull (gst)
    refreshProcStatusDisplay (gst)
    guiDisplayNinstr (gst)
}

const refreshProcessorDisplay = (gst) => (es) => {
}

// Just clear regs & memory, then refreshProcessorDisplay
function clearProcessorDisplay (gst) {
//    com.mode.devlog ('clearProcessorDisplay')
    initializeProcessorElements (gst)
    gst.es.asmListingCurrent = []
    em.clearInstrDecode (gst.es)
}

// Copy executable listing to processor asm display

function displayProcAsmListing (gst) {
//    com.mode.devlog ('displayProcAsmListing')
    const elt = document.getElementById('ProcAsmListing')
    const xs = "<pre><code class='HighlightedTextAsHtml'>"
    	+ gst.asmListingCurrent.join('\n')
	  + "</code></pre>"
    elt.innerHTML = xs
    const htLinePx = elt.scrollHeight / gst.asmListingCurrent.length
    const vat = 5 // number of visible lines above current
    const i = Math.max (0, gst.curInstrLineNo - vat)
    const y = htLinePx * i
    elt.scroll (0, y)
}

//------------------------------------------------------------------------------
// Emulator Interface to gui
//------------------------------------------------------------------------------

// The functions receive a parameter 'es' which carries the current
// emulator state.  The gui, when it calls one of the main interface
// functions, passes the global emulatorState.

// The machine state (registers and memory) are global and initialized
// by gui.  The emulator state is a global variable named
// emulatorState, defined in state.js.

// The interface to the emulator consists of the following functions,
// which are called directly by the gui. When they call other
// functions, es is passed as a parameter.  Thus only the following
// interface functions access a global variable.

// Called by ?
//   parseCopyObjectModuleToMemory (es)
    
// Called by gui.js
//   clearInstrDecode ()

// Called by events in Sigma16.html
//   procReset(es)       -- put processor into initial state
//   boot(es)            -- copy the executable into memory
//   procStep(es)        -- execute one instruction
//   procRunMainThread(es)         -- execute instructions repeatedly until halted
//   procPause(es)       -- halt execution (can resume execution later)
//   procInterrupt()   -- not implemented yet
//   procBreakpoint()  -- not implemented yet

export function refreshProcStatusDisplay (gst) {
    let xs = ab.showSCBstatus (gst.es)
    document.getElementById("procStatus").innerHTML = xs
}

export let highlightedRegisters = [];

// Update the display of all registers and memory (all of memory)

// export function displayFullState (es) {
export function displayFullState (gst) {
    com.mode.devlog ('displayFullState');
    //    updateRegisters (gst)
    newUpdateRegisters (gst)
    //    memUpdate (gst)
    memDisplay (gst)
//    memDisplayFull (gst);
}

//------------------------------------------------------------------------------
// Instruction decoding
//------------------------------------------------------------------------------

// Use instruction fields to display the decoded instruction

// Emulator state contains numeric fields, gui state contains strings

export function updateInstrDecode (gst) {
    com.mode.devlog ("updateInstrDecode");
    const es = gst.es
    const ccval = es.regfile[15].get ()
    gst.instrCodeElt.innerHTML    = es.instrCodeStr;
    gst.instrFmtElt.innerHTML     = es.instrFmtStr;
    gst.instrOpElt.innerHTML      = es.instrOpStr;
    gst.instrArgsElt.innerHTML    = showArgs(es); // instrArgsStr;
    gst.instrEAElt.innerHTML      = es.instrEAStr;
    gst.instrCCElt.innerHTML      = arch.showCC(ccval);
    gst.instrEffect1Elt.innerHTML =  showEffect(es,0);
    gst.instrEffect2Elt.innerHTML =  showEffect(es,1);
}

function showArgs (es) {
    if (es.instrFmtStr==="RRR") {
	return `R${es.ir_d},R${es.ir_a},R${es.ir_b}`;
    } else if (es.instrFmtStr==="RX") {
	return `R${es.ir_d},${arith.wordToHex4(es.instrDisp)}[R${es.ir_a}]`;
    } else {
	return "?";
    }
}

function showEffect (es,i) {
    com.mode.devlog(`showEffect ${i}`);
    if (es.instrEffect[i]) {
	let [dest,idx,val,name] = es.instrEffect[i];
	if (dest==="R") {
	    com.mode.devlog (`showEffect ${i} ${es.instrEffect[i]}`);
//	    com.mode.devlog (`showEffect ${i}  ${dest} ${idx} := ${val});
	    return `${name} := ${arith.wordToHex4(val)}`;
	} else if (dest==="M") {
	    com.mode.devlog ("showEffect M");
	    return `M[${arith.wordToHex4(idx)}]:=${arith.wordToHex4(val)}`;
	}
    } else { return ""; }
}

//-----------------------------------------------------------------------------
// Register display
//-----------------------------------------------------------------------------

// Highlighting registers to indicate accesses

// When a register is accessed, its display in the gui is highlighted
// by setting the text color.  If the register has not been used it
// has the default color black, if it has been read but not written
// its color is READ, and if it has been written its color is WRITE.
// The meanings of the tags for syntax highlighting are defined in
// Sigma16gui.css.  Normally we would use blue for READ and red for
// WRITE.

let modeHighlight = true;  // indicate get/put by setting text color

function setModeHighlight (x) {
    if (x) {
	com.mode.devlog('Setting modeHighlight to True');
	modeHighlight = true;
    }
    else {
	com.mode.devlog('Setting modeHighlight to False');
	modeHighlight = false;
	refreshRegisters(gst);
    }
}

// Refresh the registers, removing any existing highlighting, and
// highlight the current accesses

function newUpdateRegisters (gst) {
    const es = gst.es
//    console.log (`newUpdateRegisters, es: ${em.showEsInfo(es)}`)
    for (let i = 0; i < gst.es.nRegisters; i++) {
	gst.es.register[i].refresh();
    }
    // Update the new register accesses
    com.mode.devlog (`newupdateRegisters rfet=${es.copyable.regFetched}`)
    com.mode.devlog (`newupdateRegisters rsto=${es.copyable.regStored}`)
    for (let x of es.copyable.regFetched) es.register[x].highlight ("GET")
    for (let x of es.copyable.regStored) {
        es.register[x].refresh()
        es.register[x].highlight ("PUT")
    }
}

//-----------------------------------------------------------------------------
// Memory display
//-----------------------------------------------------------------------------

// Choose a range of memory locations and display them

function memDisplay (gst) {
    const cp = gst.es.copyable
    const itarget = cp.memFetchInstrLog[0] ?? 0
    const dtarget = cp.memStoreLog[0] ?? cp.memFetchDataLog[0] ?? 0
    const iRange = getMemRange (gst, itarget)
    const dRange = getMemRange (gst, dtarget)
    const a = Math.min (iRange.a, dRange.a)
    const b = Math.max (iRange.b, dRange.b)
    for (let i = a; i < b; i++) setMemString (gst, i)
    for (let x of cp.memFetchInstrLog) memHighlight (gst, x, "GET")
    for (let x of cp.memFetchDataLog)  memHighlight (gst, x, "GET")
    for (let x of cp.memStoreLog)      memHighlight (gst, x, "PUT")
    const iarr = gst.memString.slice (iRange.a, iRange.b)
    const darr = gst.memString.slice (dRange.a, dRange.b)
    const itext = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	  + iarr.join('\n')
	  + "</code></pre>";
    const dtext = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	  + darr.join('\n')
	  + "</code></pre>";
    const memElt1 = document.getElementById('MemDisplay1')
    const memElt2 = document.getElementById('MemDisplay2')
    memElt1.innerHTML = itext
    memElt2.innerHTML = dtext
    const vat = 8 // lines above target that should be visible
    scrollToTarget (memElt1, iRange, vat)
    scrollToTarget (memElt2, dRange, vat)
}

function memDisplayFull (gst) { // refactor: abstraction
    const cp = gst.es.copyable
    const itarget = cp.memFetchInstrLog[0] ?? 0
    const dtarget = cp.memStoreLog[0] ?? cp.memFetchDataLog[0] ?? 0
//    const iRange = getMemRange (gst, itarget)
//    const dRange = getMemRange (gst, dtarget)
    const iRange = {a: 0, b: 65536, n: 65536, scrollto: itarget}
    const dRange = {a: 0, b: 65536, n: 65536, scrollto: dtarget}
    const a = Math.min (iRange.a, dRange.a)
    const b = Math.max (iRange.b, dRange.b)
    for (let i = a; i < b; i++) setMemString (gst, i)
    for (let x of cp.memFetchInstrLog) memHighlight (gst, x, "GET")
    for (let x of cp.memFetchDataLog)  memHighlight (gst, x, "GET")
    for (let x of cp.memStoreLog)      memHighlight (gst, x, "PUT")
    const iarr = gst.memString.slice (iRange.a, iRange.b)
    const darr = gst.memString.slice (dRange.a, dRange.b)
    const itext = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	  + iarr.join('\n')
	  + "</code></pre>";
    const dtext = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	  + darr.join('\n')
	  + "</code></pre>";
    const memElt1 = document.getElementById('MemDisplay1')
    const memElt2 = document.getElementById('MemDisplay2')
    memElt1.innerHTML = itext
    memElt2.innerHTML = dtext
    const vat = 8 // lines above target that should be visible
    scrollToTarget (memElt1, iRange, vat)
    scrollToTarget (memElt2, dRange, vat)
}

// Use memory display mode to calculate this

function getMemRange (gst, t) {
    let a = 0
    let b = 65535
    switch (gst.memDispMode) {
    case ModeMemDisplayHBA:
        b = Math.max (gst.highBootAddress, MemDispMinSize)
        break
    case ModeMemDisplaySliding:
        const x = Math.round (gst.currentMDslidingSize / 2)
        a = t-x
        b = t+x
        break
    case ModeMemDisplayFull:
        break
    default:
        console.log (`getMemRange: invalid memDispMode`)
    }
    if (a < 0) {
        b += -a
        a = 0}
    b = Math.min (b, 65536)
    const n = b - a
    const scrollto = t - a
    const result = {a, b, n, scrollto}
//    com.mode.devlog (`getMemRange a=${a} b=${b} n=${n} t=${scrollto}`)
    return result
}

// Convert memory location to hex string

function setMemString (gst, a) {
    //    let x = gst.es.shm[ab.EmMemOffset + a] // contents of mem[a]
    let x = ab.readMem16 (gst.es, a)
    gst.memString[a] = arith.wordToHex4(a) + ' ' + arith.wordToHex4(x)
}

// Highlight memory location string to indicate fetch or store access

function memHighlight (gst, a, highlight) {
    gst.memString[a] =
	"<span class='" + highlight + "'>" + gst.memString[a] + "</span>"
}

// Scroll element displaying xs to make target line visible, leaving
// visibleAboveTarget lines visible in the window above the target
// line.

function scrollToTarget (elt, range, visibleAboveTarget) {
    const htLinePx = elt.scrollHeight / range.n
    const i = Math.max (0, range.scrollto - visibleAboveTarget)
    const y = htLinePx * i
    elt.scroll (0, y)
}

//------------------------------------------------------------------------------
// Assembly listing
//------------------------------------------------------------------------------

// The assembler provides an array of source lines, which it passes on
// to the linker and thence to the emulator.  There are two strings
// for each source line: one contains <span> elements to enable the
// fields to be highlighted, just as in the assembly listing.  The
// other omits these elements, so the entire line can be highlighted
// to indicate (with just one color for the line) the instruction that
// has just executed and the instruction that will be executed next.

// The assembler produces listing lines with <span> elements to allow
// the fields of the line to be highlighted. These are stored in
// listingHighlightedFields.  However, the emulator highlights an
// entire listing line to indicate the instruction that is currently
// executing, or that will execute next.  In order to prevent the
// highlighting of fields from overriding the highlighting of the
// current/next instruction, that is done using listingPlain.

// Prepare assembly listing when executable is booted

export function initListing (gst) {
//    com.mode.devlog ('initListing')
    gst.es.curInstrAddr = 0;
    gst.curInstrLineNo = -1;  // -1 indicates no line has been highlighted
    gst.es.nextInstrAddr = 0;
    gst.nextInstrLineNo = gst.metadata.getSrcIdx (gst.es.nextInstrAddr)
        + listingLineInitialOffset;
    highlightListingLine (gst, gst.nextInstrLineNo, "NEXT")
    displayProcAsmListing (gst)
}

// Highlighting current and next instruction in processor assembly listing

// As it executes an instruction, the emulator sets curInstrAddr and
// nextInstrAddr.  After the instruction has finished, these
// instructions are highlighted in the listing.  Any existing
// highlighting is removed, the line numbers of the current and next
// instruction are calculated, and the highlighting is applied.

function highlightListingAfterInstr (gst) {
//    com.mode.trace = true;
    com.mode.devlog ('highlightListingAfterInstr');
//    showListingParameters (gst)
    // Clear any existing statement highlighting
    if (gst.curInstrLineNo >= 0) {
            revertListingLine (gst, gst.curInstrLineNo)
            gst.curInstrLineNo = -1;
        }
    if (gst.nextInstrLineNo >= 0) {
        revertListingLine (gst, gst.nextInstrLineNo)
        gst.nextInstrLineNo = -1;
    }
    // Highlight the instruction that just executed
    gst.curInstrLineNo = gst.metadata.getSrcIdx (gst.es.curInstrAddr)
            + listingLineInitialOffset;
    highlightListingLine (gst, gst.curInstrLineNo, "CUR");
    // Highlight the instruction that will be executed next
    gst.nextInstrLineNo = gst.metadata.getSrcIdx (gst.es.nextInstrAddr)
        + listingLineInitialOffset;
    highlightListingLine (gst, gst.nextInstrLineNo, "NEXT");
    // Display the listing
    displayProcAsmListing (gst)
    com.mode.devlog ("Returning from highlightlistingAfterInstr")
//    showListingParameters (gst)
//    com.mode.trace = false;
}

export function highlightListingLine (gst, i, highlight) {
    if (i > 0) {
        gst.asmListingCurrent[i] =
            "<span class='" + highlight + "'>"
            + gst.metadata.listingPlain[i]
            + "</span>";
    }
}

function revertListingLine (gst, i) {
//    com.mode.devlog (`revertListingLine ${i} `)
    if (i > 0) {
//        com.mode.devlog (`  revert old ${gst.asmListingCurrent[i]}`)
        gst.asmListingCurrent[i] = gst.metadata.listingDec[i]
//        com.mode.devlog (`  revert new ${gst.asmListingCurrent[i]}`)
    }
}

// Given address a, the corresponding source statement is found using metadata

function showListingParameters (gst) {
    com.mode.devlog ('Proc asm listing parameters')
    com.mode.devlog ('  gst.es.curInstrAddr  = ' + gst.es.curInstrAddr)
    com.mode.devlog ('  gst.curInstrLineNo   = ' + gst.curInstrLineNo)
    com.mode.devlog ('  gst.es.nextInstrAddr = ' + gst.es.nextInstrAddr)
    com.mode.devlog ('  gst.nextInstrLineNo  = ' + gst.es.nextInstrLineNo)
}

//-----------------------------------------------------------------------------
// Emulator gui
//-----------------------------------------------------------------------------

// Calculate the value of pxPerChar, which is needed to control the
// scrolling to make the current line visible.  The calculated value
// overrides the initialized value.  The method is to measure the
// height of the listing in pixels and divide by the number of lines
// in the listing.  Some other geometric parameters are also obtained,
// but aren't currently used.

function getListingDims (gst) {
    const es = gst.es
    let e = document.getElementById('ProcAsmListing');
    let x = e.getBoundingClientRect(); // dimensions of visible listing area
    let w = e.scrollWidth; // width of the content, not used
    let h = e.scrollHeight; // height of content (not of the window)
    es.asmListingHeight = h; // save in emulator state
    com.mode.devlog (`h=${h} w=${w}`);
    let n = gst.metadata.listingPlain.length;
    com.mode.devlog(`getListingDims: n=${n}`);
    pxPerChar = n ? h/n : 10; // update this global variable, used for scrolling
    com.mode.devlog (`getListingDims: pxPerChar = ${pxPerChar}`);
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

// export let ioLogBuffer = ""; now in es
// export const procAsmListingElt = document.getElementById('ProcAsmListing');
// export let procAsmListingElt; // global variables for emulator

//-----------------------------------------------------------------------------
// Assembly listing
//-----------------------------------------------------------------------------

// Global variables for handling listing display as program runs.

let srcLine;        // copy of source statements

// Keep track of the address of the currently executing instruction,
// the address of the next instruction, and the line numbers where
// these instructions appear in the assembly listing.  -1 indicates no
// line has been highlighted

// let curInstrAddr, curInstrLineNo, saveCurSrcLine;
// let nextInstrAddr, nextInstrLineNo, saveNextSrcLine;

export function initializeSubsystems () {
//    memDisplayModeFull = false;
//     document.getElementById('PP_Toggle_Display').value = "Fast display";  
}

export function toggleFullDisplay () {
    com.mode.devlog ('toggleFullDisplay clicked');
    memDisplayModeFull = ! memDisplayModeFull;
    document.getElementById('FullDisplayToggleButton').value =
	memDisplayModeFull ? "Full display" : "Fast display";
    if (memDisplayModeFull) { memDisplayFull () }
    else { memDisplayFast ()
	 }  // loses info but makes tab switching faster
}

//-------------------------------------------------------------------------------
// Booter
//-------------------------------------------------------------------------------

// Find the executable; it may come from assembler (object code) or
// linker (executable code).

export function obtainExecutable () {
    let m = st.env.getSelectedModule();
    let exe = m.executable ? m.executable : m.objMd;
    if (exe) {
        com.mode.devlog (`Found executable for selected module`);
        return exe;
    } else {
        com.mode.devlog (`Cannot find executable`);
        return null;
    }
}

export function procBoot (gst) {
    const es = gst.es
    com.mode.devlog ("boot");
    com.mode.devlog (`current emulator mode = ${es.mode}`)
    ab.resetSCB (es)
    let m = st.env.getSelectedModule ();
    let exe = obtainExecutable ();
    const objectCodeText = exe.objText;
    const metadataText   = exe.mdText;
    es.copyable = em.initEsCopyable

    initializeProcessorElements (gst) // ????
    gst.metadata = new st.Metadata ();
    gst.metadata.fromText (metadataText);

    let objectCode = objectCodeText.split("\n");
    let xs = "";
    let fields = null;
    let isExecutable = true; // will set to false if module isn't bootable
    let location = 0; // address where next word will be stored
    document.getElementById('ProcAsmListing').innerHTML = "";
    ab.clearInstrCount
    es.ioLogBuffer = "";
    em.refreshIOlogBuffer (es);
    // reset
    ab.resetSCB (es)
    em.resetRegisters (es);
    em.memClear(es)
    em.clearMemLogging (es)
    em.clearRegLogging (es)
//    em.procReset (es)
//    em.clearLoggingData (es)
//    memUpdate (gst)
    clearClock (gst)
    gst.highBootAddress = 0

    for (let i = 0; i < objectCode.length; i++) {
        xs = objectCode[i];
        com.mode.devlog (`boot: objectCode line ${i} = <${xs}>`);
        fields = link.parseObjLine (xs);
        com.mode.devlog (`boot op=<${fields.operation}> args=<${fields.operands}>`);
        if (fields.operation == "module") {
            let modname = fields.operands[0];
            let safemodname = modname ? modname : "(anonymous)";
            com.mode.devlog (`boot: module ${safemodname}`);
        } else if (fields.operation == "org") {
            com.mode.devlog ("--- skipping org");
        } else if (fields.operation == "data") {
            com.mode.devlog ("boot: data");
            for (let j = 0; j < fields.operands.length; j++) {
                let val = arith.hex4ToWord(fields.operands[j]);
                if (!val) {com.mode.devlog(`boot: bad data (${val})`)};
                let safeval = val ? val : 0;
                em.memStore (es, location, safeval);
                gst.highBootAddress = Math.max (gst.highBootAddress, location)
                com.mode.devlog (`boot data mem[${location}]:=${val}`);
                location++;
            }
        } else if (fields.operation == "import") {
            com.mode.devlog (`boot: import (${fields.operands})`)
            isExecutable = false;
        } else if (fields.operation == "export") {
        } else if (fields.operation == "relocate") {
        } else if (fields.operation == "") {
            com.mode.devlog ("boot: skipping blank object code line");
        } else {
            com.mode.devlog (`boot: bad operation (${fields.operation})`)
            isExecutable = false;
        }
    }
    if (isExecutable) {
        com.mode.devlog ("boot ok so far, preparing...");
        newUpdateRegisters (gst)
//        memUpdate (gst)
//        memDisplayFull(gst);
        memDisplay (gst)
        //        em.clearLoggingData (gst.es)
        em.clearRegLogging (gst.es)
        em.clearMemLogging (gst.es)
        gst.asmListingCurrent = []
        gst.metadata.listingDec.forEach ((x,i) => gst.asmListingCurrent[i] = x);
        initListing (gst);
        es.curInstrAddr = 0;
        es.curInstrLineNo = -1;  // -1 indicates no line has been highlighted
        es.nextInstrAddr = 0;
        es.nextInstrLineNo = gst.metadata.getSrcIdx (es.nextInstrAddr)
            + listingLineInitialOffset;
            highlightListingLine (gst, gst.nextInstrLineNo, "NEXT");
        getListingDims(gst);
        es.pc.put (0) // shouldn't be needed?
        //        refreshRegisters (gst)
        //        updateRegisters (gst)
        let xs =  "<pre class='HighlightedTextAsHtml'>"
            + "<span class='ExecutableStatus'>"
            + "Boot was successful"
            + "</span><br>"
            + "</pre>";
        console.log (`booting: SCB status = ${ab.showSCBstatus(es)}`)
        ab.writeSCB (es, ab.SCB_status, ab.SCB_ready)
        console.log (`booted: SCB status = ${ab.showSCBstatus(es)}`)
        com.mode.devlog ("boot was successful")
    } else {
        ab.writeSCB (es, ab.SCB_status, ab.SCB_reset)
        let xs =  "<pre class='HighlightedTextAsHtml'>"
            + "<span class='ExecutableStatus'>"
            + "Boot failed: module is not executable"
            + "</span><br>"
            + "</pre>";
        document.getElementById('LinkerText').innerHTML = xs;
        com.mode.devlog ("boot failed");
        alert ("boot failed");
    }
    if (es.thread_host === com.ES_gui_thread) {
        document.getElementById("procStatus").innerHTML = ab.showSCBstatus (es)
    }
    com.mode.devlog ("boot returning");
}

//------------------------------------------------------------------------------
// Emulator control from the gui
//------------------------------------------------------------------------------

function procInterrupt (gst) {
    console.log ('*** procInterrupt')
}

function timerInterrupt (gst) {
    com.mode.devlog ("Timer Interrupt clicked")
    //    arith.setBitInRegBE (gst.es.req, arch.timerBit)
    arch.setBitInRegLE (gst.es.req, arch.timerBit)
    gst.es.req.refresh()
}

// The Pause button stops the instruction looper and displays the state.

// export function procPause(es) {
export function procPause (gst) {
    com.mode.devlog ("procPause");
    com.mode.devlog (`procPause st=${ab.readSCB (gst.es,ab.SCB_status)}`)
    com.mode.devlog (`procPause preq=${ab.readSCB (gst.es,ab.SCB_pause_request)}`)
    ab.writeSCB (gst.es, ab.SCB_pause_request, 1)
    com.mode.devlog (`procPause preq=${ab.readSCB (gst.es,ab.SCB_pause_request)}`)
    com.mode.devlog ("em wrote procPause request")
}

//------------------------------------------------------------------------------
// Elapsed time clock
//------------------------------------------------------------------------------

const ClockWidth = 7 // number of characters to display

// Clear the display of the clock
export function clearClock (gst) {
    document.getElementById("PP_time").innerHTML = "0 sec"
}

// Note the current starting time and start the interval timer
export function startClock (gst) {
//    console.log ("startClock")
    clearClock (gst)
    const now = new Date ()
    gst.startTime = now.getTime ()
    gst.eventTimer = setInterval (duringRunRefresher (gst), gst.guiRefreshInterval)
}

export function stopClock (gst) {
//    console.log ("stopClock")
    clearInterval (gst.eventTimer)
    gst.eventTimer = null
    com.mode.devlog ("stopClock")
    updateClock (gst)
}

export function updateClock (gst) {
    const now = new Date ()
    const elapsed = now.getTime () - gst.startTime
    const xs = elapsed < 1000
          ? `${elapsed.toFixed(0)} ms`
          : ` ${(elapsed/1000).toPrecision(ClockWidth)} s`
    document.getElementById("PP_time").innerHTML = xs
}

// To keep the display alive during a long run, call the
// duringRunRefresher from time to time.  It can be triggered either by
// the interval timer or by a trap.

const duringRunRefresher = (gst) => () => {
    updateClock (gst)
    refreshRFdisplay (gst)
    guiDisplayNinstr (gst)
}

// Show the current values of the register file without highlighting

export function refreshRFdisplay (gst) {
    const es = gst.es
    for (let i = 0; i < es.nRegisters; i++) {
        let j = ab.EmRegBlockOffset + i
        let x = i === 0 ? 0 : es.shm[j]
        let e = es.register[i].elt
        e.innerHTML = arith.wordToHex4 (x)
    }
}

export function test1 (es) {
    updateClock (es)
    refreshRFdiysplay (es)
}

//------------------------------------------------------------------------------
// Running the emulator
//------------------------------------------------------------------------------

// Main interface function to step one instruction; runs in main gui
// thread

export function procStep (gst) {
    const es = gst.es
    com.mode.devlog ("procStep")
    if (gst.es.thread_host != com.ES_gui_thread) {
        com.mode.devlog (`procStep: host=${gst.es.thread_host}, skipping`)
        return
    }
    ab.writeSCB (es, ab.SCB_pause_request, 0)
    let q = ab.readSCB (es, ab.SCB_status)
    console.log (`procStep SCB status = ${q} ${ab.showSCBstatus(es)}`)
    switch (q) {
    case ab.SCB_ready:
    case ab.SCB_paused:
    case ab.SCB_break:
    case ab.SCB_relinquish:
        com.mode.devlog ("procStep: main thread executing instruction...")
        em.clearMemLogging (gst.es)
        em.clearRegLogging (gst.es)
        em.executeInstruction (es)
        let qnew = ab.readSCB (es, ab.SCB_status)
        if (qnew != ab.SCB_halted) ab.writeSCB (es, ab.SCB_status, ab.SCB_ready)
        execInstrPostDisplay (gst)
//        updateRegisters (gst)
        break
    case ab.SCB_reset:
    case ab.SCB_running_gui:
    case ab.SCB_running_emwt:
    case ab.SCB_halted:
    case ab.SCB_blocked:
        com.mode.devlog ("procStep skipping instruction...")
        break
    default: com.mode.devlog (`error: procStep unknown SCB_status= ${q}`)
    }
    com.mode.devlog ("procStep finished")
}

// The runMain and runWorker functions set the preferred thread and
// then call procRun, so they set the thread choice persistently.
// runGeneric checks the emRunThread field in the gui state to decide
// which to use.

function runMain () {
    gst.options.currentThreadSelection = com.ES_gui_thread
    procRun ()
}

function runWorker () {
    gst.es.emRunThread = com.ES_worker_thread
    procRun ()
}

function runGeneric () {
    procRun ()
}


// Perform any operations on the gui display to prepare for a run

function initRun (gst) {
    startClock (gst)
}

function finishRun (gst) {
    stopClock (gst)
    execInstrPostDisplay (gst)
}

// Run instructions until stopping condition is reached.  This will be
// performed using either the main gui thread or on the worker thread.
// First decide whether to go aheead with the run; if so, decide which
// thread to run it in.

function procRun () {
    const es = gst.es
    gst.es.emRunThread = gst.options.currentThreadSelection
    const q = ab.readSCB (es, ab.SCB_status)
//    console.log (`procRun, status=${q} thread=${es.emRunThread}`)
    es.copyable = em.initEsCopyable // does this clear data from main?????
    switch (q) {
    case ab.SCB_ready:
    case ab.SCB_paused:
    case ab.SCB_blocked:
    case ab.SCB_break:
        switch (gst.options.currentThreadSelection) {
        case com.ES_gui_thread:
            console.log ("procRun starting in main gui thread")
            ab.writeSCB (es, ab.SCB_status, ab.SCB_running_gui)
            
            es.initRunDisplay (es)
            //            em.mainThreadLooper (es)
//            com.mode.trace = false
            em.mainRun (es)
            break
        case com.ES_worker_thread:
            console.log ("procRun starting in worker thread")
            ab.writeSCB (es, ab.SCB_status, ab.SCB_running_emwt)
            es.initRunDisplay (es)
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

//---------------------------------------------------------------------------
// Breakpoint
//---------------------------------------------------------------------------

// Sigma16 used to have a rich facility for setting breakpoints, and
// this will be reinstated (probably summer 2021).  (The flexible
// breakpoints were omitted during the conversion of Sigma16 from
// Haskell to JavaScript.)

// Meanhile see the trap breakpoint; for simple testing and debugging
// this will be the must useful form.

// For the time being, there is a limited external breakpoint facility
// described below.

// Temporary: enter a hex constant e.g. $02c9 into the text area and
// click Refresh.  The emulator will break when the pc reaches this
// value.  Spaces before the constant are not allowed, and the $ is
// required.  Later this will be replaced by a richer language for
// specifying the break condition.

export let breakDialogueVisible = false;

export function procBreakpoint (gst) {
    com.mode.devlog ("procBreakpoint");
    com.mode.devlog ("procBreakpoint");
    document.getElementById("BreakDialogue").style.display
	= breakDialogueVisible ? "none" : "block";
    breakDialogueVisible = !breakDialogueVisible;
}

export function hideBreakDialogue () {
    document.getElementById("BreakDialogue").style.display = "none";
    breakDialogueVisible = false;
}

function breakRefresh (gst) {
    com.mode.devlog ("breakRefresh");
    let x = document.getElementById('BreakTextArea').value;
    if (x.search(asm.hexParser) == 0) {
	let w = arith.hex4ToWord (x.slice(1));
	gst.es.copyable.breakPCvalue = w;
//	com.mode.devlog (`breakPCvalue = + ${w}`);
	com.mode.devlog (`breakPCvalue = ${w}`);
    } else {
//	com.mode.devlog (`breakRefresh cannot parse + x`);
	com.mode.devlog (`breakRefresh cannot parse + x`);
    }
}

// function breakEnable (es) {
function breakEnable (gst) {
    com.mode.devlog ("breakEnable");
    com.mode.devlog ("breakEnable");
    gst.es.copyable.breakEnabled = true;
    com.mode.devlog (`breakEnable ${gst.es.breakPCvalue}`);
}

// function breakDisable (es) {
function breakDisable (gst) {
    com.mode.devlog ("breakDisable");
    com.mode.devlog ("breakDisable");
    gst.es.copyable.breakEnabled = false;
}
//    gst.es.breakEnabled = false;

function breakClose (gst) {
    com.mode.devlog ("breakClose");
    com.mode.devlog ("breakClose");
    hideBreakDialogue ();
}

//-----------------------------------------------------------------------------
// Emulator thread
//-----------------------------------------------------------------------------

function logShmStatus (es) {
    let status = ab.showSCBstatus (es)
    let n = ab.readSCB (es, ab.SCB_nInstrExecuted)
    let cur = ab.readSCB (es, ab.SCB_cur_instr_addr)
    let next = ab.readSCB (es, ab.SCB_next_instr_addr)
    let mode = ab.readSCB (es, ab.SCB_emwt_run_mode)
    let trap =  ab.readSCB (es, ab.SCB_emwt_trap)
    let pause = ab.readSCB (es, ab.SCB_pause_request)
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

//-----------------------------------------------------------------------------
// Emulator state
//-----------------------------------------------------------------------------


function mkMainEmulatorState () {
    console.log ("mkMainEmulatorState")
    gst.es = new em.EmulatorState (
        com.ES_gui_thread,
        () => initRun (gst),
        () => updateWhileRunning (gst),
        () => finishRun (gst) )
}

//-----------------------------------------------------------------------------
// System state vector
//-----------------------------------------------------------------------------

// Memory is allocated in the main thread and made available to the
// emulator in the main thread by saving it in gst.es.vecbuf.  If
// there is a worker thread, vecbuf must be a shared array buffer, and
// it's also sent to the worker.

// to do: change es.shm to es.vec16 in emulator

function allocateStateVector () {
    const es = gst.es
    es.vecbuf = null
    console.log (`allocateStateVector:`
                 + ` bufferType = ${gst.options.bufferType.description}`)
    if (gst.options.bufferType === ArrayBufferShared) {
        console.log ("Allocating shared array buffer")
        es.vecbuf = new SharedArrayBuffer (ab.StateVecSizeBytes)
    } else if (gst.options.bufferType === ArrayBufferWebAssembly) {
        console.log ('Allocating web assembly memory')
        const initialSize = 3 // should calculate from StateVecSizeBytes
        const maxSize = 40    // calculate from options
        emcImports.imports.wam = new WebAssembly.Memory (
            { initial: initialSize, maximum: maxSize, shared: true })
        es.vecbuf = emcImports.imports.wam.buffer
        initEmCore ()
    } else { // Local
        console.log ('Allocating unshared array buffer')
        console.log (`size = ${ab.StateVecSizeBytes}`)
        es.vecbuf = new ArrayBuffer (ab.StateVecSizeBytes)
    }
    gst.options.memoryIsAllocated = true
    // Define word views into the state vector
    es.vec16 = new Uint16Array (es.vecbuf)
    es.vec32 = new Uint32Array (es.vecbuf)
 //   es.vec64 = new Uint64Array (gst.es.vecbuf) Uint64 array doesn't exist, use num
    es.shm = gst.es.vec16  // change usages of es.shm to es.vec16
    es.emRunThread = gst.options.currentThreadSelection
    initializeMainEmulator ()   // Create emulator state
    setArch16 ()
    memDisplay (gst)
    procReset (gst)
//    ab.testSysStateVec (es)  // keep this: important testing tool
}

//-----------------------------------------------------------------------------
// EMWT communications protocol
//-----------------------------------------------------------------------------

// The main gui thread and emulator thread communicate through two
// mechanisms: message passing and shared memory.  A consistent
// protocol is used for the message passing.

// The emulator state object cannot be shared between threads; only
// the shared array buffers are accessible to both.  The main thread
// creates its own emulator state, and populates it with the shared
// state array.  A reference to the shared array is sent to the worker
// thread on initialization.

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
    //    com.mode.devlog ("main gui: emwtInit")
    console.log ("main thread: emwtInit")
    //    let msg = {code: 100, payload: es.shm}
    //    let msg = {code: 100, payload: sysStateBuf}
    //    let msg = {code: 100, payload: es.vecbuf}
    //    let msg = {code: 100, payload: es.vecbuf}
    let msg = {code: 100, payload: es.vecbuf} // provide the es to emwt
    gst.emwThread.postMessage (msg)
    //    com.mode.devlog ("main gui: posted init message 100 to emwt")
    console.log ("main gui: posted init message 100 to emwt")
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
    gst.emwThread.postMessage (msg)
}

function handleEmwtStepResponse (p) {
    com.mode.devlog (`main: handle emwt step response ${p}`)
    com.mode.devlog (`main: handle emwt step response ${p}`)
    com.mode.devlog (`gui at emwt step response 1: ${em.showEsInfo (gst.es)}`)
    finishRun (gst)
    com.mode.devlog (`gui at emwt step response 2: ${em.showEsInfo (gst.es)}`)
    let newstatus = ab.readSCB (gst.es, ab.SCB_status)
    com.mode.devlog (`main handle emwt step response: status=${newstatus}`)
    if (newstatus === ab.SCB_relinquish) {
        com.mode.devlog (`***** main gui: handle worker step relinquish`)
    }
}

//----------------------------------------
// emwt 102: run
//----------------------------------------

// Initiate a run using the worker thread.  This function is the main
// gui's interface to the worker, so es should be gst.es.
// It will run instructions until a stopping condition, but will
// relinquish control to the main thread on a trap.

function emwtRun (es) {
    console.log ("main thread initiating emwt run");
    let instrLimit = 0 // disabled; stop after this many instructions
    em.showCopyable (es.copyable)
    let msg = {code: 102, payload: es.copyable}
    gst.emwThread.postMessage (msg)
    com.mode.devlog ("main thread posted start message run to emwt");
}

function handleEmwtRunResponse (p) { // run when emwt sends 202
    console.log (`handleEmwtRunResponse`)
    let status = ab.readSCB (gst.es, ab.SCB_status)
    gst.es.copyable = p
    em.showCopyable (gst.es.copyable)
    let  msg = {code: 0, payload: 0}
    com.mode.devlog (`main: handle emwt run response: p=${p} status=${status}`)
    switch (status) {
    case ab.SCB_halted:
        com.mode.devlog (`*** main: handle emwt halt`)
        finishRun (gst)
        break
    case ab.SCB_paused:
        com.mode.devlog (`*** main: handle emwt pause`)
        ab.showSCBstatus (gst.es)
        ab.writeSCB (gst.es, ab.SCB_pause_request, 0)
        ab.writeSCB (gst.es, ab.SCB_status, ab.SCB_ready)
        finishRun (gst)
        com.mode.devlog (`*** main: finished handle emwt pause`)
        break
    case ab.SCB_break:
        com.mode.devlog (`*** main: handle emwt break`)
        console.log (`*** main: handle emwt break`)
        ab.writeSCB (gst.es, ab.SCB_status, ab.SCB_ready)
        finishRun (gst)
        com.mode.devlog (`*** main: finished handle emwt break`)
        break
    case ab.SCB_blocked:
        com.mode.devlog (`*** main: handle emwt blocked`)
        break
    case ab.SCB_relinquish: // emwt halt signals halt, not relinquish
        com.mode.devlog (`*** main: handle emwt relinquish`)
        ab.showSCBstatus (gst.es)
        ab.writeSCB (gst.es, ab.SCB_status, ab.SCB_running_gui)
        em.executeInstruction (gst.es)
        ab.decrInstrCount (gst.es) // instruction was counted twice
        if (ab.readSCB (gst.es, ab.SCB_status) === ab.SCB_halted) {
            console.log ("main: handle emwt relinquish: halted")
            finishRun (gst)
        } else if (ab.readSCB (gst.es, ab.SCB_status) === ab.SCB_break) {
            console.log ("main: handle emwt relinquish: trap break")
            finishRun (gst)
        } else {
            console.log (`main: handle emwt relinquish: resuming`)
            ab.writeSCB (gst.es, ab.SCB_status, ab.SCB_running_emwt)
//            msg = {code: 102, payload: 0}
            msg = {code: 102, payload: gst.es.copyable}
            gst.emwThread.postMessage (msg)
        }
        com.mode.devlog (`*** main: finished handle emwt relinquish`)
        break
    case ab.SCB_reset:
    case ab.SCB_ready:
    case ab.SCB_running_gui:
    case ab.SCB_running_emwt:
    default:
        com.mode.devlog (`main:handleEmwtRunResponse unknown status = ${status}`)
    }
    com.mode.devlog ("main: handleEmwtRunResponse finished")
}

//----------------------------------------
// emwt 103: print state on console
//----------------------------------------

function emwtShow () {
    com.mode.devlog ("main: emwtShowRegs")
    let msg = {code: 103, payload: 0}
    gst.emwThread.postMessage (msg)
}

function handleEmwtShowResponse (p) {
    com.mode.devlog (`main: handle emwt show response ${p}`)
}

//----------------------------------------
// emwt 104: emwt test 1
//----------------------------------------

function emwtTest1 () {
    console.log ("main: emwt test 1")
    console.log (logShmStatus (gst.es))
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
    gst.emwThread.postMessage (msg)
}

function handleEmwtTest2Response (p) { // 
    console.log (`main: handle emwt test 2 response ${p}`)
}

//----------------------------------------
// Handle responses from emwt
//----------------------------------------

function initializeEmwtProtocol (es) {
    console.log ("main initializeEmwtProtocol")
    gst.emwThread.addEventListener ("message", e => {
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

// These are experimental tools that are normally disabled.  They
// aren't documented, aren't intended for users, and aren't stable.
// To use them, press F12 to display the browser console, then enter
// setModeDev() which will make the tools' buttons visible; to
// hide them again enter setModeUser().

// DevElts is list of ids of elements to be shown or hidden.  To
// ensure they are hidden by default, they should have class="Hidden"
// specified in their html element: the css for .Hidden specifies
// visibility: "hidden".

const DevElts = [ DevTools_Pane_Button,
                 Arch16button, Arch32button]
//  ,  "PP_RunGui",  "PP_Test1", "PP_Test2"]

// Define functions that show/hide the DevElts and attach the
// functions to the window to put them into scope at the top level

function setModeUser () {
    console.log ('setModeUser')
    gst.guiMode = 'User'
    DevElts.map (hideElement)
}
function setModeDev ()  {
    console.log ('setModeDev')
    gst.guiMode = 'Dev'
    DevElts.map (showElement)
}
window.setModeDev = setModeDev;
window.setModeUser = setModeUser;

//function showElement (eltName) {
function showElement (elt) {
//    document.getElementById(eltName).style.visibility = "visible";
    elt.style.visibility = "visible";
}

//function hideElement (eltName) {
function hideElement (elt) {
    //    document.getElementById(eltName).style.visibility = "hidden";
    elt.style.visibility = "hidden";
}

function devTools100 () {
    console.log ("DevTools100 clicked");
    testEmCore ()
    
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
// Test pane
//------------------------------------------------------------------------------

// From emulator.js

// In the mem display, the formatting is ok when the container
// specifies the style class.  However, when <pre> ... </pre> are
// added around the text, the font and size are wrong and the
// specified style is ignored.  Perhaps <pre> has an inappropriate
// default style that overrides the existing font.  Solution is to use
// <pre class="HighlightedTextAsHtml"> but don't put it inside a div
// with HighlightedTExtAsHtml

function testpane1() {
    com.mode.devlog ('testpane 1 clicked')
    let xs = ["<pre class='HighlightedTextAsHtml'>", 'line 1 text',
	      "<span class='CUR'>this is line 2 text</span>",
	      'and finally line 3', '</pre>'];
    com.mode.devlog ('xs = ' + xs);
    let ys = xs.join('\n');
    com.mode.devlog ('ys = ' + ys);

    let qs = ys;
    com.mode.devlog ('qs = ' + qs);
    document.getElementById('TestPaneBody').innerHTML = qs;
}

function testpane2 () {
    com.mode.devlog ('testpane 2 clicked');
}

function testpane3 () {
    com.mode.devlog ('testpane 3 clicked');
}

//-----------------------------------------------------------------------------
// Initialization
//-----------------------------------------------------------------------------

function initializeMainEmulator () {
    em.initializeMachineState (gst.es)
    initializeSubsystems ()
    clearClock (gst)
    gst.es.emRunThread = com.ES_gui_thread // default run mode
    em.procReset (gst.es)
}

function initializeGuiLayout (gst) {
    gst.showingUserGuide = true
    initialize_mid_main_resizing (gst)
    setMidMainLRratio(0.65);  // useful for dev to keep mem display visible
    gst.toggleGuideSaveRatio = gst.midLRratio
    adjustToMidMainLRratio();
    initializePane (gst);
}

function initializeGuiElements (gst) {
    hideBreakDialogue ();
//    document.getElementById('LinkerText').innerHTML = "";    
    document.getElementById('LP_Body').innerHTML = "";    
    smod.prepareChooseFiles ();
    smod.initModules (gst);
    window.mode = com.mode;
    prepareExampleText (gst)
}


function initializeTracing (gst) {
    com.mode.devlog (`Thread ${gst.es.mode} initialization complete`)
}

function initializeSystem () {
    com.mode.devlog ('Initializing system')
    gst = new GuiState ()          // Create gui state and set global variable
    adjustInitialOptions ()
    initializeGuiElements (gst)    // Initialize gui elements
    initializeGuiLayout (gst)      // Initialize gui layout
    initializeButtons (gst)
    refreshOptionsDisplay ()
    findLatestRelease (gst)
}

//-----------------------------------------------------------------------------
// Instruction set architecture selection
//-----------------------------------------------------------------------------

function setArch16 () {
    console.log ('Setting mode to S16')
    const es = gst.es
    es.addressMask = arith.word16mask
//    console.log (`addressMask (S16) = ${es.addressMask}`)
    setRegisterSize (16)
        highlightArchButton('Arch16button')
        unhighlightArchButton('Arch32button')
    document.documentElement.style
        .setProperty ('--RegValWidth', 'var(--RegValWidth16)')
    procReset (gst)
}

function setArch32 () {
    console.log ('Setting mode to S32')
    const es = gst.es
    es.addressMask = arith.word32mask
    console.log (`addressMask (S32) = ${es.addressMask}`)
    setRegisterSize (32)
        highlightArchButton('Arch32button')
        unhighlightArchButton('Arch16button')
    gst.es.pc.show = arith.wordToHex8
    document.documentElement.style
        .setProperty ('--RegValWidth', 'var(--RegValWidth32)')
    procReset (gst)
}

function setRegisterSize (size) {
    const f = size===16 ? arith.wordToHex4 : arith.wordToHex8
    for (let i = 0; i < gst.es.nRegisters; i++) {
        gst.es.register[i].show = f
    }
}

function highlightArchButton (a) {
    const s = document.getElementById(a).style
    s.background = 'aquamarine'
    s.border = '2px solid red'
}
function unhighlightArchButton (a) {
    const s = document.getElementById(a).style
    s.background = 'beige'
    s.border = '2px solid gray'
}

//-----------------------------------------------------------------------------
// Functions called by EmCore
//-----------------------------------------------------------------------------

function printnum (x) {
    console.log (`printnum: ${x}`)
}

function fooprint (x) {
    console.log (`fooprint: ${x}`)
}

function barprint (x) {
    console.log (`barprint: ${x}`)
}

//-----------------------------------------------------------------------------
// EmCore
//-----------------------------------------------------------------------------

// Most of the source modules can be loaded by the main html file,
// using ./ to find the file location.  However, the actual URL of the
// wasm file is needed, and Sigma16.html doesn't know its own URL.
// The following definitions work out EmCoreURL which is used by
// initEmCore to fetch the web assembly binary code.  This allows both
// for reading a local file from build/dev for running locally, as
// well as fetching a release over the Internet.

const URL_protocol = window.location.protocol
const URL_host = window.location.host
// If running on local build machine, use dev; otherwise use version number
const BUILDVERSION = URL_host === 'localhost:3000' ? 'dev' : ver.s16version
const EmCorePath = `build/${BUILDVERSION}/Sigma16/emcore.wasm`
const EmCoreURL = `${URL_protocol}//${URL_host}/${EmCorePath}`

function showEmcURL () {
    console.log (`URL_protocol = ${URL_protocol}`)
    console.log (`URL_host = ${URL_host}`)
    console.log (`BUILDVERSION = ${BUILDVERSION}`)
    console.log (`EmCorePath = ${EmCorePath}`)
    console.log (`EmCoreURL = ${EmCoreURL}`)
}

// Configure the web assembly memory

// const wam = new WebAssembly.Memory ({
//     initial: 3,
//     maximum: 40,
//     shared: true
// })

// Imports into Wasm from JS

const emcImports = {
    imports: { wam: null, // set by allocateStateVector
               printnum, fooprint, barprint }
    }

// Exports from Wasm to JS

// emc is an object containing functions exported by emulator core.
// The functions are defined by initEmCore; after that you can call,
// for example, emc.f1 (123).  The functions f1, f1 etc are for
// testing an ddevelopment.  Make emc accessible to the console for
// testing the web assembly functions.  E.g. enter emc.f1 (3)

const emc = {
    addplus1 : null,
    print42 : null,
    store16 : null,
    readReg16 : null,
    readReg32 : null,
    writeReg16 : null,
    writeReg32 : null,
    readMem16 : null,
    readMem32 : null,
    writeMem16 : null,
    writeMem32 : null,
}
window.emc = emc

// Initialize the core emulator.  Read the web assembly code and make
// its functions accessible to the main JavaScript program.
// WebAssembly memory must be initialized by calling
// allocateStateVector before calling initEmCore.

function initEmCore () {
    console.log ('initEmCore')
    showEmcURL ()
    WebAssembly.instantiateStreaming (fetch (EmCoreURL), emcImports)
        .then (emcExports => {
            emc.addplus1 = emcExports.instance.exports.addplus1
            emc.print42  = emcExports.instance.exports.print42
            emc.store16  = emcExports.instance.exports.store16
        })
}

function showVec16 (a,b) {
    console.log (`showVec16 a=${a} b=${b}`)
    for (let i = a; i<b; i++) {
        console.log (`  i = ${i} vec16[${i}] = ${gst.es.vec16[i]}`)
    }

}

function test_t1 () {
    console.log ('processor test t1')
    //    testEmCore ()
    console.log ('start testSysStateVec...')
    ab.testSysStateVec (gst.es)
    console.log ('...testSysStateVec finished')
}
//    ab.testSysStateVec (gst.es)  // testing
//    ab.testSysStateVec (gst.es)  // testing
//    console.log ('stopping after testSysStateVec') // testing only  ?????
//    return // testing only - skip the following ?????

function test_t2 () {
    console.log ('processor test t2')
}

function testEmCore () {
    console.log ('testEmCore')
    console.log ('call: emc.print42 ()')
    emc.print42 ()
    console.log ('let a = emc.addplus1 (12, 35)')
    let a = emc.addplus1 (12, 35)
    console.log (`a = ${a}`)
    for (let i = 0; i<8; i++) { gst.es.vec16[i] = 0 }
    console.log ('Initial memory')
    showVec16 (0, 8)
    gst.es.vec16[2] = 3
    showVec16 (0, 8)
    console.log ('calling store16 (6,42)')
    emc.store16 (6, 42)
    console.log ('After store32 (6,42)')
    showVec16 (0, 8)
    console.log (`gst.es.vec16[3] = ${gst.es.vec16[3]}`)
    console.log ('index=3 in js, index=6 in wam')
    console.log ('testEmCore returning')
}
window.testEmCore = testEmCore

//-------------------------------------------------------------------------------
// Testing
//-------------------------------------------------------------------------------

function runtests () {
    console.log ("runtests starting")
    let x
    let i
    x = 1
    i = 0
    console.log (`x=${x} i=${i} b=${arch.getBitInWordLE(x,i)}`)
    i = 1
    console.log (`x=${x} i=${i} b=${arch.getBitInWordLE(x,i)}`)
    i = 2
    console.log (`x=${x} i=${i} b=${arch.getBitInWordLE(x,i)}`)

    x = 2
    i = 0
    console.log (`x=${x} i=${i} b=${arch.getBitInWordLE(x,i)}`)
    i = 1
    console.log (`x=${x} i=${i} b=${arch.getBitInWordLE(x,i)}`)
    i = 2
    console.log (`x=${x} i=${i} b=${arch.getBitInWordLE(x,i)}`)

    x = 4
    i = 0
    console.log (`x=${x} i=${i} b=${arch.getBitInWordLE(x,i)}`)
    i = 1
    console.log (`x=${x} i=${i} b=${arch.getBitInWordLE(x,i)}`)
    i = 2
    console.log (`x=${x} i=${i} b=${arch.getBitInWordLE(x,i)}`)

    console.log ("runtests finished")
}

window.runtests = runtests

function foo (x,y) {
    console.log (`foo ${x} ${arith.wordToHex4(y)}`)
}
function showCChex () {
    foo ('ccg', arch.ccg)
    foo ('ccG', arch.ccG)
    foo ('ccE', arch.ccE)
    foo ('ccL', arch.ccL)
    foo ('ccl', arch.ccl)
    foo ('ccv', arch.ccv)
    foo ('ccV', arch.ccV)
    foo ('ccC', arch.ccC)
    foo ('ccS', arch.ccS)
    foo ('ccs', arch.ccs)
}

//-----------------------------------------------------------------------------
// Run initializers after program has been loaded
//-----------------------------------------------------------------------------

// The initializers require the DOM elements to exist and the
// functions to be defined, so they are performed after all the
// modules have been loaded.

window.onload = function () {
    console.log("starting initializers")
    com.mode.trace = false
    initializeSystem ()
    enableKeyboardShortcuts ()
    setModeUser ()
    console.log ('system is now running')
}
