// Sigma16.  Copyright (c) 2019 John O'Donnell

//---------------------------------------------------------------------------
// Global variables
//---------------------------------------------------------------------------

// Define the global variables; some are set by function initialize
// which is executed when window.onload occurs

var globalObject = this; // to enable script in userguide to define glob var
var myglobalvar = 9876; // can script in userguide see this?

var fileContents = "file not read yet"
var editorBufferTextArea; /* set when window.onload */
const fileReader = new FileReader();
var textFile = null; /* for save download */
var create;  /* for save download */
var textbox; /* for save download */

// global variables for emulator
var procAsmListingElt;

//---------------------------------------------------------------------------
// Experiments and testing
//---------------------------------------------------------------------------


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

// try to jump to a position in user guide
// doesn't work because of security restriction
// try springen('control-registers')
// conclusion: really cannot do this

function springen(anker) { 
    var childWindow =  document.getElementById("UserGuideIframeId").contentWindow;
    childWindow.scrollTo(0,childWindow.document.getElementById(anker).offsetTop);
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


//---------------------------------------------------------------------------
// Examples pane
//---------------------------------------------------------------------------

// Errors in displaying the directories: GET 404:
// www.dcs.gla.ac.uk/icons/blank.gif
// www.dcs.gla.ac.uk/:9uk/icons/text.gif

function examplesHome() {
    console.log ("examplesHome");
    document.getElementById("ExamplesIframeId").src = "./programs/Examples/";
}

function copyExampleText() {
    console.log ('copyExampleText');
    let exElt = document.getElementById('ExamplesIframeId');
    document.getElementById('EditorTextArea').value
	= exElt.contentWindow.document.body.innerHTML;
}

//    let exElt = document.getElementById('ExamplesIframeId');
//    fileContents = exElt.contentWindow.document.body.innerHTML;
//    console.log(fileContents);
//    document.getElementById('EditorTextArea').value = fileContents;

/*

function copyExampleSelect () {  // deprecated, remove this...
    console.log ('copyExampleSelect');
    let exElt = document.getElementById('ExamplesIframeId');
    let exText = exElt.contentWindow.document.body;
    exText.select();
    
}

function fileButton5 () {
    console.log ('fileButton5');
}

function selectExample () {
    console.log ('selectExample');
    let elt = document.getElementById('ExamplesIframeId');
    elt.focus ();
    elt.select ();
}
*/

// from stackoverflow...
// var myIFrame = document.getElementById("myIframe");
// var content = myIFrame.contentWindow.document.body.innerHTML;




//---------------------------------------------------------------------------
// Window sizing: adjust relative size of system and user guide
//---------------------------------------------------------------------------

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



//---------------------------------------------------------------------------
// Top level tab buttons control which pane is visible and active
//---------------------------------------------------------------------------

let breakDialogueVisible = false;

function procBreakpoint () {
    console.log ("procBreakpoint");
    document.getElementById("BreakDialogue").style.display
	= breakDialogueVisible ? "none" : "block";
    breakDialogueVisible = !breakDialogueVisible;
}

function hideBreakDialogue () {
    document.getElementById("BreakDialogue").style.display = "none";
}

function hideTabbedPane(paneId) {
//    console.log("hiding tabbed pane " + paneId);
    document.getElementById(paneId).style.display = "none";
}

function hideAllTabbedPanes() {
    hideTabbedPane("WelcomePane");
    hideTabbedPane("ExamplesPane");
    hideTabbedPane("EditorPane");
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


//---------------------------------------------------------------------------
// Example programs
//---------------------------------------------------------------------------

function insert_example(exampleText) {
    console.log('Inserting example add into editor text');
    document.getElementById('EditorTextArea').value = exampleText;
};


// Default example program for testing
const example_default =  `; Program test

       add R4,R5,R6

       lea   R4,23[R0]
       lea   R9,5[R0]
       add   R8,R4,R9
       lea   R3,1[R0]
       add   R8,R3,R8

       lea   R2,15[R0]
       lea   R3,49[R0]
       cmplt R4,R2,R3
       cmpeq R5,R2,R3
       cmpgt R6,R2,R3

       lea   R2,15[R0]
       lea   R3,15[R0]
       cmplt R4,R2,R3
       cmpeq R5,R2,R3
       cmpgt R6,R2,R3


       lea   R2,15[R0]
       lea   R3,-3[R0]
       cmplt R4,R2,R3
       cmpeq R5,R2,R3
       cmpgt R6,R2,R3



       lea   R2,-39[R0]
       lea   R3,-53[R0]
       cmplt R4,R2,R3
       cmpeq R5,R2,R3
       cmpgt R6,R2,R3


       lea   R2,-53[R0]
       lea   R3,-39[R0]
       cmplt R4,R2,R3
       cmpeq R5,R2,R3
       cmpgt R6,R2,R3




       lea   R2,2[R0]
       add   R8,R2,R8

       lea   R3,1[R0]
       add   R8,R3,R8
       lea   R2,2[R0]
       add   R8,R2,R8


       lea   R3,1[R0]
       add   R8,R3,R8
       lea   R2,2[R0]
       add   R8,R2,R8

       lea   R3,1[R0]
       add   R8,R3,R8
       lea   R2,2[R0]
       add   R8,R2,R8

       lea   R3,1[R0]
       add   R8,R3,R8
       lea   R2,2[R0]
       add   R8,R2,R8

       lea   R3,1[R0]
       add   R8,R3,R8
       lea   R2,2[R0]
       add   R8,R2,R8

       lea   R3,1[R0]
       add   R8,R3,R8
       lea   R2,2[R0]
       add   R8,R2,R8

       lea   R3,1[R0]
       add   R8,R3,R8
       lea   R2,2[R0]
       add   R8,R2,R8

       lea   R3,1[R0]
       add   R8,R3,R8
       lea   R2,2[R0]
       add   R8,R2,R8

       lea   R3,1[R0]
       add   R8,R3,R8
       lea   R2,2[R0]
       add   R8,R2,R8
x      data   2
y      data   3
`;


// Simple example program: add
const example_add = `; Program Add
     lea    R4,23[R0]      ; R4 := 23
     lea    R9,5[R0]       ; R9 := 5
     add    R7,R4,R9       ; R7 := 23 + 5

; result := x + y
     load   R1,x[R0]       ; R1 := x
     load   R2,y[R0]       ; R2 := y
     add    R3,R1,R2       ; R3 := x + y
     store  R3,result[R0]  ; result := x + y

; Terminate execution
     trap   R0,R0,R0       ; system request to terminate

; Variables
x         data   2
y         data   3
result    data   0
`;

// Test program for assembly language error checking
const example_errors = `; Test Errors
     add    R1,R2,R3    ; RRR
     xor    R15,R14,R13
     cmplt  R1,R7,R13
     jumpf  R1,skip[R2]
     cmp    R4,R9       ; RR
     load   R6,xyz[R7]  ; RX
     lea    R7,24[R0]
start                 ; just a label
loop load   R9,12[R0]   ; good operands
     jump   loop[R10]   ; 
done trap   R11,R12,R13
x    data   35
y    data   $c39f
a    data   3
z    data   -1
     lea              ; good operation
     blarg  R3,R5,R9
     add    R1,R17,R0   ; bad operands
     load   x*y         ; bad operands
     load   R1,R2,R3    ; operation/operand mismatch
     add    R1,xy[R4]   ; operation/operand mismatch
*ab
`;




//---------------------------------------------------------------------------
//  Handle window events
//---------------------------------------------------------------------------

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

//---------------------------------------------------------------------------
// Complete initialization when onload occurs
//---------------------------------------------------------------------------

window.onload = function () {
    
    console.log("window.onload activated");
    showTabbedPane("WelcomePane");
    initializeProcessorElements ();  // so far, it's just instr decode
    clearInstrDecode (emulatorState);
    
// Initialize file/module

    fileReader.onload = function (e) {
    console.log("fileReader.onload activated");
    let xs = e.target.result;
    console.log(xs);
    editorBufferTextArea.value = xs;
}

    
// Initialize the modules
    initModules ();

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
//Control registers
    pc  = mkReg ('pc',  'pcElt',  wordToHex4);
    ir  = mkReg ('ir',  'irElt',  wordToHex4);
    adr = mkReg ('adr', 'adrElt', wordToHex4);
    dat = mkReg ('dat', 'datElt', wordToHex4);
    spc = mkReg ('spc', 'spcElt', wordToHex4);
    ien = mkReg ('ien', 'ienElt', intToBit);
    controlRegisters = [pc,ir,adr,dat,spc,ien];
    nRegisters = 16;  // now it's incremented in mkReg
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
    insert_example(example_add);     // For testing and debugging
    initialize_mid_main_resizing ();
    // setMidMainLRratio(0.65);  // 0.65 is reasonable value for normal use
    setMidMainLRratio(0.65);  // useful for dev to keep mem display visible
    showSizeParameters();
    adjustToMidMainLRratio();
    initializeSubsystems ();

    // Hide the dialogues and popups
    document.getElementById("BreakDialogue").style.display = "none";
    breakDialogueVisible = false;
    
    console.log("Initialization complete");
}

