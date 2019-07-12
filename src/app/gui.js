// Sigma16
// John O'Donnell, 2019

//---------------------------------------------------------------------------
// Global variables
//---------------------------------------------------------------------------

// Define the global variables; some are set by function initialize
// which is executed when window.onload occurs

var globalObject = this; // to enable script in userguide to define glob var
var myglobalvar = 9876; // can script in userguide see this?

var isHandlerDragging = false;
var handler;
var wrapper;
var boxA;
var fileContents = "file not read yet"
var editorBufferTextArea; /* set when window.onload */
const fileReader = new FileReader();
var textFile = null; /* for save download */
var create;  /* for save download */
var textbox; /* for save download */


//---------------------------------------------------------------------------
// Testing
//---------------------------------------------------------------------------

function displayHello() {
    var msg;
    msg = document.getElementById("message");
    msg.outerHTML = "<h1>Hello, world!</h1>"; }

//---------------------------------------------------------------------------
// Experiments
//---------------------------------------------------------------------------

// Set width of help section
    
    /* try loading this directly with iframe tag
    document.getElementById("WelcomeHtml").innerHTML=
	'<object type="text/html" class="HtmlContent" data="welcome.html"></object>';
    document.getElementById("MidMainRight").innerHTML=
	'<object type="text/html" class="HtmlContent" data="../../datafiles/doc/html/index.html"></object>';
*/

// For testing, set the editor buffer to a text file
// This doesn't work, just puts a description of the object into the value
// but doesn't read the file.  Maybe try making the file a js statement
// that assigns a text constant to a variable?
// Alternative might be to read innerhtml and then to extract this?
// experiment result: can set innerHTML of a div, but not of a textarea
// Can I read the textinto the div, and then obtain the value of this,
// print it, and then set it into the value of the text area???

// Doesn't work because of cross-origin restriction


function setEdBuf1 () {
    console.log("setEdBuf1");
    document.getElementById("EdTextBufferDummyDiv").innerHTML=
	'<object type="text/html" class="UserManContent" data="../docsrc/index.md"></object>';
    var xyztemp = "xyztemp initial value";
    var xyztemp = document.getElementById("EdTextBufferDummyDiv").innerHTML;
    console.log("xyztemp = " + xyztemp);
    var foobar = xyztemp.data;
    console.log("foobar = " + foobar);
    var ifrm = document.getElementById("myIframe");
    console.log("ifrm = " + ifrm);
    var ifr_doc = ifrm.contentWindow.document;
    console.log("ifr_doc = " + ifr_doc);
    // Doesn't work because of cross-origin restriction
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

function editorButton2() {
    console.log("Editor button 2 clicked");

    // this gives description of the object, not the file contents
    document.getElementById('EditorTextArea').value =
	'<object type="text" data="./Sigma16gui.css"></object>';
    let xs = fileReader.readAsText("./Sigma16gui.css");
    console.lot(xs);

}

//---------------------------------------------------------------------------
// File handling
//---------------------------------------------------------------------------

function handleSelectedFile (flist) {
    console.log("handleSelectedFile");
    console.log(flist);
    let selectedFile = flist[0];
    console.log("selected file = " + selectedFile);
//    console.log("created fileReader" + fr);
    fileContents = fileReader.readAsText(selectedFile);
}

fileReader.onload = function (e) {
    console.log("fileReader.onload activated");
    let xs = e.target.result;
    console.log(xs);
    editorBufferTextArea.value = xs;
}

// Save a file by downloading it to user default Downloads folder



/* var textFile = null, */

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


//---------------------------------------------------------------------------
// Top level tab buttons control which pane is visible and active
//---------------------------------------------------------------------------

function hideTabbedPane(paneId) {
//    console.log("hiding tabbed pane " + paneId);
    document.getElementById(paneId).style.display = "none";
}

function hideAllTabbedPanes() {
    hideTabbedPane("WelcomePane");
    hideTabbedPane("FilePane");
    hideTabbedPane("EditorPane");
    hideTabbedPane("AssemblerPane");
    hideTabbedPane("LinkerPane");
    hideTabbedPane("ProcessorPane");
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

function file_pane_button() {
//    console.log("file_pane_button clicked")
    hideAllTabbedPanes();
    showTabbedPane("FilePane");
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


// Simple example program: add
const example_add = `; Program Add

  add R4,R5,R6

  lea   R4,23[R0]
  lea   R9,5[R0]
  add   R7,R4,R9

; result := x + y
    load  R1,x[R0]       ; R1 := x
    load  R2,y[R0]       ; R2 := y
    add   R3,R1,R2       ; R3 := x + y
    store R3,result[R0]  ; result := x + y

; Terminate execution
    trap   R0,R0,R0      ; system request to terminate

; Variables
x       data   2
y       data   3
result  ; good label
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
// Display for the user guide
//---------------------------------------------------------------------------

// Side-by-side frames that can be resized by dragging bounary between them

// Resizing the two parts of the middle section: gui and user guide
// https://stackoverflow.com/questions/46931103/
//   making-a-dragbar-to-resize-divs-inside-css-grids

/*
document.addEventListener('mousedown', function(e) {
  // If mousedown event is fired from .handler, toggle flag to true
  if (e.target === handler) {
    isHandlerDragging = true;
  }
});

document.addEventListener('mousemove', function(e) {
  // Don't do anything if dragging flag is false
  if (!isHandlerDragging) {
    return false;
  }

  // Set boxA width properly
    // [...more logic here...]
// Get offset
var containerOffsetLeft = wrapper.offsetLeft;

// Get x-coordinate of pointer relative to container
var pointerRelativeXpos = e.clientX - containerOffsetLeft;

// Resize box A
// * 8px is the left/right spacing between .handler and its inner pseudo-element
// * Set flex-grow to 0 to prevent it from growing
boxA.style.width = (pointerRelativeXpos - 8) + 'px';
boxA.style.flexGrow = 0;
    
});

document.addEventListener('mouseup', function(e) {
  // Turn off dragging flag when user mouse is up
  isHandlerDragging = false;
});

*/

//var handler = document.querySelector('.MiddleSectionResizeHandle');
//var handler = document.getElementById("foobar");
//var wrapper = document.querySelector('.MiddleSection');
//var wrapper = document.getElementById("foobarbaz");

//var boxA = document.getElementById('GuiSectionID');
//var isHandlerDragging = false;
/*
function changeMiddleSizes() {
    console.log('changeMiddleSizes');
    boxA.style.width = "300px";
    boxA.style.flexGrow = 0;

}
*/

// Initialize the GUI
// It's necessary to do this only after window.onload occurs, in order to
// ensure that the operations have been defined before trying to run them.

/* from stack overflow, so of course it doesn't work
const fileobj = document.getElementById("FileInput").files[0];
const fileresult = document.getElementById("FileInputResultDiv");
const filereader = new FileReader;
filereader.addEventListener("load", () => {
    console.log("file reader load activated");
    console.log(filereader.result);
})
filereader.readAsText(fileobj, "UTF-8");

*/

var handler = document.querySelector('.MiddleSectionResizeHandle');
var isHandlerDragging = false;

// x>0 means expand the user guide; x<0 means shrink it
function user_guide_resize(x) {
    let containerOffsetLeft = wrapper.offsetLeft;
    let w = parseInt(boxA.style.width,10);
    let y = (w+ 10*x) + "px";
    let z = w-30;
    boxA.style.width = y;
    boxA.style.flexGrow = 0; // without this they don't grow/shrink together
    document.getElementById("EditorTextArea").style.width= z + 'px';
    document.getElementById("AssemblerText").style.width = z + 'px';
    console.log('user_guide_resize ' + x
		+ ' containerOffsetLeft=' + containerOffsetLeft
		+ ' w=' + w + ' y=' + y + ' z=' + z);
}

function showSizeParameters () {
    console.log('boxA.style.width = ' + boxA.style.width);
}

document.addEventListener('mousedown', function(e) {
  // If mousedown event is fired from .handler, toggle flag to true
  if (e.target === handler) {
    isHandlerDragging = true;
  }
});

document.addEventListener('mouseup', function(e) {
  // Turn off dragging flag when user mouse is up
  isHandlerDragging = false;
});

document.addEventListener('mousemove', function(e) {
  // Don't do anything if dragging flag is false
  if (!isHandlerDragging) {
    return false;
  }

  // Get offset
  var containerOffsetLeft = wrapper.offsetLeft;

  // Get x-coordinate of pointer relative to container
  var pointerRelativeXpos = e.clientX - containerOffsetLeft;
  
  // Arbitrary minimum width set on box A, otherwise its inner content will collapse to width of 0
  var boxAminWidth = 60;

  // Resize middle left section (the system display) box A
  // * 8px is the left/right spacing between .handler and its inner pseudo-element
    // * Set flex-grow to 0 to prevent it from growing
    let boxAnewWidth = Math.max(boxAminWidth, pointerRelativeXpos - 8);
    console.log ('boxAnewWidth = ' + boxAnewWidth);
    boxA.style.width = boxAnewWidth + 'px';

// What is the purpose of setting flexGrow to 0?  why just boxA?
    boxA.style.flexGrow = 0;

    let textbufferwidth = boxAnewWidth - 30;  // how many px to use?
// The following works!!!  Need to do this in the onload initialization too
    document.getElementById("EditorTextArea").style.width
	= textbufferwidth + 'px';
    document.getElementById("AssemblerText").style.width
	= textbufferwidth + 'px';
    
// Experiment with commenting the following out; do both boxes need resizing?
    // Resize the middle right section (user guide)
    let boxBnewWidth = window.innerWidth - boxAnewWidth - 150;
    let boxBnewWidthpx = boxBnewWidth + "px";
    console.log ('boxBnewWidth = ' + boxBnewWidthpx);
    boxB.style.width = boxBnewWidthpx;

/*
  boxA.style.width = (Math.max(boxAminWidth, pointerRelativeXpos - 8)) + 'px';
    let widA = boxA.style.width;
    console.log("Resizing, boxA.style.width = " + widA);
    let widB = boxB.style.width;
    console.log("Resizing, boxB.style.width = " + widB);


    let welcomeElt = document.getElementById("WelcomeHtml");
    let temp = welcomeElt.style;
    console.log("temp WelcomeHtml.style.width = " + temp);
*/    
});

//---------------------------------------------------------------------------
// Complete initialization when onload occurs
//---------------------------------------------------------------------------

window.onload = function () {
    
    console.log("window.onload activated");
    showTabbedPane("WelcomePane");

// Initialize the modules
    initModules ();

// Initialize the registers

    // Register file
    // R0 is built specially as it is constant; all others are built with mkReg
    
    for (var i = 0; i<16; i++) {
	let regname = 'R' + i; // also the id for element name
//	console.log('xx' + regname + 'xx');
	thisReg = (i==0) ?
	    mkReg0 (regname, regname, intToHex4)
	    : mkReg (regname, regname, intToHex4);
	thisReg.regIdx = i;
	regFile[i] = thisReg;
	register[i] = thisReg;
    }
//Control registers
    pc  = mkReg ('pc',  'pcElt',  intToHex4);
    ir  = mkReg ('ir',  'irElt',  intToHex4);
    adr = mkReg ('adr', 'adrElt', intToHex4);
    dat = mkReg ('dat', 'datElt', intToHex4);
    spc = mkReg ('spc', 'spcElt', intToHex4);
    ien = mkReg ('ien', 'ienElt', intToBit);
    controlRegisters = [pc,ir,adr,dat,spc,ien];
    nRegisters = 16;
    controlRegisters.forEach (function (r) {
//	console.log('making reg ' + nRegisters + ' = ' + r.regName);
	register[nRegisters] = r;
	r.regIdx = nRegisters;
	nRegisters++;
    });

// Initialize the memory
    memInitialize();
    
    editorBufferTextArea = document.getElementById("EditorTextArea");

    handler = document.getElementById("MiddleSectionResizeHandle");
    wrapper = document.getElementById("MiddleSection");
    /*    boxA = document.getElementById("GuiSectionID"); */
    boxA = document.getElementById("MidMainLeft");
    boxB = document.getElementById("MidMainRight");
    isHandlerDragging = false;

    /* initialize sizes of left and right parts of middle section */
    let foobarb =  window.innerWidth;
    let foobarb2 = foobarb * 0.5;
    let foobarb3 = foobarb2 + "px";
    console.log ('foobarb3 = ' + foobarb3);
    boxA.style.width = foobarb3;

    /* for save download */
    create = document.getElementById('CreateFileForDownload'),
    textbox = document.getElementById('DownloadFileTextBox');
  create.addEventListener('click', function () {
    var link = document.getElementById('downloadlink');
    link.href = makeTextFile(textbox.value);
    link.style.display = 'block';
  }, false);

    resetRegisters();

    insert_example(example_errors);     // For testing and debugging
    
    console.log("Initialization complete");

    console.log("Initialization experiments");
    run();  // run current test case
//    let eltUG = document.getElementById("MidMainRight");
//    let settings = eltUG.getElementsByClassName('even');
//    console.log('settings = ' + settings);
//    console.log('settings length = ' + settings.length);
}

