//------------------------------------------------------------------------------
// module.js -- representation of source and object modules
//------------------------------------------------------------------------------

// The working program is a list of modules, one of which is the
// current working module visible in the editor and assembler.  A
// module data structure contains everything known about the module,
// whether it is source or object.

//------------------------------------------------------------------------------
// List of all modules

var s16modules = [];    // All the modules in the system
var currentModNum = 0;  // The module shown in editor and assembler

// Initialize the modules: create one initial (empty) module and make
// it the current module

function initModules () {
    s16modules = [mkModule()];
    currentModNum = 0;
}

// Get the full data structure for the current module

function getCurrentModule () {
    return s16modules[currentModNum]
}

// Return brief descriptions of all the modules
function showModules () {
    //    let xs = s16modules.length + ' modules\n';
    let xs = ' modules\n';
    for (let i = 0; i < s16modules.length; i++) {
	console.log(i);
	xs += i + '. ' + showModule (s16modules[i]);
	console.log(xs);
    }
    return xs;
}

//------------------------------------------------------------------------------
// Representation of a module

// Make a new module with empty contents; this defines the fields of a module

function mkModule () {
    console.log('mkModule');
    return {
	modName : '',              // name of module: module stmt, file name, none
	modSrc : '',               // source code
	asmStmt : [],              // statements correspond to lines of source
	symbols : [],              // symbols used in the source
	symbolTable : new Map (),  // symbol table
	nAsmErrors : 0,            // number of errors in assembly source code
	locationCounter : 0,       // address where next code will be placed
	objectCode : [],           // string hex representation of object
	asmap : [],                // array mapping address to source statement
	isExecutable : true,       // until proven otherwise
	asmListingPlain : [],      // assembler listing
	asmListingDec : []         // decorated assembler listing
    }
}

// Return a brief description of a module
function showModule (m) {
    let n = m.src ? m.src.length : 0;
    let xs = m.modName + ' (' +  n + ' characters)\n';
    return xs;
}


//------------------------------------------------------------------------------
// The modules and files pane

function fileExamples () {
    document.getElementById('EditorTextArea').innerHTML = "hello";
//	'<object type="text/directory"
//                 class="HtmlContent"
//                 data="./programs/examples">
//	</object>';
}

function fileFactorial () {
    document.getElementById('EditorTextArea').value = "goodbye";
//	= "<object type=\'text\' class=\'HtmlContent\'
//                  data=\'./programs/examples/recursion/factorial.asm.txt\'>
//           </object>";
}

const experimentTarget =
      "https://jtod.github.io/Sigma16/index.html"

function fileButton3 () {
    console.log ('fileButton3');
    
    // url (required), options (optional)
    fetch(experimentTarget, {
	method: 'get'
    }).then(function(response) {
	console.log ('fileButton3 got a response');
	console.log(response);
	console.log ('fileButton3 that was the response');
    }).catch(function(err) {
	// Error :(
	console.log('fileButton3 error')
    });
    console.log ('fileButton3 finishing');
}


function fileButton4 () {
    console.log ('fileButton4');
    document.getElementById('ExamplesDirectory').innerHTML =
	"hello this is great";
}

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
