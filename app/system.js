// System

var s16modules = [];
var currentModNum = 0;

function getCurrentModule () {
    return s16modules[currentModNum]
}

// s16modules[0].src = 'abcd';    testing for non-0 src length

// Create one initial module and make it the current module
function initModules () {
    s16modules = [mkModule()];
    currentModNum = 0;
}

function mkModule () {
    console.log('mkModule');
    return {
	modName : '',
	modSrc : '',
	asmStmt : [],
	symbols : [],
	symbolTable : new Map (),
	locationCounter : 0,
	asmListing : [],
	objectCode : [],
	asmap : [],  // array mapping address to source statement
	isExecutable : true  // until proven otherwise
    }
}

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

function showModule (m) {
    let n = m.src ? m.src.length : 0;
    let xs = m.modName + ' (' +  n + ' characters)\n';
    return xs;
}

// linker
