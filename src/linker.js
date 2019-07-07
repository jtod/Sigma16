// Linker

var object = [];
var executable = [];

// call setCurrentObjectCode in system

// Set the object code and executable to empty
function clearObject () {
    object = [];
    executable = [];
}

// Check whether the current module contains an executable; if so boot it
function bootCurrentModule () {
    console.log ('procCurrentModule');
    resetRegisters ();
    memClearAccesses();
    let m = s16modules[currentModNum];   // check that it exists ??
    let stmt = m.asmStmt;
    let locationCounter = 0;
    for (let i = 0; i < stmt.length; i++) {
	console.log('bootCM ' + i + ' => ' + stmt[i].codeWord1
		    + ' ' + stmt[i].codeWord2 );
	if (stmt[i].codeWord1 >= 0) {
	    memStore (locationCounter, stmt[i].codeWord1);
	    locationCounter++;
	}
	if (stmt[i].codeWord2 >= 0) {
	    memStore (locationCounter, stmt[i].codeWord2);
	    locationCounter++;
	}
    }
    memShowAccesses();
    memDisplay();
}



// Parse the object code
function parseObject () {
    console.log('parseObject');
}
