// System

var s16modules = [];
var currentModNum = 0;

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
	locationCounter : 0,
	asmListing : [],
	objectCode : []
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

function clearObjectCode () {
    document.getElementById('LinkerTextHtml').innerHTML	= "";
}

function setCurrentObjectCode () {
    let objHeader = "Module " + currentModNum + " object code"
    document.getElementById('LinkerTextHtml').innerHTML	=
	"<pre><span class='ListingHeader'>" + objHeader + "</span>\n"
	+ s16modules[currentModNum].objectCode.join('\n')
	+ "</pre>";
}
