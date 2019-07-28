// Linker

var exMod;           // the module that is executing
var curAsmap = [];


function clearObjectCode () {
    document.getElementById('LinkerText').innerHTML	= "";
}

function setCurrentObjectCode () {
    let objHeader = "Module " + currentModNum + " object code"
    let objText =
	"<pre class='HighlightedTextAsHtml'><span class='ListingHeader'>"
	+ objHeader + "</span>\n"
	+ s16modules[currentModNum].objectCode.join('\n')
	+ "</pre>";
    document.getElementById('LinkerText').innerHTML	= objText;

}
