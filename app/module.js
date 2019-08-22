// modules and files pane


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

