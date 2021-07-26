console.log ("reading testgui.mjs")

let workerThread = new Worker ("./testworker.mjs", {type: "module"});

console.log ("finished reading testgui.mjs")
