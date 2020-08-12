let workerCounter = 0;

onmessage = function (e) {
    console.log ("worker received a message");
    let result = e.data;
    console.log (`I am the worker, this is what I received: /${result}/`);
    postMessage (workerCounter);
    console.log (`I am the worker, replying with ${workerCounter}`);
    workerCounter++;
    
}
