let workerCounter = 0;

onmessage = function (e) {
    console.log ("worker received a message");
    let result = e.data;
    console.log (`I am the worker, this is what I received: /${result}/`);
    postMessage (workerCounter);
    console.log (`I am the worker, replying with ${workerCounter}`);
    workerCounter++;
    
}

// basic-SharedArrayBuffer-worker.js

let shared;
let index;
const updateAndPing = () => {
    ++shared[index];
    index = (index + 1) % shared.length;
    this.postMessage({type: "ping"});
};
this.addEventListener("message", e => {
    if (e.data) {
        switch (e.data.type) {
            case "init":
                shared = e.data.sharedArray;
                index = 0;
                updateAndPing();
                break;
            case "pong":
                updateAndPing();
                break;
        }
    }
});
