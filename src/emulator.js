// Emulator

// Boot from either the current module (without linking) or the load
// module (if the linker has been run).

function procBoot () {
    console.log('procBoot');
    bootCurrentModule ();
}

function procStep () {
    console.log ('procStep');
}
