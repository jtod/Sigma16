// Emulator

// Boot from either the current module (without linking) or the load
// module (if the linker has been run).

var instr = 0;
var ir_op = 0, ir_d = 0, ir_a = 0, ir_b = 0;

function clearCtlRegs () {
}

function procBoot () {
    console.log('procBoot');
    bootCurrentModule ();
}

function procStep () {
    console.log ('procStep');
    executeInstruction();
}

function executeInstruction () {
    console.log ('executeInstruction');
    memClearAccesses ();
    regClearAccesses ();

    ir.put (memFetch (pc.get()));
    pc.put (pc.get() + 1);
    console.log('pc = ' + intToHex4(pc.get()) + ' ir = ' + intToHex4(instr));
    let tempinstr = ir.get();
    ir_b = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    ir_a = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    ir_d = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    ir_op = tempinstr & 0x000f;
    console.log('instr fields = ' + ir_op + ' ' + ir_d + ' ' + ir_a + ' ' + ir_b);

    opDispatch [ir_op] ();
    
    refreshRegisters();
    regShowAccesses()
    memShowAccesses();
}

function rrr (op) {
    console.log ('rrr ' + op);
}

var opDispatch =
    [function () {rrr(rrr_add)},
     function () {rrr(rrr_sub)},
     function () {rrr(rrr_mul)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {xx()},
     function () {rx()} ]

function rrr (f) {
    console.log ('rrr');
    f ();
}

function rrr_add () {
    console.log ('rrr_add');
}

function rrr_sub () {
    console.log ('rrr_sub');
}

function rrr_mul () {
    console.log ('rrr_mul');
}

function rrr_div () {
    console.log ('rrr_div');
}

function xx () {
    console.log ('xx');
}

function rx () {
    console.log ('rx');
}



