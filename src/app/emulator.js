// Emulator

// Boot from either the current module (without linking) or the load
// module (if the linker has been run).

var instr = 0;
var ir_op = 0, ir_d = 0, ir_a = 0, ir_b = 0;  // instruction fields
var ea = 0;  // effective address

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

// memClearAccesses, memShowAccesses, and memDisplay are very slow...

function executeInstruction () {
    console.log ('executeInstruction');
    memClearAccesses ();
    memDisplay ();
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
    
    regShowAccesses()
    memShowAccesses();
    memDisplay ();
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
     function () {handle_rx()} ]


function rrr (op) {
    console.log ('rrr');
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let cc = regFile[15].get();
    let [ primary, secondary ] = op (a,b,cc);
    console.log ('rrr primary = ' + primary + ' secondary = ' + secondary);
    regFile[ir_d].put(primary);
    if (ir_d<15) { regFile[15].put(secondary) }
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

function handle_rx () {
    console.log ('handle rx' + ir_b);
    rxDispatch[ir_b]();
}


var rxDispatch =
    [function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)}]

function rx(f) {
    console.log('rx');
    adr.put (memFetch (pc.get()));
    pc.put (pc.get() + 1);
    ea = regFile[ir_a].get() + adr.get();
    console.log('rx ea = ' + intToHex4(ea));
    f();
}

function rx_lea () {
    console.log('rx_lea');
    regFile[ir_d].put(ea);
}
