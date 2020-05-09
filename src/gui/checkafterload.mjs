// testmod4.mjs

import * as ver from './version.mjs';
import * as com from './common.mjs';
import * as smod from './s16module.mjs';
import * as arch from './architecture.mjs';
import * as arith from './arithmetic.mjs';
import * as state from './state.mjs';
import * as ed from './editor.mjs';
import * as asm from './assembler.mjs';
import * as link from './linker.mjs';
import * as em from './emulator.mjs';
import * as gui from './gui.mjs';

// version.mjs
console.log ("***** version.mjs");
let xyz = ver.s16version;
console.log (`testmod4: xyz=${xyz}`);

// s16module.mjs
console.log ("***** s16module.mjs");
console.log (`sel=${smod.selectedModule}`);
smod.setSelectedModule(5);
console.log (`sel=${smod.selectedModule}`);

// architecture.mjs
console.log ("***** architecture.mjs");
console.log (`${arch.formatSize (arch.RX)}`);

// arithmetic.mjs
console.log ("***** arithmetic.mjs");
console.log (arith.wordToHex4(42));
console.log (arith.hex4ToWord("001b"));

// state.mjs
console.log ("***** state.mjs");

// editor.mjs
console.log ("***** editor.mjs");

// assembler.mjs
console.log ("***** assembler.mjs");

// linker.mjs
console.log ("***** linker.mjs");

// emulator.mjs
console.log ("***** emulator.mjs");
console.log (`highlightedRegisters len = ${em.highlightedRegisters.length}`);

// gui.mjs
console.log ("***** gui.mjs");



