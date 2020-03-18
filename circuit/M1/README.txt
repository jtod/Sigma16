---------------------------------------------------------------------------
Sigma16 M1 processor circuit
---------------------------------------------------------------------------

To get started quickly
~~~~~~~~~~~~~~~~~~~~~~

  1. Change to this directory.
  2. Launch ghci, with a search path including the Hydra source.
  3. Load a machine language program.
  4. Run a simulation.

For example:

  ghci
  :load ProgArrayMax
  main

Files
~~~~~

ALU.hs           -- specifies the M1 alu
ALU_Run.hs       -- simulation driver and test data for the ALU
Control.hs       -- specifies the M1 control
Control_Run.hs   -- simulation driver and test data for the control
CPU.hs           -- specifies the full M1 system
CPU_Run.hs       -- simulation driver for the full system

ProgArrayMax.hs  -- A machine language program to test the CPU
                 -- This is the main program

---------------------------------------------------------------------------
Interpreting the simulation output
---------------------------------------------------------------------------

The M1 is a digital circuit, and its inputs and outputs are bit
signals.  The simulation would be unreadable if presented with 1s and
0s for input and output.  Therefore a simulation driver is used.  This
is defined in CPU_Run.hs, and it converts the inputs (the hex string
constants in ProgArrayMax.hs) into input bit signals, and it converts
the output bit signals coming from the circuit into readable formatted
strings.  This document ignores the formatting details, and focuses
exclusively on how the digital circuit (M1.hs) runs the program.

The simulation output first identifies itself:

...........................................................................
System M1 simulation
...........................................................................

The system begins by using direct memory access to load the machine
language program into memory, starting at location 0.  The executable
(program followed by data) consists of 31 words.  To see this, note
that the last word in the executable (labelled max) has address 001e,
which is 30, and there are 31 locations from 0 through 30.
Alternatively, you can just count the hex constants in the machine
language.  Therefore the first 31 clock cycles are "stolen" by the DMA
controller in order to load the executable code into memory.

The output is divided into sections, one for each clock cycle.  The
first line of a section gives the cycle number, followed by a label
"Computer system inputs", followed by the values of four signals that
pertain to the entire system.  The first section (clock cycle 0) is
used by the DMA to store the first word of the executable ($1100) into
the first word of memory ($0000).  As the DMA is active, the dma
control signal is 1, and the address (dma_a) and data (dma_d) are set
to the corresponding values.  At the clock tick that ends cycle 0, the
memory will perform Mem[0] := 1100.

...........................................................................
   0.  Computer system inputs
         reset=0 dma=1 dma_a=0000 dma_d=1100
...........................................................................

The remainder of the section for Cycle 0 contains nothing of interest,
as the processor is idle.  In Cycle 1, the next word of the machine
language (in ArrayMax you'll see this is 0018) is loaded into the next
memory location (0001), so the driver sets the following system
inputs:

...........................................................................
         reset=0 dma=1 dma_a=0001 dma_d=0018
...........................................................................

The simulation driver continues this for 31 cycles, loading the
machine language into memory one word at a time.  The last word (0000,
the initial value of max) is stored at location 001e in Cycle 30.

...........................................................................
  30.  Computer system inputs
         reset=0 dma=1 dma_a=001e dma_d=0000
...........................................................................

At this point, the simulation driver has completed the input, and the
computer's memory contains the executable program with its data (all
31 words of it!).

In Cycle 31, the driver starts the control algorithm by asserting the
main system reset signal.  The effect of this is to put a 1 on a
signal that will, at the next clock tick, enter the state flip flop
for the first state in the control algorithm, st_instr_fet.

...........................................................................
  31.  Computer system inputs
         reset=1 dma=0 dma_a=0000 dma_d=0000
...........................................................................

The effect of the reset occurs at the clock tick between cycles 31 and
32, so it becomes visible when we examine the values of signals during
cycle 32.  Now reset and dma are both 0, and they will remain so for
the duration of the execution.  The Control state section shows that
the flip flop named st_instr_fet contains 1, while all others contain
0.

The section "Control signals" shows the value of every control signal,
as generated by the control circuit.  It has set the following signals
to 1: ctl_alu_a, ctl_alu_b, ctl_ir_ld, ctl_pc_ld, ctl_ma_pc,
ctl_x_pc.  The others are all 0.

First let's look at how the control signals are generated; then we
consider what they mean and what effect they have on the datapath.

All of the control signals are generated by an or gate that or's the
flip flops for the states in which that signal is asserted.  In M1.hs,
you can see that the or gates for these signals include st_instr_fet
among their inputs.  For example, here is the definition of the signal
ctl_alu_a:

  ctl_alu_a   = any1 [st_instr_fet,st_load0,st_ldval0,st_neg,st_cmpeq,
                       st_cmplt,st_cmpgt,st_jumpf0]

The any1 building block computes the logical or of the word; since
st_instr_fet is 1, the output of any1 is 1 and ctl_alu_a has the value
1.  The same holds for ctl_alu_b, ctl_ir_ld, ctl_pc_ld, ctl_ma_pc, and
ctl_x_pc.  All of the other signals do not have st_instr_fet among the
inputs to their any1 circuit, so those signals are 0.

..............................................................................
  32.  Computer system inputs
         reset=0 dma=0 dma_a=0000 dma_d=0000

       Control state
         st_instr_fet = 1  st_dispatch = 0       st_nop = 0     st_load0 = 0
             st_load1 = 0     st_load2 = 0    st_ldval0 = 0    st_ldval1 = 0
               st_add = 0       st_sub = 0       st_neg = 0      st_mul0 = 0
            st_store0 = 0    st_store1 = 0    st_store2 = 0     st_cmpeq = 0
             st_cmplt = 0     st_cmpgt = 0    st_jumpt0 = 0    st_jumpt1 = 0
            st_jumpt2 = 0    st_jumpf0 = 0    st_jumpf1 = 0    st_jumpf2 = 0
             st_jump0 = 0     st_jump1 = 0     st_trape = 0     st_trapf = 0

       Control signals
         ctl_alu_a  = 1 ctl_alu_b  = 1 ctl_alu_c  = 0 ctl_alu_d  = 0
         ctl_rf_ld  = 0 ctl_rf_alu = 0 ctl_rf_sd  = 0 ctl_ir_ld  = 1
         ctl_pc_ld  = 1 ctl_ad_ld  = 0 ctl_ad_alu = 0 ctl_ma_pc  = 1
         ctl_x_pc   = 1 ctl_y_ad   = 0 ctl_sto    = 0
..............................................................................

Now that we have seen how the control signals are generated, we move
on to their meaning and effect.  The control algorithm (see M1.hs)
contains the following definition for this state:

  st_instr_fet:
    ir := mem[pc], pc++;
       {ctl_ma_pc, ctl_ir_ld, ctl_x_pc, ctl_alu=alu_inc, ctl_pc_ld}

The first line is just the name of the state.  The second line is a
comment describing the effect on the datapath, and the third line is
the set of control signals that are 1 during the clock cycle when the
machine is in this state.  (The keyword "assert" should be in front of
the list of signals, but was omitted here.)

There are two ways to understand what's going on here: you can look at
the datapath and see how the control signals cause it to perform the
actions described in the comment, and you can look at the entire
control algorithm and see how these actions contribute to the
execution of machine language programs.

A good way to proceed is to follow the control algorithm as it
executes the first instruction.  For each statement, check that the
control circuit has set the right state flip flop and is asserting the
right control signals.  Look at the register and processor signal
values provided by the simulation driver, and check that the datapath
is doing the right thing.

It isn't feasible to follow a long simulation in complete detail.
That's why we use computers to perform simulations!  But there are a
lot of interesting points that arise deep into the simulation output.
For example, it's interesting to watch the control algorithm as it
executes a conditional jump, and the best way to study this is to
observe a conditional jump that actually jumps and another one that
does not.

You can locate a state in which the processor is about to execute a
certain type of instruction by scanning down the simulation output,
looking for a 1 in the corresponding state.  (A good way to do this is
to load the simulation output into a text editor, and to do a textual
search.)

For example, the findmax programs executes some jumpf instructions.
Scan through the output, looking for a point where st_jumpf0 = 1.
This occurs in cycles 59, 65, 86, 92, 113, 119, 138, 144, and 163.
Reading the control algorithm, we see that a decision is made base on
the first operand register:

  case reg[ir_sa] = 0 of
    False:
      break
    True:
      st_jumpf1:
        pc := reg[ir_sa] + ad
           {ctl_y_ad, ctl_alu_abcd=0000, ctl_pc_ld}

If the condition for jumping is False, the control algorithm is
finished with the instruction, and it continues on to st_instr_fet
(this is the "break" in the pseudo-code comment).  However, if the
condition for jumping is True, the control algorithm enters state
st_jumpf1, where it performs the jump by loading the pc register with
the jump target.

So let's look at the cycles where the machine is in state st_jumpf0,
and find out which state it enters next.  This requires a bit of study
of the simulation output, and here is the result:

 Cycle  Next cycle    Next state     Jumped?
 ~~~~~  ~~~~~~~~~~    ~~~~~~~~~~     ~~~~~~~
   59       60       st_instr_fet      no
   65       66       st_instr_fet      no
   86       87       st_instr_fet      no
   92       93       st_instr_fet      no
  113      114       st_instr_fet      no
  119      120       st_jumpf1        yes
  138      139       st_instr_fet      no
  144      145       st_jumpf1        yes
  163      164       st_jumpf1        yes

So we have found several examples where the jump is taken, and several
where it is not.  It's worth picking one example of each case, and
studying it in detail.

An especially valuable experiment is to pick some key points in the
simulation so that you can follow the execution of the machine
language program, as it searches through the array looking for the
maximal element.





Word notation
~~~~~~~~~~~~~

In a word, bit indices are 0, 1, ..., n-1 where bit 0 is most
significant.

w!!i                   i'th bit of word w
field w i j            bits i..i+j-1 of word w


Constant signals
~~~~~~~~~~~~~~~~

zero                   signal with constant 0 value
one                    signal with constant 1 value

Logic gates
~~~~~~~~~~~

inv                    inverter
and2, and3, and4       and gate with 2, 3, 4 inputs
nand2, nand3, nand4    and gate with 2, 3, 4 inputs
or2, or3, or4          or gate with 2, 3, 4 inputs
nor2, nor3, nor4       nor gate with 2, 3, 4 inputs
xor2, xor3, xor4       xor gate with 2, 3, 4 inputs


Flip flops
~~~~~~~~~~

dff                    delay flip flop


Building block circuits
~~~~~~~~~~~~~~~~~~~~~~~

fanout n b             connect bit b to n outputs, forming a word
boolword n b           form an n-bit word, lsb = b, other bits = 0
any1                   or the bits in a word: result is 1 if any 1 bit
winv w                 invert the bits in a word
mux1w                  use 1-bit control to select between two words
bitslice2 x y          convert pair of words to word of pairs
mux2                   use two bit control to select one of four inputs


Registers
~~~~~~~~~

reg n ld x                 n-bit register with load control ld, data input x
regfile n k ld d sa sb x   register file with 2^k registers, each n-bits wide,
                           load control ld, destination address d,
                           reads out registers sa and sb, data input x
