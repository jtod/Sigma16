---------------------------------------------------------------------------
	     Notes on M1 circuit for Sigma16 architecture
			 John O'Donnell, 2015
---------------------------------------------------------------------------

This is a quick summary and reference for the M1 circuit, which
implements the Sigma16 architecture.

Author
~~~~~~
  Dr John O'Donnell
  School of Computing Science
  University of Glasgow
  Glasgow G12 8QQ
  United Kingdom
  Email:  john.odonnell@glasgow.ac.uk
  Web:    www.dcs.gla.ac.uk/~jtod/
  Copyright 2015, John O'Donnell

Quick start
Introduction
Building block circuits
   reg: n-bit register component
   mux1w: multiplexer with 1-bit control
   register file
Instruction set architecture of Sigma16
The M1 circuit
   ir register (instruction register)
   pc register (program counter)
   ad register (address register)
   memory
   list of control signals
Running machine language on the circuit
   Interpreting the simulation output

---------------------------------------------------------------------------
			     Quick start
---------------------------------------------------------------------------

The following command runs the Sigma16 machine language program
ArrayMax on the M1 circuit, using the Hydra hardware description
language and simulator.  The simulation output is saved in the file
simulation.txt.

   $ ghc -e main ArrayMax >simulation.txt

There is a section below on running machine language programs on the
circuit.


---------------------------------------------------------------------------
			     Introduction
---------------------------------------------------------------------------

Sigma16 is a 16-bit instruction set architecture, and M1 is a digital
circuit that implements it.  M1 is designed for simplicity rather than
speed; more advanced circuits introduce techniques that make the
system faster at the cost of greater complexity.

M1 is specified using the Hydra hardware description language, which
generates and simulates circuits.  There is also a simulation driver,
M1run, which takes a machine language program, loads it into the
system memory, and then simulates the circuit.  The effect of this is
to execute the program on the circuit.  This is fundamentally
different from emulation: M1 is pure hardware, consisting entirely of
flip flops, logic gates, and wires.

The simulation driver takes a machine language program as input, in
the form of a list of strings giving the contents of memory words in
hexadecimal.  It produces formatted output giving the value, for each
clock cycle, of the most important internal signals in the circuit.
Hydra, and has a simulator that can run machine language programs.


------------------------------------------------------------------------
		   reg :  n-bit register component
------------------------------------------------------------------------

Use it to hold an n-bit number, and load a new value when you want

  Interface
    -- size paramemter n
    -- two inputs: a load control and an n-bit data input
    -- output: contents of the register
  Example:  pc = reg n ctl_pc_ld q
     suppose n=16 (as it is in Sigma16)
     r is the 16-bit output of the register
     ctl_pc_ld is a 1-bit control input
     Output pc is the contents of the register
     At next clock tick, if ctl_pc_ld=1 then pc := q


------------------------------------------------------------------------
		mux1w : multiplexer with 1-bit control
------------------------------------------------------------------------

Use it to choose from among several different values

  Interface
    -- first argument:  1-bit control input
    -- second and third arguments: data word inputs
    -- output: a word

  Example:  mux1w ctl_adr_alu memdat r
    ctl_adr_alu is a control bit
    memdat, r are data word inputs
      If ctl_adr_alu=0 then output = memdat
      If ctl_adr_alu=1 then output = r


------------------------------------------------------------------------
			    Register file
------------------------------------------------------------------------

   readouts:
     a = reg[rf_sa]
     b = reg[rf_sb]
   at clock tick:
     if ctl_rf_ld=1
        then reg[ir_d] := p

ctl_rf_ld = 1 means "load p into reg[ir_d]"

........................................................
rf_sa is the first register to read out from regfile
a = reg[rf_sa]
It is a choice from among {ir_sa, ir_d}

rf_sa =
  if ctl_rf_sd = 0
    then ir_sa
    else ir_d

Normally, we want to use ir_sa as the first register to read out.  In
add R1,R2,R3 the ir_sa field is 2 and we want to read reg[2].  In this
case, make ctl_rf_sd = 0.

In a store instruction, we need to read out the register in ir_d.  For
example, in store R5,xyz[R0] we have ir_d=5 and that's the register
that needs to be read.  In this case, make ctl_rf_sd = 1.

........................................................
rf_sb is the second register to read out from regfile
b = reg[rf_sb]
It is always ir_sb

........................................................
p is the data input to regfile (the value to load into destination)

p = a choice from among {memdat, r, pc}
p = if ctl_rf_pc = 0
      then p = (if ctl_rf_alu -0
                  then memdat
                  else r)
      else pc

This table summarises the value of p:

ctl_rf_pc   ctl_rf_alu     p
--------------------------------
    0           0        memdat
    0           1         r
    1           0         pc
    1           1         pc



------------------------------------------------------------------------
				Memory
------------------------------------------------------------------------

Every cycle, the memory outputs mem[ma]
At clock tick:
  if m_sto
    then mem[ma] := memdat

  {This is a slight simplification: there are some further
   issues concerning memory and Input/Output}

ma = memory address, a choice among {ad, pc}
ma = if ctl_ma_pc = 0
       then ad
       else pc

ctl_ma_pc = 1 means "use pc as the memory address"


---------------------------------------------------------------------------
	       Instruction set architecture of Sigma16
---------------------------------------------------------------------------

The M1 circuit does not fully implement Sigma16.  It doesn't handle
overflow, interrupts, or I/O apart from the bootstrap loader, and the
following instructions are not implemented: mul, div, the logic and
shift instructions.

RRR instructions
~~~~~~~~~~~~~~~~

 op   format  mnemonic   operands   action
---- -------- ---------- ---------- ------------------------------------
 0     RRR    add        R1,R2,R3   R1 := R2+R3
 1     RRR    sub        R1,R2,R3   R1 := R2-R3
 2     RRR    mul        R1,R2,R3   R1 := R2*R3, R15 := high word
 3     RRR    div        R1,R2,R3   R1 := R2/R3, R15 := R2 mod R3
 4     RRR    cmplt      R1,R2,R3   R1 := R2<R3
 5     RRR    cmpeq      R1,R2,R3   R1 := R2=R3
 6     RRR    cmpgt      R1,R2,R3   R1 := R2>R3
 7     RRR    inv        R1,R2,R3   R1 := bwinv R2
 8     RRR    and        R1,R2,R3   R1 := R2 bwand R3
 9     RRR    or         R1,R2,R3   R1 := R2 bwor R3
 a     RRR    xor        R1,R2,R3   R1 := R2 bwxor R3
 b     RRR    shiftl     R1,R2,R3   R1 := R2 shiftl R3 bits
 c     RRR    shiftr     R1,R2,R3   R1 := R2 shiftr R3 bits
 d     RRR    trap       R1,R2,R3   trap interrupt
 e     XX                           (expand to XX format)
 f     RX                           (expand to RX format)

RX instructions
~~~~~~~~~~~~~~~

 op   b   format  mnemonic   operands   action
---- --- -------- ---------- ---------- ---------------------------------
 f    0     RX    lea        Rd,x[Ra]   Rd := x+Ra
 f    1     RX    load       Rd,x[Ra]   Rd := mem[x+Ra]
 f    2     RX    store      Rd,x[Ra]   mem[x+Ra] := Rd
 f    3     RX    jump       x[Ra]      pc := x+Ra
 f    4     RX    jumpf      Rd,x[Ra]   if Rd==0 then pc := x+Ra
 f    5     RX    jumpt      Rd,x[Ra]   if Rd/=0 then pc := x+Ra
 f    6     RX    jal        Rd,x[Ra]   Rd := pc, pc := x+Ra


---------------------------------------------------------------------------
			    The M1 circuit
---------------------------------------------------------------------------

M1 implements a subset of the Sigma16 architecture.  It is designed
for simplicity, and not optimized for speed.  It assumes that the
memory runs at cache speed, and there is no separate cache.  There is
a rudimentary I/O system that uses DMA to boot up the system by
inputting a block of data and then jumps to location 0 to begin
execution.  There is no instruction level parallelism.

Some of the features of Sigma16 are not implemented in M1.  That means
that some programs will give different results in the Sigma16 emulator
and on the M1 circuit.  These features include some unimplemented
instructions (mult, div, shifting, and logic).

The M1 circuit consists of three main subsystems: datapath, control,
and memory.  Inside the datapath is another subsystem, the alu.  Each
of these is defined in a separate module, and there are separate
simulation drivers for the alu, datapath, and control.  The m1 circuit
connects the subsystems together and handles bootstrapping.


---------------------------------------------------------------------------
		    ALU (arithmetic & logic unit)
---------------------------------------------------------------------------

Inputs to ALU are x and y

Operation to be performed is specified by an ALU opcode consisting of
4 bits: ctl_alu_a, ctl_alu_b, ctl_alu_c, ctl_alu_d.  In the table
below, call them just a,b,c,d.

The ALU performs addition, subtraction, negation, increment, and
comparision for <, =, and >.  It receives four control signals
(a,b,c,d) that determine which function it will compute.  These are
shown in the following table, where don't-care values are left blank.

Note that the alu doesn't implement some of the operations used by RRR
instructions.  Those instructions will actually do nothing.

  |~~~~~~~~~~~~~~~~~~~~~~~~|
  | Function |  a,b   c,d  |
  |~~~~~~~~~~|~~~~~~~~~~~~~|
  |   x+y    |  0 0        |
  |   x-y    |  0 1        |
  |    -x    |  1 0        |
  |   x+1    |  1 1   0 0  |
  |   x<y    |  1 1   0 1  |
  |   x=y    |  1 1   1 0  |
  |   x>y    |  1 1   1 1  |
  ~~~~~~~~~~~~~~~~~~~~~~~~~~

........................................................
x is the first data input to the alu
x = a choice from among {a, pc}
x = if ctl_x_pc = 0
      then a
      else pc

ctl_x_pc = 1 means "make x = value in pc register"

........................................................
y is the second data input to the alu
y = a choice from among {b, ad}
y = if ctl_y_ad = 0
      then b
      else ad

ctl_y_ad = 1 means "make y = value in ad register"



------------------------------------------------------------------------
		 ir register (instruction register)
------------------------------------------------------------------------  

  holds the instruction currently being executed
At clock tick
  if ctl_ir_ld = 1
    then ir := memdat


------------------------------------------------------------------------
		    pc register (program counter)
------------------------------------------------------------------------

  holds the address of the next instruction word to be fetched
At clock tick
  if ctl_pc_ld = 1
    then pc := q

q is the data input to the pc register
q is a choice from among {r, ad}
q = if ctl_pc_ad = 0
      then r
      else ad


------------------------------------------------------------------------
		    ad register (address register)
------------------------------------------------------------------------

(sometimes called adr, just a spelling inconsistency.
 ad and adr are the same register)

At clock tick
  if ctl_adr_ld = 1
     then ad := (if ctl_adr_alu = 0
                  then memdat
                  else r)

This register holds the second word of an RX instruction
  (when this is fetched, we need adr := memdat)

It is also is used for calculating the effective address
  (when this is calculated by the ALU, we need adr := r)


------------------------------------------------------------------------
		       List of control signals
------------------------------------------------------------------------

   ctl_rf_alu,  -- Input to register file is ALU output r (if 0, use m)
   ctl_rf_sd,   -- Use ir_d as source a address (if 0, use ir_sa)
   ctl_alu_a,   -- 4-bit alu operation code (see section on the ALU)
   ctl_alu_b,   --   "
   ctl_alu_c,   --   "
   ctl_alu_d,   --   "
   ctl_ir_ld,   -- Load ir register (if 0, remain unchanged)
   ctl_pc_ld,   -- Load pc register (if 0, remain unchanged)
   ctl_pc_ad,   -- Input to pc is ad (if 0, r)
   ctl_adr_ld,  -- Load ad register (if 0, remain unchanged)
   ctl_adr_alu, -- Obtain ad input from alu (if 0, from memory data input)
   ctl_ma_pc,   -- Transmit pc on memory address bus (if 0, transmit addr)
   ctl_x_pc,    -- Transmit pc on x (if 0, transmit reg[sa])
   ctl_y_ad,    -- Transmit ad on y (if 0, transmit reg[sb])
   ctl_sto      -- Memory store (if 0, fetch)


---------------------------------------------------------------------------
	       Running machine language on the circuit
---------------------------------------------------------------------------

Create a directory containing the following files:

  ALU.hs                ALU circuit
  ALUrun.hs             simulate the ALU by itself
  ControlSignals.hs     defines the set of control signals
  Datapath.hs           specifies the datapath circuit
  DatapathRun.hs        simulate the datapath by itself
  Control.hs            control algorithm and control circuit
  ControlRun.hs         simulation driver for control
  Memory.hs             specifies the memory circuit
  M1.hs                 specifies the full system circuit
  M1run.hs              simulation driver for full system

  ArrayMax.asm.txt      Sigma16 source program in assembly language
  ArrayMax.hs           Sigma16 program in machine language

The program ArrayMax traverses an array of numbers to find the maximum
element.  The program is written by hand in assembly language, in
ArrayMax.asm.txt.  It was translated to machine language by the
Sigma16 application.  This machine language is copied into
ArrayMax.hs, in the form of a list of strings containing hex words.

ArrayMax.hs executes the program by invoking the circuit simulator on
M1.  This loads the machine language code into the memory of the M1
circuit and simulates the circuit at the level of logic gates and flip
flops.

The simulation driver produces voluminous output that lets you see
what is happening inside the circuit.  You can run the simulation two
ways: interactively with ghci, or as a batch process with ghc.

Interactive execution
~~~~~~~~~~~~~~~~~~~~~

While developing a circuit, it is easiest to use ghci.  This is
interactive and provides commands that are helpful for looking at
error messages, debugging, and performing quick experiments.  Enter
these commands ($ is the shell prompt, : is the ghci prompt).

   $ ghci
   :load ArrayMax
   main
   :q

Batch execution
~~~~~~~~~~~~~~~

For long simulations it is easiest to use ghc.  This runs the
simulations faster, and it can save the simulation output in a text
file.  That's helpful because you can use a text editor to search the
simulation output for significant events (this is described later).
At the shell prompt $, enter this command.  It runs the machine
language program in ArrayMax by simulating the M1 circuit, and saves
the simulation output in the file simulation.txt.

   $ ghc -e main ArrayMax >simulation.txt

The Sigma16 application
~~~~~~~~~~~~~~~~~~~~~~~

There is also a separate application called Sigma16 that provides a
graphical user interface, an integrated development environment (IDE),
an assembler, linker, and emulator.  The emulator executes machine
language programs by software interpretation.  The emulator provides a
reference semantics for Sigma16, and you can use it to check whether
M1 is working correctly.


---------------------------------------------------------------------------
	      Interpreting the simulation output
---------------------------------------------------------------------------

The M1 system contains a lot of components and signals, and its
simulation produces a large amount of output.  This section explains
what the simulation driver does, and how to interpret its output.

M1 is a digital circuit, and its inputs and outputs are bit signals.
There are hundreds of bits for every clock cycle, and even a small
program will execute for hundreds of clock cycles.  The simulation
would be unreadable if all this data were represented with 1s and 0s.
Using hex notation helps, but massive hex data is still hard to read.
Therefore a simulation driver is used, which

* converts the inputs (the machine language code written as hex string
  constants) into input bit signals,

* invokes the circuit simulator, and

* converts the output bit signals produced by the circuit into
  readable formatted strings.

We will use the program ArrayMax as a running example.  There is an
assembly language source program, ArrayMax.asm.txt.  This needs to be
translated to machine language ("assembled").  That can be done with
the Sigma16 application, which contains an assembler and emulator.

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
Clock cycle 0
Computer system inputs
         reset=0 dma=1 dma_a=0000 dma_d=f100
ctl_start = 0
...........................................................................

During Cycle 0, the system is using DMA to load the first word of the
machine code into memory at address 0.  This process --- placing
initial data into the memory --- is called bootstrapping.  The value
of the first word is f100 and it goes into address 0.  Therefore the
system sets these signals: reset=0 (don't start the processor yet);
dma=1 (place a word of input into memory), dma_a=0000 (the address to
put the word is 0000), and dma_d=f100 (the value of the data).  The
processor isn't running and not ready to start, so ctl_start=0.  These
signals are set during cycle 0, and the state change they request will
occur at the next clock tick, which ends cycle 0 and begins cycle 1.
The remainder of the simulation output for Cycle 0 contains nothing of
interest, as the processor is idle.

In Cycle 1, the next word of the machine language (checking the
machine language code, you'll see this is 0001) is loaded into the
next memory location (0001), so the driver sets the following system
inputs:

...........................................................................
Clock cycle 1
Computer system inputs
         reset=0 dma=1 dma_a=0001 dma_d=0001
...........................................................................

The simulation driver continues to load the machine language into
memory one word at a time.  The last word, 0019, is stored at location
001e at the tick ending Cycle 30.

...........................................................................
Clock cycle 30
Computer system inputs
         reset=0 dma=1 dma_a=001e dma_d=0019
ctl_start = 0
...........................................................................

At this point, the system as completed the bootstrapping input, and
the computer's memory contains the executable program with its data.

In Cycle 31, the m1 system starts the control algorithm by asserting
the main system reset signal.  This causes, in turn, the value of
ctl_start to become 1.  The effect of this is that, at the next clock
tick, the control circuit will put a 1 in the state flip flop for the
first state in the control algorithm, which is st_instr_fet.  Of
course, during cycle 31 the control is not running yet, so
st_instr_fet is still 0.

...........................................................................
Clock cycle 31
Computer system inputs
         reset=1 dma=0 dma_a=0000 dma_d=0000
ctl_start = 1

Control state
   st_instr_fet = 0  st_dispatch = 0       st_add = 0       st_sub = 0
...........................................................................

The effect of the reset occurs at the clock tick between cycles 31 and
32, so it becomes visible when we examine the values of signals during
cycle 32.  Now reset and dma are both 0, and they will remain so for
the duration of the execution.  The Control state section shows that
the flip flop named st_instr_fet contains 1, while all others contain
0.

The simulation driver outputs the values of key signals each clock
cycle.  This is one of the basic functions of every simulation driver.
However, M1run goes beyond that: it contains algorithms that watch the
signal values, looking for important events, and when it detects one
it prints a special message surrounded by two lines of asterisks.
During Cycle 31 the driver detects a very important event, as it sees
that ctl_start=1, and it prints the following:

************************************************************************
Reset: control algorithm starting
************************************************************************

As you scroll through the simulation output, messages like this will
appear from time to time.  A subtle but important point is that there
isn't any code that simply prints such a message when the simulation
reaches a certain point.  Instead, the driver observes and analyses
the signals coming from the circuit.

The section "Control signals" shows the value of every control signal,
as generated by the control circuit.  It has set the following signals
to 1: ctl_alu_a, ctl_alu_b, ctl_ir_ld, ctl_pc_ld, ctl_ma_pc,
ctl_x_pc.  The others are all 0.

First let's look at how the control signals are generated; then we
consider what they mean and what effect they have on the datapath.

Each control signal is generated by an or gate that or's the flip
flops for the states in which that signal is asserted.  In Control.hs,
you can see that the or gates for these signals that are 1 in Clock
Cycle 32 include st_instr_fet among their inputs.  For example, here
is the definition of the signal ctl_alu_a:

      ctl_alu_a   = orw [st_instr_fet,st_load0,st_store0,st_lea0,
                         st_cmpeq,st_cmplt,st_cmpgt,st_jumpf0,st_jal0]

The orw building block computes the logical or of the word; since
st_instr_fet is 1, the output of orw is 1 and ctl_alu_a has the value
1.  The same holds for ctl_alu_b, ctl_ir_ld, ctl_pc_ld, ctl_ma_pc, and
ctl_x_pc.  All of the other signals do not have st_instr_fet among the
inputs to their any1 circuit, so those signals are 0.

........................................................................
Clock cycle 32
Computer system inputs
         reset=0 dma=0 dma_a=0000 dma_d=0000
ctl_start = 0

Control state
   st_instr_fet = 1  st_dispatch = 0       st_add = 0       st_sub = 0
        st_mul0 = 0     st_cmplt = 0     st_cmpeq = 0     st_cmpgt = 0
       st_trap0 = 0      st_lea0 = 0      st_lea1 = 0     st_load0 = 0
       st_load1 = 0     st_load2 = 0    st_store0 = 0    st_store1 = 0
      st_store2 = 0     st_jump0 = 0     st_jump1 = 0    st_jumpf0 = 0
      st_jumpf1 = 0    st_jumpt0 = 0    st_jumpt1 = 0      st_jal0 = 0
        st_jal1 = 0

Control signals
    ctl_alu_a   = 1  ctl_alu_b   = 1  ctl_alu_c   = 0  ctl_alu_d   = 0
    ctl_x_pc    = 1  ctl_y_ad    = 0  ctl_rf_ld   = 0  ctl_rf_pc   = 0
    ctl_rf_alu  = 0  ctl_rf_sd   = 0  ctl_ir_ld   = 1  ctl_pc_ld   = 1
    ctl_ad_ld   = 0  ctl_ad_alu  = 0  ctl_ma_pc   = 1  ctl_sto     = 0

Datapath
    ir = 0000  pc = 0000  ad = 0000   a = 0000   b = 0000   r = 0001
     x = 0000   y = 0000   p = f100  ma = 0000  md = 0000 cnd = 0

Memory
   ctl_sto = 0      m_sto = 0
     m_addr = 0000  m_real_addr = 0000  m_data = 0000  m_out =f100
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
This occurs in cycles 55, 66, 80, 91, 107, 118, 132, 143, and 159.
Reading the control algorithm, we see that a decision is made base on
the first operand register the ir_d register:

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
   55       56       st_instr_fet      no
   66       67       st_jumpf1        yes
   80       81       st_instr_fet      no
   91       92       st_instr_fet      no
  107      108       st_instr_fet      no
  118      119       st_jumpf1        yes
  132      133       st_instr_fet      no
  143      144       st_instr_fet      no
  159      160       st_instr_fet      no

So we have found several examples where the jump is taken, and several
where it is not.  It's worth picking one example of each case, and
studying it in detail.

An especially valuable experiment is to pick some key points in the
simulation so that you can follow the execution of the machine
language program, as it searches through the array looking for the
maximal element.

