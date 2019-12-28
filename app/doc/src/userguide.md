% User Guide

# Introduction

Sigma16 is a computer architecture designed for research and teaching
in computer systems.  This application provides a complete environment
for experimenting with the architecture, including an editor,
assembler, linker, emulator, and an integrated development
environment.  There is a digital circuit that implements the
architecture, which is specified using the Hydra functional hardware
description language.  Hydra can simulate the circuit, and machine
language programs can run on both the emulator and the circuit.

This reference manual is organised by topic, with chapters on the
architecture, the assembly language, and the emulator.  However, it's
a good idea to begin with an overview of how the whole system works,
and to be able to write and run simple programs, before delving into
the details.  For a quick start, begin with the tutorials, which show
you how to enter and run a program, and how to use the programming
environment.

# Tutorials

The following short tutorials show you how to use the system.  You can
keep the tutorials visible in the right panel while following along
with the exercises in the main panel.

### Tutorial: run an example program

*Topics:* components of the user interface, adjusting size of the user
guide, the tabs for switching among the working areas, and the steps
for running a program.  Registers, constants and addition.

Let's start by looking at the different parts of the user interface.
The main window contains three main sections.  The largest area, on
the left side, is the *main working area*.  When the program launches,
this will show the Welcome pane.  The *user guide* is on the right
side.  At the top is a row of buttons (Welcome, Examples, ...).  These
select which page is displayed in the main working area.

You can adjust how much space is used for the working area and the
user guide by clicking the arrow buttons on the right side of the
button row.  These move the boundary between the two areas left or
right.  If you reisze the window, the relative sizes of the working
area and the user guide will remain the same.

A good way to get started is to go through the entire process of
entering a simple program, assembling it, and running it.  This
section shows you how.  For now, we focus just on how to use the
software tools; an explanation of the Sigma16 architecture comes
later.

When you launch the application, a window will appear in your browser.
It has several panes, with buttons at the top to switch between panes.
The Welcome pane will be visible.

 * Click **Editor** to show the Editor Pane. *(You can edit code here
   or use an external text editor and copy/paste text into the
   editor.)*

 * Click **Example** to load a simple example program.

 * Click the **Assembler** tab.  The assembler page is where you
   translate a program from assembly language to machine language.

 * Click the **Assemble** button.  The assembly listing, showing the
   translation to machine language, will appear.  This example program
   doesn't produce any error messages, so we can move on.

 * For this simple example, we don't need the Linker, so you can skip
   it. *(The linker is needed for programs with multiple modules, or
   with external references, or that need relocation.)*
  
 * Click the **Processor** tab, which shows the main components of the
   processor. This is where you can run programs using the emulator.
   The state of the processor is displayed as it runs your program,
   and the assembly listing is shown below, to help you follow the
   execution of the program.

 * Click **Boot**.  This reads the machine language program into the
   memory, and you can see it in the Memory display.  There are two
   independent views into the memory; this is convenient for looking
   at the machine language code in one view and the data in the other
   view.  (Despite the two views, there is just one memory!)  At this
   point the pc register contains 0, meaning that the next instruction
   to be executed is the one in memory location 0.  The ir and other
   registers also contain 0, but that is just the initial value.

 * Click **Step**.  The emulator executes a single instruction and
   displays the effects on the registers and memory: blue for using a
   value, and red for modifying it.

 * Click Step repeatedly to watch the program execute, instruction by
   instruction.

 * To rerun the program, click Boot again.

 * To run the program slowly, click **Run Display**, which is similar
   to clicking Step repeatedly.  To run the program faster but without
   updating the display after each instruction, click Run.  At any
   time you can click Pause to stop the processor, and you can resume
   execution with any of Step, Slow, or Run.

  * To set a breakpoint, click Breakpoint and enter the stopping
    condition in the dialogue box.  For example, to stop when the pc
    register becomes $01b7, enter BPeq BPpc (BPhex "01b7").  Then
    click Run, and the emulator will run at full speed until the pc
    reaches the specified value; then it will stop so you can examine
    the state of the machine.

### Tutorial: load, store, and arithmetic

### Tutorial: comparisons and jumps

### Tutorial: effective addresses and arrays

### Tutorial: further instructions

# Architecture

An *instruction set architecture* is a precise specification of all
aspects of a machine that are visible to a programmer.  It includes a
description of the registers, memory, data representations, and all
the instructions, but does not include components of the
implementation that are not visible to a machine language programmer.


## Data representation

Sigma16 is a 16-bit architecture, and every data value is a 16-bit
word.  Integers are represented in 16-bit two's complement notation.
The bits of a word are numbered from left to right, starting with 0.
Thus the leftmost (most significant) bit of a word is bit 0, and the
rightmost (least significant) is bit 15.

## Register file

The **register file** is a set of 16 general registers that hold a 16
bit word.  A register is referenced by a 4-bit binary number.  In
assembly language, we use the notations R0, R1, R2, ..., R9, R10, R11,
R12, R13, R14, R15 to refer to the registers.

Sigma16 is a load/store style architecture; that is, it does not
combine memory accesses with arithmetic.  All calculations are carried
out in the register file, and explicit load and store instructions
must be used to copy data between the memory and the register file.

There are some programming conventions that use certain registers for
special purposes.  The hardware does not enforce, or even know about,
these conventions, and you do not have to follow the conventions in
programming.  However, it is necessary to obey the conventions in
order to use the standard software libraries in your program.  See the
section on Programming for a discussion of these standard usage
conventions.

### R0 holds the constant 0

One of the registers, R0, has a special property: it always contains
the constant 0.  It is legal to perform an instruction that attempts
to load some other value into R0, but the register will still contain
0 after executing such an instruction.  Such an instruction will
simply have no lasting effect.

### R15 is the condition code register

Several instructions produce status information: the result of a
comparison, whether there was an overflow, etc.  This information is
automatically loaded into R15, which is the condition code register.
The description of each instruction states whether R15 is modified,
and what goes into it.

The bits in R15 are indexed from bit 0 (the most significant, or
lefttmost bit) to bit 15 (the least significant, or rightmost).  The
condition code bits that have specific meanings are called *flags*.

Table: Condition code flags

<table>
  <tr>
    <th>Bit</th>
    <th>Flag</th>
    <th>Meaning</th>
  </tr>
  <tr>
    <td>0</td>
    <td>**G**</td>
    <td> gt (or gt 0) unsigned (binary)</td>
  </tr>
  <tr>
    <td>1</td>
    <td>**g**</td>
    <td> gt (or gt 0) signed (two's complement)</td>
  </tr>
  <tr>
    <td>2</td>
    <td>**E**</td>
    <td>= (or =0) word, signed, unsigned</td>
  </tr>
  <tr>
    <td>3</td>
    <td>**l**</td>
    <td>   lt (or lt 0) signed (two's complement)</td>
  </tr>
  <tr>
    <td>4</td>
    <td>**L**</td>
    <td>    lt (or  lt 0) unsigned (binary)</td>
  </tr>
  <tr>
    <td>5</td>
    <td>**V**</td>
    <td>    unsigned overflow (binary)</td>
  </tr>
  <tr>
    <td>6</td>
    <td>**v**</td>
    <td>    signed overflow (two's complement)</td>
  </tr>
  <tr>
    <td>7</td>
    <td>**C**</td>
    <td>    carry propagation (binary)</td>
  </tr>
  <tr>
    <td></td>
    <td></td>
    <td></td>
  </tr>

</table>

The condition code flags are defined to make the condition code easier to read in
hex:

* The first (leftmost) hex digit holds the comparison flags

* The second hex digit holds the carry and overflow flags

* The third hex digit holds the stack error flags

conditional jump

jumpc0  jump if cc bit is 0
jumpc1  jump if cc bit is 1

## Memory

The memory is a hardware array of words that are accessed by address.
A memory address is 16 bits wide, and there is one memory location
corresponding to each address, so there are 2^16 = 64k memory
locations.  Each memory location is a 16-bit word.

### Effective address

The effective address is defined to be the binary sum of the
displacement and the index register.

## Instruction control registers

There are several **program control registers** that enable the
processor to keep track of the state of the running program.  These
registers are rarely used directly by the machine language program,
but they are essential for keeping track of the execution of the
program, and some instructions use them directly.

 * ir -- instruction register (16-bit word)

 * pc -- program counter (16-bit word)

 * adr -- address register (16-bit word)

 * dat -- data register (16-bit word)
 
 * status -- collection of control flags

### Status register flags

The processor can be executing in several modes, which are determined
by the **system control registers**.

 * sys (bit 0)  -- system state (1-bit flag)

 * ie (bit 1)   -- interrupts enabled (1-bit flag)

Table: Processor status flags

<table>
  <tr>
    <th>Bit</th>
    <th>Flag</th>
    <th>Meaning</th>
  </tr>
  <tr>
    <td>0</td>
    <td>**U**</td>
    <td>User state</td>
  </tr>
  <tr>
    <td>1</td>
    <td>**E**</td>
    <td>Interrupts enabled</td>
  </tr>
</table>

## Interrupts and exceptions

 * imask
 
 * ireq
 
 * istatus
 
 * ipc
 
 * ivect


### Mask and request flags

<table>
  <tr>
    <th>Bit</th>
    <th>Flag</th>
    <th>Meaning</th>
  </tr>
  <tr>
    <td>0</td>
    <td>Trap</td>
    <td>Trap</td>
  </tr>
  <tr>
    <td>0</td>
    <td>Trap</td>
    <td>Trap</td>
  </tr>
  <tr>
    <td>1</td>
    <td>Overflow</td>
    <td>Trap</td>
  </tr>
  <tr>
    <td>2</td>
    <td>Div0</td>
    <td>Trap</td>
  </tr>
  <tr>
    <td>3</td>
    <td>StackFault</td>
    <td>Trap</td>
  </tr>
  <tr>
    <td>4</td>
    <td>SegFault</td>
    <td>Trap</td>
  </tr>
  <tr>
    <td>5</td>
    <td>Privelege</td>
    <td>Trap</td>
  </tr>
  <tr>
    <td>6</td>
    <td>Timer</td>
    <td>Interrupt</td>
  </tr>
  <tr>
    <td>7</td>
    <td>Input</td>
    <td>Interrupt</td>
  </tr>
  <tr>
    <td>8</td>
    <td>Output</td>
    <td>Interrupt</td>
  </tr>
</table>

## Instruction representation

There are three instruction formats:

* RRR -- (1 word) Instructions that perform operations on data
         in registers, but not referring to memory.

* RX  -- (2 words) Instructions that specify a memory location,
         as well as a register operand.

* EXP -- (2 words) Expanded (or experimental) instructions, for
         instructions that cannot be encoded as RRR or RX.

The first word of an instruction contains four 4-bit fields, named op
(bits 0-3), d (bits 4-7), sa (bits 8-11), and sb (bits 12-15).

<table class="wordlayout"">
 <tr>
  <th>op</th>
  <th>d</th>
  <th>a</th>
  <th>b</th>
 </tr>
 <tr>
  <td>0-3</td>
  <td>4-7</td>
  <td>8-11</td>
  <td>12-15</td>
 </tr>
</table>

Each instruction has a 4-bit field called the opcode (op for short).
This gives 16 values of the opcode: 14 of them (0 through 13) denote
the 14 RRR instructions.  If the op field is 14 (hex e) the
instruction is EXP format and has a secondary opcode in the a and b
fields.  If the op field contains 15 (hex f) the instruction is RX
format with a secondary opcode in the b field.

### RRR format

An RRR instruction contains an operation code (op), and specifies
three operands registers using the d, sa, and sb fields.  It is
represented as one word, which is divided into four fields:

  * op  (4 bits, starting from bit 0) Operation code
  * d   (4 bits, starting from bit 4) Destination register
  * sa  (4 bits, starting from bit 8) Source a register
  * sb  (4 bits, starting from bit 12) Source b register

<table class="wordlayout"">
  <tr>
    <th>op</th>
    <th>d</th>
    <th>a</th>
    <th>b</th>
  </tr>
  <tr>
    <td>d</td>
    <td>1</td>
    <td>2</td>
    <td>3</td>
  </tr>
  <tr>
    <td>0-3</td>
    <td>4-7</td>
    <td>8-11</td>
    <td>12-15</td>
  </tr>
</table>

The op field of an RRR instruction must be in the range from 0 through
13 (hex 0 through d).  This allows for a total of 14 distinct RRR
instructions.  If the op field is outside this range, it indicates an
"expanding opcode": 14 (hex e) indicates the EXP format, and 15 (hex
f) indicates the RX format.

A RRR instruction is written with an instruction name (menonic) and
three register operands.  For example, the "trap" instruction has
mnemonic 13 (and the hex value of 13 is d), so the assembly language
"trap R4,R12,R2" is translated to machine language as d4c2.

In most cases, an RRR instruction takes two operands in registers
specified by the a and b fields and produces a result which is loaded
into the register specified by the d field.  A typical example of an
RRR instruction is add R4,R9,R2, which adds the contenst of registers
R9 and R2, and loads the result into R4.  It's equivalent to R4 :=
R9 + R2.

### RX format

The RX instruction format is used for instructions that use a memory
address, which is specified by an index register and a displacement.
The name of the format describes briefly the two operands: a register
(R) and an indexed memory address (X).

An RX instruction contains two operands: one is a memory address, and
the other is a register.  Typical RX instructions are loads, stores,
and jumps.  The instruction consists of two consecutive words.  The
first has the same format as an RRR instruction, with four fields: op,
d, sa, sb.  The second word is a single 16-bit binary number, and is
called the displacement.

The memory address is specified in two parts: an index register and
the displacement.  The index register is specified in the sa field.
In assembly language, the notation used is number[reg], where the
number is the value of the displacement, and the reg is the index
register.  Thus $20b3[R2] means the address has displacement $20b3 and
the index register is R2.

When the machine executes an RX instruction, it begins by calculating
the effective address.  This is abbreviated "ea", and its value is the
sum of the displacement and the contents of the index register.

RX instructions are represented in two words, and they use an
"expanding opcode".  That is, the op field of the first word of the
instruction contains the constant f (the bits 1111) for every RX
instruction, and the sb field is used to hold a secondary opcode
indicating which RX instruction it is.

The register operand is specified in the d field.  For several RX
instructions, this is indeed the destination of the instruction: for
example, load places data into Rd.  However, a few RX instructions use
the d field differently (see, for example, the conditional jump
instructions).

The memory address is specified using the sa field and the
displacement, which is the entire second word of the instruction.

  * op field (bits 0-3 of ir) is f for all RX instructions
  * d field (bits 4-7 of ir) has several uses
  * a field (bits 8-11 of ir) is index register for effective address
  * b field (bits 12-15 of ir) is secondary opcode
  * disp (displacement) is the second word of the instruction
  * ea (effective address) = displacement + r[a]

### EXP format

The EXP instruction format is used for expanded instructions cannot be
represented using the RRR or RX formats.  It provides many unused
opcodes, so it is useful for experimental instructions.

An EXP instruction consists of two words.  The first word has a
constant hex e in the op field, which indicates the EXP format.  The a
and b fields together form an 8-bit secondary opcode, allowing for 256
distinct EXP instructions.  The d field in the first word, and all of
the second word, hold operands which depend on the particular
instruction.

First word of the instruction

<table class="wordlayout"">
<tr>
<th>op</th>
<th>d</th>
<th>a</th>
<th>b</th>
</tr>
<tr>
<td>0-3</td>
<td>4-7</td>
<td>8-11</td>
<td>12-15</td>
</tr>
</table>

Second word of the instruction

<table class="wordlayout"">
  <tr>
    <th>p</th>
    <th>q</th>
    <th>r</th>
    <th>s</th>
  </tr>
  <tr>
    <td>0-3</td>
    <td>4-7</td>
    <td>8-11</td>
    <td>12-15</td>
  </tr>
</table>

## Summary of notation

  ea
  m[ea]
  r[d]
  r[a]
  r[b]
  q#r[d]
  lsb q

# Instruction set

The following sections describe the instructions in groups organized
by their function.  Some of the groups contain instructions with
different formats.  From the programmer's perspective the function is
more important, so these groups are useful in finding the right
instruction to use.  (From the perspective of designing a digital
circuit to impleemnt the architecture, the format is essential.)

## Arithmetic instructions

### add, sub

add R1,R2,R3
sub R1,R2,R3

The last two registers (R2 and R3) are the *operands*; the sum (for
add) or difference (for sub) is placed in the first register (R1),
which is called the *destination*.  The add instruction corresponds to
an assignment statement. These are RRR instructions: add has opcode 0;
sub has opcocde 1.

    Code      Assembly          Effect
    062c      add R6,R2,R12     ; R6 := R2 + R12
    0d13      add R13,R1,R3     ; R13 := R1 + R3



Code    Assembly          Effect
-----   ----------------  ------------------
062c    add R6,R2,R12     ; R6 := R2 + R12
0d13    add R13,R1,R3     ; R13 := R1 + R3

The add instruction can be used for both binary addition (on natural
numbers) and for two's complement addition (on signed integers).

* 16-bit natural numbers are unsigned integers 0, 1, 2, ..., 65535.
  If two natural numbers are added, the result is a natural number
  (the result cannot be negative).  If the result is 65536 or larger,
  it cannot be represented as a 16 bit binary number.  If this
  happens, the destination register is set to the lower 16 bits of the
  true result, and the binary overflow flag is set in the Condition Code.
  
* 16-bit two's complement numbers are signed integers -32999?, ...,
  -1, 0, 1, ..., 32???.  If two signed integers are added, the result
  is a signed integer.  If the result is less than -32000 or greater
  than 32000, then the result cannot be represented as a 16 bit two's
  complement number.  If this happens, the destination register is set
  to the lower 16 bits of the true result, and the two's complement
  overflow flag is set in the Condition Code.  Furthermore, the
  overflow flag is set in the req register.  If interrupts are enabled
  and the overflow flag is 1 in the mask register, then an interrupt
  will occur immediatelhy after the add instruction executes.

###, sub



add. The two operands are fetched from registers, added, and the sum
is loaded into the destination register.

### mul

### div

### addc

## Accessing memory

### lea

The effective address is defined to be the binary sum of the
displacement and the index register.

### load

Load copies the word in memory at the effective address into the
destination register.

general form: load Rd,disp[Ra]

effect: reg[Rd] := mem[disp+reg[Ra]]

format: RX

    load  R12,count[R0]   ; R12 := count
    load  R6,arrayX[R2]   ; R6 := arrayX[R2]
    load  R3,$2b8e[R5]    ; R3 := mem[2b8e+R5]

### store

Store copies the word in the destination register into memory at the
effective address.  Unlike most instructions, store treats the
"destination register" as the source of data, and the actual
destination is the memory location.

    general form: store Rd,disp[Ra]
    effect: mem[disp+reg[Ra]] := reg[Rd]
    format: RX


    store  R3,$2b8e[R5]
    store  R12,count[R0]
    store  R6,arrayX[R2]


### save, restore

### Representation of stacks

A stack is represented as a block of words with consecutive addresses
sb, sb+1, sb+2, ..., sl.  There are three instructions that operate on
stacks: push, pop, and top.  Each of these instructions checks to
ensure that the operation is valid, and indicates an error condition
of not.  Four variables are needed to use a stack:

* sb (stack base) = address of the first word in the block

* sl (stack limit) = address of the last word in the block; for the
  stack to function properly it is necessary that sb < sl.

* st (stack top) = pointer to the top element.  There are three cases.
  If the stack contains at least one element, then st is its address
  and sb <= st <= sl.  If the stack is empty, then st=sb-1.  If the
  stack is full, then st=sl.
  
* x (data) = value to be pushed onto the stack, or result of a pop or
  top operation

There are three instructions that operate on stacks: push, pop, and
top.  Push inserts the value in the destination register at the top of
the stack, pop removes the top of the stack and loads it into the
destination register, and top loads the top element into the
destination register without removing it from the stack.  For all
three instructions, if the operation is impossible then an error is
signaled in the condition code register and the interrupt request
register.

### push

    push R1,R2,R3

    R1 = value to push onto stack in memory
    R2 = stack top
    R3 = stack limit

Push the word in R1 onto a stack with top R2 and limit R3.  If the
stack is full, nothing is stored into memory and an error is indicated
in the condition code and interrupt request registers; an interrupt
will occur if interrupts are enabled and the stack mask bit is set.

Push has the following semantics:

    if R2<R3
      then R2 := R2+1; mem[R2] := R1
      else R15.sovfl := 1, req.StackBounds := 1
  
If R2=R3 this means the stack completely fills the block, and there is
no space to store a new element.  In this case, the push instruction
does not store R1: it doesn't modify memory outside the block, and it
doesn't overwrite data in the stack.  Instead, the instruction
indicates a stack overflow by setting the sovfl (stack overflow) bit
in the condition code (R15), and it also sets the stack fault bit in
the interrupt request register.  If interrupts are enabled and the
stack fault bit is set in the interrupt mask register, then an
interrupt will occur after the push instruction completes.  But there
will be no interrupt if interrupts are disabled, or the stack fault
bit is not set in the mask register.

Push is an EXP format instruction comprising two words:

------ ------ ------ ------
  op     d      a      b   
 **e**   R1          **5**   
  0-2    3-6   7-10  11-15  
------ ------ ------ ------

### pop

The pop instruction removes the top element from a stack and loads it
into the destination register.  (See push for a description of
representation of a stack.)

    pop R1,R2,R3

    R1 = value popped from stack
    R2 = stack top
    R3 = stack base

    if R2>R3
      then R1 := mem[R2]; R2 := R2 - 1
      else R15.StackUnderflow := 1, req.StackBounds := 1

### top

     top  R1,R2,R3

     R1 = value of top of stack
     R2 = stack top
     R2 = stack base

     if R2>R2
       then R1 := mem[R1]
       else R15.StackEmpty := 1, req.StackBounds := 1


## Comparisons

### cmplt, cmpeq, cmpgt

### cmp

## Jumps

### jump

### jumpf, jumpt

### jumpc0, jumpc1

### jal

### Aliases for conditional jumps

## Bits and Booleans

### inv

### and, or, xor

### shiftl, shiftr

### getbit

### putbit

### putbitinv

## System control

### trap

### getctl, putctl

### rfi


## Summary of the instruction set

The following table shows the complete instruction set.  The
instructions are in order of increasing operation code.

***

 -----------------------------------------------
 Mnemonic Format Op   Notes  Effect
 -------- ------ ---  -----  -------------------
  add     RRR    0    E      r[d] := r[a] + r[b]   signed +

  sub     RRR    1    E      r[d] := r[a] - r[b]   signed -

  mul     RRR    2           rem#r[d] := r[a] * r[b] signed *

  div     RRR    3    E      r[d] := r[a] div r[b],
                             R15 := r[a] rem r[b]

  cmplt   RRR    4           r[d] := r[a] < r[b]

  cmpeq   RRR    5           r[d] := r[a] = r[b]

  cmpgt   RRR    6           r[d] := r[a] > r[b]

  cmp     RRR    7           R15 := comparison result
                             (both signed, unsigned)

  inv     RRR    8           r[d] := inv r[a]

  and     RRR    9           r[d] := r[a] and r[b]

  or      RRR    a           r[d] := r[a] or r[b]

  xor     RRR    b           r[d] := r[a] xor r[b]

          RRR   c            reserved; currently nop

  trap    RRR    d    E      xa := pc, pc := 0


  lea     RX     f,0         r[d] := ea

  load    RX     f,1  E      r[d] := m[ea]

  store   RX     f,2  E      m[ea] := r[d]

  jump    RX     f,3  E      pc := ea

  jumpc0  RX     f,4         if r[15] AND d == 0 then pc := ea

  jumpc1  RX     f,5         if r[R15] AND d /= 0 then pc := ea

  jumpf   RX     f,6  E      if not r[d] then pc := ea

  jumpt   RX     f,7  E      if r[d] then pc := ea

  jal     RX     f,8  E      r[d] := pc, pc := ea


  trap    RRR    d    E      xa := pc, pc := 0

  lea     RX     f,0         r[d] := ea

  load    RX     f,1  E      r[d] := m[ea]

  store   RX     f,2  E      m[ea] := r[d]

  jump    RX     f,3  E      pc := ea

  jumpf   RX     f,4  E      if not r[d] then pc := ea

  jumpt   RX     f,5  E      if r[d] then pc := ea

  jal     RX     f,6  E      r[d] := pc, pc := ea

  test    EXP                r[d] := r[a] [bit b]

  addl    EXP    e,0a        rem#r[d] := r[a] + r[b] + lsb rem

  shiftl  RRR    b           rem#r[d] := r[a] shl b

  shiftr  RRR    c           rem#r[d] := r[a] shr b

-------------------------------------------

***
    
# Assembly Language

A computer is a digital circuit that executes programs in machine
language, which is hard for humans to read because it consists
entirely of numbers.  Assembly language provides a readable notation
for writing machine language programs.  It uses names for instructions
and variables, as well as other notations to make the code easier to
understand.

An instruction in machine language is just one or more words (often
written in hexadecimal notation), while the corresponding instruction
in assembly language uses mnemonic names so the programmer doesn't
have to memorise all the operation codes, addresses of variables, and
so on. For example, the assembly language statement mul R12,R3,R8 is
more readable than the corresponding machine language instruction
2c38.  However, the assembly language still gives the programmer
complete control over every bit a program.

Each line of source code is an assembly language statement.  Unlike
higher level languages, assembly language statements are not nested.
There are three kinds of assembly language statement:

* Most statements specify either an instruction or some constant data.

* Some statements are full line comments.

* Other statements are *directives*, which control the behavior of the
assembler but don't generate any code.

## Notation

A name must begin with a letter (a-z or A-Z), and may contain letters,
digits, or underscore characters.
	
Constants can be written in decimal, hexadecimal, or binary:

* Decimal constants consist of a sequence of digits, with an optional
  leading - sign.  Examples: 42 55039 -1

* Hexadecimal constants are written with a dollar sign $ followed by
  four hex digits (0 1 2 3 4 5 6 7 8 9 a b c d e f).  Examples: $0249
  $c78a

* Binary constants are written with a hash sign # followed by any
  number of 0 or 1 characters.  You can write fewer than 16 bits; they
  will be padded on the left with zeros.  Examples: #1101
  #000100000001101

An expression denotes a 16-bit word.  Usually they are used to specify
a word of machine language, either an address or a data constant.

## Statement formats

A statement may contain several fields.  A field consists of non-space
characters (with one exception: a space may appear in a string
literal).  Fields are separated from each other by one or more white
space characters.

  * Label.  The label field is optional.  If present, the label must
    be a name and it must begin in the first character of the line.
    If the first character is a space, then that line has no label.  
	
  * Operation.  The operation field specifies an instruction or
    assembler directive.  It must be preceded by one or more white
    space characters.  Every statement (apart from a full line
    comment) must have an operation field, which must be a name.

  * Operands.  The operands field specifies operands for an
    instruction or arguments for assembly directive.  Most
    instructions and assembler directives require operands, but some
    do not.
	
  * Comment.  All text that follows white space after the operands
    field is a comment, and is ignored by the assembler.  If one or
    more of the other fields (label, operation, operands) is missing,
    the comment must be preceded by a semicolon to prevent it from
    being interpreted as operands.  The rule is: all text after a
    semicolon is a comment, and all text after white space following
    operands is a comment.  A statement where the first non-space
    character is a semicolon is a full line comment.  If the statement
    has no operands, then all text after the operation field is a
    comment.  It is good practice always to begin a comment with a
    semicolon.


## Instructions

## Directives

A directive is a statement that doesn't generate an instruction, but
which gives further information about how to translate the program to
object code.

#### module

A program may be organized as a collection of modules, where each
module appears in a separate file.  When several modules are present,
each one needs a unique name.  The purpose of the module statement is
to specify this name.

A small program can be written as just one module, and it may have a
module name but this is not required.

The module statement defines the name of a module.  The statement is
optional.  If present, it must be the first statement in the program,
apart from full line comments and blank lines.  A file may contain
only one module statement, and if the statement "MyModuleName module"
is present the file name should be MyModuleName.asm.txt.

Examples

    quicksort module
    main      module
    myprog    module

### org

The org statement specifies where in memory instructions and data
should be placed when the program is booted.  It takes an operand
consisting of a word value, and it sets the location counter to this
value.

Examples

    org   $0f20    ; subsequent instructions start from 0f20
    org  *+50      ; skip 50 words

The assembler initializes the location counter to 0 before it begins
translating an assembly language module.  This means that, in effect,
every module begins with org 0.

### import

Examples

    myfunction  import


### export

Examples

       export   xyz,fcn,k

## Assembly listing

The first section of the assembly listing shows each line of the
source program.  The line number appears first, followed by the memory
address that the instruction on this line will be placed in.  The
address is given as a 4 digit hexadecimal number, and it is binary
(not two's complement).  Next comes the machine language code
generated by the line of source code.  If the line contains a two-word
instruction, there will be two 4-digit hexadecimal values; for a
one-word instruction there will be one hex number, and if the line
doesn't produce any code these fields will be blank.  After the code,
the original source statement appears.

The second section of the assembly listing is the *Symbol Table*.
This shows each identifier (or "symbol") that appears in the program,
the address allocated for the symbol, the source code line where it
was defined, and the source code lines where it was used.

# Linker

(The linker is not fully implemented in this version of the
application; it's on the way.)

## Object language statements

hex4 means a 4-digit hexadecimal constant, such as 3b9f.

name means an identifier, beginning with a letter and comprising
letters, digits, _

org  hex4                       set location counter
data hex4 ... hex4              load words, incrementing loc counter
relocate hex4                   add relocation to word at address
external name                   insert value of external name
       


## Programs, modules, and files

The system is designed to allow programs that consist of several
modules, but also to allow programs consisting of one standalone
module.  In addition, an aim is to do this simply and intuitively, so
that you can ignore the issues of modules and linking if you just want
to write a andalone program.

There is a standard convention for file names.  If, for example, you
have a program named MyProgram, then the files associated with it
should be as follows:

    -----------------  -----------
    module name        MyProgram
    source file        MyProgram.asm.txt
    object file        MyProgram.obj.txt
    assembly listing   MyProgram.lst.txt
    -----------------  -----------

## Simple standalone programs

If a program does not import any names, it is *standalone*.  The text
of the program is shown in the editor pane.  Go to the Assembler pane
and click Assemble.  If there are no errors, go directly to the
Processor pane (you can skip the Linker) and click Boot.  This will
read the machine language into the memory, and now you can run the
program.

## Programs with multiple modules

A module consists of program text, and it may have an optional file,
and an optional module name.

  * The module text is a sequence of assembly language statements.
    The text may exist only in the editor buffer, or it may be
    associated with a file.

  * If there is a file, this may be specified either as a path (the
    unique identification of the file (C:\\Users\...\prog.asm.txt), or
    as just a filename (myprogram.asm.txt) which is relative to the
    current directory.

To edit a file, the modDir and modName are both optional.  An edited
file may have a module name specified with a module statement.  To
read or save a file, both the module directory and name must be known.

Editor operations on files and modules

Each operation that changes the editor buffer (New, Open, Close)
checks first to see whehter the buffer has been changed since it was
last saved.  If so, a dialogue asks whether the file should be saved.

  * New -- Check whether text in the editor buffer has been saved; if
    not, ask whether to save it.  Create a new module with empty text
    and no file name, add it to the module set, and select it as the
    current module.  Clear the text in the editor buffer.

  * Open -- Check whether text in the editor buffer has been saved; if
    not, ask whether to save it.  Enter the open file dialogue where
    an existing file can be found by navigation or by typing in its
    name.  If the dialogue is cancelled, the module set and editor
    buffer are left unchanged.  If a file is selected in the dialogue,
    and it is already in the module set, then it is selected in the
    current module.  Otherwise, a new module is created with the
    file's contents, and is selected as the current module.  and The
    file is loaded into the editor buffer and added to the module set.

  * Refresh -- The file corresponding to the current module is read,
    and its contents are loaded into the editor buffer.

  * SaveAs -- Enters the save file dialogue where the directory and
    file name can be chosen.  The editor buffer is written into this
    file.

  * Save -- Writes the editor buffer into the current file and
    directory.  If either the module name or directory is not known,
    this reverts to a SaveAs.

  * Select -- Opens a list of all modules; you can click one of them
    which is then set as the current module.
  
  * Close -- Check whether text in the editor buffer has been saved;
    if not, ask whether to save it. The module is removed from the
    module set, and the editor buffer is cleared.  The first module
    (module number 0) is selected as the current module, but if there
    is no module at all, an empty module is created and selected as
    current (in effect, if there is only one module and you close it,
    an automatic New is performed).

  * Example -- Reads in a very simple example program and sets it as
    the current module.  This is a standalone program; you can simply
    click Editor: Example, then Assembler: Assemble, then Processor:
    Boot, and run the program.  This example is used in the first
    "getting started" tutorial.  The example program is also available
    in the Examples directory, accessible through Editor: Open.

Select is for switching among the existing modules, while New and Open
are for introducing a new module.

# Programming

Coming soon.

# Circuit

* Download Hydra and save it in your workspace.  Suppose its location
  is /c/Users/me/a/b/c/Hydra.

* Open a shell and enter circuit/M1.  Run a test program,
  e.g. ArrayMax.hs, as follows:

~~~~
ghci -i/c/Users/me/a/b/c/Hydra/src/haskell/ ArrayMaxRun
~~~~

The command can be simplified by defining a .ghci file, which will be
loaded automatically when you invoke ghci.  You can put the .ghci file
in your $HOME directory.  See the GHC User Guide for more details
about configuring ghci.

~~~~
:set -i/c/Users/a/b/c/Hydra/src/haskell/
~~~~

On development machine (Spectre) use this path:

~~~~
ghci -i/c/Users/johnt/OneDrive/home/docs/research/Hydra/current/Hydra/src/haskell/
ArrayMaxRun
~~~~

$HOME/.ghci contains:

~~~~
:set -iC:\Users\johnt\OneDrive\home\docs\research\Hydra\current\Hydra\src\haskell
~~~~

## M1 circuit

M1 is a relatively simple digital circuit that implements a subset of
the Sigma16 instruction set architecture.  M1 uses sequential control
and does not have instruction level parallelism.  The circuits
comprising M1 are defined in **circuit/M1**.

A few Sigma16 machine
language programs, prepared to run on M1, are defined in
**circuit/examples/M1examples**.  To run these, you need to ensure
that both Hydra and M1 are in the ghci search path.  See above for
making Hydra available.  The M1 circuit is placed on the search path
by including the following file circuit/examples/M1examples/.ghci:

    :set -i../../M1

Enter the directory and enter these commands:

    ghci
    :load ArrayMaxRun
    :main

These will execute the ArrayMax program on the digital circuit, and
that will produce a lot of detailed simulation output showing the
values in key registers, flip flops, and signals.

# Installation

## Browser compatibility

The application currently works with Chrome and Firefox, and
possible Edge.

## Where to find the software

The software is available on the Internet at the [Sigma16 home
page](https://jtod.github.io/Sigma16/), which contains a link to the
latest version, some previous versions, related documents, and more
information about the project.

## How to run Sigma16

There are a number of ways to run the software, but it's recommended
that you try the easiest way.

### The easiest way: just click a link

Run the app with two clicks:

  1. Visit the [Sigma16 homepage](https://jtod.github.io/Sigma16/index.html)
  2. Click on *Launch the latest release*

This will run the app in your browser, and you don't need to download
or install anything.  For most users this is the recommended method.

### Download and open file in browser

If you don't always have Internet access, you can download the files
and ope the app in your browser.  This is easy to do and allows you to
run Sigma16 even when you don't have access to the Internet.  However,
it is a little more awkward to run the example programs.

You can also download the software onto your computer and run it
locally.  This exactly like the first method, except the web pages are
coming from your computer's files rather than the Internet.

Visit the [Sigma16 home
page](https://jtod.github.io/Sigma16/index.html) and download the zip
file for the latest release.  Unzip and click on index.html in the
folder and follow the directions.

You might also be able to launch it by [clicking this
link](../../Sigma16.html) although that might not work; it depends on
whether all the necessary files are accessible.

### Run locally with npm and electron

The previous methods run the app in a browser.  There are some
security restrictions on what a browser can do, and these make it
slightly awkward for the program to read and write files.  You can
perform a local installation which will run Sigma16 as a standalone
app on your computer, rather than in a browser, and this makes it a
little more convenient to use.

If you don't want to bother with downloading some software tools and
running an installation script, you can skip to the next method, which
is to download a precompiled executable for your platform.  But it's
straightforward to build Sigma16 on your computer:

  * You need a shell such as bash or Windows PowerShell.  Every
    computer will have one.
	
  * Install the npm package manager for JavaScript, available at
    [https://www.npmjs.com/get-npm](https://www.npmjs.com/get-npm).
    Download the installer and follow the instructions.

  * Install the *electron* package.  In your shell, enter *npm install
    electron --save-dev*.  (Why is it called "electron"?  Originally,
    there was a text editor named *Atom* written in JavaScript, and it
    needed a shell.  So it's the electron shell for atom...
	
  * Download the source for Sigma16.  Unzip it, and cd into the app
    directory.  Enter *make all*.
	
  * To launch Sigma16, enter *npm run start*.  Now you'll have
    enhanced file access, as well as a few other minor enhancements,
    and you don't need Internet access.

### Compiling a standalone executable

First install npm, which also gives you Node.js.  Use npm to install
electron.

    cd src
    npm install
    npm start

Clone this repository, then enter the repository, install
dependencies, and run the program:
	
    git clone https://github.com/electron/electron-quick-start
    cd electron-quick-start
    npm install
    npm start

## Workflow for development

### Making a release on github pages

make a local directory for editing the project page on github pages.
cd into this directory.

    git clone https://github.com/jtod/jtod.github.io
	git add jtod.github.io
	cd jtod.github.io
	git branch adds16dir
	git checkout adds16dir
	git status
	mkdir Sigma16
	using emacs, create index.html and Sigma16/index.html

To build a release, the necessary files in the current version need to
be copied from the development directory into the web page directory
in jtod.github.io/Sigma16.  However, it doesn't work very well simply
to copy the contents of the Sigma16 development directory into the
release directory because all the .gitignore files should be skipped.
Consequently a script is used to automate the construction of a
release, and the script will copy exactly the required files.
	
    git remote add webpage https://github.com/jtod/jtod.github.io
	git remote
	git status
	git push webpage adds16dir         adds16dir is the local branch
	
On github page, made a pull request and selected adds1dir.  There were
no merge conflicts and the pull request was executed and cleared.  The
home page seems to come from the README file but refreshing it gives
my index.html.

To build standalone version using Node.hs and electron.  In the
src/app directory, use the following commands.  They create a file
package-lock.json and a directory node_modules, both in src/app.

    npm install
    npm start

The pkg program can generate a native executable which doesn't require
the user to have npm installed.
	
    npm install -g pkg 
	pkg --help
	pkg main.js
	
Maybe the bin entry in package.json would allow the command to be just
pkg . ?  In src directory tried this:

    $ pkg  -t win-x64 app
	
	
Lots of warnings:

    > Warning Cannot include directory %1 into executable.
      The directory must be distributed with executable as %2.
      app\node_modules\electron\dist
      path-to-executable/electron/dist
    > Warning Cannot include file %1 into executable.
      The file must be distributed with executable as %2.
      app\node_modules\sliced\index.js
      path-to-executable/node_modules/sliced/index.js
    > Warning Cannot include file %1 into executable.
      The file must be distributed with executable as %2.
      app\node_modules\deep-defaults\lib\index.js
      path-to-executable/node_modules/deep-defaults/index.js

Turns out that to build an app with electron one of the following
specific tools is needed:

    electron-forge
    electron-builder
    electron-packager

Trying electron-builder

    npm install electron-builder --save-dev   in src/app
    npm run mkdist
	
### Version number

The version number is needed in several places.  To keep it
consistent, there is only one primary place where it should be
specified manually: in the version property in app/package.json.

The makefile extracts the version number from that file, and (1)
defines a make variable; (2) writes the Sigma16/VERSION file with just
the version number, and (3) writes Sigma16/app/version.js which is
just defines the version number as a global constant.  

### Running in a browser.

### Running a standalone pre-compiled executable

# About Sigma16

## Author

The architecture and software tools were designed by John O'Donnell.

## License

This is free and open source software and is distributed under the GPL
License, either version 3 or (at your option) any later version.  See
the LICENSE and LICENSE_GPL3 files.  This program is experimental
software, and is under development.

See the README and LICENSE files in the Sigma16 directory.

  * [README](file:README.html)
  * [LICENSE](file:LICENSE.html)
  * [Full text of GPL3 license](file:LICENSE_GPL3.html)

## Implementation of Sigma16

The program is written in JavaScript, and normally uses a web browser
to display the graphical user interface.

The circuits that implement Sigma16 are defined using the  Hydra
hardware description language, which is an embedded domain specific
language implemented in Haskell.

The documentation is written in markdown and prepared for a web
browser using pandoc.

## Changes underway

Support relocatable code

* Change jump, jumpt, jumpf, jal to use pc-relative addressing
* Introduce a jalext that uses indexed absolute address, for external

Arithmetic
Add/subtract with carry

Input/Output
dma channels
Ready
Ack

