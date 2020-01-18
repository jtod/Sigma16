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
you how to enter and run a program and how to use the programming
environment.

# Tutorials

The following short tutorials introduce the system; full details
appear in later sections.  You can keep the tutorials visible in the
right panel while following along with the exercises in the main
panel.

### Hello, world!

The Sigma16 IDE (the main "app") is implemented in JavaScript and runs
in a web browser.  There are additional components to the system that
must be downloaded and run on a computer; these include a digital
circuit that implements the architecture and a high speed emulator.
You can run the IDE simply by [clicking a link to Launch Sigma16 to
run in your browser](https://jtod.github.io/S16/dev/app/Sigma16.html).
See the [Sigma16 Home Page](https://jtod.github.io/S16/) for further
information, documentation, and executable versions.  For now, we will
just work with the IDE running in a browser.

The main window contains three main sections.  The largest area, on
the left side, is the *main working area*.  When the program launches,
this will show the Welcome page.  The *user guide* is on the right
side.  At the top is a row of buttons (Welcome, Examples, etc.).
These select which page is displayed in the main working area.

It's convenient to see the main working area and the user guide side
by side.  Begin by resizing the entire window (bigger is better).
Then you can change the amount of space given to the user guide by
clicking the arrow symbols on the right side of the top button bar.
These arrows will expand or shrink the user guide: the small arrows
adjust by one pixel, the larger arrows by ten pixels.  If you resize
the entire browser window, Sigma16 will maintain the same relative
sizes of the architecture and user guide sections.

You can also open the User Guide in a separate browser tab or window.
The Welcome page contains a link to do this.

A good way to get started is to go through the entire process of
running a simple program.  For now, we focus just on how to use the
software tools; an explanation of the Sigma16 architecture comes
later.

The main working area has several pages, with buttons at the top to
switch between them:

* **Welcome** contains some introductory information, release notes,
   and links.
   
* **Examples** contains a collection of assembly language programs,
   organized by topic.

* **Modules** shows a summary of all the files and modules you
  currently have open.

* **Editor** shows the selected module, where it can be edited.

* **Assembler** translates a program from assembly language to machine
   language, and shows the assembly isting as well as the object code.

* **Linker** combines a collection of object code modles into a single
  executable program.

* **Processor** shows the components of the architecture and executes
  machine language programs.

Let's begin by running a simple example program.

* Click **Editor**, then **Simple Example**.  This will enter a small
  assembly language program into the editor window.  Later, we'll load
  some of the more complex example programs into the editor, and you
  can also modify a program or type in a new one from scratch.

* Click **Assembler** and then the **Assemble** button.  This
  translates the program in the Editor page from assembly language to
  machine language.  The **Assembly Listing** button displays the
  result of the translation, including any error messages.  The
  **Object Code** button displays the output from the translation.

* For this simple example, we don't need the Linker, so you can skip
  it.  The linker is needed for larger and more complex programs with
  multiple modules, or with external references, or that need
  relocation.
  
* Click **Processor**, which shows the main components of the computer
  architecture, including registers and memory.  These components are
  explained later.  For now, just note that this page is where you can
  run programs using the emulator.

* Still on the Processor page, click **Boot**.  This reads the machine
  language program into the memory, and you can see it in the Memory
  display.  The source code (the assembly language) appears in the
  bottom section.

* Click **Step**.  The processor executes a single instruction and
  displays the effects on the registers and memory: blue for using a
  value, and red for modifying it.  The assembly listing shows the
  instruction that just executed by highlighting it in red.  It also
  shows the instruction that will execute next by highlighting it in
  blue.  This is just to make it easier to follow what is happening;
  the actual machine ignores the assembly language listing and doesn't
  even "know" that it exists.

* Click Step repeatedly to watch the program execute, instruction by
  instruction.  When the program terminates, the small window labelled
  **Emulator** will display Halted.

* To rerun the program, click Boot again.

To exit the app, just close the browser window or tab.  This should
put up a dialogue box warning that any unsaved data may be lost and
asking you to confirm.

### Registers and arithmetic

Registers are like variables in a programming language.  They are even
more similar to the registers in a calculator.

Registers can hold variables


 *  We often think of a variable as \stress{a box that can hold a
    number}
 *  A register can hold a variable!
 *  An add instruction (or sub, mul, div) is like an assignment
  statement
 *  \stress{add R2,R8,R2} means \stress{R2 := R8 + R2}
  \begin{enumerate}
   *  Evaluate the right hand side \stress{R8 + R2}
   *  The operands (R8, R2) are not changed
   *  Overwrite the left hand side (destination) (R2) with the
    result
   *  The old value of the destination is destroyed
   *  It is \important{not a mathematical equation}
   *  It is \important{a command to do an operation and put the
      result into a register, overwriting the previous contents}
  \end{enumerate}
 *  Assignment is often written \stress{R2 := R8 + R2}
 *  The \stress{:=} operator means \emph{assign}, and does not mean
  \emph{equals}

Why write a notation like add R5,R2,R3 instead of R5 := R2 + R3?

* It's more consistent because \emph{every} instruction will be
  written in this form: a keyword for the operation, followed by the
  operands

* The notation is related closely to the way instructions are
  represented in memory, which we'll see later

A simple program.  The problem:

* Given three integers in R1, R2, R3
* Goal: calculate the sum R1+R2+R3 and put it in R4

~~~~
    add  R4,R1,R2    ;  R4 := R1+R2   (this is a comment)
    add  R4,R4,R3    ;  R4 := (R1+R2) + R3
~~~~


There are four arithmetic instructions, to perform addition,
subtraction, multiplication, and division.  (There are also a few more
not discussed here.)

The add instruction takes the contents of two operand registers and
places their sum into a destination register.  It is written with the
operation *add*, and the three registers separated by commas: *add
destination,firstOperand,secondOperand*.  For example, add R2,R5,R8
calculates the sum of R5 and R8 and places the result into R2.  The
effect can be described with an assignment statement: R2 := R5+R8.

To place a constant into a register, use the lea instruction.  The
destination is a register, and the operand is a constant followed by
[R0].  For example, to load 42 into register 3, write

~~~~
    lea   R2,42[R0]   ; R2 := 42
~~~~
    
For now, just ignore the [R0], but this is a required part of the
instruction.  Later we'll see why [R0] is there, along with more
capabilities of the lea instruction.

~~~~
    lea   R5,3[R0]    ; R5 := 3
    lea   R8,4[R0]    ; R8 := 4
    add   R2,R5,R8    ; R2 := R5 + R8 = 3+4 = 7
~~~~
    
hexadecimal

Replace 3+4 by 23+5.  Now the result is 28.  The processor page shows
numbers in hexadecimal, so 28 is displayed as 001c (i.e. 28 = 1*16 +
12, and the hex digit for 12 is c).

~~~~
    lea   R5,23[R0]   ; R5 := 23
    lea   R8,5[R0]    ; R8 := 5
    add   R2,R5,R8    ; R2 := R5 + R8 = 23+5 = 28 = $001c
~~~~
    

Further instructions

~~~~
    add   R1,R2,R3
    sub   R1,R2,R3
    mul   R1,R2,R3
    div   R1,R2,R3
~~~~


More arithmetic instructions.  There are instructions for the basic
arithmetic operations

~~~~
 add  R4,R11,R0   ; R4 := R11 + R0
 sub  R8,R2,R5    ; R8 := R2 - R5
 mul  R10,R1,R2   ; R10 := R1 * R2
 div  R7,R2,R12   ; R7 := R2 / R12
~~~~

Every arithmetic operation takes its operands from registers, and
loads the result into a register.

In the lea instruction, the constant value can be specified using
either decimal or hexadecimal notation.  Indicate hexadecimal in
assembly language by putting $ before the number.  Thus $00a5 and 0165
both represent the integer 165.

Most instructions follow a similar pattern, where the first operand is
the destination where the result is placed, and the subsequent
operands are the arguments to the computation.  This is the same
convention used in assignment statements in many programming
languages: the registers in sub R1,R2,R3 appear in the same order as
the varaibles in R1 := R2-R3.

An arithmetic instruction performs just one operation.  Several
instructions are needed to evaluate a larger expression, such as 7 +
10*9:

~~~~
   lea   R1,7[R0]
   lea   R2,10[R0]
   lea   R3,9[R0]
   mul   R2,R2,R3
   add   R1,R1,R2
~~~~

Generally you can use any register you like; in the previous example
we could have used R12, R6, and R8 instead of R1, R2, R3.  Registers
R1 through R14 are all the same.  However, two of the registers are
different:

* R0 contains the constant 0 and it will never change.  Any time an
  instruction uses R0, the value it gets will be 0.  It is legal for
  an instruction to attempt to modify R0 (for example, add R0,R3,R4 is
  legal) but after executing this instruction R0 still contains 0.
  The reason for this is that we frequently need to have access to a
  register containing 0.
  
* R15 contains a number of bits that provide some information about an
  instruction.  For example, if an addition produces a result that is
  too large to fit in a register, a special flag indicating this is
  set in R15.  Many of the instructions, including all the arithmetic
  instructions, change the value of R15 as well as placing the result
  in the destination register.  For this reason, R15 cannot be used to
  hold a variable: its value would be destroyed almost immediately.
  
To summarise, Registers R1 through R14 are all identical and can be
used for variables.  R0 contains 0 and will never change.  R1 changes
very frequently and can be used to determine various error conditions
and other information about an instruction.

Every register contains one word of data.  On the Sigma16
architecture, a word is 16 bits of data.  This is enough to represent
any of the following:

* An integer between ? and ?

* A natural number between 0 and 65,535

* A character

Example

* Suppose we have variables a, b, c, d
* R1=a, R2=b, R3=c, R4=d
* We wish to compute R5 = (a+b) * (c-d)

~~~~
    add   R6,R1,R2     ; R6 := a + b
    sub   R7,R3,R4     ; R7 := c - d
    mul   R5,R6,R7     ; R5 := (a+b) * (c-d)
~~~~

Good comments make the code easier to read!

**General form of arithmetic instruction**

~~~~
op d,a,b
~~~~

----  ------------------------------------------------
 op    operation: add, sub, mul,div
  d    destination register: where the result goes
  a    first operand register
  b    second operand register
----  ------------------------------------------------

Meaning  R_d := R_a  op \ R_b

Example: add R5,R2,R12  ; R5 := R2+R12


**Register R0 and R15 are special**

You should not use R0 or R15 to hold ordinary variables.

**R0 always contains 0**
  
* Any time you need the number 0, it's available in R0

* You cannot change the value of R0

* add R0,R2,R3 ; does nothing --- R0 will not change

* add R5,R2,R3 ; fine - you can change all other registers

* It is legal to use R0 as the destination, but it will still be 0
  after you do it!
  

**R15 holds status information**
  
* Some instructions place additional information in R15 (is the
  result negative? was there an overflow?)

* Therefore the information in R15 is transient

* R15 is for temporary information; it's not a safe place to keep
  long-term data

### Accessing memory

Only 14 registers can be used to hold variables, but most programs
need many more variables.

The Memory is a large set of locations


### Comparisons and jumps

### Instruction control

### Effective addresses and arrays

### Further topics

To run the program slowly, click Step repeatedly.  To run the program
faster but without updating the display after each instruction, click
Run.  At any time you can click Pause to stop the processor, and you
can resume execution with either Step or Run.

There are two independent views into the memory; this is convenient
for looking at the machine language code in one view and the data in
the other view.  (Despite the two views, there is just one memory!)
At this point the pc register contains 0, meaning that the next
instruction to be executed is the one in memory location 0.  The ir
and other registers also contain 0, but that is just the initial
value.

To set a breakpoint, click Breakpoint and enter the stopping condition
in the dialogue box.  For example, to stop when the pc register
becomes $01b7, enter BPeq BPpc (BPhex "01b7").  Then click Run, and
the emulator will run at full speed until the pc reaches the specified
value; then it will stop so you can examine the state of the machine.

# Architecture

Why use Sigma16?

Our focus is on fundamental concepts, ideas and principles.  Sigma16
illustrates the fundementals of computer systems but it avoids
unnecessary complexity.  For example, Sigma16 has just one word size
(16 bits) while most commercial machines provide a variety.  That
variety is useful for practical applications but it complicates many
of the details while not adding any new fundamental ideas.  Most
commercial computers that achieve success in the marketplace
eventually become encrusted with complications that help support
backward compatibility; this can lead to great complexity.

Sigma16 is a 16-bit architecture, and every data value is a 16-bit
word.  Integers are represented in 16-bit two's complement notation.
The bits of a word are numbered from left to right, starting with 0.
Thus the leftmost (most significant) bit of a word is bit 0, and the
rightmost (least significant) is bit 15.

## Subsystems

The system contains several main subsystems.  The most important of
these are *registers*, *memory*, and *logic and arithmetic*, and
*Input/Output*.  These are described in detail later, but here is a
brief synopsis:

* A register is a digital circuit that can retain one word of data.  A
  new value can be loaded into a register, and the current contents
  may be read out.  There are a number of special registers as well as
  a *register file* that contains 16 registers.

* The memory can hold a large number of words.  It's similar to the
  register file, but significantly slower and much larger.

* The ALU (arithmetic and logic unit) is a circuit that can do
  arithmetic, such as addition, subtraction, comparison, and some
  other operations

* The Input/Output system can transfer data between the computer and
  the outside world.

## Register file

The **register file** is a set of 16 general registers that hold a 16
bit word.  A register is referenced by a 4-bit binary number.  In
assembly language, we use the notations R0, R1, R2, ..., R9, R10, R11,
R12, R13, R14, R15 to refer to the registers.  The state of the
register file can be written as a table showing the value of each
register:

   Register    Contents
  ~~~~~~~~~~  ~~~~~~~~~~
     R0          0000
     R1          fffe
     R2          13c4
     ...         ...
     R14         03c8
     R15         0020

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
    <th>Symbolic name</th>
    <th>Meaning</th>
  </tr>
  <tr>
    <td>0</td>
    <td>**G**</td>
    <td>sysccG</td>
    <td> gt (or gt 0) unsigned (binary)</td>
  </tr>
  <tr>
    <td>1</td>
    <td>**g**</td>
    <td>sysccg</td>
    <td> gt (or gt 0) signed (two's complement)</td>
  </tr>
  <tr>
    <td>2</td>
    <td>**E**</td>
    <td>sysccE</td>
    <td>= (or =0) word, signed, unsigned</td>
  </tr>
  <tr>
    <td>3</td>
    <td>**l**</td>
    <td>sysccl</td>
    <td>   lt (or lt 0) signed (two's complement)</td>
  </tr>
  <tr>
    <td>4</td>
    <td>**L**</td>
    <td>sysccL</td>
    <td>    lt (or  lt 0) unsigned (binary)</td>
  </tr>
  <tr>
    <td>5</td>
    <td>**V**</td>
    <td>sysccV</td>
    <td>    unsigned overflow (binary)</td>
  </tr>
  <tr>
    <td>6</td>
    <td>**v**</td>
    <td>sysccv</td>
    <td>    signed overflow (two's complement)</td>
  </tr>
  <tr>
    <td>7</td>
    <td>**C**</td>
    <td>sysccC</td>
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

Instructions specify memory addresses in two parts: the
*displacement*, which is a word representing a binary number, and the
*index*, which is one of the registers in the register file.  For
example, a memory address could be specified as $003c[R5]; the
displacement is 003c and the index is R5.

When the instruction is executed, the computer calculates the
*effective address* by adding the value of the displacement and the
value in the index register.  If R5 contains 2, then the effective
address of $003c[R5] is 003e.

This scheme may seem more complicated than simply specifying the
address directly, but it is extraordinarily flexible.  If the machine
language just gave the address as a single binary number, it would be
limited to accessing simple static variables.  The effective address
mechanism is simple to implement in hardware, as you can see in the
digital circuit processor, yet it allows the implementation of local
variables, records, arrays, pointers and linked data structures, jump
tables, and more.  These techniques are described later.

## Control registers

There are several instruction control registers that enable the
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

### Interrupts and exceptions

 * mask
 
 * req
 
 * istat
 
 * ipc
 
 * vect

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

Instructions are represented in the memory of the computer using
words, just like all other kinds of data.  From the programmer's
perspective, an instruction is like a simple statement in a
programming language.  From the circuit designer's perspective,
however, instructions must be processed using logic gates, and the
specific way it is represented as a word of bits is important.

An instruction specifies several pieces of information.  For example,
add R1,R2,R3 is an instruction that says four things: it's an
addition, the result goes into R1, and the operands come from R2 and
R3.  Therefore to represent instructions we need to organize a word as
a collection of several *fields*.

The particular scheme for describing an instruction as a collection of
fields is called an *instruction format*.  The key to understanding
the interface between machine language and digital circuit design is
to master the instruction formats.

### Instruction fields

An instruction may consist of one word or two words, depending on the
instruction format.  The first word of every instruction has the
following fields.

* op  (bits 0-3) opcode, determines instruction format
* d   (bits 4-7) 4-bit destination
* a   (bits 8-11) 4-bit operand
* b   (bits 12-15) 4-bit operand, or expanded opcode for RX
* ab  (bits 8-15)  8-bit expanded opcode for EXP

An instruction may either use a and b as separate 4-bit fields, or it
can combine them into a single 8-bit field called ab.

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

Every instruction has a 4-bit field called the opcode (op for short).
This gives 16 values of the opcode: 14 of them (0 through 13) denote
the 14 RRR instructions, described later.  If the op field is 14 (hex
e) the instruction is EXP format and has a secondary opcode in the a
and b fields.  If the op field contains 15 (hex f) the instruction is
RX format with a secondary opcode in the b field.  The instruction
formats are described below.

A second word is needed to represent RX, EXP4 and EXP8 formats.  There
are individual names for the individual 4-bit fields, as well as names
(disp, gh) for larger fields.

* e (bits 0-3) 4-bit operand
* f (bits 4-7) 4-bit operand
* g (bits 8-11) 4-bit operand
* h (bits 12-15) 4-bit operand
* gh (bits 8-15) 8-bit operand
* disp (bits 0-15) 16 bit operand "displacement"

### Instruction formats

There are several instruction formats.  The core architecture (the
simplest part of the system) uses two:

* RRR -- Instructions that perform operations on data in registers,
         but not referring to memory.  The representation is one word.

* RX --  Instructions that specify a memory location as well as a
         register operand.  The representation is two words.
        
The advanced parts of the architecture provide additional instructions
and these are represented with a a format called EXP.  An EXP
instruction contains 14 (hex e) in the op field, and the a and b
fields are combined into a single 8-bit number that contains a
secondary opcode.  This means that the EXP format allows for 256
instructions.  This greatly expands the number of instructions that
can be accommodated, and it allows for experimental instructions for
research purposes.  (The name EXP stands simultaneously for both
EXPansion and EXPerimentation.)

There are three variants of the EXP format.  The secondary opcode (in
the a and b fields) specifies the EXP variant as well as the
individual instruction.  The variants are:

* EXP0 -- An is one word, and only one operand field (the d field) is
          available.  (There are 0 fields in the second word; hence
          the name EXP0.)  The op field contains e, and the a and b
          fields are combined to form an 8-bit opcode.

* EXP4 -- The instruction is two words, with a 4-bit operand in the
          first word and four more in the second word.  (It's called
          EXP4 because the second word contains 4 4-bit operands.)
          
* EXP8 -- The instruction is two words, with three 4-bit operands and
          an 8-bit operand.  (It's called EXP8 because it provides an
          8-bit operand.)

The following table summarises the instruction formats.  The core of
the architecture needs only the first two (RRR and RX).  The more
advanced features require the 

Table: **Machine language instruction formats**

------------------------------------------------------------
 Format   Size   Opcode   Operands   Example
-------- ------ -------- ---------- --------------------
 RRR      1      op       d,a,b      add Rd,Ra,Rb
 
 RX       2      op,b     d,disp,a   load Rd,disp[Ra]
 
 EXP0     1      op,ab    d          rfi

 EXP4     2      op,ab    d,e,f,g,h  extract Re,Rf,Rg,Rh
 
 EXP8     2      op,ab    d,e,f,gh   save Rd,Re,gh[Rf]
------------------------------------------------------------

### Assembly language statement formats

Assembly language statements generally correspond to the instruction
formats, but there is not an exact correspondence for several reasons:

* Sometimes an instruction is written in assembly language with a
  field omitted which exists in the machine language code but is
  ignored.  For example, the instruction *inv R1,R2* generates an RRR
  instruction, but the third operand field is omitted because the
  invert function takes only one operand, not two.
  
* Sometimes two instructions look the same in assembly language but
  use different machine language instruction formats.  For example,
  *add R1,R2,R3* and *push R1,R2,R3* look similar, but *add* uses the
  RRR instruction format and *push* uses the EXP4 instruction format.
  The reason for this is that there are not enough bits in the op
  field to accommodate all the instructions with three register
  operands, so an *expanding opcode* is used.  Thus push is
  represented with op=14 (indicating EXP format), and the EXP4 variant
  is used for this instruction.
  
* The 4-bit fields are sometimes used to denote a register from the
  register file (R3), or a control register (mask), or a constant .
  In assembly language the constants are written just as a number
  (e.g. shiftl R1,R2,5).  Control registers are written by name rather
  than their number in the control register file (e.g. getctl
  R3,mask).

Table: **Assembly language statement formats**

-------------------------------------------------------
 Asm   Example                  ML formats
------- ---------------------  ---------------------------
 RRR     add Rd,Ra,Rb           RRR
 RX      lea Rd,disp[Ra]        RX

 RR      inv Rd,Ra              RRR (b ignored), RREXP
 JX      jump disp[Ra]          RX (b ignored)
 KX      jumpc0 d,disp[Ra]      RX (d is constant)

 NO      rfi                    R (d ignored)
 RRK     shiftl Rd,Ra,k         EXP4
 RRKK    getbit Re,Rf,g,h       EXP4 (d ignored)
 RREXP   execute Re,Rf          EXP4
 RCEXP   getctl R1,mask         EXP8
-------------------------------------------------------

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

The EXP instruction format is used for expanded instructions that are
not represented using the RRR or RX formats.  It provides many unused
opcodes, so it is useful for experimental instructions.  The name EXP
stands simultaneously for *expansion* and *experimental*.

There are several sub-formats, or variations, of the EXP format.
These are described in more detail below.  Most of the sub-formats
represent an instruction with two words.

* The first word has the same fields as for RRR and RX instructions:
  the 4-bit fields op, d, a, b.  All EXP instructions have a constant
  hex e in the op field, which indicates the EXP format.  The a and b
  fields together form an 8-bit secondary opcode (this is called the
  ab field), allowing for 256 distinct EXP instructions.  The d field
  in the first word, and all of the second word (if any), hold
  operands which depend on the particular variant of the EXP format.
  
* The second word (if it appears) contains four 4-bit fields named e,
  f, g, h.  In some variants these hold separate 4-bit values, while
  in some cases g and h form a single 8-bit value (called the gh field).

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
    <th>e</th>
    <th>f</th>
    <th>g</th>
    <th>h</th>
  </tr>
  <tr>
    <td>0-3</td>
    <td>4-7</td>
    <td>8-11</td>
    <td>12-15</td>
  </tr>
</table>

To summarise, an EXP instruction may use the fields op, d, ab, e, f,
g, h.  The g and h fields can be combined into a single 8-bit field gh
All EXP instructions combine the a and b fields into a single 8-bit
field called ab.  Some EXP instructions combine the g and h fields
into a single 8-bit field called gh.

The EXP format has the following variants.

* EXP0 format takes no operands.  The instruction format uses just one
  word (there is no second word with the e, f, g, h fields).  The op
  field contains 14 (hex e), the d field is ignored (the assembler
  sets it to 0), and the ab field contains an 8 bit expanded operation
  code.  Example: *rfi* is an instruction with no operands and with
  secondary opcode 0.  Its representation is e000.  The EXP0 format
  allows rfi to be represented in one word without using up one of the
  limited and valuable opcodes avaiable for RRR instructions.
  
* The RREXP format takes two register operands, which are in the e and
  f fields of the second word. The d field of the first word and the g
  and h fields of the second word are ignored (the assembler will set
  these to 0).  Any RREXP instruction could be reprsented as RRR, but
  there are only a few RRR opcodes avaiable, so uncommon instructions
  that require two registers are represented as RREXP.  Example:
  *execute R5,R6* is RREXP.

* The RCEXP format takes two register operands; the first is a general
  register and the second is a control register.  An example of the
  operand field is *R3,mask*.  The operands are in the e and f fields
  of the second word.  The d field of the first word and the g and h
  fields of the second word are ignored (the assembler will set these
  to 0.)  The first operand is an element of the register file (for
  example, R4).  The second operand is a control register, which is
  specified by a 4-bit number.  In assembly language, we normally
  refer to the control registers by name rather than number, to make
  it easier to remember which is which.  For example, *getctl
  R3,status* has RCEXP format.
  
* The RRREXP format takes three register operands, which are in the f,
  g, and h fields of the second word. An example of the operand field
  is $R1,R2,R3*.  The d field of the first word and the e field of the
  second word are ignored (the assembler will set these to 0).  The
  RRREXP instructions would be a natural fit for the RRR format, but
  there are not enough RRR opcodes available, so the EXP format is
  used to expand the number of instructions that can be represented.
  For example, *push R5,R8,R9* has RRREXP format.
  
* The RRKEXP format takes two register operands and a 4-bit constant
  number.  An example of the operand field is *R1,R2,13*.  The
  register operands are in the f and g fields of the second word, and
  constant is in the h field of the second word.  The d field of the
  first word and the e field of the second word are ignored (the
  assembler sets these to 0).  For example, *shiftr R3,R6,7* has
  RRKEXP format.
  
* The RRKKEXP format takes two register operands and two 4-bit
  constant binary number operands.  The register operands are in the e
  and f fields of the second word, while the two constants are in the
  g and h fields.  The d field of the first word is ignored (the
  assembler sets it to 0).
  
* The RRXEXP format takes two register operands as well as a memory
  address specified with an 8-bit offset and index register.  Thus
  these instructions require three registers to be specified, as well
  as the offset.  Thus every bit of both instruction words is needed
  to represent an RRXEXP format instruction.  In assembly language,
  the memory address is written as *offset[Rh]* where *offset* is an
  -bit binary number and Rh is a register.  The effective memory
  address is *offset+Rh*. This is similar to ordinary memory
  addresses; the only difference is that it uses an 8-bit offset
  rather than a 16-bit displacement. For example, *save R1,R9,2[R14]*
  has RRKEXP format.  

# Instruction set

The following sections describe the instructions in groups organized
by their function.  Some of the groups contain instructions with
different formats.  From the programmer's perspective the function is
more important, so these groups are useful in finding the right
instruction to use.  (From the perspective of designing a digital
circuit to impleemnt the architecture, the format is essential.)

## Arithmetic instructions

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

### add

The instruction add Rz,Rx,Ry  has operands Rx and Ry and destination
Rz.  It fetches the operands reg[x] and reg[y],
calculates the sum reg[x] + reg[y], and loads the result into the destination
reg[z].  The effect is reg[z] := reg[x] + reg[y].  For example, add
R5,R12,R2 performs R5 := R12 + R3.

The add instruction is RRR format with opcode=0.  Given destination z
and operands x and y (where z, x, y are hex digits), add Rz,Rx,Ry is
reprseented by 0zxy.

  Code    Assembly          Effect
  -----   ----------------  ------------------
  062c    add R6,R2,R12     ; R6 := R2 + R12
  0d13    add R13,R1,R3     ; R13 := R1 + R3
  
In addition to setting the destination register, the add instruction
sets several bits in the condition code R15 and may set a bit in the
req register.

---------  ---------------------
 R15.ccG    result >bin 0
 R15.ccg    result >tc 0
 R15.ccE    result = 0
 R15.ccl    result <tc 0
 R15.CCL    0
 R15.ccV    bin overflow
 R15.CCv    tc overflow
 R15.CCc    carry output
---------  ---------------------

### sub

sub R1,R2,R3

### mul

The multiply instruction calculates the integer (two's complement)
product of the operands Ra and Rb, and places the result in the
destination register Rd.

If the magnitude of the product is too large to be representable as a
16 bit two's complement integer, this is an overflow.  If this
happens, the integer overflow bit is set in the condition code (F15)
and the integer overflow bit is also set in the interrupt request
register (req), and the lower order 16 bits of the product are loaded
into Rd.

### div

### addc

The **binary add with carry** instruction *addc Rd,Re,Rf* calculates the sum
of the binary numbers in the operand registers Re and Rf as well as
the carry bit in the condition code.  The sum is loaded into the
destination register Rd and the carry output is set in the condition
code register.  Overflow is not possible with this instruction.

## Accessing memory

### lea

The **load effective address** instruction *lea Rd,disp[Rx]* calculates
the effective address of the operand disp[Rx] and places the result in
the destination register Rd.  The effective address is the binary sum
disp+Rx.

### load

The **load** instruction *load Rd,disp[Rx]* calculates the effective
address of the operand disp[Rx] and copies the word in memory at the
effective address into the destination register Rd.  The effective
address is the binary sum disp+Rx.

-----------------   -----------------------------------------
general form        load Rd,disp[Ra]
effect              reg[Rd] := mem[disp+reg[Ra]]
machine format      RX
assembly format     RX
-----------------   -----------------------------------------

Examples
~~~~
   load  R12,count[R0]   ; R12 := count
   load  R6,arrayX[R2]   ; R6 := arrayX[R2]
   load  R3,$2b8e[R5]    ; R3 := mem[2b8e+R5]
~~~~

### store


The **store** instruction *store Rd,disp[Rx]* calculates the effective
address of the operand disp[Rx] and the value of the destination
register Rd into memory at the effective address.  The effective
address is the binary sum disp+Rx.

-----------------   -----------------------------------------
general form        store Rd,disp[Ra]
effect              mem[disp+reg[Ra]] := reg[Rd]
machine format      RX
assembly format     RX
-----------------   -----------------------------------------

Store copies the word in the destination register into memory at the
effective address.  This instruction is unusual in that it treats the
"destination register" as the source of data, and the actual
destination which is modified is the memory location.

Most instructions take data from the rightmost operands and modify the
leftmost destination, just like an assignment statement (x := y+z).
However, the store instruction operates in the opposite direction.
The reason for this has to do with the circuit design of the
processor.  Although the "left to right" nature of the store
instruction may look inconsistent from the programmer's point of view,
it actually is more consistent from the deeper perspective of circuit
design.


Examples

~~~~
   store  R3,$2b8e[R5]
   store  R12,count[R0]
   store  R6,arrayX[R2]
~~~~

### save

The **save** instruction stores a sequence of adjacent registers into
memory starting from the effective address.  It is equivalent to a
fixed sequence of store instructions.  The purpose of save is to copy
the state of registers into memory during a procedure call or a
context switch.

Typically, save is used as part of a procedure call and restore is
used as part of the return.

The instruction *save Rd,Re,gh[Rf]* stores the contents of Rd, Rd+1,
..., Re into memory at consecutive locations beginning with
mem[gh+Rf].

The instruction is EXP format, and the displacement is limited to 8
bits, because it is specified in the gh field (the rightmost 8 bits)
of the second word of the instruction.

For example, consider this instruction:
~~~~
   save  R3,R10,4[R14]
~~~~

The effect is equivalent to

~~~~
   store  R3,4[R14]
   store  R4,5[R14]
   store  R5,6[R14]
   store  R6,7[R14]
   store  R7,8[R14]
   store  R8,9[R14]
   store  R9,10[R14]
   store  R10,11[R14]
~~~~

### restore

The **restore** instruction copies a sequence of consecutive memory
locations starting from the effecive address into a sequence of
adjacent registers.  It is equivalent to a fixed sequence of load
instructions.  The purpose of restore is to restore the state of
registers from memory after a procedure call or a context switch.

Typically, save is used as part of a procedure call and restore is
used as part of the return.

The instruction *restore Rd,Re,gh[Rf]* copies the contents of memory
at consecutive locations beginning with mem[gh+Rf] into registers Rd,
Rd+1, ..., Re.

The instruction is EXP format, and the displacement is limited to 8
bits, because it is specified in the gh field (the rightmost 8 bits)
of the second word of the instruction.  The assembly language
statement format is RRXEXP.

For example, consider this instruction:
~~~~
   restore  R3,R10,4[R14]
~~~~

The effect is equivalent to

~~~~
   load  R3,4[R14]
   load  R4,5[R14]
   load  R5,6[R14]
   load  R6,7[R14]
   load  R7,8[R14]
   load  R8,9[R14]
   load  R9,10[R14]
   load  R10,11[R14]
~~~~


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

The comparison instructions compare the values in two operand
registers and produce one or more Booleans indicating the relation
between the operands.  The result can either loaded into a register as
a word containing a Boolean (cmplt, cmpeq, cmpgt), or it can be
indicated by setting a number of separate Booleans in the condition
code register R15 (cmp).

### cmplt

The **compare for less than** instruction *cmplt Rd,Ra,Rb* performs an
integer comparison of the operands.  If Ra<Rb then the destination Rd
is set to 0001; otherwise it is set to 0000.

The comparison treats the operands as signed integers, not as binary
numbers.  For example, ffff is the two's complement representation of
-1, while 0002 is the two's complement representation of 2, so ffff <
0002.

The result of a comparison is a Boolean, either false or true.  False
is represented as 0000, and true is represented as 0001.

### cmpeq

The **compare for equality** instruction *cmpeq Rd,Ra,Rb* performs an
bitwise comparison of the operands.  If Ra and Rb contain the same
value then the destination Rd is set to 0001; otherwise it is set to
0000.

This instruction can be used to compare for equality of either
integers (binary) numbers or integers (two's complement).

The result of a comparison is a Boolean, either false or true.  False
is represented as 0000, and true is represented as 0001.

### cmpgt

The **compare for greater than** instruction *cmpgt Rd,Ra,Rb* performs
an integer comparison of the operands.  If Ra>Rb then the destination
Rd is set to 0001; otherwise it is set to 0000.

The comparison treats the operands as signed integers, not as binary
numbers.  For example, ffff is the two's complement representation of
-1, while 0002 is the two's complement representation of 2, so 0002 >
ffff.

The result of a comparison is a Boolean, either false or true.  False
is represented as 0000, and true is represented as 0001.

### cmp

The **compare** instruction *cmp Ra,Rb* performs both an integer (two's
complement) and a natural (binary) comparison of the operands.  The
results are indicated in several of the flags in the condition code
register, which is R15.

## Transfer of control

### jump

The **jump** instruction *jump disp[Ra]* transfers control to the
instruction in memory at the effective address *disp+Ra*.

### jumpf

The **jump if false** instruction *jumpf Rd,disp[Ra]* checks the
Boolean value of Rd.  If this is *false*, the instruction transfers
control to the instruction in memory at the effective address
*disp+Ra*; otherwise the instruction does nothing.

### jumpt

The **jump if true** instruction *jumpf Rd,disp[Ra]* checks the
Boolean value of Rd.  If this is *true*, the instruction transfers
control to the instruction in memory at the effective address
*disp+Ra*; otherwise the instruction does nothing.

### jumpc0

The **jump if condition code bit is 0** instruction *jumpc0
k,disp[Ra]* checks the value of bit k in R15, which is the condition
code register.  If this bit is *0*, then the instruction transfers
control to the instruction in memory at the effective address
*disp+Ra*; otherwise the instruction does nothing.

### jumpc1

The **jump if condition code bit is 1** instruction *jumpc1
k,disp[Ra]* checks the value of bit k in R15, which is the condition
code register.  If this bit is *1*, then the instruction transfers
control to the instruction in memory at the effective address
*disp+Ra*; otherwise the instruction does nothing.

### jal

The **jump and link** instruction *jal Rd,disp[Ra]* loads the address
of the following instruction into the destination register Rd, and
transfers control to the instruction in memory at the effective
address *disp+Ra*.

The value loaded into Rd is the address of the instruction that
immediately follows the jal.  This instruction is used for calling
procedures or functions: the effective address is the location of the
procedure, and the return address is in Rd.  The procedure can then
return by jumping to the return address, e.g. with *jump 0[Rd]*.

### Aliases for conditional jumps

jumplt jumple jumpne jumpeq jumpge jumpgt

## Bit fields

The following instructions treat a word as a sequence of bits, and
operate on the individual bits.

### inv

The **invert** instruction *inv Rd,Ra* inverts the bits in the operand Ra and
places the result in the destination Rd.  The operand Ra is not
changed.  Inverting a bit means changing 0 to 1, and changing 1 to 0.

Suppose R8 contains 035f.  Then the following instruction will set R3
to fca0, and leave R8 unchanged.

~~~~
   inv R3,R8
~~~~

### and

The **logical and** instruction *inv Rd,Ra,Rb* calculates the logical
"and" (conjunction) of the bits in the operands Ra and Rb, and places
the result in the destination Rd.  The operand registers are not
changed.  The logical and of two bits is 1 if both bits are 1, and 0
otherwise.

### or

The **logical or** instruction *inv Rd,Ra,Rb* calculates the logical
"inclusive or" (disjucntion) of the bits in the operands Ra and Rb,
and places the result in the destination Rd.  The operand registers
are not changed.  The logical or of two bits is 1 if either or both of
the bits are 1, and 0 otherwise.

### xor

The **logical exclusive or** instruction *inv Rd,Ra,Rb* calculates the
logical "exclusive or" of the bits in the operands Ra and Rb, and
places the result in the destination Rd.  The operand registers are
not changed.  The logical exclusive or of two bits is 1 if one or the
other of the bits is 1, and 0 otherwise.

The exclusive or of two bits is the same as the inclusive or if either
bit is 0.  The only time it is different is if both bits are 1: in
that case exclusive or gives 0 but inclusive or gives 1.

### shiftl

The instruction shiftl Rd,Ra,k shifts the value in the operand
register Ra by k bits to the left, and the result is placed in the
destination register Rd.  The operand Ra is not modified.  During the
shift, the leftmost k bits of the value are discarded and the
rightmost k bits become 0.

~~~~
   shiftl  R2,R3,5
~~~~

The instruction format is EXP, and the assembly language statement format
is RRKEXP

### shiftr

Shift to the right.

The instruction shiftr Rd,Ra,k shifts the value in the operand
register Ra by k bits to the right, and the result is placed in the
destination register Rd.  The operand Ra is not modified.  During the
shift, the rightmost k bits of the value are discarded and the
leftmost k bits become 0.

The following instruction shifts the value in R3 to the right by 5
bits and place the result in R2.  The operand register R3 is not
changed.

~~~~
   shiftr  R2,R3,5
~~~~

The instruction format is EXP, and the assembly language statement format
is RRKEXP

### getbit

The bit at position k in the operand Ra is converted to a Boolean and
loaded into the destination register Rd.  Bit 0 is the leftmost (most
significant) bit, and bit 15 is the rightmost (least significant).

~~~~
   getbit Rd,Ra,k
~~~~



### getbiti

### putbit

### putbiti

### extract

Extract a field from Rf starting in bit position g, or size h bits,
and place the result into Re.

~~~~
   extract Re,Rf,g,h
~~~~

Format RRKKEXP.  The operands are all in the second word of the instruction.


### execute

The *execute* instruction commands the computer to execute another
instruction which is in the register file.  The first word of this
target instruction is in Re and the second word (in case it's a
two-word instruction) is in Rf.  If Re contains an RRR instruction,
then Rf is ignored.

~~~~
   execute Re,Rf
~~~~

Format RREXP.  Both operands are in the second word of the
instruction, in the e and f fields.

## System control

### trap

### getctl

### putctl

### rfi

The **return from interrupt** instruction performs the following
operations simultaneously and atomically:

* status := istat
* pc := ipc

It is important that this is an atomic operation.  If either of the
two register updates were performed before the other, then the second
one would not function correctly.  In the digital circuit implementing
the processor, these two register updates are genuinely simultaneous:
they happen during the same clock cycle, at exactly the same time.

The instruction format is EXP0.  As there is no operand, every rfi
instruction has the same machine language representation: e000.  The
fields are op, d, ab; op=14 to indicate EXP format, d=0 because it is
unused, and ab=0 because this is the secondary opcode of rfi.

This instruction is privileged.  Since the instruction changes the
status register, it can be used to perform a context switch.

## Trap operations

### Halt

### Nonblocking read

### Write

### Blocking readline

### Nonblocking readline

## List of instructions

Table: **Instruction set**

-------------------------------------------------------
 Mne     AL     ML    Op   Effect
------- ----- ------ ---- --------------------
add      RRR   RRR    0    Rd := Ra + Rb

sub      RRR   RRR    1    Rd := r[a] - Rb

mul      RRR   RRR    2    Rd := Ra * Rb

div      RRR   RRR    3    Rd := Ra / Rb,
                          R15 := Ra rem Rb

cmp      RRR   RRR    4       R15 := Ra ? Rb

cmplt    RRR   RRR    5       Rd := Ra < Rb

cmpeq    RRR   RRR    6       Rd := Ra = Rb

cmpgt    RRR   RRR    7       Rd := Ra > Rb

inv      RR    RRR    8       Rd := inv Ra

and      RRR   RRR    9       Rd := Ra and Rb

or       RRR   RRR    a       Rd := Ra or Rb

xor      RRR   RRR    b       Rd := Ra xor Rb

nop      RRR   RRR    c       no operation

trap     RRR   RRR    d       

lea      RX    RX     f,0     Rd := Ra+disp

load     RX    RX     f,1     Rd := mem[Ra+disp]

store    RX    RX     f,2     mem[Ra+disp] := Rd

jump     JX    RX     f,3     pc := Ra+disp

jumpc0   KX    RX     f,4     if R15.k=1 then pc := Ra+disp

jumpc1   KX    RX     f,5     if R15.k=0 then pc := Ra+disp

jumpf    RX    RX     f,6     if Rd=0 then pc := Ra+disp

jumpt    RX    RX     f,7     if Rd/=0 then pc := Ra+disp

jal      RX    RX     f,8     Rd := pc, pc := Ra+disp

testset  RX    RX     f,9     Rd := mem[Ra+disp],
                              mem[Ra+disp] := 1
                             
rfi      NO    R      e,0    pc := ipc,
                             status := istatus
                             
execute  RR    RREXP  e,8

getctl   RC    RCEXP  e,10   Rd := Rc

putctl   RC    RCEXP  e,11   Rc := Rd

push     RRR   RRREXP e,18

pop      RRR   RRREXP e,19

top      RRR   RRREXP e,1a

shiftl   RRK   RRKEXP e,20  Rd := Ra shl k

shiftr   RRK   RRKEXP e,21  Rd := Ra shr k

getbit   RRKK  RRKEXP e,22  Rd := R15.k

getbiti  RRKK  RRKEXP e,23  Rd := inv R15.k

putbit   RRKK  RRKEXP e,24  R15.k := Ra.15

putbiti  RRKK  RRKEXP e,25  R15.k := inv Ra.15

extract  RRKK  RRKKEXP e,38

save     RRX   RRXEXP  e,40 mem[Rb+ofs] := Re,
                            mem[Rb+ofs+1] := Re+1,
                            ...,
                            mem[Rb+ofs+(f-e+1)] := Rf,

restore  RRX   RRXEXP  e,41 

-------------------------------------------------

    
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

A directive is an assembly language statement that doesn't generate an
instruction, but which gives further information about how to
translate the program to object code.

Directive formats

~~~~
ASMDIR    operand:    xyz module
ASMDIRX   expression  org 234
ASMDIRNS  list of names   import m1,m2 export a,b,c
~~~~

~~~~
abc        Module
x          import   Mod1
y          import   Mod1
z          import   Mod2
           export   x,y,z
           org      34
           org      $02bf
           org      *+100
codeWrite  equ  2
codeRead   equ  1

astart     data 5
           data 9
           data 78
aend
asize      equ  aend-astart
~~~~

#### Expressions

An expression may be used in a directive (e.g. org xyz) or in an
instruction (e.g. lea R2,aend-astart[R0]).

Expressions are useful in professional systems programming.  However,
they can easily confuse beginners.  It is essential to understand that
the arithmetic in an expression is evaluated at assembly time, not at
run time.  It's a good idea not even to mention expressions in the
early stages of teaching computer systems.

Consider how expressions are evaluated.  If an expression is used to
define size of a block of memory, its value would need to be known
before any labels appearing after the block can be resolved.  This
could lead to unnecessary complexity.

* A simple and clean rule is that an org statement must be of the form
  constant or *+constant, but *+label is disallowed.  The principle is
  that a first pass through the assembly source code must resolve the
  values of all names.  But this is needlessly restrictive.

* The important point is that during the second pass it should be
  possible to evaluate all expressions without backtracking.  This can
  be achieved by another approach: simply to disallow forward
  references in expressions.  For example this could be allowed

~~~~
a  equ  123
...
   org  *+a
~~~~
   
   
An expression may be

* A literal constant

* An identifier defined within the module

* The sum of a local relocatable epression and a constant

* The difference between two local locatable expressions

* An identifier imported from another module.  In this case, the
  expression may not contain any operators or other terms: the entire
  expression must just be that identifier.
  

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

The import statement states that the value of an identifier is defined
in another module.  During the assembly of the module containing the
import, the identifier is given a provisional value of 0, but this
will be replaced by the actual value by the linker.  For example,

~~~~
    x          import   Mod1
~~~~

a

says that x is a name that can be used in this module, but it is
defined in Mod1 and its actual value will remain unknown until the
linker sets it later on.    

### export

The export statement states that the value of an identifier should be
made available for other modules to import.  For example, this module
defines a function and exports it so other modules can import and call
it:

~~~~
Mod1     module
         export fcn

fcn      add    R1,R1,R1
         jump   0[R12]
~~~~


### equ

~~~~
trapWrite   equ  2
~~~~

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

The assembler doesn't translate a source module in assembly langauge
directly to raw machine language.  Instead, it produces an *object
module* which contains some additional metadata along with the machine
language code.  This metadata enables the linker to combine the object
module with other modules into an *executable module*.

## Object code

Ojbect modules are specified in an object code language with a simple
syntax and only a few types of statement.  Each object statement is
written on line line.  It
begins with a keyword indicating the type of statement, followed by
white space, followed by an operand which must not contain any
spaces.  Operands may contain either hex constants or identifiers.

* In the object language, hex constants are written as four
  characters, using digits 0-9 a-f.  Unlike assembly language, a hex
  constant is not preceded by $.

* Identifiers have the same syntax as in assembly language: a string
  of letters, digits, and underscore characters, beginning with a
  letter.

The object language has six statements: module, org, data, import,
export, and relocate.  These are related to corresponding statements
in assembly language, but their syntax is different.  For example, an
import statement in assembly language will generate one or more import
statements in the object code, but those statements have a different
syntax and contain different information.

### module statement

Declare the name of the module.  The argument must be an indentifier.
The following statement says that this is the object code for module
named abc.

~~~~
module abc
~~~~

### org statement

The org statement sets the location counter to a specified address.
Currently the argument must be a a 4-digit hexadecimal constant, such
as 3b9f.

~~~~
org  0a04
~~~~

### data statement

The data statement specifies a sequence of constants to be placed in
consecutive memory locations starting at the location counter, subject
to relocation.  Its argument is a list of one or more 4-digit hex
constants separated by commas.

A long block of data can be broken up into several data statements.
Suppose x1, x2, etc are 4-digit hex constants.  Then

~~~~
data  x1,x2,x3,x4,x5,x6
~~~~

is equivalent to

~~~~
data x1,x2,x3
data x4,x5,x6
~~~~

Suppose

* The module's relocation constant is r

* The location counter has been set to c

* The i'th constant (counting from 0) in a data statement is x.

Then the linker will set mem[r+c+i] := x.

One point to watch out for is that an assembly language data statement
uses $ to indicate that a number is a hex constant (e.g. $03b7) but
the object language data statement requires all numbers to be 4-digit
hex constants, and does not require (or allow) a preceding $ character

### import statment

~~~~
import mod3,sqr,addr,addr,...
~~~~


### export statement

An export statement says that the module is making the value of a
symbol available for use in other modules, which may import it.  The
statement takes two operands: the name being exported and the value,
which must be a 4-digit hex constant.  It makes no difference whether
the name is relocatable, as the linker performs any relocation before
writing the exported value into other modules that import it.
Examples:

~~~~
export  haltcode,0
export  fcn,002c
~~~~

### relocate statement

The relocate statement specifies a list of addresses of words that
must be relocated.  Suppose the value x is specified in a relocate
statement, and the linker is relocating the module by offset y.  Then
the linker will set mem[x+y] = obj[x]+y.

~~~~
relocate hex4,hex4,...
~~~~


## Executable code

An executable module is written in the same language as object
modules.  The only difference is that an executable module must
contain only these types of statement: module, data, org.  It is now
allowed to contain any of the following statments: import, export,
relocate.

If an assembly language program doesn't contain any import or export
directives, then its object code won't contain any import, export, or
relcate statements.  In this case, the object code is already
executable and does not require linking: it can be booted directly by
the processor.

The booter (invoked by clicking the Boot button in the processor page)
reads in the currently selected module and checks to see whether it is
a valid executable module.  If so, it loads the code into the memory.
If not, it indicates that the program cannot be booted.

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

## Using the instructions

**load and store**
  
* Use load to copy a variable (either ordinary variable x or array
  element a[i]) from memory to register.

* Use store to copy the value in a register into memory.
  
**lea**
  
* (Common usage) Use lea to put a constant into a register: *lea
  R2,39[R0] ; R2 = 39*

* (Common usage) Use lea to put the *address* of a variable into a
  register.  (Use load to put the value of the varable into a
  register.)

* Example: to write a string, you need to (1) put the code for write
  in R1 (this is likely to be a constant , so use lea); (2) put the
  address of the string in R2 (use lea); and (3) put the length of the
  string in R3 (this may be a constant, so use lea); then finally
  *trap R1,R2,R3*

* (Less common usage) Use lea to add a binary constant to a register
  containing a binary number: *lea R5,1[R5] ; R5 = R5 + 1*.

## Computer Architecture
















  



### Memory


### Limitation of register file: it's small}


 *  The register file is used to perform calculations
 *  In computing somethine like \texttt{x := (2*a + 3*b) / (x-1)},
  all the arithmetic will be done using the register file
 *  But it has a big limitation:
  
   *  There are only 16 registers
   *  And most programs need more than 16 variables!
  
 *  Solution: the \alert{memory} is large and can hold far more data
  than the register file





### Memory}


 *  The memory is similar to the register file: it is a large
  collection of words
 *  A variable name (x, sum, count) refers to a word in memory
 *  Some differences between memory and register file:
  
   *  The memory is \alert{much larger}: 65,536 locations (the
    register file has only 16)
   *  The memory cannot do arithmetic
  
 *  So our strategy in programming:
  
   *  Keep data permanently in memory
   *  When you need to do arithmetic, copy a variable from memory to
    a register
   *  When finished, copy the result from a register back to memory
  





### Registers and memory


 *  The \alert{register file}
  
   *  16 registers 
   *  \stress{Can do arithmetic, but too small to hold all your
      variables}
   *  Each register holds a 16-bit word
   *  Names are R0, R1, R2, $\ldots$, R15
   *  You can do arithmetic on data in the registers
   *  \alert{Use registers to hold data temporarily that you're
      doing arithmetic on}
  
 *  The \alert{memory}
  
   *  65,536 memory locations
   *  Each memory location holds a 16-bit word
   *  Each memory location has an \alert{address} 0, 1, 2, $\ldots$,
    65,535
   *  The machine cannot do arithmetic on a memory location
   *  \alert{Use memory locations to store program variables
      permanently.  Also, use memory locations to store the program.}
  





### Copying a word between memory and register

There are two instructions for accessing the memory


 *  \alert{load} copies a variable from memory to a register
  
   *  \stress{\texttt{load R2,x[R0]}} copies the variable \texttt{x}
    from memory to register R2
   *  \stress{R2 := x}
   *  R2 is changed; x is unchanged
  
 *  \alert{store} copies a variable from a register to memory
  
   *  \stress{\texttt{store R3,y[R0]}} copies the word in register
    R3 to the variable \texttt{y} in memory
   *  \stress{y := R3}
   *  y is changed; R3 is unchanged
  
 *  Notice that we write \alert{[R0]} after a variable name.  Later
  we'll see the reason.





### An assignment statement in machine langauge

\alert{x := a+b+c}

\vspace{0.5em}
~~~~
       load   R1,a[R0]      ; R1 := a
       load   R2,b[R0]      ; R2 := b
       add    R3,R1,R2      ; R3 := a+b
       load   R4,c[R0]      ; R4 := c
       add    R5,R3,R4      ; R5 := (a+b) + c
       store  R5,x[R0]      ; x := a+b+c
~~~~

\begin{enumerate}
 *  Use \important{load} to \stress{copy variables from memory to
    registers}
 *  Do arithmetic with \important{add, sub, mul, div}
 *  Use \important{store} to \stress{copy result back to memory}
\end{enumerate}




### Why do we have registers and memory


 *  The programmer has to keep track of which variables are
  currently in registers
 *  You have to use load and store instructions to copy data between
  the registers and memory
 *  Wouldn't it be easier just to get rid of the distinction between
  registers and memory?  Do all the arithmetic on memory
 *  Short answer:
  
   *  Yes, it's possible to design a computer that way
   *  But it makes the computer \emph{very much slower}
   *  With modern circuits, a computer without load and store
    instructions (where you do arithmetic on memory locations) would
    run between 100 and 1,000 times slower
  





### Constants: the lea instruction


 *  The RTM has an instruction that loads a constant into a register
 *  Use the \alert{lea} instruction
 *  \stress{lea \important{R2},\important{57}[R0]} loads the
  constant 57 into R2: \stress{R2 := 57}
 *  \emph{Actually, lea does much more than this --- later we'll see
    some advanced applications}
 *  General form: \stress{lea
    \important{$R_d$},\important{const}[R0]}
 *  You must write [R0] after the constant; we'll see the reason for
  this later on





### Example using lea

~~~~
; R3 := R1 + 39*R2

    lea   R4,39[R0]    ; R4 := 39
    mul   R3,R4,R2     ; R3 := 39 * R2
    add   R3,R1,R3     ; R3 := R1 + (39*R2)
~~~~




### Stopping the program

The last instruction should be

~~~~
     trap   R0,R0,R0   ; halt
~~~~

This tells the computer to halt; it stops execution of the program




### Defining variables

To define variables x, y, z and give them initial values

~~~~
x    data   34    ; x is a variable with initial value 34
y    data    9    ; y is initially 9
z    data    0    ; z is initially 0
abc  data  $02c6  ; specify initial value as hex
~~~~

The data statements should come \emph{after} all the instructions in
the program (we'll see why later)




### A complete example program


~~~~
; Program Add
; A minimal program that adds two integer variables

; Execution starts at location 0, where the first instruction will be
; placed when the program is executed.

      load   R1,x[R0]   ; R1 := x
      load   R2,y[R0]   ; R2 := y
      add    R3,R1,R2   ; R3 := x + y
      store  R3,z[R0]   ; z := x + y
      trap   R0,R0,R0   ; terminate

; Static variables are placed in memory after the program

x     data  23
y     data  14
z     data  99
~~~~
}


### Programming languages and Compiling


### Compiling


 *  The computer cannot execute programs in a high level language
 *  Therfore we must \emph{translate} a program into assembly
  language
 *  {\color{blue}Translating from a high level programming language
  to assembly language is called} \alert{compiling}
 *  This is done by software called a \alert{compiler}: it reads in
  a program in e.g. C++ and translates it to assembly language
 *  There are many benefits of using compilers
  
   *  We can have many compilers, one for each language, so a
    computer can run programs in \emph{many} languages
   *  The compilers can make programming easier: good error
    messages, etc.
   *  Languages can be designed to fit well for different purposes
  
 *  For each type of high level language construct, we will
  translate to assembly language following a standard pattern




#### High level constructs


### Statements

A program contains

 *  Statements that perform calculations
  
   *  Assignment statements
  
 *  Statements that determine what order the calculations occur in
  
   *  Conditionals: if---then---else
   *  Loops: while, repeat, for
   *  Structuring computation: functions, procedures,
    coroutines, recursion
  
 *  These are called \alert{control structures}





### High level control structures


 *  Notation
  
   *  $S$, $S_1$, $S_2$, etc. means ``any statement'' (e.g. an
    assignment statement)
   *  $bexp$ means any Boolean expression (an expression that is either
    True or False).  Examples \texttt{x>3}
  
 *  \textbf{Block.} We can treat several consecutive statements as
  just a single statement: \alert{$\{ S_1; S_2; S_3; \}$}
 *  \textbf{if-then.}  \alert{\texttt{if bexp then S;}}
 *  \textbf{if-then-else.} \alert{\texttt{if bexp then $S_1$ else
      $S_2$;}}
 *  \textbf{while-loop.} \alert{\texttt{while bexp do $S$}}
 *  And there are many more




#### Low level constructs


### Low level constructs


 *  Assignment statements:  \alert{x := a * 2}
 *  Goto:   \alert{goto computeTotal}
 *  Conditional:  \alert{if $x<y$ then goto loop}
 *  First we translate high level constructs into these low level
  statements
 *  Then translate the low level statements into assembly language



### The Goto statement

~~~~
        S;
loop:   S;
        S;
        S;
        goto loop;
~~~~


 *  Many (not all) programming languages have a \alert{goto}
  statement
 *  Any statement may have a \alert{label} (for example ``loop'')
 *  Normally execution proceeds from one statement to the next, on
  and on
 *  A \alert{goto L} transfers control to the statement with label L





### Using the goto statement


 *  The first programming language (Fortran, 1955) didn't have fancy
  control structures --- you had to do nearly everything with goto
 *  But goto leads to unreadable programs and unreliable software
 *  The modern view:
  
   *  In a high level language, \alert{you should \emph{not} use
      goto}
   *  For low level programming --- like assembly language --- the
    goto serves as the \alert{foundation} for implementing the higher
    level control statements
  
 *  We will use two forms:
  
   *  goto L
   *  if b then goto L
  





### The conditional goto statement


 *  if bexp then goto label
 *  bexp is a Boolean expression: $x<y$, $j=k$, $abc>def$
 *  If the bexp is True the statement goes to the label
 *  Otherwise we just move on the the next statement
 *  \important{The only thing you can put after \bluetext{then} is a
    \bluetext{goto} statement}




### Jumping and comparing

#### Unconditional jump


### Jumping


 *  The foundation of control structures is \alert{jump}
  instructions
 *  \alert{Jumping} is the machine language equivalent of
  \alert{goto}
 *  An instruction may have a \emph{label}
 *  The label is a name, starting with a letter, and must appear
  starting in the first character of a line
 *  The unconditional instruction \alert{jump loop[R0]} means
  \alert{goto loop}




#### Comparison instructions


### Comparison instruction: Boolean form


   *  cmplt R2,R5,R8
   *  Means ``compare for Less Than''
   *  The operands are compared: R5 $<$ R8
   *  This gives a Boolean, 0 (for False) or 1 (for True)
   *  That Boolean result is loaded into the destination R2
   *  There are three of these instructions
    
     *  cmplt --- compare for Less Than
     *  cmpeq --- compare for Equal
     *  cmpgt --- compare for Greater Than
    
  


#### Conditional jumps


### Conditional jumps: Boolean decision


 *  There are two instructions: you can jump if a Boolean is False
  or True
 *  jumpf --- jump if False
  
   *  jumpf R4,aardvark[R0]
   *  Means if R4 contains False, then goto aardvark
   *  0 means False, so this means if R4=0 then goto aardvark
  
 *  jumpt --- jump if True
  
   *  jumpt R5,banana[R0]
   *  Means if R5 contains True, then goto banana
   *  Any number other than 0 means True, so this means if R5 $\neq$
    0 then goto banana
  




### Compilation patterns

### Compilation patterns


 *  Each programming construct can be translated according to a
  standard pattern
 *  It's useful to translate in two steps:
  
   *  First, translate complex statements to simple high level
    statements (go to label, if b then goto label)
   *  The ``goto form'' of the algorithm corresponds closely to
    machine instructions
   *  Then it's straightforward to complete the translation to
    assembly language
  
   *  Assignment statements --- loads, then arithmetic, then store
   *  goto label --- jump label[R0]
   *  if b then goto label --- jumpt R5,label[R0] where R5 contains
    b
   *  if not b then goto label --- jumpf R5,label[R0] where R5
    contains b
  
   *  This approach clarifies how the algorithm works
  





### Compiling an assignment statement

Load the operands; do calculations; store results

~~~~
; x := a + b*c;
   load  R1,a[R0]   ; R1 = a
   load  R2,b[R0]   ; R2 = b
   load  R3,c[R0]   ; R3 = c
   mul   R4,R2,R3   ; R4 = b*c
   add   R4,R1,R4   ; R4 = a + (b*c)
   store R4,x[R0]   ; x := a+(b*c)
~~~~



#### if bexp then S


### if bexp then S

~~~~
if x<y
  then {statement 1;}
statement 2;
~~~~

\alert{Translates into}

~~~~
   R7 := (x < y)
   jumpf R7,skip[R0]
   instructions for statement 1
skip
   instructions for statement 2 
~~~~




### Example: code with if-then

Source program fragment:

~~~~
x := 2;
if y>x
   then { a := 5; }
b := 6;
~~~~




### Example: translating if-then

{\small
~~~~
; x := 2;
      lea     R1,2[R0]    ; R1 := 2
      store   R1,x[R0]    ; x := 2

; if y>x
      load    R1,y[R0]    ; R1 := y
      load    R2,x[R0]    ; R2 := x
      cmpgt   R3,R1,R2    ; R3 := (y>x)
      jumpf   R3,skip[R0] ; if y <= x then goto skip

;  then { a := 5; }
      lea    R1,5[R0]     ; R1 := 5
      store  R1,a[R0]     ; a := 5

; b := 6;
skip  lea    R1,6[R0]     ; R1 := 6
      store  R1,b[R0]     ; b := 6
~~~~
}



#### if bexp then S1 else S2


### if bexp then S1 else S2

~~~~
if x<y
  then { S1 }
  else { S2 }
S3
~~~~

Compiled into:
~~~~
   R5 := (x<y)
   jumpf R5,else[R0]
; then part of the statement
   instructions for S1
   jump   done[R0]
; else part of the statement
else
   instructions for S2 
done
   instructions for statement S3
~~~~



#### while bexp do S



### while b do S

~~~~
while i<n do
  { S1 }
S2
~~~~

Compiled into:
~~~~
loop
   R6 := (i<n)
   jumpf  R6,done[R0]
   ... instructions for the loop body S1 ...
   jump   loop[R0]
done
  instructions for S2
~~~~




### Infinite loops

~~~~
while (true)
  {statements} 
~~~~

Compiled into:
~~~~
loop
   ... instructions for the loop body ...
   jump   loop[R0] 
~~~~



#### Nested statements


### Nested statements


 *  For each kind of high level statement, there is a pattern for
  translating it to
  \begin{enumerate}
   *  Low level code (goto)
   *  Assembly language
  \end{enumerate}
 *  In larger programs, there will be \alert{nested statements}


~~~~
if b1
  then { S1;
         if b2 then {S2} else {S3};
         S4;
       }
  else { S5;
         while b3 do {S6};
       }
S7
~~~~




### How to compile nested statements


 *  A \alert{block} is a sequence of instructions where
  
   *  To execute it, \alert{always start with the first statement}
   *  When it finishes, it \alert{always reaches the last statement}
  
 *  Every statement should be compiled into a block of code
 *  This block may contain internal structure --- it may contain
  several smaller blocks --- but to execute it you should
  \alert{always begin at the beginning and it should always finish at
    the end}
 *  The patterns work for nested statements
 *  You need to use new labels (can't have a label like ``skip'' in
  several places)



### Programming technique


### Programming technique

There are two ways to handle variables:


 *  The \emph{statement-by-statement style}:
  
   *  {\color{blue}Each statement is compiled independently.}
   *  load, arithmetic, store
   *  Straightforward but inefficient.
   *  {\color{blue}Use this style if you feel confused.}
  
 *  The \emph{register-variable style}:
  
   *  {\color{blue}Keep variables in registers across a group of
      statements}
   *  Don't need as many loads and stores
   *  More efficient
   *  You have to keep track of whether
    variables are in memory or a register.
   *  \alert{Use comments to show register usage.}
   *  Real compilers use this style.
   *  {\color{blue}Use this style if you like the shorter code it
      produces.}
  




### Examples of the two styles

We'll translate the following program fragment to assembly
language, using each style:

~~~~
x = 50;
y = 2*z;
x = x+1+z;
~~~~



#### Statement-by-statement style


### Example of statement-by-statement style

{\small
~~~~
; x = 50;
     lea    R1,$0032   ; R1 = 50
     store  R1,x[R0]   ; x = 50

; y = 2*z;
     lea    R1,$0002   ; R1 = 2
     load   R2,z[R0]   ; R2 = z
     mul    R3,R1,R2   ; R3 = 2*z
     store  R3,y[R0]   ; y = 2*z

; x = x+1+z;
     load   R1,x[R0]   ; R1 = x
     lea    R2,1[R0]   ; R2 = 1
     load   R3,z[R0]   ; R3 = z
     add    R4,R1,R2   ; R4 = x+1
     add    R4,R4,R3   ; R4 = x+1+z
     store  R4,x[R0]   ; x = x+1+z
~~~~
}



#### Register-variable style}


### Example of register-variable style

{\small
~~~~
; Usage of registers
;   R1 = x
;   R2 = y
;   R3 = z

; x = 50;
     lea    R1,$0032   ; x = 50
     load   R3,z[R0]   ; R3 = z
     lea    R4,$0002   ; R4 = 2
; y = 2*z;
     mul    R2,R4,R3   ; y = 2*z
; x = x+1+z;
     lea    R4,$0001   ; R4 = 1
     add    R1,R1,R4   ; x = x+1
     add    R1,R1,R3   ; x = x+z
     store  R1,x[R0]   ; move x to memory
     store  R2,y[R0]   ; move y to memory
~~~~
}


                                
#### Comparison of the styles


### Comparison of the two styles


 *  Statement by statement
  
   *  Each statement is compiled into a separate block of code.
   *  Each statement requires loads, computation, then stores.
   *  A variable may appear in several different registers.
   *  There may be a lot of redundant loading and storing.
   *  The object code corresponds straightforwardly to the source
    code, but it may be unnecessarily long.
  
 *  Register variable
  
   *  The instructions corresponding to the statemnts are mixed
    together.
   *  Some statements are executed entirely in the registers.
   *  A variable is kept in the same register across many
    statments.
   *  The use of loads and stores is minimised.
   *  The object code is concise, but it's harder to see how it
    corresponds to the source code.
  
 *  It's possible to have a mixture of the styles: you don't have
  to follow one or the other all the time.






## Machine language

### Machine language: representing instructions in memory


 *  The actual bits representing an instruction (written in hex) ---
  \alert{0d69} -- is \alert{machine language}
 *  The actual hardware runs the machine language --- it's just
  looking at the numbers
 *  The text notation with names --- \alert{add R13,R6,R9} --- is
  \alert{assembly language}
 *  Assembly language is for humans, machine language is for
  machines
 *  Both \alert{specify the program in complete detail}, down to the
  last bit




### What's in the memory?


 *  All your program's data
  
   *  Variables
   *  Data structures, arrays, lists
  
 *  \alert{And also the machine language program itself!}


\begin{block}{The \textbf{stored program computer}}
  The program is stored inside the computer's main memory, along with
  the data.\\
  \vspace{1em} An alternative approach is to have a separate memory to
  hold the program, but experience has shown that to be inferior for
  general purpose computers.  (Special-purpose computers often do
  this.)
\end{block}




### Instruction formats: different types of instruction


 *  Sigma16 has \important{three kinds of instruction:}
  
   *  \alert{RRR} instructions use the \alert{registers}
   *  \alert{RX} instructions use the \alert{memory}
   *  \alert{EXP} instructions use \alert{registers and constant}
  
 *  Each kind of instruction is called an \alert{instruction format}
 *  All the instructions with the same format are similar
 *  \alert{Each instruction format has a standard representation in
    the memory.}




### Instruction formats: representing instructions

 
 *  The machine language program is in the memory 
 *  So we need to represent each instruction as a word
 *  An \important{instruction format} is \stress{a systematic way to
    represent an instruction using a string of bits, on one or more
    words}
 *  Every instruction is either RRR, RX, or EXP 
  
   *  An RRR instruction is represented in \alert{one word}
    (remember, a word is 16 bits)
   *  An RX or EXP instruction is represented in \alert{two words}
  
 *  We just need to learn three ways to represent an instruction!
 *  For now, we just need RRR and RX (EXP is needed only for some
  more advanced instructions, which we'll see later)





### Fields of an instruction word


 *  An instruction word has 16 bits
 *  There are four fields, each 4 bits
 *  We write the value in a field using \important{hexadecimal}
  \begin{enumerate}
   *  hex digits: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f
   *  represent numbers 1, $\ldots$, 15
  \end{enumerate}
 *  The fields have standard names:
  
   *  op --- holds the operation code
   *  d  --- usually holds the destination register
   *  a  --- usually holds the first source operand register
   *  b --- usually holds the second source operand register
  




#### RRR instructions


### RRR instructions


 *  Every RRR instruction consists of
  
   *  An operation (e.g. add)
   *  Three register operands: a destination and two operands
   *  The instruction performs the operation on the operands and
    puts the result in the destination
  




### Representing RRR


 *  Example:  add R3,R12,R5
 *  We need to specify \alert{which} RRR instruction this is.  Is it
  add? sub? mul? another?
 *  This is done with an \alert{operation code} --- a number that
  says what the operation is
 *  There are about a dozen RRR instructions, so a 4-bit operation
  code suffices
 *  We also need to specify three registers: destination and two
  source operands
 *  There are 16 registers, so a particular one can be specified by
  4 bits
 *  Total requirements: 4 fields, each 4 bits --- total 16 bits
 *  An RRR instruction exactly fills one word





### Some RRR instructions


 *  All RRR instructions have the same form, just the operation
  differs
  
   *  add  R2,R2,R5   \qquad  ; R2 = R2 + R5
   *  sub  R3,R1,R3   \qquad  ; R3 = R1 - R3
   *  mul  R8,R6,R7   \qquad  ; R8 = R6 * R7
  
 *  In ``add R2,R5,R9'' we call R5 the \alert{first operand}, R9 the
  \alert{second operand}, and R2 the \alert{destination}
 *  It's ok to use the same register as an operand and destination!
 *  Later we'll see some more RRR instructions, but they all have
  the same form as these do





### A few RRR operation codes

\begin{tabular}{lc}
  mnemonic  &  operation code  \\
  \hline
  add       &  0 \\
  sub       &  1 \\
  mul       &  2 \\
  div       &  3 \\
   $\vdots$ & $\vdots$ \\
  trap      &  d \\
  \hline
\end{tabular}

\vspace{2em}
\alert{Don't memorise this table!  You just need to understand how
  it's used.}




### Example of RRR

{\Large\color{red}
~~~~
add  R13,R6,R9
~~~~
}


 *  Since each field of the instruction is 4 bits, written as a hex
  digit
 *  The opcode (operation code) is 0
 *  Destination register is 13 (hex d)
 *  Source operand registers are 6 and 9 (hex 6 and 9)
 *  So the instruction is:



\color{red}
\fbox{\Large 0d69}




#### RX instructions


### RX instructions


 *  Every RX instruction contains two operands:
  
   *  A \important{register}
   *  A \important{memory location}
  
 *  We have seen several so far:
  
   *  lea  R5,19[R0]  ; R5 = 19
   *  load R1,x[R0]  ; R1 = x
   *  store R3,z[R0] ; z = R3
   *  jump  finished[R0] ; goto finished
  




### RX instructions

A typical RX instruction: \alert{load R1,x[R0]}

 *  The first operand (e.g. R1 here) is called the \emph{destination
    register}, just like for RRR instructions
 *  The second operand \alert{x[R0]} specifies a \alert{memory
    address}.
 *  Each variable is kept in memory at a specific location: we talk
  about \alert{the address of a variable}
 *  The memory operand has two parts:
  
   *  The variable x is a name for the address where x is kept ---
    called the \alert{displacement}
   *  The R0 part is just a register, called the \alert{index
      register}
  




### Format of RX instruction

{\Large\color{red}
~~~~
load R1,x[R0]
~~~~
}


 *  There are two words in the machine language code
 *  The first word has 4 fields: op, d, a, b
  
   *  op contains f for every RX instruction
   *  d contains the register operand (in the example, 1)
   *  a contains the index register (in the example, 0)
   *  b contains a code indicating \emph{which} RX instruction this
    is (1 means load)
  
 *  The second word contains the displacement (address) (in the
  example, the address of x)


Suppose x has memory address 0008.  Then the machine code for load
R1,x[R0] is:


\color{red}
\fbox{\Large\vbox{\hbox{f101}\hbox{0008}}}





### Operation codes for RX instructions


 *  Recall, for RRR the op field contains a number saying
  \emph{which} RRR instruction it is
 *  For RX, the op field \emph{always contains f}
 *  So how does the machine know \emph{which} RX instruction it is?
 *  Answer: there is a secondary code in the b field



\begin{tabular}{ll}
  mnemonic  &  RX operation code (in b field) \\
  \hline
  lea       &  0 \\
  load      &  1 \\
  store     &  2 \\
   $\vdots$ & $\vdots$ \\
  \hline
\end{tabular}




### The assembler


### Assembly language

 *  Humans write \stress{assembly language}
  
   *  The program is text:  {\color{red}add R4,R2,R12}
   *  It's easier to read
   *  You don't need to remember all the codes 
   *  Memory addresses are \stress{\emph{much easier}} to handle
  
 *  The machine executes \stress{machine language}
  
   *  The program is words containing 16-bit numbers:
    {\color{red}042c}
   *  It's possible for a digital circuit (the computer) to execute
   *  No names for instructions or variables: everything is a number
  




### The assembler


 *  A human writes a machine-level program in assembly language
 *  A software application called the \alert{assembler} reads it in,
  and translates it to machine language
 *  What does the assembler do?
  
   *  When it sees an instruction mnemonic like add or div, it
    replaces it with the operation code (0, 3, or whatever).
   *  The assembler helps with variable names --- the machine
    language needs addresses (numbers) and the assembler calculates
    them
  





### Assembly language


 *  Each statement corresponds to one instruction
 *  You can use names (add, div) rather than numeric codes (0, 3)
 *  You can use variable names (x, y, sum) rather than memory
  addresses (02c3, 18d2)
 *  You write a program in assemply language
 *  The \emph{assembler} translates it into machine language
 *  What's the relationship between compilers and assemblers?
  
   *  Compilers translate between languages that are very different
   *  Assemblers translate between very similar languages
  





### A sequence of RRR instructions

\textbf{Assembly language}
~~~~
    add    R3,R5,R1
    sub    R4,R2,R3
    mul    R1,R9,R10   
~~~~

\vspace{1em}
\alert{Run the assembler$\ldots$}
\vspace{2em}

\textbf{Machine language}
~~~~
    0351
    1423
    219a
~~~~




### Variable names and addresses


 *  Each variable needs to be declared with a \alert{data} statement
 *  x  data  23
 *  This means: allocate a word in memory for x and initialize it to
  23
 *  \alert{The data statements should come after the trap
    instruction that terminates the program}





### Instructions in assembly language


 *  The syntax is simple, but you have to follow the form of the
  instructions exactly!
 *  RRR instructions
  
   *  Typical example: \important{add  R8,R2,R12}
   *  R8 is the \stress{destination} (where the result goes) 
   *  R2 and R12 are the \stress{sources} (the operands to be added)
  
 *  RX instructions
  
   *  RX instructions specify a \stress{register} and a
    \stress{memory location}
   *  Typical example: \important{load R3,x[R0]}
   *  Meaning of load:  R3 = x
   *  \important{store R3,y[R0]}
   *   Meaning of store: y = R3
   *  \important{load} copies from \important{memory to register}
   *  \important{store} copies from \important{register to memory}
  



### How the assembler allocates memory


### How the assembler allocates memory}

\begin{enumerate}
 *  The assembler maintains a variable called the \important{location
    counter}.  This is the address where it will place the next piece
  of code.
 *  Initially the location counter is 0.
 *  The assembler reads through each line of code
  \begin{enumerate}
   *  If there is a label, it remembers that the value of the label
    is the current value of the location counter.  This goes into the
    \important{symbol table}
   *  The assembler decides how many words of memory this line of
    assembly code will require (add needs one word, load needs two),
    and adds this to the location counter.
  \end{enumerate}
 *  Then the assembler reads through the assembly language program
  \emph{a second time}
 *  Now it generates the words of object code for each statement.
  If there is a reference to a label that appears farther on
  (e.g. load x, or jump loop) it looks up the value of the label in
  the symbol table.
\end{enumerate}




### Program structure


 *  A complete program needs
  
   *  Good comments explaining what it is
   *  The actual program --- a sequence of instructions
   *  An instruction to stop the program: \alert{trap R0,R0,R0}
   *  Declarations of the variables: \alert{data} statements
  
 *  Why do we put the instructions first, and define the variables
  at the end?
  
   *  The assembler can find the definitions because it reads the
    program twice: the first pass finds all the labels, the second
    pass generates the machine language code
   *  The computer will start executing at memory address 0, so
    there had better be in instruction there, not data!
  




### Example program Add


~~~~
; Program Add
; A minimal program that adds two integer variables

; Execution starts at location 0, where the first instruction will be
; placed when the program is executed.

      load   R1,x[R0]   ; R1 := x
      load   R2,y[R0]   ; R2 := y
      add    R3,R1,R2   ; R3 := x + y
      store  R3,z[R0]   ; z := x + y
      trap   R0,R0,R0   ; terminate

; Static variables are placed in memory after the program

x     data  23
y     data  14
z     data  99
~~~~
}



### Snapshot of memory: example program Add


\begin{tabular}{c|c|l}
  address & contents & what the contents mean \\
  \hline
     0000 & f101 & first word of load R1,x[R0]  \\
     0001 & 0008 & second word: address of x \\
     0002 & f201 & first word of load R2,y[R0] \\
     0003 & 0009 & second word: address of y \\
     0004 & 0312 & add R3,R1,R2 \\
     0005 & f302 & first word of store R3,z[R0] \\
     0006 & 000a & second word: address of z \\
     0007 & d000 & trap R0,R0,R0 \\
     0008 & 0017 & x = 23 \\
     0009 & 000e & y = 14 \\
     000a & 0063 & z = 99 \\
  \hline
\end{tabular}




### Control registers


### Boot: reading in the program


 *  The program is placed in memory starting at location 0
 *  The program should finish by executing the instruction ``trap
  R0,R0,R0''
 *  Normally, trap R0,R0,R0 should be the last instruction of the
  program (i.e. the program begins execution with the first
  instruction, and ends execution with the last, although it may
  jump around during execution)
 *  After the trap R0,R0,R0 come the data statements, which tell
  the assembler the names of the variables and their initial values
 *  These conventions were typical for early computers; later we
  will discuss how the operating system interacts with user
  programs





### Control registers


 *  Some of the registers in the computer are accessible to the
  programmer: R0, R1, R2, $\ldots$, R15
 *  There are several more registers that {\color{blue}the machine
    uses to keep track of what it's doing}
 *  These are called \alert{``control registers''}
 *  They are (mostly) invisible to the program





### Keeping track of where you are



When you ``hand execute'' a program, you need to know
  
   *  Where you are (point a finger at the current instruction)
   *  What you're doing (read the current instruction)
  
 *  \emph{The computer needs to know this too!}
 *  The \alert{PC register} (``program counter'') contains
  \alert{the address of the next instruction} to be executed
 *  The \alert{IR} (``instruction register'') contains \alert{the
    instruction being executed right now}
 *  If an RX instruction is being executed, the \alert{ADR}
  (``address register'') contains \alert{the memory address} of the
  second operand.





### Following PC and IR control registers


 *  Try running a simple program
 *  Step through the execution
 *  Before each instruction executes, look at the PC and IR
  registers
 *  Notice that PC always contains the \alert{address of the next
    instruction} and IR always contains the \alert{current
    instruction}
 *  {\color{blue}The control registers help to understand in detail
    what the machine is doing.}




### Assembly language syntax


### Assembly language syntax


 *  The syntax of assembly language is simple and rigid
 *  See the document \important{Sigma16 Programming Reference},
  where you will find these notes and additional tips and techniques



### Fields separated by spaces


 *  An assembly language statement has \alert{four fields, separated
    by space}
  
   *  label (optional) -- if present, must begin in leftmost
    character
   *  operation load, add, etc.
   *  operands: R1,R2,R3 or R1,x[R0]
   *  comments: ; x = 2 * (a+b)
  
 *  \alert{There cannot be any spaces inside a field}
  
   *  R1,R12,R5 is ok
   *  R1, R12,R5 is wrong
  


{\color{blue}
~~~~
loop   load   R1,count[R0]    ; R1 = count
       add    R1,R1,R2        ; R1 = R1 + 1
~~~~
}

The assember first breaks each statement into the four fields; then it
looks at the operation and operands.



### Correct form of operand field


 *  RRR
  
   *  Exactly three registers separated by commas
   *  Example: \alert{R8,R13,R0}
  
 *  RX
  
   *  Two operands: first is a register, second is an address
   *  Address is a name or constant followed by [register]
   *  Example: \alert{R12,array[R6]}
  



### Each of these statements is wrong!


~~~~
    add   R2, R8, R9     Spaces in the operand field
    store x[R0],R5       First operand must be register, second is address
  loop load R1,x[R0]     Space before the label
    jumpt R6,loop        Need register after address:  loop[R0]
    jal   R14, fcn[R0]   Space in operand field
~~~~
}

If you forget some detail, look at one of the example programs



### Writing constants


 *  In assembly language, you can write constants in either decimal
  or hexadecimal
  
   *  \alert{decimal: }  50
   *  \alert{hexadecimal:} \$0032
  


Examples:

~~~~
   lea   R1,40[R0]      ; R1 = 40
   lea   R2,$ffff[R0]   ; R2 = -1

x  data  25
y  data  $2c9e
~~~~



### Good style


 *  It isn't enough just to get the assembler to accept your program
  without error messages
 *  Your program should be \alert{clear and easy to read}
 *  This requires good style
 *  \alert{Good style saves time writing the program and getting it
    to work}
 *  A sloppy program looks unprofessional


### Comments


 *  In Sigma16, a semicolon \alert{;} indicates that the rest of the
  line is a comment
 *  You can have a full line comment: just put ; at the beginning
 *  You should use good comments in all programs, regardless of
  language
 *  But they are even more important in machine language, because
  the code needs more explanation
 *  At the beginning of the program, use comments to give the name
  of the program and to say what it does
 *  Use a comment on every instruction to explain what it's doing



### Indent your code consistently

Each field should be lined up vertically, like this:

 \color{blue}
~~~~
    load   R1,three[R0]  ; R1 = 3
    load   R2,x[R0]      ; R2 = x
    mul    R3,R1,R2      ; R3 = 3*x
    store  R3,y[R0]      ; y = 3*x
    trap   R0,R0,R0      ; stop the program
~~~~
}

Not like this:

 \color{red}
~~~~
    load   R1,three[R0]     ; R1 = 3
  load  R2,x[R0] ; R2 = x
       mul R3,R1,R2           ; R3 = 3*x
 store         R3,y[R0]      ; y = 3*x
   trap  R0,R0,R0      ; stop the program
~~~~
}

{\color{blue}The exact number of spaces each field is indented isn't
  important; what's important is to \alert{make the program neat and
    readable.}}



### Use spaces, not tabs


 *  To indent your code, always use spaces
 *  Don't use tabs!
 *  In general, \alert{never use tabs} except in the (rare) cases
  they are actually required
  
   *  The tab character was introduced to try to mimic the tab key
    on old mechanical typewriters
   *  But \alert{software does not handle tab consistently}
   *  If you use tabs, your can look good in one application and a
    mess in another
  
 *  It's easy to indent with spaces, and it works everywhere!



### Address arithmetic

### Why [R0]?


 *  So far, we have always been writing [R0] after constants or
  names
  
   *  lea  R2,39\alert{[R0]}
   *  load  R3,xyz\alert{[R0]}
   *  store R4,total\alert{[R0]}
  
 *  Why?
 *  This is part of a general and powerful technique called
  \alert{address arithmetic}



### Address arithmetic


 *  Every piece of data in the computer (in registers, or memory) is
  a \alert{word}
 *  A word can represent several different kinds of data
  
   *  So far, we've just been using \alert{integers}
   *  Represented with \alert{two's complement}: $-2^{15}, \ldots,
    -1, 0, 1, 2, \ldots, 2^{15}-1$

 *  Now, we'll start doing computations with \alert{addresses} too
 *  Addresses are \alert{unsigned numbers}: $0, 1, 2, \ldots, 65535$



### What can you do with address arithmetic?


 *  Powerful data structures
  
   *  \emph{Today:} Arrays
   *  Pointers and records
   *  Linked lists, queues, dequeues, stacks, trees, graphs, hash
    tables, $\ldots$ Subject of \emph{Algorithms and Data Structures}
  
 *  Powerful control structures
  
   *  Input/Output
   *  Procedures and functions
   *  Recursion
   *  Case dispatch
   *  Coroutines, classes, methods
  




## Arrays

### Data structures
 

 *  An ordinary variable holds one value (e.g. an integer)
 *  A \emph{data structure} can hold many individual elements
 *  A data structure is a \alert{\emph{container}}
 *  The simplest data structure: \emph{array}
 *  There are many more data structures!
 *  The key idea: {\redtext we will do arithmetic on addresses}


### Arrays


 *  In mathematics, an array (vector) is a sequence of indexed
  values $x_0, x_1, \ldots, x_{n-1}$
  
   *  $x$ is the entire array
   *  $x_3$ is one specific element of the array with index 3
   *  It's useful to refer to an arbitrary element by using an
    integer variable as index: $x_i$
  
 *  Arrays are ubiquitous: used in all kinds of applications
 *  In programming languages, we refer to $x_i$ as \texttt{x[i]}


### Representing an array


 *  An array is represented in a computer by placing the elements in
  consecutive memory locations
 *  The array x starts in memory at some location: here, it's 01a5
 *  The address of the array x is the address of its first element
  x[0]
 *  The elements follow in consecutive locations


\vspace{1em}

\begin{tabular}{lccccccccccc}
value   &          & x[0] & x[1] & x[2] & x[3]
  & x[4] & x[5] & x[6] & \\
address & $\ldots$ & 01a5 & 01a6 & 01a7 & 01a8
  & 01a9 & 01aa & 01ab & $\ldots$ \\
\end{tabular}


\vspace{1em}

\fbox{\alert{\Large The address of x[i] is x+i}}


### Allocating an array


 *  An array is in memory along with other data --- after the trap
  that terminates the program
 *  You can allocate the elements and give them initial value with
  data statements
 *  Use the name of the array as a label on the first element (the
  one with index 0)
 *  Don't put labels on the other elements


### Example of array allocation

~~~~
   ...
      trap   R0,R0,R0  ; terminate

; Variables and arrays

abc   data    25       ; some variable
n     data     6       ; size of array x

x     data    13       ; x[0]
      data   189       ; x[1]
      data   870       ; x[2]
      data    42       ; x[3]
      data     0       ; x[4]
      data  1749       ; x[5]

def   data     0       ; some other variable
~~~~

### What about big arrays?


 *  In the programs we'll work with, the arrays will be small (a
  dozen elements or so)
 *  In real scientific computing, it's common to have large arrays
  with thousands --- or even millions --- of elements
 *  It would be horrible to have to write thousands of data statements!
 *  In large scale software, arrays are allocated
  \alert{dynamically} with help from the \alert{operating system}
  
   *  The user program calculates how large an array it wants, and
    stores that in a variable (e.g. n = 40000)
   *  It uses a trap to request (from the operating system) a block
    of memory big enough to hold the array
   *  The operating system returns the address of this block to the
    user program
  
 *  We won't be doing this: we will just allocate small arrays using
  data statements


### Indexed addressing

### Accessing an element of an array


 *  Suppose we have array x with elements x[0], x[1], ..., x[n-1]
 *  Elements are stored in consecutive memory locations
 *  Use the label x to refer to the array; x is also the location of
  x[0]
 *  {\redtext The address of x[i] is x+i}
 *  To do any calculations on x[i], we must load it into a register,
  or store a new value into it
 *  \emph{But how?}
 *  If you try \texttt{load R4,x[R0]} the effect will be R4 := x[0]
 *  We need a way to access x[i] where i is a variable


### Effective address


 *  An RX instruction specifies addresses in two parts, for example
  \alert{result[R0]} or \alert{x[R4]} or \alert{\$00a5[R2]}
  
   *  The \alert{displacement} is a 16 bit constant (you can write
    the number, or use a name --- the assembler will put in the
    address for you)
   *  The \alert{index register} is written in brackets
  
 *  The machine adds the displacement to the value in the index
  register --- this is called the \alert{effective address}
 *  The instruction is performed using the effective address


### Using the effective address

The addressing mechanism is flexible!


 *  You can access an ordinary variable:\\ \alert{load R2,sum[R0]}\\
  R0 always contains 0, so the effective address is just the address
  of sum
 *  You can access an array element: if R8 contains an index i,
  then\\ \alert{load R2,x[R8]}\\ will load x[i] into R2
 *  There's more: effective addresses are used to implement
  pointers, functions, procedures, methods, classes, instances, jump
  tables, case dispatch, coroutines, records, interrupt vectors,
  lists, heaps, trees, forests, graphs, hash tables, activation
  records, stacks, queues, dequeues, $\ldots$


### Addressing modes


 *  An \alert{addressing mode} is a scheme for specifying the
  address of data
 *  Sigma16 has one addressing mode: displacement[index], e.g. x[R4]
 *  Many older computers provided many addressing modes: one for
  variables, another for arrays, yet another for linked lists, still
  another for stacks, and so on
 *  It's more efficient to provide just one or two flexible
  addressing modes, rather than a big collection of them


### Using effective address for an array

Suppose we want to execute \alert{x[i] := x[i] + 50}

~~~~
    lea   R1,50[R0]   ; R1 := 50
    load  R5,i[R0]    ; R5 := i
    load  R6,x[R5]    ; R6 := x[i]
    add   R6,R6,R1    ; R6 := x[i] + 50
    store R6,x[R5]    ; x[i] := x[i] + 50
~~~~

### Array traversal and for loops

### Array traversal


 *  A typical operation on an array is to \alert{traverse} it
 *  That means to perform a calculation on each element


Here's a loop that doubles each element of x:

~~~~
i := 0;
while i < n do
  { x[i] := x[i] * 2;
    i := i + 1;
  }
~~~~
  
### For loops


 *  A for loop is designed specifically for array traversal
 *  It handles the loop index automatically
 *  It sets the index to each array element index and executes the
  body
 *  The intuition is \emph{``do the body for every element of the
  array''}


~~~~
for i := exp1 to exp2 do
   { statements }
~~~~

### Array traversal with while and for

Here is the program that doubles each element of x, written with both
constructs

~~~~
i := 0;
while i < n do             for i := 0 to n-1 do
  { x[i] := x[i] * 2;          { x[i] := x[i] * 2; }
    i := i + 1;
  }
~~~~

### Translating the for loop to low level

High level for loop (with any number of statements in the body)
~~~~
for i := exp1 to exp2 do
   { statement1;
     statement2;
   }
~~~~

Translate to low level with this pattern:

~~~~
       i := exp1;
loop:  if i > exp2 then goto loopdone;
       statement1;
       statement2;
       i := i + 1;
       goto loop;
loopdone:
~~~~

It's straightforward to complete the translation to assembly
language.

### Alternative syntax for for loops

In languages derived from C (C++, Java, C\#, and many more) you will
see for loops written like this:

~~~~
for (i=x; i<y; i++)
   { S1; }
S2;
~~~~

### Example program ArrayMax


 *  A complete programming example
 *  The problem: find the maximum element of an array
 *  To do this we need to
  
   *  Allocate an array
   *  Loop over the elements
   *  Access each element
   *  Perform a conditional
  
 *  This example puts all our techniques together into one program


### State what the program does

; Program ArrayMax
; John O'Donnell

;---------------------------------------------------------------------
; The program finds the maximum element of an array.  It is given
;   *  a natural number n, assume n>0
;   *  an n-element array x[0], x[1], ..., x[n-1]
; and it calculates
;   * max = the maximum element of x

; Since n>0, the array x contains at least one element, and a maximum
; element is guaranteed to exist.
~~~~
}

### High level algorithm}

;---------------------------------------------------------------------
; High level algorithm

;   max := x[0];
;   for i := 1 to n-1 do
;       { if x[i] > max
;           then max := x[i];
;       }
~~~~
}


%---------------------------------------------------------------------

### Translate high level code to low level ``goto form''

It's easier to check that this low level is equivalent to both the
high level algorithm and the assembly language, rather than
translating all the way to assembly language in one giant step.

;---------------------------------------------------------------------
; Low level algorithm

;     max := x[0]
;     i := 1
; forloop:
;     if i >= n then goto done
;     if x[i] <= max then goto skip
;     max := x[i]
; skip:
;     i := i + 1
;     goto forloop
; done:
;     terminate
~~~~
}



### Specify how the registers are used

The program is written in the ``register variable style''.


~~~~

;---------------------------------------------------------------------
; Assembly language

; Register usage
;   R1 = constant 1
;   R2 = n
;   R3 = i
;   R4 = max
~~~~
}

### Block of statements to initialise registers

; Initialise
       lea    R1,1[R0]         ; R1 = constant 1
       load   R2,n[R0]         ; R2 = n
; max := x[0]
       load   R4,x[R0]         ; R4 = max = x[0]
; i := 1
       lea    R3,1[R0]         ; R3 = i = 1
     
### Beginning of loop

; Top of loop, determine whether to remain in loop
forloop
; if i >= n then goto done
       cmp    R3,R2            ; compare i, n
       jumpge done[R0]         ; if i>=n then goto done

### Body of loop: if-then

; if x[i] <= max then goto else
       load   R5,x[R3]         ; R5 = x[i]
       cmp    R5,R4            ; compare x[i], max
       jumple skip[R0]         ; if x[i] <= max then goto skip

; max := x[i]
       add   R4,R5,R0          ; max := x[i]

### End of loop

skip
; i := i + 1
       add    R3,R3,R1         ; i = i + 1
; goto forloop
       jump   forloop[R0]      ; go to top of forloop
~~~~

### Finish

; Exit from forloop
done   store R4,max[R0]        ; max = R4
; terminate
       trap  R0,R0,R0          ; terminate

### Data definitions

; Data area

n        data   6
max      data   0
x        data  18
         data   3
         data  21
         data  -2
         data  40
         data  25

## Programming tips

Standard idioms for common programming problems  

### A useful convention


 *  The instruction set is designed to be regular, and to follow
  consistent conventions
  
   *  This makes programming easier
   *  It also helps with the hardware design!
  
 *  For most instructions, the operands follow the pattern of an
  assignment statement: information goes right to left
  
   *  Assignment statement: \alert{reg1 := reg2 + reg3}
   *  Add instruction: \alert{add R1,R2,R3}
   *  The two operands on the right (R2, R3) are added, and placed in the
    destination on the left (R1)
   *  Load instruction: \alert{load R1,x[R0]} means \alert{R1 := x}
  
 *  \alert{An exception: store}
  
   *  \alert{store R1,x[R0]} means x := R1: the information goes from
    left to right
   *  Why?  Doing it this way makes the digital circuit (the
    processor) a little bit faster
  



### Programming tip: Copying one register to another


 *  Here's a useful tip --- a standard programming technique
 *  Sometimes you want to copy a value from one register to another
  
   *   R3 := R12
  
 *  There's a standard way to do it:
  
   *  \texttt{add R3,R12,R0 ; R3 := R12}
  
 *  The idea is that R12 + 0 = R12!
 *  Why do it this way?  \emph{It's actually more efficient than
    providing a separate instruction just to copy the register!}


### Using load and store


 *  A common error is to confuse load and store
 *  The main points to remember:
  
   *  We need to keep variables in memory (most of the time) because
    memory is big --- there aren't enough registers to hold all your
    variables
   *  The computer hardware can do arithmetic on data in registers,
    but it cannot do arithmetic on data in memory
   *  Therefore, to do arithmetic on variables, you must
    \begin{enumerate}
     *  Copy the variables from memory to registers (\alert{load})
     *  Do the arithmetic in the registers (\alert{add},
      \alert{sub}, $\ldots$)
     *  Copy the result from registers back to memory
      (\alert{store})
    \end{enumerate}
  




### Compilation patterns



 *  We have looked at several high level programming constructs
  
   *  if \emph{b} then \emph{S}
   *  if \emph{b} then \emph{S} else \emph{S}
   *  while \emph{b} do \emph{S}
   *  for \emph{var} := \emph{exp} to \emph{exp} do \emph{S}
  
 *  There is a standard way to translate each to low level form:
  assignment, goto \emph{L}, if \emph{b} then goto \emph{L}
 *  The low level statements correspond closely to instructions



### Follow the patterns!

You should use these patterns as you write your programs because


 *  This helps you understand \emph{precisely} what high level
  language constructs mean---this is one of the aims of the course.
 *  This is essentially how real compilers work, and this is another
  aim of the course.
 *  This saves time because
  
   *  It's quicker to catch errors at the highest level
    (e.g. translating if-then-else to goto) rather than the lowest
    level (instructions)
   *  It makes the program more readable, and therefore faster to
    check and to debug
  
 *  This leads to good comments that make the program more readable
 *  This approach scales up to large programs
 *  \alert{Experienced programmers recognise the patterns, so if you
    use them your code is easier to read and debug and maintain}


### How can you tell if you're using the pattern?


 *  Each pattern contains
  
   *  Changeable parts: boolean expressions, integer expressions,
    statements
   *  Fixed parts: goto, if-then-goto
   *  The labels have to be different every time, but the structure
    of the fixed parts never changes
  
 *  Example: translating a while loop
  
   *  There should be one comparison, one conditional jump at the
    start of the loop
   *  There should be one unconditional jump at the end of the loop
  


### Are you using the pattern?

\textbf{High level code:}

{
\qquad \bluetext{while} \redtext{bexp} \bluetext{do}\\
\qquad \qquad \redtext{S}
}

\vspace{1em}
\textbf{The pattern for translation to low level:}

{
\qquad \bluetext{label1}\\
\qquad \qquad \bluetext{if} \redtext{bexp} \bluetext{= False then goto label2}\\
\qquad \qquad \redtext{S}\\
\qquad \qquad \bluetext{goto label1}\\
\qquad \bluetext{label2}
}

\vspace{1em}

 *  The blue text is fixed (except you need to use unique labels)
 *  There should be one comparison, one conditional jump at the
  start of the loop
 *  There should be one unconditional jump at the end of the loop


### Can you gain efficiency by violating the pattern?


 *  No!  Example: avoid the cost of jumping to a test that jumps out
  of the loop by transforming the while loop to\\
  {\qquad \bluetext{label1}\\
    \qquad \qquad \bluetext{if} \redtext{bexp} \bluetext{= False then goto label2}\\
    \qquad \bluetext{label3}\\
    \qquad \qquad \redtext{S}\\
    \qquad \qquad \bluetext{if} \redtext{bexp} \bluetext{= True then goto label3}\\
    \qquad \bluetext{label2}}\\
 *  But consider:
  
   *  Even if the loop executes a million times, this version saves
    \emph{at most} one jump instruction
   *  And the code is longer, which likely makes it slower (because
    of cache---haven't reached that topic yet)
   *  And when you do this in a large program it becomes incomprehensible
   *  \alert{Aim for readability and correctness}
  


### Comments


 *  Initial comments to identify the program, author, date
 *  Early comments to say what the program does
 *  High level algorithm (in comments)
 *  Translation to low level algorithm (in comments)
 *  Translation to assembly language (with comments)
  
   *  Copy the low level algorithm comments, and paste, so you have
    two copies
   *  The first copy remains as the low level algorithm
   *  In the second copy, insert the assembly language code
   *  Every low level statement should appear as a comment in the
    assembly code
  



### Write the comments first!


 *  The program development methodology entails writing the comments
  \emph{first}
 *  Avoid the temptation of writing code first, hacking it until it
  appears to work, and then adding comments
 *  The comments, the high and low level algorithms, \emph{help you
    to get it correct!}


### Why is goto controversial?


 *  If you develop code randomly, with goto jumping all over the
  place, the program is hard to understand, unlikely to work, and
  difficult to debug
 *  This has given the goto statement a bad reputation
 *  But goto is \emph{essential} for a compiler because it's
  essentially the jump instruction
 *  The compilation patterns provide a \emph{safe} and
  \emph{systematic} way to introduce goto into a program
 *  But if you ignore the patterns, you lose these advantages
 *  \important{Unstructured goto leads to complicated code}



  \includegraphics[scale=0.35,clip=true]
    {../figures/jpg/am_i_a_horse.jpg}


### Goto considered harmful: \emph{CACM} \textbf{11}(3), March 1968}


  \includegraphics[scale=0.5,clip=true]
    {../figures/png/goto_considered_harmful.png}



### What happened next?


 *  Considered harmful
  
   *  Dozens (hundreds?) of \emph{X considered harmful} essays
  
 *  Goto elimination
  
   *  Theorem: \emph{every} program using goto can be expressed
    without goto, using while and if-then-else
  
 *  Structured programming
  
   *  A positive, effective way to develop programs (instead of
    focusing on eliminating goto)
  


## Records

### Records}

A \alert{record} contains several \alert{fields}.  Access a field with
the dot (.) operator

~~~~
;   x, y :
;     record
;       { fieldA : int;
;         fieldB : int;
;         fieldC : int;
;       }
;
;   x.fieldA := x.fieldB + x.fieldC;
;   y.fieldA := y.fieldB + y.fieldC;
~~~~

(Some programming languages call it a \alert{tuple} or
\alert{struct}.)

### Defining some records

~~~~
; Data definitions

; The record x
x
x_fieldA   data   3    ; offset 0 from x  &x_fieldA = &x
x_fieldB   data   4    ; offset 1 from x  &x_fieldB = &x + 1
x_fieldC   data   5    ; offset 2 from x  &x_fieldC = &x + 2

; The record y
y
y_fieldA   data  20    ; offset 0 from y  &y_fieldA = &y
y_fieldB   data  21    ; offset 1 from y  &y_fieldB = &y + 1
y_fieldC   data  22    ; offset 2 from y  &y_fieldC = &y + 2
~~~~
  
### Naming each field explicitly

~~~~
; ------------------------------------------------------------
; Simplistic approach, with every field of every record named
; explicitly

; In record x,  fieldA := fieldB + fieldC

; x.fieldA := x.fieldB + x.fieldC
    load   R1,x_fieldB[R0]
    load   R2,x_fieldC[R0]
    add    R1,R1,R2
    store  R1,x_fieldA[R0]
~~~~

This is awkward---but there's a better way!

### Pointers

So far, we have been finding a piece of data by giving it a label

~~~~
     load    R2,xyz[R0]
...
xyz  data  5
~~~~

An alternative way to find the data is to make a \alert{pointer} to it

\vspace{1em}
\alert{A pointer is an address}

\vspace{1em}
\texttt{\&x} means the address of x: a pointer to x.  You can apply
the \& operator to a variable but not to a complex expression

 *  \texttt{\&x} is ok
 *  \texttt{\&(3*x)} is not ok


\vspace{1em}
\texttt{*p} means the value that p points to.  You can apply the *
operator to any pointer.

### Expressions using pointers


 *  The \& operator gives the address of its operand
  
   *  \alert{p := x} puts the \alert{\emph{value}} of x into p
   *  \alert{p := \& x} puts the \alert{\emph{address}} of x into p.
    The address of x is called a \emph{pointer to x}, and we say ``p
    points at x''.

 *  The * operator follows a pointer and gives whatever it points to
  
   *  \alert{*p} is an expression whose value is whatever p points
    at
   *  \alert{y := p} stores p into y, so y is also now a pointer to x
   *  \alert{y := *p} \emph{follows the pointer} p, gets the value
    (which is x) and stores that in y




### The \& operator requires only one instruction: lea!

~~~~
     lea   R5,x[R0]    ; R5 := &x
...
     lea   R6,y[R0]    ; R6 := &y
     store R6,p[R0]    ; p := &y
...
x    data  25
y    data  0
p    data  0
~~~~

### The * operator requires only one instruction: load!

~~~~
     load   R7,p[R0]    ; R7 := p
     load   R8,0[R7]    ; R8 := *p
~~~~

### Flexibility of load and lea

We have now seen two ways to use lea:

 *  To load a constant into a register: lea R1,42[R0] ; R1 := 42
 *  To create a pointer: lea R2,x[R0] ; R2 := \&x
 *  lea can do more, too --- can you figure out what?


And there are several ways to use load:

 *  To load a variable into a register: load R3,x[R0] ; R3 := x
 *  To access an array element: load R4,a[R5] ; R4 := a[R5]
 *  To follow a pointer: load R6,0[R7] ; R6 := *R7



### Following a pointer to the address of x gives x}

The value of \texttt{*(\&x)} is just \texttt{x}!

\vspace{1em}

~~~~
   lea    R4,x[R0]    ; R4 := &x
   load   R5,0[R4]    ; R5 := *(&x) = x

   load   R6,x[R0]    ; R6 := x
~~~~


### Review: accessing a variable the ordinary way

\alert{Low level language (same as high level)}
~~~~
   x := x + 5;
~~~~

\vspace{1em}
\alert{Assembly language}
~~~~
; Accessing variable x by its address, with R0
     lea    R1,5[R0]     ; R1 := 5 (constant)
     load   R2,x[R0]     ; R2 := x
     add    R2,R2,R1     ; R2 := x + 5
     store  R2,x[R0]     ; x := x + 5
~~~~

### Accessing a variable through a pointer

\alert{Low level language (same as high level)}
~~~~
   x := x + 5;
~~~~

\vspace{1em}
\alert{Assembly language}
~~~~
; Put a pointer to x into R3, which contains the address of x
; R3 := &x
     lea    R3,x[R0]     ; R3 := &x

; Add 5 to whatever word R3 points to
     lea    R1,5[R0]     ; R1 := 5 (constant)
     load   R4,0[R3]     ; R4 := *R3
     add    R4,R4,R1     ; R4 := *R3 + 5
     store  R4,0[R3]     ; *R3 := *R3 + 5
~~~~

Equivalent to \texttt{*(\&x) := *(\&x) + 5}


### Why is the pointer helpful?


 *  We can write a block of code that accesses variables through
  pointers.  
 *  This can be \emph{reused}, by executing it with the pointer set
  to point to different data.
 *  Later, we'll see additional benefits of using pointers.



### Access a record using a pointer

~~~~
; Make the same code work for any record with the same fields

; Set x as the current record by making R3 point to it
    lea    R3,x[R0]    ; R3 := &x

; Perform the calculation on the record that R3 points to
    load   R1,1[R3]    ; R1 := (*R3).fieldB
    load   R2,2[R3]    ; R2 := (*R3).fieldC
    add    R1,R1,R2    ; R1 := (*R3).fieldB + (*R3).fieldC
    store  R1,0[R3]    ; *R3.fieldA := (*R3).fieldB + (*R3).fieldC
~~~~


## Requests to the Operating System


 *  Many operations cannot be performed directly by a user program
  because
  
   *  \alert{(Main reason): user could violate system security}
   *  Also, some operations are difficult to program
   *  The code would need to change when OS is updated
  
 *  The program requests the operating system to perform them
 *  An OS request is performed by executing a \texttt{trap}
  instruction, such as \texttt{trap R1,R2,R3}
 *  \alert{A trap is a jump to the Operating System} (and you don't
  have to give the address to jump to)
 *  We use pointers to tell the operating system what to do


### Typical OS requests


 *  The type of request is a number, placed in R1, and operands (if
  any) are in R2, R3
 *  The specific codes used to make a request are defined by the
  operating system, not by the hardware
 *  {\color{blue}This is a major reason why compiled programs run
  only on one operating system}
 *  Typical requests:
  
     *  Terminate execution of the program
     *  Read from a file
     *  Write to a file
     *  Allocate a block of memory
  


### Termination


 *  A program cannot stop the machine; it requests the operating
  system to terminate it
 *  The operating system then removes the program from its tables
  of running programs, and reclaims any resources dedicated to the
  program
 *  In Sigma16, you request termination by \texttt{trap R0,R0,R0}


### Character strings: pointer to array of characters


 *  A string like \texttt{The cat in the hat} is represented as an
  array of characters
 *  Each element of the array contains one character
 *  If you are writing a string to output, the last character of the
  string should be a ``newline character''



### Write operation on Sigma16}

To write a string of characters


 *  trap R1,R2,R3
 *  R1 --- 2 is the code that indicates a write request
 *  R2 --- address of first word of string to write
 *  R3 --- length of string (the last word should be newline
  character)
 *  See example program Write.asm.txt



### Writing a string}

To write a string named \stress{out}, we use (1) lea to load a
constant, (2) lea to load the address of an array, (3) load to get a
variable


~~~~
; write out (size = k)
     lea    R1,2[R0]         ; trap code: write
     lea    R2,animal[R0]    ; address of string to print
     load   R3,k[R0]         ; string size = k
     trap   R1,R2,R3         ; write out (size = k)

     trap   R0,R0,R0         ; terminate

k    data   4    ; length of animal
; animal = string "cat"
animal
     data   99   ; character code for 'c'
     data   97   ; character code for 'a'
     data  116   ; character code for 't'
     data   10   ; character code for newline
~~~~
}

## Procedures


 *  Often there is a sequence of instructions that comes up again
  and again
  
   *  For example: sqrt (square root)
   *  It takes a lot of instructions to calculate a square root
   *  An application program may need a square root in many
    different places
  
 *  We don't want to keep repeating the code
  
   *  It's tedious
   *  It wastes space (all those instructions require memory!)
  
 *  The aim: \alert{write it once} and \alert{reuse the same
    instructions many times}



 *  Write the code \alert{one time} --- the block of code is called
  a procedure (or subroutine, function)
 *  Put the instructions off by themselves somewhere, not in the
  main flow of instructions
 *  Give the block of code a label (e.g. work) that describes what
  it does
 *  Every time you need to perform this computation,
  \alert{\textbf{call} it}: go to work
 *  When it finishes, \alert{the procedure needs to \textbf{return}:
    go back to the instruction \emph{\textbf{after}} the one that
    jumped to it}


### Call and return


 *  One idea is just to use jump instructions for both call and
  return
 *  But that isn't actually sufficient --- let's look in more detail
  at what happens


### Returning to the instruction after the call


 *  Suppose a procedure named {\color{blue}dowork} is used in
  several places
 *  Each call jumps to the same place (the address of the first
  instruction of the procedure
 *  But the calls \alert{come from different places}
 *  Therefore the procedure must finish by \alert{returning to
    different places}


### Calling and returning

Here is a main program that calls a procedure ``dowork'' several
times.  (It takes the value in R1 and doubles it, and the main program
would use the result but we ignore that here.)

\vspace{1em}

\begin{tabular}{llll}\large
       & load  & R1,x[R0]        & ; R1 = x \\
       &\tikz[baseline, inner sep=0]{\node[anchor=base](call1){``goto''};}
          & dowork[R0]  & ; \alert{call} the procedure ``dowork'' \\
       & \tikz[baseline, inner sep=0]{\node[anchor=base](ret1){load};}
          & R1,y[R0]        & ; R1 = y \\
       &\tikz[baseline, inner sep=0]{\node[anchor=base](call2){``goto''};}
          & dowork[R0]  & ; \alert{call dowork} \\
\vspace{2em}
       & \tikz[baseline, inner sep=0]{\node[anchor=base](ret2){sub};}
          & R5,R6,R7        & ; R5 = R6-R7 \\

dowork &\tikz[baseline, inner sep=0]{\node[anchor=base](entry){add};}
       & R1,R1,R1        & ; R1 = R1+R1 \\
     & \tikz[baseline, inner sep=0]{\node[anchor=base](return){``return''};}
       &           & ; {\color{blue}return} --- go back to the caller \\
\end{tabular}

\vspace{2ex}
\alert{The return must go to different points in the program---how can
  it do that?}

\tikz[overlay]\draw<2->[thick,red,->]
   (call1) to [out=180, in=175] (entry.west);
\tikz[overlay]\draw<3->[thick,blue,->]
   (return.east) to [out=10, in=0] (ret1.east);
\tikz[overlay]\draw<4->[thick,red,->]
   (call2) to [out=2000, in=150] (entry.north west);
\tikz[overlay]\draw<5->[thick,blue,->]
   (return.north east) to [out=45, in=0] (ret2.east);


### The jump-and-link instruction: jal


 *  When the main program calls the subroutine, it needs to
  \alert{remember where the call came from}
 *  This is the purpose of the \alert{jal} instruction --- jump and
  link
 *  \alert{jal R5,dowork[R0]}
  
   *  A \alert{pointer to the next instruction after the jal} ---
    the return address --- is loaded into the destination register
    (e.g. R5)
   *  Then the machine jumps to the effective address
  


### Jumping

All jump instructions (jump, jal, jumplt, etc.) refer to
\alert{effective addresses}


 *  jump \alert{loop[R0]} \\
  {\color{blue} goto loop}
 *  jump \alert{0[R14]} \\
  {\color{blue} goto instruction whose address is in R14}
 *  jump \alert{const[R2]} \\
  {\color{blue} goto instruction whose address is const+R2}


### Implementing call and return


 *  To call a procedure \texttt{dowork}: \alert{jal R13,dowork[R0]}
  
   *  The address of the instruction \emph{after} the jal is placed
    in R13
   *  The program jumps to the effective address, and the procedure
    starts executing
  
 *  To return when the procedure has finished: \alert{jump 0[R13]}
  
   *  The effective address is 0 + the address of the instruction
    after the jal
   *  The program jumps there and the main program resumes
  


### Calling with jal and returning with jump

\begin{tabular}{llll}\large
       & load  & R1,x[R0]        & ; R1 = x \\
       &\tikz[baseline, inner sep=0]{\node[anchor=base](call1){jal};}
          & R13,dowork[R0]  & ; \alert{call dowork} \\
       &  \tikz[baseline, inner sep=0]{\node[anchor=base](ret1){load};}
           & R1,y[R0]        & ; R1 = y \\
       &\tikz[baseline, inner sep=0]{\node[anchor=base](call2){jal};}
       & R13,dowork[R0]  & ; \alert{call dowork} \\
\vspace{2em}
       & \tikz[baseline, inner sep=0]{\node[anchor=base](ret2){sub};}
          &R5,R6,R7 & ; R5 = R6-R7 \\

dowork &\tikz[baseline, inner sep=0]{\node[anchor=base](entry){add};}
       & R1,R1,R1        & ; R1 = R1+R1 \\
     & \tikz[baseline, inner sep=0]{\node[anchor=base](return){jump};}
       & 0[R13]          & ; {\color{blue}return} \\
\end{tabular}

\vspace{2ex}

 *  \alert{call} --- jal puts a pointer to the next instruction into
  R13
 *  {\color{blue}return} --- follow the pointer in R13


\tikz[overlay]\draw<2->[thick,red,->]
   (call1) to [out=180, in=175] (entry.west);
\tikz[overlay]\draw<3->[thick,blue,->]
   (return.east) to [out=10, in=0] (ret1.east);
\tikz[overlay]\draw<4->[thick,red,->]
   (call2) to [out=2000, in=150] (entry.north west);
\tikz[overlay]\draw<5->[thick,blue,->]
   (return.north east) to [out=45, in=0] (ret2.east);

### Parameter passage


 *  There are several different conventions for passing argument to
  the function, and passing the result back
 *  What is important is that the caller and the procedure agree on
  how information is passed between them
 *  If there is a small number of arguments, the caller may put them
  in registers before calling the procedure
 *  If there are many arguments, the caller builds an array or
  vector (sequence of adjacent memory locations), puts the arguments
  into the vector, and passes the address of the vector in a register
  (typically R1)
 *  A simple convention: \alert{the argument and result are passed
    in R1}


### Functions


 *  A \alert{function} is a procedure that
  
   *  Receives a parameter (a word of data) from the caller
   *  Calculates a result
   *  Passes the result back to the caller when it returns
  
 *  A \alert{pure function} is a function that doesn't do anything
  else --- it doesn't change any global variables, or do any
  input/output



### Example: Passing argument and result in R1

~~~~
; Main program
       load  R1,x[R0]       ; arg = x
       jal   R13,work[R0]   ; result = work (x)
       ...
       load  R1,y[R0]       ; arg = y
       jal   R13,work[R0]   ; result = work (y)
       ...

; Function work (x) = 1 + 7*x
work   lea   R2,7[R0]       ; R7 = 2
       lea   R3,1[R0]       ; R3 = 1
       mul   R1,R1,R2       ; result = arg * 7
       add   R1,R3,R1       ; result = 1 + 7*arg
       jump  0[R13]         ; return
~~~~
  
### Procedure calls another procedure

### What if a procedure calls another procedure?

  
   *  The simplest kind of procedure
    
     *  Call it with \texttt{jal R13,procname[R0]}
     *  It returns by executing \texttt{jump 0[R13]}
    
  

### Limitations of basic call


   *  If the procedure modifies any registers, it may destroy
    data belonging to the caller
   *  If the procedure calls another procedure, it can't use R13
    again.  Each procedure would need a dedicated register for its
    return address, limiting the program to a small number of
    procedures
   *  The basic call mechanism doesn't allow a procedure to call
    itself (this is called \alert{recursion})
  

### R13 overwritten: proc1 returns to the wrong place!

\begin{tabular}{llll}\large
  & load  & R1,x[R0]        & ; R1 := x \\
  &\tikz[baseline, inner sep=0]{\node[anchor=base](call1){jal};}
          & R13,proc1[R0]  & ; \alert{call proc1} \\
\vspace{0.25em} % space for R14 box
  &  \tikz[baseline, inner sep=0]{\node[anchor=base](ret1){load};}
           & R1,y[R0]        & ; R1 := y \\
  &&&\hspace{4cm}
      \tikz[baseline, inner sep=0]
        {\node[anchor=base](RETADR){\fbox{R13}};}\\
 % & \tikz[baseline, inner sep=0]{\node[anchor=base](ret2){store};}
   & store
          &R1,result[R0] & ; result := $2 \times (2x)^2$ \\

proc1 &\tikz[baseline, inner sep=0]{\node[anchor=base](proc1entry){lea};}
       & R2,1[R0]        & ; R2 := 1 \\
proc1 &\tikz[baseline, inner sep=0]{\node[anchor=base](proc1return){add};}
       & R1,R1,R1        & ; R1 := R1+1 \\
  &\tikz[baseline, inner sep=0]{\node[anchor=base](call2){jal};}
       & R13,proc2[R0]  & ; \alert{R1 := square (R1) } \\
  &\tikz[baseline, inner sep=0]{\node[anchor=base](ret2){add};}
       & R1,R1,R1        & ; R1 := R1+1 \\
  \vspace{2em}
  & \tikz[baseline, inner sep=0]{\node[anchor=base](return1){jump};}
       & 0[R13]          & ; {\color{blue}return} \\

square &\tikz[baseline, inner sep=0]{\node[anchor=base](entry){mul};}
       & R1,R1,R1        & ; R1 = R1*R1 \\
  & \tikz[baseline, inner sep=0]{\node[anchor=base](return2){jump};}
       & 0[R13]          & ; {\color{blue}return} \\

\tikz[overlay]\draw<2->[thick,red,->]
   (call1) to [out=180, in=175] (proc1entry.west);
\tikz[overlay]\draw<2>[thick,green,->]
   (RETADR.west) to [out=180, in=0] (ret1.east);
\tikz[overlay]\draw<3->[thick,red,->]
   (call2) to [out=2000, in=150] (entry.north west);
\tikz[overlay]\draw<3->[thick,green,->]
   (RETADR.west) to [out=180, in=0] (ret2.east);
\tikz[overlay]\draw<4->[thick,blue,->]
   (return2.north east) to [out=45, in=0] (ret2.east);
\tikz[overlay]\draw<5->[thick,blue,->]
   (return1.east) to [out=45, in=0] (ret2.east);
\tikz[overlay]\draw<5->[thick,orange,->]
   (return1.east) to [out=45, in=0] (ret1.east);

\end{tabular}
 

### Saving state


 *  Calling a procedure creates new information
  
   *  The return address
   *  Whatever values the procedure loads into the registers
  
 *  But this new information could overwrite essential information
  belonging to the caller
 *  We need to \alert{save the caller's state} so the procedure
  won't destroy it


### The wrong way to save state


 *  Suppose we just have a variable saveRetAdr
 *  Store R13 into it in the procedure, load that when we return
 *  Now it's ok for proc1 to call proc2
 *  But if proc2 calls proc3 we are back to the same problem: it
  doesn't work!
 *  The solution: a \alert{stack}


### Saving registers


 *  Most procedures need to use several registers
 *  It's nearly impossible to do \emph{anything} without using some
  registers!
 *  The first thing a procedure should do is to \emph{save the
    registers} it will use by copying them into memory (with store
  instructions).
 *  The last thing it should do before returning is to \emph{restore
  the registers} by copying their values back from memory (with load
  instructions).


### Where can the registers be saved?


 *  It won't work to copy data from some of the registers to other
  registers!
 *  It's essential to save the data into memory
 *  Two approaches
  
   *  Allocate fixed variables in memory to save the registers into
    --- simple but doesn't allow recursion
   *  Maintain a \alert{stack} in memory, and \alert{push} the data
    onto the stack --- this is the best approach and is used by most
    programming languages
  


### Who saves the state: the caller or the procedure?


   *  Two approaches:
   *  Caller saves (used occasionally)
    
     *  Before calling a procedure, the caller saves the
      registers, so all its essential data is in memory
     *  After the procedure returns, the caller does whatever
      loads are needed
    
   *  Callee saves (usually the preferred solution)
    
     *  The caller keeps data in registers, and assumes that the
      procedure won't disturb it
     *  The first thing the procedure does is to save the
      registers it needs to use into memory
     *  Just before returning, the procedure restores the
      registers by loading the data from memory
    
  

### Stack of return addresses


 *  To allow a large number of procedures, we can't dedicate a
  specific register to each one for its return address
 *  Therefore we
  
   *  Always use the same register for the return address in a jal
    instruction (we will use R13)
   *  The first thing a procedure does is to store its return
    address into memory
   *  The last thing the procedure does is to load its return
    address and jump to it
   *  The return addresses are pushed onto a \emph{stack}, rather
    than being stored at a fixed address
  


## Stacks


 *  A \alert{stack} is a container
 *  Initially it is empty
 *  You can \alert{push} a value onto the stack; this is now sitting
  on the top of the stack
 *  You can \alert{pop} the stack; this removes the \alert{most
    recently pushed} value and returns it
 *  A stack allows access only to the top value; you cannot access
  anything below the top
 *  We can save procedure return addresses on a stack because return
    always needs the most recently saved return address


### Initially the stack is empty}


\begin{tabular}{|c|}
   \\
   \\
   \\
\hline
\end{tabular}



### Call procedure, push return address $a$


\begin{tabular}{|c|}
   \\
   \\
  $a$\\
\hline
\end{tabular}



### Call another procedure, push return address $b$


\begin{tabular}{|c|}
   \\
  $b$\\
  $a$\\
\hline
\end{tabular}



### Return: pop produces return address $b$


\begin{tabular}{|c|}
   \\
   \\
  $a$\\
\hline
\end{tabular}


### Call some procedure, push return address $c$


\begin{tabular}{|c|}
   \\
  $c$\\
  $a$\\
\hline
\end{tabular}



### Call a procedure, push return address $d$


\begin{tabular}{|c|}
  $d$\\
  $c$\\
  $a$\\
\hline
\end{tabular}


### The call stack


 *  Central technique for
  
   *  Preserving data during a procedure call 
   *  Holding most of your variables
  
 *  It goes by several names; these are all the same thing
  
   *  call stack
   *  execution stack
   *  ``The stack''
  
 *  It's important!
  
   *  Most programming languages use it
   *  Computers are designed to support it
   *  Often referred to (Stack Overflow web site, etc.)
  



### Stack frames


 *  There is a \alert{call stack} or \alert{execution stack} that
  maintains complete information about all procedure calls and returns
 *  Every ``activation'' of a procedure pushes a \alert{stack frame}
 *  When the procedure returns, its stack frame is popped (removed)
  from the stack
 *  R14 contains the address of the current (top) stack frame
 *  The stack frame contains:
  
   *  A pointer to the previous stack frame (this is required to
    make the pop work)
   *  The return address (saved value of R13)
   *  The saved registers (so the procedure can use the registers
    without destroying information)
   *  Local variables (so the procedure can have some memory of its
    own to use)
  


### Implementing the call stack


 *  Dedicate R14 to the \alert{stack pointer}
 *  This is a programming convention, not a hardware feature
 *  When the program is started, R14 will be set to point to an
  empty stack
 *  When a procedure is called, the saved state will be pushed onto
  the stack: store a word at 0[R14] and add 1
 *  When a procedure returns, it pops the stack and restores the
  state: subtract 1, load from 0[R14]
 *  The program should never modify R14 apart from the push and pop



## Retrospective


### What is a computer program?


 *  A beginner's view
  
   *  The computer runs programs
   *  A program is lines of code of (Python, C, whatever)
  
 *  The strange program shows how wrong that is!
 *  A more sophisticated view
  
   *  The lines of source code are input to the assembler (compiler)
    which generates the \emph{initial} value of the machine language
   *  When the program is booted, the initial machine language is
    stored in memory
   *  The computer executes the machine language instructions in
    memory; the original assembly language code (labels and all) no
    longer exists
  
 *  Essential concepts:
  
   *  Source code and object code
   *  Compile time and run time
  




### What is a variable?


 *  Beginner's view
  
   *  A variable is a box with a name that holds a value
   *  An expression can use the value in the box; an assignment can
    modify the value in the box
   *  In assembly language, you define a variable with a data statement
  
 *  A more sophisticated view
  
   *  Variables are distinct from variable names: many variables may
    have the same name
   *  A variable has a scope in a program: a region where it
    corresponds to a particular box
   *  Variables do not correspond to data statements: they are
    created and destroyed dynamically as a program runs
   *  Initialising a variable is not the same as assigning a value
    to it
  


### Review of procedures: Call with jal, return with jump


 *  To call a procedure \texttt{dowork}: \alert{jal R13,dowork[R0]}
  
   *  The address of the instruction \emph{after} the jal is placed
    in R13
   *  The program jumps to the effective address, and the procedure
    starts executing
  
 *  To return when the procedure has finished: \alert{jump 0[R13]}
  
   *  The effective address is 0 + the address of the instruction
    after the jal
   *  The program jumps there and the main program resumes
  


### Review: Basic calls with jal

  
   *  The simplest kind of procedure
    
     *  Call it with \texttt{jal R13,procname[R0]}
     *  It returns by executing \texttt{jump 0[R13]}
    
  

### Review: Activation records, a.k.a. stack frames


 *  There is a \alert{call stack} or \alert{execution stack} that
  maintains complete information about all procedure calls and returns
 *  Every ``activation'' of a procedure pushes a \alert{stack frame}
 *  When the procedure returns, its stack frame is popped (removed)
  from the stack
 *  R14 contains the address of the current (top) stack frame
 *  The stack frame contains:
  
   *  A pointer to the previous stack frame (this is required to
    make the pop work)
   *  The return address (saved value of R13)
   *  The saved registers (so the procedure can use the registers
    without destroying information)
   *  Local variables (so the procedure can have some memory of its
    own to use)
  


### Review: Sequence of stack operations


\hbox{
\begin{tabular}{|c|}
  \\
  \hline
  \\
  \hline
  \\
  \hline
  \phantom{a}\\
  \hline
\end{tabular}
\hspace{3ex}
\begin{tabular}{|c|}
  \\
  \hline
  \\
  \hline
  \\
  \hline
  a\\
  \hline
\end{tabular}
\hspace{3ex}
\begin{tabular}{|c|}
  \\
  \hline
  \\
  \hline
  b\\
  \hline
  a\\
  \hline
\end{tabular}
\hspace{3ex}
\begin{tabular}{|c|}
  \\
  \hline
  c\\
  \hline
  b\\
  \hline
  a\\
  \hline
\end{tabular}
\hspace{3ex}
\begin{tabular}{|c|}
  \\
  \hline
  \\
  \hline
  b\\
  \hline
  a\\
  \hline
\end{tabular}
\hspace{3ex}
\begin{tabular}{|c|}
  \\
  \hline
  d\\
  \hline
  b\\
  \hline
  a\\
  \hline
\end{tabular}
\hspace{3ex}
\begin{tabular}{|c|}
  e\\
  \hline
  d\\
  \hline
  b\\
  \hline
  a\\
  \hline
\end{tabular}
\hspace{3ex}
\begin{tabular}{|c|}
  \\
  \hline
  d\\
  \hline
  b\\
  \hline
  a\\
  \hline
\end{tabular}
\hspace{3ex}
\begin{tabular}{|c|}
  \\
  \hline
  \\
  \hline
  b\\
  \hline
  a\\
  \hline
\end{tabular}
}


empty; push a; push b, push c, pop returns c, push d, push e, pop
returns e, pop returns d, $\ldots$

### Variables

### Access to variables


 *  Depending on the programming language, there are several
  different ways that variables can be allocated
 *  For each of these, there is a corresponding way to access the
  variable in memory
 *  Three key issues:
  
   *  The \emph{lifetime} of a variable: when it is created, when
    it is destroyed
   *  The \emph{scope} of a variable: which parts of the source
    program are able to access the variable
   *  The \emph{location} of a variable: what its address in memory
    is
  
 *  The compiler generates the correct object code to access each
  variable


### Three classes of variable


 *  \emph{Static variables.} (Sometimes called \emph{global
    variables}) --- visible through the entire program
 *  \emph{Local variables.} (Sometimes called \emph{automatic
    variables}) --- visible only in a local procedure
 *  \emph{Dynamic variables.} (Sometimes called \emph{heap
    variables}) --- used in object oriented and functional languages


### Static variables


 *  The lifetime of a static variable is the entire execution of a
  program
  
   *  When the program is launched, its static variables are created
   *  They continue to exist, and to retain their values, until the
    program is terminated
  
 *  In C, you can declare a variable to be static.  In Pascal, all
  global variables (i.e. all variables that aren't defined locally)
  are static
 *  \emph{So far, we have been using static variables}




### Combining static variables with code

The simple way we have been defining variables makes them static

~~~~
     load  R1,x[R0]    ; R1 := x
   ...
     trap  R0,R0,R0    ; terminate

; Static variables

x    data    0
n    data  100
~~~~

These variables exist for the entire program execution.  There is one
variable x, and one variable n.
  
### Disadvantages of combining variables and code


 *  The executable code cannot be shared.
  
   *  Suppose two users want to run the program.
   *  Each needs to have a copy of the entire object, which
    contains both the instructions and the data
   *  That means the instructions are duplicated in memory
   *  This is inefficient use of memory
  
 *  To avoid the duplication of instructions, we need to separate
  the data from the code
 *  Modern operating systems organise information into
  \alert{segments}
  
   *  A \alert{code segment} is read-only, and can be shared
   *  A \alert{data segment} is read/write, and cannot be shared
  




### Local variables


 *  Local variables are defined in a function, procedure, method,
  or in a begin...end block, or a \{...\} block
 *  A local variable has one name, but there may be many
  instances of it if the function is recursive
 *  Therefore local variables cannot be stored in the static data
  segment
 *  They are kept in stack frames
 *  The compiler (or assembler) works out the address of each
  local variable \emph{relative to the address of the stack frame}
 *  The variables are accessed using the stack frame register

### Accessing local variables


~~~~
      load  R1,x[R14]   ; access local variable x; R14 points to frame
~~~~
}


 *  The compiler (or the programmer) works out the exact format
  of the stack frame
 *  Each local variable has a dedicated spot in the stack frame,
  and its address (relative to the frame) is used in the load
  instruction


### Dynamic variables


 *  A \emph{dynamic variable} is created explicitly (e.g. using
  \emph{new} in Java)
 *  It is not limited to use in just one function
 *  The lifetime of a dynamic variable does not need to follow the
  order that stack frames are pushed or popped
 *  So dynamic variables can't be kept in the static data segment,
  and they can't be kept on the stack


### The Heap


 *  Languages that support dynamic variables (Lisp, Scheme, Haskell,
  Java) have a region of memory called the \emph{heap}.
 *  The heap typically contains a very large number of very small
  objects
 *  The heap contains a \emph{free space list}, a data structure
  that points to all the free words of memory.
 *  The heap is maintained by the language ``runtime system'', not
  by the operating system.
 *  When you do a \emph{new}, a (small) amount of memory is
  allocated from the heap and a pointer (address) to the object is
  returned
 *  When the object is no longer required, the memory used to hold
  it is linked back into the free space list.


### The call stack


 *  Each procedure call pushes information on the stack
 *  The information needed by the procedure is in the stack frame
  (also called activation record)
 *  Each procedure return pops information off the stack
 *  A register is permanently used as the \alert{stack pointer}
  
   *  For each computer architecture, there is a standard register
    chosen to be the stack pointer
   *  In Sigma16, R14 is the stack pointer
   *  When you call, you push a new stack frame and increase R14
   *  As a procedure runs, it access its data via R14
   *  When you return, you set R14 to the stack frame below
  


### Simplest stack: return addresses


\footnotesize
\begin{tabular}{|c|}
  return address \\
  return address \\
  return address \\
  return address \\
  \hline
\end{tabular}


Just save the return address on the stack

### Saved registers


\footnotesize
\begin{tabular}{|c|}
  save R4         \\
  save R3         \\
  save R2         \\
  save R1         \\
  return address \\
  \hline
  save R1         \\
  return address \\
  \hline
  save R3         \\
  save R2         \\
  save R1         \\
  return address \\
  \hline
\end{tabular}


Save the registers the procedure needs to use on the stack, and
restore them before returning.  This way the procedure won't crash the
caller

### Dynamic links


\footnotesize
\begin{tabular}{r|c|}
\hline  
4& save R3         \\
3& save R2         \\
2& save R1         \\
1& return address  \\
0& \hbox{\tikz[baseline, inner sep=0]{\node[anchor=base](dlink3){dynamic link};}} \\
\hline  
2& save R1         \\
1& return address  \\
0& \hbox{\tikz[baseline, inner sep=0]{\node[anchor=base](dlink2){dynamic link};}} \\
\hline  
3& save R2         \\
2& save R1         \\
1& return address  \\
0& \hbox{\tikz[baseline, inner sep=0]{\node[anchor=base](dlink1){dynamic link};}} \\
\hline  
  $\cdots$ \\
\end{tabular}


\tikz[overlay]\draw<1->[thick,red,->]
  (dlink3.east) to [out=0, in=0] (dlink2.north east);
\tikz[overlay]\draw<1->[thick,red,->]
  (dlink2.east) to [out=0, in=0] (dlink1.north east);

 
 *  Problem: since each activation record can have a different size,
  how do we pop the top one off the stack?
 *  Simplest solution: each activation record contains a pointer
  (called dynamic link) to the one below


### Local variables


\footnotesize
\begin{tabular}{r|c|}
  \hline
  6& y \\
  5& x \\
  4& save R3         \\
  3& save R2         \\
  2& save R1         \\
  1& return address  \\
  0& \hbox{\tikz[baseline, inner sep=0]{\node[anchor=base](dlink3)
     {dynamic link};}} \\
  \hline  
  3& pqrs            \\
  2& save R1         \\
  1& return address  \\
  0& \hbox{\tikz[baseline, inner sep=0]{\node[anchor=base](dlink2)
     {dynamic link};}} \\
  \hline
  5& b \\
  4& a \\
  3& save R2 \\
  2& save R1 \\
  1& return address \\
  0& \hbox{\tikz[baseline, inner sep=0]{\node[anchor=base](dlink1)
     {dynamic link};}} \\
  \hline  
  $\cdots$ \\
\end{tabular}


\tikz[overlay]\draw<1->[thick,red,->]
  (dlink3.east) to [out=0, in=0] (dlink2.north east);
\tikz[overlay]\draw<1->[thick,red,->]
  (dlink2.east) to [out=0, in=0] (dlink1.north east);

The procedure keeps its local variables on the stack
  
### Static links for scoped variables


\footnotesize
\begin{tabular}{r|c|}
  \hline
  7& y \\
  6& x \\
  5& save R3         \\
  4& save R2         \\
  3& save R1         \\
  2& static link  \\
  1& return address  \\
  0& \hbox{\tikz[baseline, inner sep=0]{\node[anchor=base](dlink3)
     {dynamic link};}} \\
  \hline  
  4& pqrs            \\
  3& save R1         \\
  2& static link  \\
  1& return address  \\
  0& \hbox{\tikz[baseline, inner sep=0]{\node[anchor=base](dlink2)
     {dynamic link};}} \\
  \hline
  6& b \\
  5& a \\
  4& save R2 \\
  3& save R1 \\
  2& static link  \\
  1& return address \\
  0& \hbox{\tikz[baseline, inner sep=0]{\node[anchor=base](dlink1)
     {dynamic link};}} \\
  \hline  
  $\cdots$ \\
\end{tabular}


\tikz[overlay]\draw<1->[thick,red,->]
  (dlink3.east) to [out=0, in=0] (dlink2.north east);
\tikz[overlay]\draw<1->[thick,red,->]
  (dlink2.east) to [out=0, in=0] (dlink1.north east);


### Accessing a word in the stack frame


 *  Work out a ``map'' showing the format of a stack frame
 *  Describe this in comments (it's similar to the register usage
  comments we've been using)
 *  Suppose local variable, say ``avacado'', is kept at position 7
  in the stack frame
 *  To access the variable:
  
   *  load  R1,7[R14]   ; R1 := avacado
   *  store  R1,7[R14]   ; R1 := avacado
   *  Also, we can define the symbol ``avacodo'' to be 7, and write:
   *  load  R1,avacado[R14]   ; R1 := avacado
   *  store  R1,avacado[R14]   ; R1 := avacado
  
 *  These are called \alert{local variables} because every call to a
  procedure has its own private copy


### Example from factorial program (see below)

These comments document the structure of a stack frame for the program:


~~~~
; Structure of stack frame for fact function
;    6[R14]   origin of next frame
;    5[R14]   save R4
;    4[R14]   save R3
;    3[R14]   save R2
;    2[R14]   save R1 (parameter n)
;    1[R14]   return address
;    0[R14]   pointer to previous stack frame
~~~~
}

### Recursive factorial


 *  In the Sigma16 examples, there is a program called
  \texttt{factorial}
 *  This program illustrates the full stack frame technique
 *  It uses recursion --- a function that calls itself
 *  Note: the best way actually to compute a factorial is with a
  simple loop, \emph{not} with recursion
 *  But recursion is an important technique, and it's better to
  study it with a simple example (like factorial) rather than a
  complicated ``real world'' example


### About the factorial program

  
   *  Comments are used to identify the program, describe the
    algorithm, and document the data structures.
   *  Blank lines and full-line comments organise the program
    into small sections.
   *  The caller just uses jal to call the function.
   *  The function is responsible for building the stack frame,
    saving and restoring registers.
   *  The technique of using the stack for functions is general,
    and can be used for large scale programs.
  


### Statement of problem, and register usage

;-----------------------------------------------------------------------
; factorial.asm.txt
;-----------------------------------------------------------------------

; This program for the Sigma16 architecture uses a recursive function
; to compute x! (factorial of x), where x is defined as a static
; variable.

; The algorithm uses a recursive definition of factorial:
;   if n <=1
;     then factorial n = 1
;     else factorial n = n * factorial (n-1)

; Register usage
;   R15 is reserved by architecture for special instructions
;   R14 is stack pointer
;   R13 is return address
;   R2, R3, R4 are temporaries used by factorial function
;   R1 is function parameter and result
;   R0 is reserved by architecture for constant 0

### Format of main program stack frame

;-----------------------------------------------------------------------
; Main program

; The main program computes result := factorial x and terminates.

; Structure of stack frame for main program
;    1[R14]   origin of next frame
;    0[R14]   pointer to previous stack frame = nil

### Main program initialisation

; Initialise stack
     lea    R14,stack[R0]      ; initialise stack pointer
     store  R0,0[R14]          ; previous frame pointer := nil

### Main program calls factorial

; Call the function to compute factorial x
     load   R1,x[R0]           ; function parameter := x
     store  R14,1[R14]         ; point to current frame
     lea    R14,1[R14]         ; push stack frame
     jal    R13,factorial[R0]  ; R1 := factorial x

### Main program finishes

; Save result and terminate
     store  R1,result[R0]      ; result := factorial x
     trap   R0,R0,R0           ; terminate

### Description of factorial function

;-----------------------------------------------------------------------
factorial
; Function that computes n!
;   Input parameter n is passed in R1
;   Result is returned in R1

### Format of stack frame for factorial

; Structure of stack frame for fact function
;    6[R14]   origin of next frame
;    5[R14]   save R4
;    4[R14]   save R3
;    3[R14]   save R2
;    2[R14]   save R1 (parameter n)
;    1[R14]   return address
;    0[R14]   pointer to previous stack frame

### Factorial: build stack frame

; Create stack frame  
     store  R13,1[R14]         ; save return address
     store  R1,2[R14]          ; save R1
     store  R2,3[R14]          ; save R2
     store  R3,4[R14]          ; save R3
     store  R4,5[R14]          ; save R4
~~~~
}

### Factorial: check for base or recursion case

; Initialise
     lea    R2,1[R0]           ; R2 := 1

; Determine whether we have base case or recursion case
     cmpgt  R10,R1,R2           ; R10 := n>1
     jumpt  R10,recursion[R0]   ; if n>1 then go to recursion case

### Factorial: base case

; Base case.  n<=1 so the result is 1
     lea    R1,1[R0]           ; factorial n = 1
     jump   return[R0]         ; go to end of function

### Factorial: recursion case

; Recursion case.  n>1 so factorial n = n * factorial (n-1)
recursion
     sub    R1,R1,R2           ; function paramemter := n-1

; Call function to compute factorial (n-1)
     store  R14,6[R14]         ; point to current frame
     lea    R14,6[R14]         ; push stack frame
     jal    R13,factorial[R0]  ; R1 := factorial (n-1)
     load   R2,2[R14]          ; R2 := saved R1 = n
     mul    R1,R2,R1           ; R1 := n * fact (n-1)

### Factorial: restore registers and return

; Restore registers and return; R1 contains result
return
     load   R2,3[R14]       ; restore R2
     load   R3,4[R14]       ; restore R3
     load   R4,5[R14]       ; restore R4
     load   R13,1[R14]      ; restore return address
     load   R14,0[R14]      ; pop stack frame
     jump   0[R13]          ; return

### Static data area

;-----------------------------------------------------------------------
; Static data segment

x       data   5
result  data   0
stack   data   0   ; stack extends from here on...

### Summary


 *  Variables defined with data statement are static
  
   *  Each static variable must have a unique name
   *  Static variables exist through entire execution of program
  
 *  Variables defined in a procedure are local
  
   *  Different procedures can use the same name for different
    variables
   *  Local variables are kept in the stack frame
   *  Call--push stack frame; return--pop stack frame
   *  R14 points to current stack frame; local variables are
    accessed using R14
  


## Linked lists

### Review of pointers


 *  p := \&x  \qquad p is a pointer to x
~~~~
    lea   R5,x[R0]   ; R5 := &x
~~~~
 *  y := *p   \qquad y is the value that p points to
~~~~
    load   R6,0[R5]   ; R6 := *R5
~~~~


### Nodes


 *  A linked list consists of a linear chain of \emph{nodes}
 *  A node is a \alert{record with two fields}
   
   *  \emph{\alert{value}} is a word containing useful information,
    the \emph{content} of the node.  May be an integer, character, or
    even a pointer to something else.
   *  \emph{\alert{next}} is a word containing a pointer to the next
    node in the list
  
 *  The last node in the list has a special value \emph{\alert{nil}}
  in the \emph{next} field
 *  \emph{nil} is represented by 0 (so you can't have a pointer to
  memory location 0, but normally that's where the program will be so
  you wouldn't want that anyway)


### Accessing the fields of a node


 *  Suppose p is a pointer to a node
 *  load  R1,p[R0]  ; R1 := p
 *  load  R2,0[R1]  ; R2 := (*p).value
 *  load  R3,1[R1]  ; R3 := (*p).next


### Representing a linked list

\begin{tabular}{r|c|c|c|}
  & a & mem[a] & mem[a+1] \\
\hline
  & 0 & &   \\
  & 1 & &   \\
  & \tikz[baseline, inner sep=0]{\node[anchor=base](addr2){2};}
      &37
      &\tikz[baseline, inner sep=0]{\node[anchor=base](ptr2){6};} \\
  & \tikz[baseline, inner sep=0]{\node[anchor=base](addr3){3};}
      &42
      & 0 \\
  & 4 & &   \\
p & \tikz[baseline, inner sep=0]{\node[anchor=base](addr5){5};}
      &24
      & \tikz[baseline, inner sep=0]{\node[anchor=base](ptr5){2};} \\
  &  \tikz[baseline, inner sep=0]{\node[anchor=base](addr6){6};}
      & 97
      & \tikz[baseline, inner sep=0]{\node[anchor=base](ptr6){3};} \\
  & 7 &   \\
  & 8 &  \\
\end{tabular}

p = 5, and the list p = [24, 37, 97, 42]

\tikz[overlay]\draw<2->[thick,red,->]
  (ptr5.east) to [out=0, in=0] (addr2.east);
\tikz[overlay]\draw<3->[thick,red,->]
  (ptr2.east) to [out=0, in=0] (addr6.east);
\tikz[overlay]\draw<4->[thick,red,->]
  (ptr6.east) to [out=0, in=0] (addr3.east);

### Basic operations on lists


 *  Three key operations:
  
   *  Is a list p empty?
   *  What's the value in a node?
   *  What's the next node
  
 *  The following code assumes that all the pointer variables (p, q)
  are in memory, so they must be loaded and stored
 *  In practice, we often keep the pointers in registers so you
  don't need all those loads and stores


### Is list p empty?


 *  Nil is 0, so the list that p points at is empty iff p=0
 *  Generally it is unsafe to perform an action on a list p unless p
  actually points to a node, so this test is commonly needed


~~~~
    load   R1,p[R0]
    cmpeq  R2,R1,R0
    jumpt  R2,pIsEmpty[R0]
; No, p is not empty
 ...
 ...
pIsEmpty
; Yes, p is empty
~~~~

### Get value in node that p points at: x := *p.value


 *  x := *p.value
 *  This is safe to do only if p is not empty
 *  The value field of a node is at offset 0 in the node record


~~~~
   load   R1,p[R0]    ; R1 := p
   load   R2,0[R1]    ; R2 := *p.value
   store  R2,x[R0]    ; x := *p.value
~~~~


### Get pointer to next node in a list: q := *p.next


 *  q := *p.next
 *  This is safe to do only if p is not empty
 *  The next field of a node is at offset 1 in the node record


~~~~
   load   R1,p[R0]    ; R1 := p
   load   R2,1[R1]    ; R2 := *p.next
   store  R2,q[R0]    ; q := *p.next
~~~~

### Traversing a list

A while loop is the best looping construct for traversing a list

ListSum (p)
  { sum := 0;
    while p /= nil do
      { x := (*p).value;
        sum := sum + x;
        p := (*p).next;
      }

### Search a list p for a value x

Again, the best looping construct is a while loop

~~~~
ListSearch (p, x)
  { found := False;
    while p /= nil && not found do
      { found := x = (*p).value;
        p := (*p).next;
      }
    return found;
  }
~~~~

This is a good example of the proper use of a while loop

 *  The condition checks for end of data, and also for early
  completion
 *  There is no break statement or goto
 *  The loop works even if the original list p is nil


### cons --- constructing a list be consing a value to the front


 *  Suppose p = [23, 81, 62]
 *  q := cons (56, p)
 *  After computing q, we have
  
   *  q = [56, 23, 81, 62] \qquad \alert{q is the same as p but with
    56 attached to the front}
   *  p = [23, 81, 62] \qquad \alert{p is unchanged}
  


### Implementing cons

~~~~
cons (x, p)
  { q := newnode ();
    (*q).value := x;
    (*q).next := p;
    return q;
  }
~~~~


 *  No change is made to p, or to the node p points to
 *  A new node is allocated and set to point to p
 *  A pointer to the new node is returned
 *  A function like cons --- which produces a new result but does
  not modify its arguments --- is called a \emph{\alert{pure
      function}}


### Getting a new node from avail list

~~~~
if avail = nil
  then { error "fatal error: out of heap" }
  else { newnode := avail;
         avail := (*avail).next;
         return newnode;
       }
~~~~

### Inserting a node with x where p points

~~~~
r := newnode ();
(*r).value := x;
(*r).next := (*p).next;
(*p).next := r;
~~~~


 *  Notice that we can insert x \emph{after} the node that p points
  to
 *  But we cannot insert x \emph{before} that node
 *  It's common, in list algorithms, to have two pointers moving
  along through the list, one lagging an element behind the other, to
  make insertion possible


### List header


 *  Suppose we have a list p and a value x
 *  We want to insert x into the list p at an arbitrary point
 *  Another pointer q points to the insertion position
 *  A slightly awkward problem: the code to insert x at the front of
  the list is slightly different from the code to insert x after some
  element (*q)
  
   *  If somewhere in the middle, we can insert x \emph{after} the
    node that q points to
   *  The insertion algorithm will change (*q).next
   *  But if we need to insert x at the beginning of the list, we
    cannot do that; instead the pointer p needs to be changed
  
 *  Solution: don't use an ordinary variable for p; make a
  \emph{\alert{header node}} whose next field points to the list


### Deleting a node


 *  Need a pointer p into the list; the node \emph{after} p will be
  deleted
 *  Just change (*p).next to skip over the next node, and point to
  the one after
 *  The node being deleted should be returned to the avail list, so
  it can be reused 


### Code for deleting a node

If p points to a node, delete the node after that, assuming it exists

~~~~
delete (p)
  { if p /= nil
    then { q := (*p).next;
           if q /= nil
             then { (p*).next := (*q).next;
                    (*q).next := avail;
                    avail := q;
                  }
         }
  }
~~~~


 *  We can't delete the node p points to, we can only delete the
  following node, which q points at
 *  If you know that p cannot be nil, the first test can be omitted
 *  We do need to check whether q = nil; if it is, there's no node
  to delete
 *  It doesn't matter whether (*q).next is nil
 *  It's important to check whether the operation we're performing
  is actually possible!


### Space leaks


 *  If you return a deleted node to the avail list, it can be reused
 *  If you don't, this node becomes inaccessible: it doesn't hold
  useful data, yet it can't be allocated
 *   \alert{this is a
  bug in the program}
 *  Over time, as a program runs, more and more nodes may become
  inaccessible: a \alert{space leak}


### Memory management


 *  It's a bug if you delete a node that contains useful data
 *  It's a bug if you don't delete a node that doesn't contain
  useful data
 *  With complicated data structures, this can be difficult
 *  A common solution is \alert{garbage collection}
  
   *  The program doesn't explicitly return nodes to the avail list
   *  Periodically, the \emph{garbage collector} traverses all data
    structures and marks the nodes it finds
   *  Then the GC adds all unmarked nodes to the avail list
  


### Sharing and side effects


 *  Suppose p = [6, 2, 19, 37, 41]
 *  Traverse a few elements, and set q to point to the 19 node
 *  Now q = [19, 37, 41] and p is unchanged
 *  Then delete the second element of q.  The result is
  
   *  q = [19, 41]
   *  p = [6, 2, 19, 41] \quad \alert{Modifying q has also modified
    p}
  
 *  This is called a \alert{side effect}
 *  Sometimes you want this to happen, sometimes not, so it's
  important to be careful about it!


## Comparing lists and arrays}


 *  Lists and arrays are two different kinds of data structure that
  contain a sequence of data values
 *  How do you decide which to use?
 *  Consider the properties of lists and arrays, and the needs of
  your program
 *  And there are many other data structures to choose from, which
  you'll encounter as you learn computer science


### Accessing elements


 *  Direct access to an element
  
   *  Array: gives direct access (``random access'') to element with
    arbitrary index $i$
   *  List: gives direct access only to an element you have a
    pointer to; random access is inefficient
  
 *  Traversal
  
   *  Array: initialise $i$ to 0; repeatedly set \texttt{i := i+1};
    terminate when $i \geq n$ (that's the purpose of a \textbf{for}
    loop)
   *  List: initialize $p$ to point to the list; repeatedly set
    \texttt{p := (*p).next}; terminate when \texttt{p = nil}
  


### Usage of memory


 *  Memory needed per element
  
   *  Array: need just the memory required for the element itself
    (typically a word)
   *  List: need a node for each element, which also requires space
    for the next pointer (typically a word)
   *  So typically, an array with $n$ elements needs $n$ words,
    while a list requires $2 \times n$ words
  
 *  Flexibility
  
   *  An array has fixed size and needs to be allocated fully
   *  A list has variable size and needs only enough memory to hold
    its nodes
  


### More general data structures


 *  We can put several pointer fields in each node, and produce an
  enormous variety of data structures, tailored for the needs of an
  application program
 *  Just a few examples
  
   *  Doubly linked list: each node contains two pointers, one to
    the previous node and one to the next.  Allows traversal both
    directions.
   *  Circular list: there is no ``last'' node where next=nil;
    instead, \emph{every} node points to the next node, and the list
    loops back to itself.  There is no ``first'' or ``last'' node.
  


## Abstract data type}


 *  A stack is an \alert{abstract data type}
  
   *  The idea: define the type by the \alert{operations} it
    supports, not by the code that implements it
   *  \alert{This is useful because there may be different
      implementations of an ADT, and which implementation is best may
      depend on the application using it}
  
 *  The stack ADT is defined by the \alert{operations} it supports:
  push, pop
 *  There are several completely different ways to implement a stack
  
   *  We have already seen how to implement a stack with an array
   *  We can also do it with a linked list
  


### Linked list implementation of stack


 *  A linked list gives easy access to the front of the list, and a
  stack gives easy access to the top of the stack.
 *  Represent Empty stack as nil
 *  Push x is implemented by stack := cons (x, stack)
 *  Pop x is implemented by stack := (*stack).next


### Array implementation of stack


 *  We can implement a stack using an array
 *  There is a variable \emph{stLim} which gives the size of the
  array --- this is the limit on the maximum number of elements that
  can be pushed
 *  There is a variable \emph{stTop} that gives the current number
  of elements in the stack


### Relationship between arrays and stacks


 *  Array
  
   *  A container that holds many elements
   *  Each element has an index (which is an integer)
   *  You can access any element x[i]
   *  You can access the elements in any order
  
 *  Stack
  
   *  A container that holds many elements
   *  You can only access the top element, and you don't need to
    know its index
   *  You can (and must) access the elements in \alert{last in ---
    first out} order
  


### Pushing x onto a stack

~~~~
; push the x onto the stack
; stack[stTop] := R1; stTop := stTop + 1

push   load  R1,x[R0]       ; R1 := x
       load  R2,stTop[R0]   ; R2 := stTop
       store R1,stack[R2]   ; stack[stTop] := x
       lea   R3,1[R0]       ; R3 := constant 1
       add   R2,R2,R3       ; R2 := stTop + 1
       store R2,stTop[R0]   ; stTop := stTop + 1
~~~~

### Pop a stack, returning x

~~~~
; pop the stack, store top element into x
; stTop := stTop - 1; x := stack[stTop]

pop    load  R2,stTop[R0]   ; R2 := stTop
       lea   R3,1[R0]       ; R3 := constant 1
       sub   R2,R1,R3       ; R2 := stTop - 1
       load  R1,stack[R2]   ; R1 := stack[stTop-1]
       store R1,x[R0]       ; x := stack[stTop-1]
       store R2,stTop[R0]   ; stTop := stTop - 1
~~~~


### Error checking

### Issues with simplest implementation}


 *  It doesn't check for errors!
  
   *  If push is called when stack is full, data will be written
    outside the array
   *  If pop is called when stack is empty, a garbage result will be
    returned
  
 *  Either of these errors may cause the program to get wrong
  answers or to crash


### Robust software


 *  Fragile software will respond to a minor problem by going
  haywire: might crash, or produce wrong answers
 *  \alert{Robust software} checks for all errors and does something
  appropriate; a minor problem doesn't turn into a major one


### Error checking and error handling


 *  Software should not assume everything is ok --- it should check
  for errors
  
   *  push (x) when the stack is full
   *  x := pop () when the stack is empty
  
 *  If an error is detected, the error must be \emph{handled}
 *  There are many approaches
  
   *  Produce a message and terminate the program
   *  Return an error code to the calling program and let it decide
    what to do
   *  Throw an exception, which will interrupt the calling program,
    and invoke its error handler
  


For simplicity, we will terminate the program if an error occurs.

### Error checking: push

If the stack is full, there is no space to store the new element, so
push fails

~~~~
; push (v)
; if stTop >= stLim
;     then
;        terminate because the stack is full: cannot push
;     else
;        stack[stTop] := v
;        stTop := stTop + 1
;        return ()
~~~~


### Error checking: pop

If the stack is empty, there is no element to return, so pop fails

~~~~
; v = pop ()
;   if stTop == 0
;     then
;       terminate because the stack is empty: cannot pop
;     else
;       stTop := stTop - 1
;       v := stack[stTop]
;       return (v)
~~~~

## Programming techniques


 *  Compound Boolean expressions: ``short circuit'' evaluation
 *  The condition code and ``cmp jumpgt'' style comparisons
 *  loops: for loop, while loop, repeat until loop
 *  Input/Output: write characters, not numbers


### Compound Boolean expressions

Notation: various programming languages use several slightly different
notations are used for Boolean operators

~~~~
i<n and x[i]>0      i<n && x[i]>0     i<n & x[i]>0
i<n or  j<n         i<n || j<n        i<n | x[i]>0
~~~~

### ``Short circuit'' expressions


 *  Suppose \texttt{x} is an array with \texttt{n} elements
 *  Consider \texttt{i<n \&\& x[i]>0}
 *  If the first expression \texttt{i<n} is False, then the whole
  expression is False
 *  In that case, there is no need to evaluate the second expression
  \texttt{x[i]>0}
 *  We can ``short circuit'' the evaluation
 *  Big advantage: if \texttt{i<n} is False, then \texttt{x[i]} does
  not exist and evaluating it could cause an error
 *  So it is \emph{essential} not to evaluate the second expression
  if the first one is false  





### Implementing a compound boolean expression

~~~~
while i<n && x[i]>0 do S

; if not (i<n && x[i]>0) then goto loopDone
    load   R1,i[R0]          ; R1 := i
    load   R2,n[R0]          ; R2 := n
    cmplt  R3,R1,R2          ; R3 := i<n
    jumpf  R3,loopDone[R0]   ; if not (i<n) then goto loopDone
    load   R3,x[R1]          ; R3 := x[i]   safe because i<n
    cmpgt  R4,R3,R0          ; R4 := x[i]>0
    jumpf  R4,loopDone[R0]   ; if not (x[i]>0) then goto loopDone
~~~~

This is better than evaluating both parts of the expression and
calculating logical and

### Condition code



 *  We have seen one style for comparison and conditional jump
~~~~
   cmplt  R3,R8,R4
   jumpt  R3,someplace[R0]
~~~~
 *  There is also another way you can do it
~~~~
   cmp    R8,R4           ; no destination register
   jumplt someplace[R0]   ; jump if less than
~~~~
 *  The cmp instruction sets a result (less than, equal, etc) in R15
  which is called the \alert{condition code}
 *  There are conditional jumps for all the results: jumpeq, jumplt,
  jumple (jump if less than or equal), etc
 *  An advantage is that you don't need to use a register for the
  boolean result


#### Repeat-until loop

This is similar to a while loop, except you decide whether to continue
at the end of the loop

~~~~
repeat
  {S1; S2; S3}
until i>n;
~~~~

This is equivalent to

~~~~
S1; S2; S3;
while not (i>n) do
  {S1; S2; S3}
~~~~

The while loop is used far more often, but if you need to go through
the loop at least one time, the repeat-until is useful

### Input/Output


 *  A character is represented by a code using ASCII or Unicode
  
   *  http://www.asciitable.com/
   *  https://unicode-table.com/en/
  
 *  digit characters 0..9 have codes (in decimal) 48..57
  
   *  Example: '3' is represented by the number 51, not by the
    number 3
  
 *  lower case a..z have codes (in decimal) 97..122
 *  upper case A..Z have codes (in decimal) 65..90
 *  To print a number, we need to convert it to a string of characters


### Converting a number to a string


 *  We actually need to do arithmetic to convert a binary number to
  decimal, and to a string of decimal digits
 *  The lab exercise gives the algorithm to do this
 *  It needs to divide the number by 10 to get the quotient and the
  remainder
 *  div R1,R2,R3
  
   *  Divides R2/R3
   *  The quotient goes into R1 (the destination register)
   *  The remainder goes into R15 (always R15, you cannot change
    this)
  
 *  The algorithm repeatedly divides the number by 10; the remainder
  is used to get a digit character


### Arrays and pointers

We have seen how to access an array element using an index register

~~~~
; R5 := x[i]
    load   R2,i[R0]    ; R2 := i
    load   R5,x[R2]    ; R5 := x[i]
~~~~

### Sum of an array x using index: high level

Suppose x is an array of numbers, and sizeX is the number of
elements.  We want to add up all the elements of x.

\vspace{1em}
A for loop is convenient (the whole purpose of the for loop is for
writing this kind of loop):

~~~~
   sum := 0;
   for i := 0 to sizeX do
     { sum := sum + x[i]; }
~~~~

You can also use a while loop:

~~~~
   sum := 0;
   i := 0;
   while i < sizeX do
     { sum := sum + x[i];
       i := i + 1; }
~~~~

### Arrays and pointers


 *  There is also another way to access an array element, \alert{using
    pointers instead of indexes}
 *  To do this, we will perform \alert{arithmetic on pointers}



### Accessing an array element using a pointer


 *  Create a pointer p to the beginning of the array x, so p is
  pointing to x[0]\\ \alert{lea R1,x[R0]}
 *  To access the current element, follow p:\\ \alert{load R2,0[R1]}
 *  To move on to the next element of the array, increment p\\
  \alert{lea R1,1[R1]}
 *  Notice that we are doing \alert{arithmetic on pointers}


~~~~
x   data  34   ; first element of x
    data  82
    data  91
    data 29    ; last element of x
xEnd           ; address of first word after array x
~~~~

### Sum of an array x using pointers: high level

~~~~
   sum := 0;
   p := &x;
   q := &xEnd;
   while p<q do
     { sum := sum + *p;
       p := p + 1; }
~~~~


 *  In assembly language, we can use lea to increment the pointer.
 *  Suppose p is in R1, then\\ \texttt{  lea R1,1[R1] ; p := p + 1}
 *  We are incrementing p by the size of an array element


### Sum of an array x using pointers: assembly language


~~~~
;   R1 = p = pointer to current element of array x
;   R2 = q = pointer to end of array x
;   R3 = sum of elements of array x

    lea   R1,x[R0]      ; p := &x
    lea   R2,xEnd[R0]   ; q := %xEnd
    add   R3,R0,R0      ; sum := 0
sumLoop
    cmplt R4,R1,R2      ; R4 := p<q
    jumpf sumLoopDone   ; if not p<q then goto sumLoopDone
    load  R4,0[R1]      ; R4 := *p  (this is current element of x)
    add   R3,R3,R4      ; sum := sum + *p
    lea   R1,1[R1]      ; p := p+1  (point to next element of x)
    jump  sumLoop[R0]   ; goto sumLoop
sumLoopEnd

x   data  23   ; first element of x
    data  42   ; next element of x
    data 19    ; last element of x
xEnd
~~~~
}

### Comparing the two approaches


 *  Accessing elements of an array using index
  
   *  Get x[i] with load  R5,x[R1] where R1=i
   *  Move to next element of array by i := i+1
   *  Determine end of loop with \texttt{i < xSize}
   *  Know in advance how many iterations: xSize
   *  A for loop is convenient
  
 *  Accessing elements of an array using pointer
  
   *  Initialize p with lea R1,x[R0]
   *  Get x[i] with load  R5,0[R1] where R1=p
   *  Move to next element of array by \texttt{p := p+1}
   *  Determine end of loop with \texttt{p < q} (q points to end of array)
   *  Don't need to know in advance how many iterations
   *  Need to use a while loop
  
 *  Both techniques are important
 *  \alert{If you have an array of records, it's easier to use a
    pointer}


### Records

~~~~
program Records
 { x, y :
     record
       { fieldA : int;
         fieldB : int;
         fieldC : int; }
   x.fieldA := x.fieldB + x.fieldC;
   y.fieldA := y.fieldB + y.fieldC;
 }
~~~~

Suppose we have an array of these records, and want to

 *  set fieldA := fieldB + fieldC in every record in the array
 *  Calculate the sum of the fieldA in every record


### Traverse array of records with indexing

~~~~
sum := 0;
for i := 0 to nrecords do
  { RecordArray[i].fieldA :=
       RecordArray[i].fieldB + RecordArray[i].fieldC;
    sum := RecordArray[i].fieldA; }
~~~~


 *  This is ok
 *  But it is a little awkward


### Traverse array of records with pointers: high level

~~~~
sum := 0;
p := &RecordArray;
q := &RecordArrayEnd;
while p < q do
  { *p.fieldA := *p.fieldB + *p.fieldC;
    sum := sum + *p.fieldA;
    p := p + RecordSize; }
~~~~

In professional programming, this is often preferred because accessing
the elements of the records is easier (it's easier to access an
``element of an element'' via pointer)

### Traverse array of records with pointers: low level

~~~~
;    sum := 0;
;    p := &RecordArray;
;    q := &RecordArrayEnd;
; RecordLoop
;    if (p<q) = False then goto recordLoopDone;
;    *p.fieldA := *p.fieldB + *p.fieldC;
;    sum := sum + *p.fieldA;
;    p := p + RecordSize;
;    goto recordLoop;
; RecordLoopDone
~~~~

### Traverse array of records with pointers: assembly language


~~~~
;   R1 = sum
;   R2 = p (pointer to current element)
;   R3 = q (pointer to end of array)
;   R4 = RecordSize

    lea    R1,0[R0]                 ; sum := 0
    lea    R2,RecordArray[R0]       ; p := &RecordArray;
    lea    R3,RecordArrayEnd[R0]    ; q := &RecordArray;
    load   R4,RecordSize[R0]        ; R4 := RecordSize
RecordLoop
    cmplt  R5,R2,R3                 ; R5 := p<q
    jumpf  R5,RecordLoopDone[R0]    ; if (p<q) = False then goto RecordLoopDone
    load   R5,1[R2]                 ; R5 := *p.fieldB
    load   R6,2[R2]                 ; R6 := *p.fieldC
    add    R7,R5,R6                 ; R7 := *p.fieldB + *p.fieldC
    store  R7,0[R2]                 ; *p.fieldA := *p.fieldB + *p.fieldC
    add    R1,R1,R7                 ; sum := sum + *p.fieldA
    add    R2,R2,R4                 ; p := p + RecordSize
    jump   RecordLoop[R0]           ; goto RecordLoop
RecordLoopDone
~~~~
}

### Stack overflow


 *   The mechanism for calling a procedure and returning is fairly
  complicated
 *  Rather than introducing all the details at once, we have looked
  at several versions, introducing the concepts one at a time
 *  Now we introduce the next level:
  
   *  Simplify calling a procedure
   *  The procedure checks for stack overflow
  
 *  We need two more registers dedicated to procedures
  
   *  R12 holds stack top (the highest address in current stack frame)
   *  R11 holds stack limit (the stack is not allowed to grow beyond
    this address)
  


### Register usage

See the PrintIntegers program for examples

~~~~
; Global register usage
;   R0 = constant 0
;   R1, R2, R3 are used for parameters and return values
;   R4 - R10 are available for local use in a procedure
;   R11 = stack limit
;   R12 = stack top
;   R13 = return address
;   R14 = stack pointer
;   R15 is transient condition code
~~~~

### Initialize the stack

~~~~
; Structure of stack frame for main program, frame size=1
;    0[R14]  dynamic link is nil

; Initialize the stack
    lea    R14,CallStack[R0]  ; initialise stack pointer
    store  R0,0[R14]          ; main program dynamic link = nil
    lea    R12,1[R14]         ; initialise stack top
    load   R1,StackSize[R0]   ; R1 := stack size
    add    R11,R14,R1         ; StackLimit := &CallStack + StackSize
~~~~

### Calling a procedure


 *  To call a procedure PROC:
  
   *  Place any parameters you're passing to PROC in R1, R2, R3
   *  jal R13,PROC[R0]
  


### Structure of Procedure stack frame

(This is procedure PrintInt, see lab exercise)

~~~~
; Arguments
;   R1 = x         = two's complement number to print
;   R2 = FieldSize = number of characters for print field
;        require FieldSize < FieldSizeLimit

; Structure of stack frame, frame size = 6
;    5[R14]  save R4
;    4[R14]  save R3
;    3[R14]  save R2 = argument fieldsize
;    2[R14]  save R1 = argument x
;    1[R14]  return address
;    0[R14]  dynamic link points to previous stack frame
~~~~

### Called procedure creates its stack frame

~~~~
PrintInt
; Create stack frame
    store  R14,0[R12]          ; save dynamic link
    add    R14,R12,R0          ; stack pointer := stack top
    lea    R12,6[R14]          ; stack top := stack ptr + frame size
    cmp    R12,R11             ; stack top ~ stack limit
    jumpgt StackOverflow[R0]   ; if top>limit then goto stack overflow
    store  R13,1[R14]          ; save return address
    store  R1,2[R14]           ; save R1
    store  R2,3[R14]           ; save R2
    store  R3,4[R14]           ; save R3
    store  R4,5[R14]           ; save R4
~~~~

### Procedure finishes and returns

~~~~
; return
    load   R1,2[R14]        ; restore R1
    load   R2,3[R14]        ; restore R2
    load   R3,4[R14]        ; restore R3
    load   R13,1[R14]       ; restore return address
    load   R14,0[R14]       ; pop stack frame
    jump   0[R13]           ; return
~~~~

### Stack overflow

If the stack is full and a procedure is called, this is a fatal error

~~~~
StackOverflow
    lea    R1,2[R0]
    lea    R2,StackOverflowMessage[R0]
    lea    R3,15[R0]   ; string length
    trap   R1,R2,R3    ; print "Stack overflow\n"
    trap   R0,R0,R0    ; halt

StackOverflowMessage
    data    83   ; 'S'
    data   116   ; 't'
    data    97   ; 'a'
    data    99   ; 'c'
    data   107   ; 'k'
...
~~~~

### Blocks


 *  You'll need to learn many programming languages, eventually
 *  There are concepts that appear in most languages
 *  It's useful to focus on the general concept
 *  One important concept is a block
 *  A \alert{block} or \alert{compound statement} is a single
  statement that contains several statements
 *  The purpose is to let you have a group of statements in a loop,
  or controlled by a conditional
 *  The \alert{syntax} is the detailed punctuation used to indicate
  a block, and this varies in different languages
  
   *  There are lots of small syntax differences between languages
   *  Some languages use := to mean assign, = to mean equals
   *  Some languages use = to mean assign, == to mean equals
  


#### Syntax for blocks

### Python syntax style for blocks: layout

The layout (the indentation) determines what is inside the if
statement

~~~~
a = 1
if  x < y :
  b = 2
  c = 3
d = 4
~~~~

If you change the indentation, you change the meaning of the program

\vspace{1em} In Python, you write \texttt{:} to mean \emph{then}, and
\texttt{else} to mean \emph{else}

### Algol/Pascal style for blocks: begin---end

~~~~
a := 1;
if x<y
  then begin b := 2;
             c := 3
       end
  else begin d := 4;
             e := 5
       end;
f := 6;
~~~~

You write \texttt{then} to mean \emph{then}, and \texttt{else} to mean
\emph{else}

\vspace{1em}
Statements must be separated by semicolon ;

### C style for blocks: braces
  
~~~~
a = 1;
if (x < y) b = 2;
else
   { d = 4;
     e = 5; }
f = 6;
~~~~

In C you don't write \texttt{then} at all, but this means the
condition \texttt{x < y} must be enclosed in parentheses so the
compiler can tell where the condition ends and the then-statement
begins

### Block structured style for blocks: matching keywords

~~~~
a := 1;
if x<y
  then b := 2;
       while i<n do
           sum := sum+x[i];
          i := i+1
       endwhile
  else d := 4
       e := 5
endif;
f := 6;
~~~~

This style introduces a lot of keywords (endif, endwhile, endfor,
endrepeat) but you don't need braces around a block.  It makes code
more readable and enables the compiler to produce better error
messages

### Enter at beginning, exit at end


 *  A common programming style is to require
  
   *  Each block enters only at the beginning of the block
   *  Each block exits only at the end of the block
  
 *  This style is helpful in some programming languages, but in some
  languages it sometimes makes code less readable
 *  In assembly language, and for compilation patterns, it is
  necessary to follow this style
 *  In high level languages this style is sometimes helpful, but not
  always


### Single entrance/exit for compilation patterns


 *  It is straightforward to translate high level control constructs
  using the compilation patterns
 *  These \emph{require} that the blocks of code always start at the
  beginning and finish at the end
 *  It's bad to jump into the middle of a block, or exit out of the
  middle because
  
   *  The compilation patterns won't work correctly
   *  You'll have to duplicate a lot of code
   *  Example: returning from a procedure requires restoring the
    registers, resetting the stack pointer, loading the return address
   *  That code should not be duplicated in several places in a
    procedure
  


### Systematic approach to programming?


 *  Start by understanding what your program should do
 *  Express the algorithm using high level language notation (and
  it's ok to mix in some English too)
 *  Translate the high level to the low level
  
   *  Assignment statements: x := expression
   *  I/O statements: Write string
   *  goto L
   *  if boolean then goto L
  
 *  Translate the low level to assembly language
 *  Retain the high and low level code as comments
 *  Do hand execution at every level


### Why use this systematic approach to programming?


 *  It enables you to write correct code at the outset, and minimise
  debugging
 *  If there is a bug, it helps you to catch it early (e.g. in
  translation to goto form)
 *  If there's a bug in an instruction, the comments enable you to
  find it quickly
  
   *  A common error is to use a wrong register number: add R9,R3,R4
   *  Poor comments don't help:  \alert{add R9,R3,R4 ; R9 := R3+R4}
   *  Good comments help a lot: \alert{add R9,R3,R4 ; x := alpha +
    (a[i]*b[i])}
   *  Look at the register usage comments (oops, x is in R8, not R9,
    now I know how to fix it and I don't have to read the entire
    program)
  
 *  Professional software needs to be maintained; the comments make
  the software easier to read and more valuable


### Nested conditionals

#### Nested if-then-else

Conditional statements can be nested deeply

~~~~
if b1
  then S1
       if b2
         then S3
         else S4
       S5
  else S6
       if b3
         then S7
       S8
~~~~

### Special case of nested if-then-else

Often the nesting isn't random, but has this specific structure:
  
~~~~
if b1
  then S1
  else if b2
         then S2
         else if b3
                then S3
                else if b4
                       then S4
                       else if b5
                              then S5
                              else S6
~~~~
(Some languages require punctuation to avoid ambiguity)

### Another way to write it

To avoid running off the right side of the window, it's usually
indented like this:

~~~~
if b1
  then S1
else if b2
  then S2
else if b3
  then S3
else if b4
  then S4
else if b5
  then S5
else S6
~~~~

### Some programming languages have elsif or elif

~~~~
if b1 then S1
  elif b2 then S2
  elif b3 then S3
  elif b4 then S4
  eleif b5 then S5
  else S6
~~~~


 *  It avoids ambiguity
 *  It signals to the compiler and to a human programmer that this
  specific construct is being used
 *  It allows good indentation layout without violating the basic
  principle of indentation
 *  Some languages have this, some don't


### Case and jump tables

### A common application: numeric code

Nested if-then-else but the boolean conditions are not arbitrary: they
are checking the value of a code:
  
~~~~
if code = 0
  then S1
else if code = 1
  then S2
else if code = 2
  then S3
else if code = 3
  then S4
else if code = 4
  then S5
~~~~


### The case statement

~~~~
case n of
   0 -> Stmt
   1 -> Stmt
   2 -> Stmt
   3 -> Stmt
   4 -> Stmt
   5 -> Stmt
   else -> Stmt // handle error
~~~~

This means: execute the statement corresponding to the value of n

\vspace{1em}

Many programming languages have this; the syntax varies a lot but that
isn't what's important

### Example: numeric code specifies a command


~~~~
; The input data is an array of records, each specifying an operation
;    Command : record
;       code : Int     ; specify which operation to perform
;       i    : Int     ; index into array of lists
;       x    : Int     ; value of list element

; The meaning of a command depends on the code:
;    0  terminate the program
;    1  insert x into set[i]
;    2  delete x from set[i]
;    3  return 1 if set[i] contains x, otherwise 0
;    4  print the elements of set[i]
~~~~
}

### Selecting the command with a case statement

; Initialize
;    BuildHeap ()

; Execute the commands in the input data
;    finished := 0
;    while InputPtr <= InputEnd && not finished
;       CurrentCode := (*InputPtr).code
;       p := set[*InputPtr]     ; linked list
;       x := (*InputPtr).x      ; value to insert, delete, search

;       case CurrentCode of
;          0 : <CmdTerminate>
;          1 : <CmdInsert>
;          2 : <CmdDelete>
;          3 : <CmdSearch>
;          4 : <CmdPrint>
;          else : <>
;       InputPtr := InputPtr + sizeof(Command)
; Terminate the program
;    halt

### Finding a numeric code


 *  It's tedious and inefficient to go through the possible values
  of a numeric code in sequence
  
   *  If you're looking up Dr Zhivago in the phone book, do you look
    at Arnold Aardvark, and Anne Anderson, on on and on?
   *  You go straight to the end of the book
  
 *  We want to find the statement corresponding to a numeric code
  directly, without checking all the other values


### A problem with efficiency


 *  The problem

 *  There are many applications of case statements
 *  They are executed frequently
 *  The number of cases can be large (not just 4 or 5; can be
  hundreds)
 *  The implementation of the if-then-else requires a separate
  compare and jump for each condition

 *  The solution
  
   *  A technique called \alert{jump tables}
  


### Jump tables: the basic idea


 *  For each target statement (S1, S2, S3, etc) in the conditional,
  introduce a jump to it: jump S1[R0], jump S2[R0], etc
 *  Make an \alert{array} ``jt'' of these jump instructions
~~~~
   jt[0] = jump S0[R0]
   jt[1] = jump S1[R0]
   jt[2] = jump S2[R0]
   jt[3] = jump S3[R0]
   jt[4] = jump S4[R0]
~~~~
 *  Given the \emph{code}, Jump to jt[code]
 *  Each element of the array is an instruction that requires two
  words
 *  So jump to \&jt + 2 $\times$ code


### Jump table

; Jump to operation specified by code
    add    R4,R4,R4            ; code := 2*code
    lea    R5,CmdJumpTable[R0] ; R5 := pointer to jump table
    add    R4,R5,R4            ; address of jump to operation
    jump   0[R4]               ; jump to jump to operation
CmdJumpTable
    jump   CmdTerminate[R0]    ; code 0: terminate the program
    jump   CmdInsert[R0]       ; code 1: insert
    jump   CmdDelete[R0]       ; code 2: delete
    jump   CmdSearch[R0]       ; code 3: search
    jump   CmdPrint[R0]        ; code 4: print

CmdDone
    load   R5,InputPtr[R0]
    lea    R6,3[R0]
    add    R5,R5,R6
    store  R5,InputPtr[R0]
    jump   CommandLoop[R0]

### We have to be careful!


 *  What if code is negative, or larger than the number of cases?
 *  The jump to the jump table could go anywhere!
 *  It might not even go to an instruction
 *  But whatever is in memory at the place it goes to, will be
  interpreted as an instruction that will be executed
 *  The program will go haywire
 *  Debugging it will be hard: the only way to catch the error is to
  read the code and/or to single step
 *  Solution: before jumping into the jump table, check to see if
  code is invalid (too big or too small)


### Checking whether the code is invalid


~~~~
CommandLoop
    load   R5,InputPtr[R0]     ; R1 := InputPtr
    load   R4,0[R5]            ; R4 := *InputPtr.code
; Check for invalid code    
    cmp    R4,R0               ; compare (*InputPtr).code, 0
    jumplt CmdDone[R0]         ; skip invalid code (negative)
    lea    R6,4[R0]            ; maximum valid code
    cmp    R4,R6               ; compare code with max valid code
    jumpgt CmdDone[R0]         ; skip invalid code (too large)

...
    
CmdDone
    load   R5,InputPtr[R0]
    lea    R6,3[R0]
    add    R5,R5,R6
    store  R5,InputPtr[R0]
    jump   CommandLoop[R0]

## What's the significance of root?


 *  Many \alert{processes} are running in the computer
 *  Each has its own set of privileges
 *  There is one special user ``root'' which has \emph{all}
  privileges: \alert{root can do anything at all}
 *  The effect of this change to the source code (a change of
  \emph{one character!} is
  
   *  If a user program calls this obscure function with just the
    right set of obscure options, \alert{it suddenly gains full
      control over the machine}
  
 *  Don't worry --- \alert{this faulty code never made it into the
    master copy of Linux}


## Example: Ordered lists program

Concepts used in the program:

Array of records, Representing a command as a records, Traversing an
array of records, Case statement and jump table

Traversing a list to print its elements, Insertion in list keeping the
elements in ascending order, Deletion from a list, Searching a list

### Ordered lists

There is an array of lists, initially empty.  There are nlists of them.

~~~~
list[0] = [ ]
list[1] = [ ]
...
list[nlists-1] = [ ]
~~~~

At all times as the program runs, the lists are ordered: their
elements are increasing

~~~~
list[0] = [4, 9, 23, 51 ]
list[1] = [7, 102, 238 ]
...
list[nlists-1] = [2, 87, 89, 93, 103, 195 ]
~~~~


### Commands

The program executes commands:


 *  Terminate --- the program finishes
 *  Insert into list i the value x --- modify list[i] so it contains
  x, while maintaining the ascending order
 *  Delete from list i the value x --- modify list[i] so x is
  removed, but don't do anything if x isn't in the list
 *  Search list i for x --- print Yes if x is in the list, No
  otherwise
 *  Print i --- the numbers in list[i] are printed


#### Example

 *  Insert into list[3] the value 23  \qquad \alert{[23]}
 *  Insert into list[3]the value 6   \qquad \alert{[6, 23]}
 *  Insert into list[3] the value 67    \qquad \alert{[6, 23, 67]}
 *  Insert into list[3] the value 19    \qquad \alert{[6, 19, 23, 67]}
 *  Print list[3] \qquad \alert{6 19 23 67}

### Why are ordered lists useful?


 *  This is one way to arrange a database: think of the elements as
  persons' names, or matriculation numbers
 *  Sometimes you want to process all the data in a container in a
  specified order
 *  If the data is ordered, it's faster to find a particular item
  (on average you only have to check half of the items)
 *  An ordered list can be used to represent a set


### Where do the commands come from?


 *  In a real application, we would read the commands from input
 *  But in this program, each command is represented as a record
 *  The entire input is a static array of records defined with
  \texttt{data} statements
 *  This is easier because
  
   *  If you read from an input device, it's necessary to convert
    the input character string to numbers
   *  In testing a program, it's convenient to have input data that
    is \alert{fixed and repeatable}
   *  Don't want to have to type in the same input every time you
    run the program!
  



### Representing a command


 *  Each command is a record with three fields
  
   *  A code indicating which kind of command
   *  A number i indicating which list we're operating on
   *  A value x which might be inserted etc
  
 *  Each record must have these three fields
 *  Some commands don't use them all (e.g. Print just needs i, not
  x)
 *  The main program uses a \alert{case} statement to handle each
  command, and implements this with a \alert{jump table}



### Reading a program before writing

  
   *  You should \emph{read and understand} the program before
    modifying it
    
     *  Reading a program is an important skill you will need
      throughout your career
     *  The program is filled with examples so it is excellent
      revision material
     *  You need to understand a program before you'll be able to
      make changes to it
    
   *  One of the aims of the exercise is to get experience with
    reading a longer program---don't skip this!
  


### Some tips on testing and debugging

  
   *  Debugging has two phases:
    \begin{enumerate}
     *  Diagnosis: finding out what went wrong and why
     *  Correction: fixing the error
    \end{enumerate}
   *  The most important point: don't just make random changes to
    the code and hope for the best---instead, find out what the error
    is and fix it cleanly
  


### Reading and testing a program

  
   *  A good way to understand a section of assembly language
    instructions is to step through it, one instruction at a time
    \begin{enumerate}
     *  Check that the instruction did what you expected it to do
     *  Check that the instruction is consistent with its comment
     *  Try to relate the instruction with the bigger picture: what
      is it doing in the context of the program?
    \end{enumerate}
   *  Coverage
    \begin{enumerate}
     *  You don't need to step through a set of instructions a huge
      number of times
     *  If there's a loop, step through two or three iterations
     *  If possible, arrange test data so the loop will terminate
      after just a few iterations
     *  But try to step through as much of the program as possible
     *  This is called \alert{coverage}: try to cover all of the
      program with your testing
    \end{enumerate}
  


### Breakpoints

  
   *  It's a good idea to step through a program one instruction at
    a time, so you understand clearly what each instruction is doing
   *  However, in a longer program this isn't always feasible
    
     *  The OrderedLists program has to build the heap when it
      starts; this may take several thousand instructions before it
      even really gets going!
    
   *  Solution: \alert{breakpoints}
    
     *  Find the address of an instruction where you want to start
      single stepping
     *  Enter this address as a breakpoint
     *  Click Run to execute the program at full speed; when it
      reaches the breakpoint it will stop
     *  Then you can single step to examine what the instructions
      are doing
    
  


### How to set a breakpoint

  
   *  On the Processor pane, click Breakpoint.  It will say
    ``Breakpoint is off''
   *  Enter the breakpoint command and click Set Breakpoint
   *  \texttt{BPeq BPpc (BPhex "01a6")}
   *  It will say ``Breakpoint is on''.  Click Close
   *  On Processor, click Run.  It will stop when the pc register
    gets the value you specified
  



## Trees

### Tree


 *  A node doesn't have to have two fields named \emph{value} and
  \emph{next} --- it's normal to define a specific node type for an
  application program.
 *  Nodes with \emph{value} and \emph{next} can be connected into a
  \alert{linked list}.
 *  Nodes can also have with several fields containing data, not
  just one ``value'' field.
 *  And a node can have several pointer fields$\ldots$
 *  Common case: a \alert{binary tree} has two pointers in each
  node, named \alert{left} and \alert{right}.
 *  Each of these can either contain nil, or point to another node.




  Node : record
    value    ; the actual data in the node
    left     ; left subtree is a pointer to a Node
    right   ;  right subtree is a pointer to a Node



 *  Similar to a node for a linked list, but with two pointers
 *  There can also be several fields for data, not just one
  ``value'' field
 *  And we could have more than just two pointers



### A binary tree

\begin{tikzpicture}
  \node {root}
  child {node {a}}
  child {node {b}
    child {node {x}}
    child {node {y}}
};
\end{tikzpicture}

\vspace{1em}

In computer science, for some reason we draw trees upside down

\vspace{1em}
Suppose p is a pointer to the tree

 *  (*p).left is the pointer to the left subtree
 *  (*p).right is the pointer to the right subtree



### Applications of trees

Trees are used everywhere in programming

 *  To hold structured data
 *  To make programs faster (\emph{much} faster)




### Holding structured data


 *  A compiler reads in program text, which is just a character
  string: a sequence of characters.
 *  It needs to represent the deep structure underlying that
  sequence of characters.
 *  This is done by building a tree (the part of a compiler that
  takes a character string and produces a tree is called the
  \alert{parser}).





### Parsing

~~~~
x := a + b * c
~~~~

\begin{tikzpicture}
  \node {assignment}
  child {node {x}}
  child {node {+}
    child {node {a}}
    child {node {*}
      child {node {b}}
      child {node {c}}}};
\end{tikzpicture}



### Another application of jump tables!


* In complicated applications, trees normally have \alert{several
  different types of node}

* Examples: operations with 1 operand; operations with 2 operands;
  control constructs with a boolean expression and two statements,
  etc.

* So there are several different kinds of record

* Each record has a \emph{code} in the first word

* The value of the code determines how many more words there are
  in the record, and what they mean

* When a program has a pointer to a node, it needs to examine the
  code and take different actions depending on what the code is

* This is done with a jump table




### Searching


 *  Suppose we have a large number of records (e.g. a database)
 *  We want to search the database for an entry where a field has a
  certain value (e.g. search for a record where the MatricNumber field
  is 123456)
 *  If you just have these records in an array, or a linked list,
  you have to search them one by one
 *  On average, you have to look at half the entries in the database
  to find the one you want
 *  If you double the size of the database, you double the average
  time to look up an entry
 *  Terminology: this is called \emph{linear time} or $O(n)$
  complexity



### A better approach


 *  Linear search is silly if you can place the records in order
 *  You're trying to find the telephone number of John Smith in the
  phone book
 *  Would you do this?
  \begin{enumerate}
   *  It isn't Aardvark, Aaron
   *  It isn't Acton, Rebecca
   *  It isn't Anderson, Susan
   *  It isn't Atwater, James
   *  $\ldots$ 8 million more unsuccsessful searches because this is
    the Los Angeles directory
  \end{enumerate}
 *  That's silly!
 *  Open the book to the middle, notice that S is in the second half
 *  Open the book to the middle of the second half $\ldots$
 *  Each time you look at an entry in the book, you discard
  \alert{half of the remaining possibilities}



### Binary search tree

\begin{tikzpicture}
  [level 1/.style={sibling distance=8em},
  level 2/.style={sibling distance=5em},
  ]
  \node {31}
  child {node {7}
    child {node {5}}
    child {node {9}}}
  child {node {56}
    child {node {41}}
    child {node {78}}};
\end{tikzpicture}


 *  At every level: if a node contains $x$, then
  
   *  every node in the left subtree is less than $x$, and
   *  every node in the right subtree is greater than $x$.
  
 *  You can search the tree by starting at the root, and at every
  step you \emph{know} whether to go left or right



### Algorithmic Complexity


 *  Complexity is concerned with \alert{how the execution time grows
    as the size of the input grows}
 *  This is expressed as a function of the input size $n$
 *  Normally we don't care about the \emph{exact} function, and we
  use O-notation.  Instead of a funciton like $f(n) = 4.823 \times n$,
  we just write  $f(n) = O(n)$
  
   *  $O(1)$ --- if input grows, the execution time remains
    unchanged.  This is unrealistic: the program cannot even look at
    the input!
   *  $O(n)$ --- if the input is 5 times bigger, the execution time
    is 5 times bigger.  This is the best you can hope for
   *  $O(n^2)$ --- if the input is 5 times bigger, the time is 25
    times bigger
  


### Algorithm is more important than small optimisation


 *  Some programmers spend lots of effort trying to save one or two
  instructions in a piece of a program
  
   *  But it doesn't matter much whether a program takes 2.00032
    seconds or 2.00031 seconds
  
 *  It's much more important to use a suitable \alert{algorithm}
  
   *  On small data it doesn't make much differnce
   *  On large (realistic) data, a better algorithm makes a huge
    difference
  


### Complexity for search


 *  Ordered lists
  
   *  The Ordered Lists program has an operation to search a list
    for a value x
   *  On average, you need to look through half of the data to find
    out whether x is present
   *  If the list were \emph{not} ordered, you would need to look
    through \emph{all} of the data to determine whether x is present
   *  So the ordered list makes the search about twice as fast
   *  But in either case, this is $O(n)$ --- if you double the data
    size, the average time is doubled
  
 *  Binary search tree
  
   *  The number of comparisons needed is roughly the height of the
    tree
   *  If the tree is \emph{balanced}, the time complexity is
    $O(\log n)$
  


### How much faster?


 *  With a linear data structure (array, linked list)
  
   *  Each time you compare a database entry with your key, you
    eliminate one possibility
   *  The time is proportional to the size of the database
   *  It's called \emph{linear time} --- time = $O(n)$
   *  \alert{For 2 million records, you need a million comparisons}
  
 *  With a binary search tree
  
   *  Each time you compare a database entry with your key, you
    eliminate (on average) half of the possibilities
   *  The time is proportional to \emph{the logarithm of the size}
    of the database
   *  It's called \emph{log time} --- time = $O(\log n)$
   *  \alert{For 2 million records, you need 21 comparisons}
   *  There's a saying: \alert{``logs come from trees''}
  


### A common pitfall


 *  When you're writing a program, it's natural to test it with
  small data
 *  Even if the algorithm has bad complexity, the testing may be
  fast
 *  But then, when you run the program on real data, the execution
  time is intolerable
 *  That means going back and starting over again
 *  So it's a good idea to be aware of the complexity of your
  algorithm from the beginning


### How bad can complexity be?

Order of magnitude estimate of time for input of size $n$


\begin{tabular}{ccccl}
n & $\log n$ & $n \log n$ & $n^2$ & $2^n$ \\
\hline
 1  &  1  &  1    & 1     & 2 \\
 10 &  3  & 30    & 100   & 1,000 \\
 100 & 7  & 700   & 10,000 &1267650600228229401496703205376 \\
1,000 & 10 & 10,000 & 1,000,000 & $>$ age of universe \\ 
\end{tabular}



Lots of real problems have data size larger than 1,000.
Lots of algorithms have exponential complexity: $2^n$.

## Interrupts

### Control constructs

  Control constructs determine the order of execution of statements
  
  
   *  We have seen some high level control constructs
    
     *  if b then S
     *  if b then S1 else S2
     *  while b do S
     *  for var := expr1 to expr2 do S
     *  procedure
     *  And there are plenty more
    
   *  These are implemented using just a couple of low level control
    constructs
    
     *  goto L 
     *  if b then goto L
  
   *  But there is one more low level primitive: \alert{interrupts}
  

### Another kind of control: losing control!

 *  Control constructs built on goto and if-then-goto let the
  program decide what to do next
 *  Sometimes we want \emph{something else} --- not the program ---
  to decide what to do next

### Interrupts


 *  The hardware provides \emph{interrupts} which are used to
  implement processes
 *  An interrupt is an \alert{automatic jump}
 *  It goes either to the operating system or to an error handler
 *  But it is not the result of a jump instruction --- it happens
  automatically when an external event occurs
 *  The program that was running never jumped to the OS - the
  processor just stops executing its instructions, and starts
  executing the OS instead
 *  It's like talking to a group of people, and suddenly you get
  interrupted!


### What causes an interrupt


 *  An error in a user program: e.g. overflow (result of arithmetic
  is too large to fit in a registers)
 *  A trap: this is an explicit jump to the operating system, but
  the program doesn't specify the address
 *  An external event: a disk drive needs attention \emph{right
  now}, or the timer goes off


### What happens when an interrupt occurs


 *  The computer is a digital circuit
 *  Without interrupts, it repeats forever
  \begin{enumerate}
   *  Fetch the instruction at the address in the pc register
   *  Execute the instruction
  \end{enumerate}
 *  With interrupts, it repeats this forever:
  \begin{enumerate}
   *  Check to see if there is an interrupt request
   *  If there is, savepc := pc, \alert{pc := address of code in
      operating system}
   *  Fetch the instruction at the address in the pc register
   *  Execute the instruction
  \end{enumerate}
 *  Since the pc has been modified, the next instruction will not be
  part of the program that was interrupted --- it will be the
  operating system  


### Saving state


 *  Remember, an interrupt is a jump to the OS
 *  This requires setting the address of OS in the pc register
 *  But if we simply assign a value to pc, the computer has
  forgotten where the interrupted program was
 *  Therefore the hardware must ``save state'': savepc := pc
 *  The OS has a special instruction that enables it to get the
  value of savepc


### How interrupts are used
  
   *  Interrupts can be used to \alert{catch errors} in a program,
    e.g. arithmetic overflow (the result is too big to fit in a
    register)
    
     *  If an overflow occurs (or divide by zero, or some other
      error) we want the program to jump to an error handler
    
   *  Trap is similar to an interrupt, and is used to \alert{request
      service} from the operating system
    
     *  User program can't halt the machine, but uses trap to ask
      the OS to stop running the program
    
   *  They can be used to provide \alert{quick service} to an
    Input/Output device
    
     *  A disk drive may generate an interrupt when the spinning
      platter reaches a certain point, and it needs service right away
      --- within a tight deadline
    
   *  Interrupts are used to implement \alert{concurrent processes}
    
     *  The operating system gives each process a \alert{time slice}
      in round-robin order, so each process makes progress
    
  

### Interrupts and programming languages


   *  Most programming languages don't provide the ability to work
    directly with interrupts
   *  But many programming language provide \alert{exceptions}
    
     *  Without an exception handler, a division by 0 might
      terminate the program
     *  In the program, you can set an exception handler: a
      procedure to execute if a division by 0 occurs
     *  The compiler might implement this in several different ways:
      
       *  It could put in explicit comparison and conditional jumps
        to check each division
       *  Or it could set up an interrupt handler (this requires
        negotiation with the operating system)
      
    
  

### Using interrupts to catch errors

### Catching errors


 *  As a program runs, it may accidentally produce an error
 *  Two examples:
  
   *  An arithmetic instruction produces a result that's too large
    to fit in a register: this is called \alert{overflow}
   *  A divide instruction attempted to divide by 0
  
 *  It's better to \emph{detect} the error and \emph{do something
  about it}
 *  This makes software \alert{robust}
 *  If the program just keeps going, it's likely to produce wrong
  results and it won't tell
 *  Two approaches for catching errors (use one or the other):
  
   *  Explicit error checking
   *  Interrupts
  



### Explicit error checking


 *  Most computers have a \emph{condition code register} with a bit
  indicating each kind of error
 *  Sigma16 uses R15, and a bit in R15 indicates whether overflow
  occurred
 *  Every time you do an add (or other arithmetic instruction), that
  bit is set to 0 if it was ok, and 1 if there was overflow
 *  You can check for this with a conditional jump, and then take
  appropriate action
 *  Of course, you have to decide what the appropriate action is!


~~~~
   add      R2,R5,R4     ; x := a + b
   jumpovfl TooBig[R0]   ; if overflow then goto TooBig
~~~~


### Problems with explicit error checking


 *  You have to put in the jumpovfl \emph{after every arithmetic
  instruction}
 *  This makes the program considerably longer
 *  It's also inefficient: those conditional jumps take time
 *  It is ``fragile'': if you forget the jumpovfl \emph{even once}
  in a big program, that program can malfunction


### A better approach: interrupts!


 *  Most computers (including Sigma16) can also perform an interrupt
  if an overflow (or other error) occurs
 *  The digital circuit checks for overflow (or other error)
  \emph{after every arithmetic operation}
 *  If the error occurred, the circuit performs an interrupt
 *  The OS then decides what to do
 *  User program can tell the OS in advance ``in case of overflow,
  don't kill me, but jump to this address: TooBig''
 *  There is a special trap code for making this request
 *  In some programming languages, this is called ``setting an
  exception handler'' or ``catching exceptions''
 *  There is a special control register with a bit that specifies
  whether overflow should trigger an interrupt



### Why are interrupts better than explicit checking?


 *  Interrupts guarantee that \emph{every} operation is checked
 *  It is faster: the circuit can do this checking with essentially
  zero overhead
 *  It is easier: the programmer doesn't have to worry about it
 *  The program is shorter: don't need a jump after every arithmetic
  instruction



### Concurrent processes

### Interrupts and processes


 *  One of the central features provided by an operating system is
  \emph{concurrent processes}
  
   *  A process is a running program
   *  Think of a program as a document: it's just sitting there
   *  A process is all the action that happens when a program is
    executed: it has its variables, the variables change over time,
    Input/Output happens, $\ldots$
   *  Several different processes may be running on the same program
    (e.g. multiple tabs on a web browser)
   *  Each process has its own variables
  
   *  Processes are implemented using interrupts
   *  The idea: the OS gives a user program a \emph{time slice}
   *  The user is interrupted, and the OS can then run a different
    program


### Waiting for I/O = wasted time


 *  Motivation for processes comes from I/O
 *  The problem:
  
   *  Instructions execute quickly --- typically about 0.3ns (about
    $3 \times 10^{9}$ per second)
   *  Input/output is much slower, especially if mechanical devices
    are involved
   *  An I/O operation runs slower than an instruction by a factor
    ranging from $10^{4}$ to $10^{8}$
   *  For comparison, a supersonic jet fighter is only $10^{3}$
    times faster than a turtle.
  
 *  If a program does \emph{compute} $\cdots$ \emph{print} $\cdots$
  \emph{compute} $\cdots$ it is likely to spend a lot of time waiting
  for the I/O


### A process must sometimes wait

\includegraphics[scale=1.0]{../figures/png/process-running-waiting.png}


### Don't wait --- switch to another process

\includegraphics[scale=1.0]{../figures/png/multiple-processes.png}


### Don't wait --- switching to another program


 *  When a program needs to perform I/O, it
  
   *  Requests the operating system to do the I/O
   *  The OS \emph{starts} the I/O but doesn't wait for it to finish
   *  The OS then allows a \emph{different program} to run for a while
   *  Eventually, when the I/O operation finishes, the OS allows the
    original program to resume
  
 *  This leads to an operating system running a large number of
  separate programs
 *  Each running program is called a \alert{process}


### Concurrent processes


 *  A \emph{process} is a running program
 *  At an instant of time, the computer is physically executing just
  one instruction (which belongs to one process)
 *  From time to time (around 100 or more times per second), the
  system will transfer control from one process to another one ---
  this is called a \alert{process break}
 *  Time scale:
  
   *  At the scale of a nanosecond ($10^{-9}$ second) the computer
    is executing just one instruction belonging to a process; all
    other processes are doing nothing
   *  At the scale of human perception ($10^{-2}$ second) it appears
    that \emph{all} the processes are making smooth processes
  
 *  A motion picture is just a sequence of still photographs but
  displaying them rapidly gives the impression of continuous motion


### Operating system kernel


 *  A process does not transfer control to another process
 *  How could it?  When you write a program, you don't know what
  other programs will be running when this one is!
 *  A process break means
  
   *  Running process jumps to the operating system kernel
   *  The kernel is the innermost, core, central part of the OS
   *  The kernel has a table of all the processes
   *  (On Windows: right-click the toolbar, launch the Task Manager,
    click Processes tab)
   *  The kernel chooses another process to run and jumps to it
  


### Events that can trigger an interrupt


 *  There is a timer that ``bings'' periodically --- each time it
  goes off it generates an interrupt
 *  When an Input/Output device has competed a read or write, it
  generates an interrupt



### Interrupts and preemptive scheduling


 *  When the operating system jumps to a user process, it sets a
  \alert{timer} which will ``go off'' after a set amount of time
  (e.g. 1ms --- $10^{-3}$ second)
 *  When does a running process jump to the operating system?
  
   *  When the timer goes off
   *  When the process makes an I/O request
  
 *  This guarantees that the process won't run forever and
  \emph{freeze the system} even if it goes into an infinite loop


### The Scheduler


 *  The core of an operating system is the scheduler
 *  It maintains a list of all the processes
 *  When an interrupt occurs:
  
   *  The process that was running stops executing instructions: it
    has been interrupted
   *  The OS takes any necessary action (e.g. service the I/O
    device)
   *  Then the OS jumps to the scheduler
   *  The scheduler chooses a different process to run
   *  It sets the timer and jumps to that process
  


### Mouse


 *  The mouse isn't connected to the cursor on the screen!
 *  When you move the mouse, it generates an interrupt
 *  The OS reads the mouse movement
 *  Then it calculates where the cursor should be and redraws it
 *  This happens many times per second, giving the illusion of
  smooth movement



  %\includegraphics[scale=0.5]{../figurespng/two-mice.png}
  \vspace{-2cm}
  \includegraphics[scale=0.3]{../figures/pdf/two-mice-heart.pdf}


### How interrupts are implemented

### How interrupts are implemented

  
   *  Interrupts cannot be implemented in software!
   *  The processor (the CPU) repeatedly goes through a sequence of
    steps to execute instructions
   *  This is the \alert{control algorithm} and it's performed by a
    digital circuit in the processor (the \alert{control circuit})
   *  Interrupts are implemented by the control circuit
  

### Control


 *  We have seen the RTM, which executes operations like reg[d] :=
  reg[a] + reg[b]
 *  This is the core of a processor!
 *  We have seen the control registers: pc, ir, adr
 *  The processor uses these to keep track of what it is doing


### The Control Algorithm


 *  The behaviour of the entire processor is defined by a
  \emph{control algorithm}
 *  We can describe this using a special notation (which looks like
  a simple programming language, but it is not a program)
  
   *  Notations
   *  The control algorithm
  
 *  We can implement the control algorithm using flip flops and
  logic gates



### Registers


 *  pc (program counter) contains address of the next instruction
 *  ir (instruction register) contains the current instruction (or
  first word of an RX instruction)
 *  adr (address register) holds the effective address for RX
  instructions
 *  reg[a] (register file) contains 16 registers for use by user
  program



### Notation


 *  pc, ir, adr --- contents of these 16-bit registers
 *  ir\_op,ir\_d, ir\_a, ir\_d --- 4-bit fields in the ir
 *  reg[x] --- the register in the register file with address x
 *  mem[x] --- the memory location with address x



### Infinite loop


 *  In hardware, we \emph{need} infinite loops
 *  The computer should never stop executing instructions!


~~~~
repeat forever
  action
  action
  ...
  action
~~~~


### Case dispatch


 *  We often have an \emph{operation code} --- a binary number, with
  $k$ bits (e.g. 4 bits)
 *  There are $2^{k}$ alternative actions to take, depending on the
  value of the code


~~~~
case opcode
  0: action
  1: action
  ...
  15: action
~~~~


### Control algorithm

~~~~
repeat forever
  if interrupt_request
    then savepc := pc
         pc := 0  ; address of interrupt handler in OS
    else ir := mem[pc],  pc := pc + 1            ; fetch instruction
         case ir_op
         0: reg[ir_d] := reg[ir_a] + reg[ir_b]   ; add
         1: reg[ir_d] := reg[ir_a] - reg[ir_b]   ; sub
         2: reg[ir_d] := reg[ir_a] * reg[ir_b]   ; mul
         ... more RRR instructions are similar
         ...
         15: adr := mem[pc], pc := pc + 1        ; displacement
             adr := adr + reg[ir_a]              ; effective address
             case ir_b
               0: reg[ir_d] := adr               ; lea
               1: reg[ir_d] := mem[adr]          ; load
               2: mem[adr] := reg[ir_d]          ; store
               3: pc := adr                      ; jump
               ... more RX instructions are similar
               ...
~~~~


## Languages and systems

### Advice on software engineering

What should a software engineer study if they want to learn how
to write efficient code?

Hyde, R. (2009). The Fallacy of Premature
Optimization. \emph{Ubiquity}, Association for Computing Machinery,
February 2009.  \weblink{https://doi.org/10.1145/1569886.1513451}

### What should a software engineer study?

Hyde says (quotation):

* The first subject to master is \alert{machine organization
  (elementary computer architecture)}. Because all real programs
  execute on real machines, you need to understand how real machines
  operate if you want to write efficient code for those machines.

* The second subject to study is \alert{assembly language
  programming}. Though few programmers use assembly language for
  application development, assembly language knowledge is critical if
  you want to make the connection between a high-level language and
  the low-level CPU. $\ldots$ It doesn't really matter which assembly
  language you learn nor does it matter which CPU's instruction set
  you study. What you really need to learn are the basic operational
  costs of computation.

* The third important subject a software engineer should study is
  basic compiler construction, to learn \alert{how compilers translate
  high-level language statements into machine code}.

### Syntax, semantics, compilation

  
   *  Primary aspects of a programming language
    
     *  Syntax is the \emph{form} of a program
     *  Semantics is the \emph{meaning} of the program
     *  Compilation (or interpretation) is how the language is
      implemented so it can run on a computer
    
  

#### Syntax --- easier but less important


 *  Syntax is the \alert{form} of a program
 *  Did you spell the keywords correctly? Is the punctuation right?
 *  Syntax is easy
  
   *  The rules are clear cut
   *  If in doubt, just look it up
   *  Example: various languages have different names for the same
    thing: \emph{Bool},\emph{Boolean}, \emph{Logical}.  These
    differences are superficial
  


### Syntax errors


 *  Compilers insist that the syntax is right
  
   *  In English, if you spell a word wrong or have a missing comma
    you'll (probably) still be understood
    
     *  But --- see \alert{Eats, Shoots and Leaves} by Lynn Truss.
      Note the comma!  What was meant was {\bluetext``The panda eats
        shoots and leaves''}, not {\bluetext ``The panda eats, shoots,
        and leaves''}
    
  
 *  Can't a compiler be equally forgiving?
  
   *  There were experiments with compilers that guess what the
    programmer meant
   *  It was a disaster: the compiler nearly always guessed
    correctly$\ldots$
   *  But occasionally it would guess wrong
    
     *  \alert{How can you debug a program when the compiler didn't
        translate what you wrote, but something different?}
     *  You have to debug code that is not in the file!  And you
      cannot see it!
     *  You \alert{want} the compiler to insist that the syntax is
      absolutely correct
    
  


### Example of syntax: operator precedence


 *  Expressions can contain many operations, but the computer can do
  only one operation at a time
 *  We can make the operations explicit by using parentheses around
  each operation
 *  You don't have to write the parentheses, but the compiler needs
  to know where they go!
 *  a + b + c is parsed as (a + b) + c
 *  a + b * c is parsed as a + (b * c)


### Deeper example of syntax: ambiguity


 *  A language is \emph{ambigous} if a sentence in the language can
  have two different meanings
 *  English is full of ambiguity
 *  Programming languages are designed to avoid ambiguity, most of
  the time
 *  If ambiguity is possible, the compile needs to know how to
  resolve it, \emph{and so does the programmer}


### Ambiguity in if-then-else

This is ambiguous: \alert{which if does the ``else S2'' belong to?}
~~~~
if b1 then if b2 then S1 else S2
~~~~

There are two interpretations, and they lead to different results

~~~~
if b1 then { if b2 then S1 else S2 }
  b1 = true,  b2 = true     S1
  b1 = true,  b2 = false    S2
  b1 = false, b2 = true
  b1 = false, b2 = false

if b1 then { if b2 then S1 } else S2
  b1 = true,  b2 = true     S1
  b1 = true,  b2 = false
  b1 = false, b2 = true     S2
  b1 = false, b2 = false    S2
~~~~


### How does Python prevent ambiguity?


 *  The structure of the program is determined by
  \alert{indentation}
  
   *  This means it is essential to indent the program properly
   *  It makes the program structure highly visible to the programmer
  
 *  Some languages use braces to indicate structure (e.g. C, Java)
  
   *  The compiler ignores the indentation, and uses the braces
   *  Programmers tend to focus on the indentation and may overlook
    the braces
   *  This is more error-prone
  


### goto


 *  Usually programs are more readable and more reliable if written
  with while loops, if-then-elif-elif-else, for loops, and similar
  higher level constructs
 *  Programs that jump around randomly with goto statements are
  harder to understand, and likely to contain bugs
 *  This gave the goto statement a bad reputation
 *  But
  
   *  The goto statement is simply a jump instruction, and it is
    \emph{essential} for use at low level
   *  There are some circumstances where goto may be the best
    solution, but these are rare
  





  



### Implicit goto statements: break and continue

* Some programming languages provide restricted forms of goto: break
  and continue
  
* These are goto statements without a label, but with a predefined
  destination they go to

* Advantage --- you may recognise the pattern being used by a
  programmer

* Disadvantage --- you need to know for sure to where your goto goes

* C and its descendants have break, as does Python

* Break is a goto that goes to the end of the \alert{innermost} loop
  it's in

* In Python  you can have an else clause to execute when a for or
  while loop finishes, but this is skipped if you terminate the loop
  with break

* What if you want to break out of several loops?.  There is no good
  way to do this in Python. It's best to restructure your program, to
  avoid break
  
Break is a goto that doesn't say where it goes to.

~~~~
for i in range (1, 5):
    print (i)
    for j in range (20, 22):
        print (j)
        if i == 3:
            break
        for k in range (40, 43):
            print (k)
            if k == 41:
                break
            print (k)
~~~~

The continue statement

* In C and Python, continue goes to the end of the current loop
  which then continues executing

* It's a way of staying in the loop but skipping the statements
  after the continue

* Warning! Several programming languages have a statement that is
  *spelled* continue --- but it does nothing and is equivalent to the
  pass statement in Python

Break and continue: translation to low level

~~~~
loop1
   if (i<n)=False then goto loop1done
   ...
   if ... then goto loop1      ; This is a continue statement
   ...
   if ... then goto loop1done  ; This is a break statement
   ...
   goto loop1
loop1done
~~~~

  
* continue goes to the top of the innermost loop containing the
  continue: it skips the rest of \emph{this iteration}

* break goes to right after the end of the innermost loop containing
  the break: it \emph{exits this loop}
  
### Semantics
 
* Semantics means the **meaning**

* The semantics of a language is \emph{what a program in
  the language means}

* The semantics of a program is \emph{the meaning of the
  program}. Given its inputs, what are its outputs?

How is the semantics of a language defined?

Using natural language to describe it.  This may be a vague
description in English of what each construct does, or a carefully
written description in English, written to be as precise as possible
  
Using mathematics or program transformation
  
* Denotational semantics: a precise mathematical specification. Given
  a program, it gives a mathematical function from program inputs to
  outputs

* Operational semantics: gives a sequence of reduction rules that give
  a precise model of the program's execution

* Transformational semantics: a translation from a program into a
  simpler language, where the semantics is assumed to be clear

Why do the translation from high to low level?

* This is the semantics of the high level constructs

* It explains precisely what the high level means

* It shows what the compiler will do with the program (compilers
  do intermediate level translations like this; many compilers use
  several intermedate steps)

* It makes explicit the execution time of the construct

* The low level is very close to assembly language

* It's easier to go from high level to assembly language in two small
  steps, rather than one giant leap

Watch out for loose explanations

* Here's a quotation from
  https://docs.python.org/3.7/tutorial/introduction.html.
    
* ``The while loop executes as long as the condition (here:
   a < 10) remains true
    
That is wrong!

It *says* that if a is changed in the middle of the
loop, making it less than 10, the loop will stop executing.
But the computer does not continuously monitor a<10 and
break out of the loop as soon as it becomes false.

To see what happens, look at the compilation pattern: the translation
to low level ``goto'' form.  The while loop checks the boolean a<10 at
the top of the loop; if false it exits the loop, and if true it
executes the entire body of the loop even if the boolean became false
in the middle

### Semantics of while

**while b do S** is translated to

~~~~
L1: if not b then goto L2
    S
    goto L1
L2:
~~~~

If you understand the meaning of :=, goto, if b then goto, you can
understand the meaning of every high level construct.  Note that b is
an arbitrary boolean expression; S is an arbitrary statement; L1 and
L2 are *fresh labels: they can't be used anywhere else

### Lists, +, +=, iterators, and for loops

     *  How are lists represented?
     *  What do the + and += operators do?
     *  What is an iterator, and how does it work?
     *  What does a for loop do?  How does it compare with a while
      loop?


### Let's do some matrix calculations

Set up a list of 5 elements

~~~~
coords = [[0,0]] * 5
print ('coords = ', coords)  # [[0,0], [0,0], [0,0], [0,0], [0,0]]
~~~~

The result is

~~~~
coords =  [[0, 0], [0, 0], [0, 0], [0, 0], [0, 0]]
~~~~

Looks good!  Now set some sub-elements:

~~~~
coords[3][1] = 7
coords[4][0] = 5
~~~~

{\redtext
Result may not be what we expected, or wanted:
~~~~
coords[2] =  [5, 7]
coords =  [[5, 7], [5, 7], [5, 7], [5, 7], [5, 7]]
~~~~
}

What's going on?



### What does += mean in Python?

  
   *  Google search for \alert{+= Python}
    
     *  ``\alert{The expression a += b is shorthand for a = a + b},
      where a and b can be numbers, or strings, or tuples, or lists
      (but both must be of the same type).''
    
   *  Stack overflow search for \alert{+= Python}
    
     *  ``+= adds another value with the variable's value and assigns
      the new value to the variable.''
     *  ``It adds the right operand to the left.  x += 2 means x = x
      + 2.  It can also add elements to a list''
    
   *  \alert{These statements are all wrong}
  


### Is a += b shorthand for a = a + b?

Try \alert{a = a + [3, 4]}
~~~~
print ('Defining a = a + [3, 4]')
a = [1, 2]
b = a
print ('a = ', a, '  b = ', b)
a = a + [3, 4]
print ('a = ', a, '  b = ', b)
~~~~

The results: {\bluetext a =  [1, 2, 3, 4]   b =  [1, 2]}
\vspace{1em}
Now try \alert{a += [3, 4]}
~~~~
print ('Defining a += [3, 4]')
a = [1, 2]
b = a
print ('a = ', a, '  b = ', b)
a += [3, 4]
print ('a = ', a, '  b = ', b)
~~~~

The results: {\bluetext a = [1, 2, 3, 4],  b = [1, 2, 3, 4]}

\alert{So a += b is \emph{not} shorthand for a = a + b}
  

### How do we figure out problems like this?

  
   *  Need to understand all the fundamental concepts of the
    language
   *  It's best to study \alert{authoritative} source
    \alert{systematically}
   *  Develop a clear \alert{model} for what the language constructs
    really mean
   *  And here's some advice: from Stack Overflow
    
     *  ``it's a basic operator for python (and many other languages
      too), you should start with google, if you never read any python
      references. ``
    
   *  This may be ok if you already understand all the foundations
    and just want to look up a detail, but otherwise \emph{this is
      poor advice}
   *  Let's look in more detail at lists$\ldots$
  


#### Are two nodes with the same value identical?

### Are two nodes with the same value identical?}

  Define b = a \alert{--- this is pointer assignment}
  
~~~~
a = [1, 2, 3, 4]
b = a
print ('(1)  a = ', a, ' b = ', b)
~~~~

  Both a and b have the value [1, 2, 3, 4], but are the actual nodes
  in their representations distinct, or shared?  \vspace{1em}
  
  Try modifying an element of a and see if it changes b, and vice
  versa

~~~~
a[1] = 100
b[3] = 300
print (' a = ', a, ' b = ', b)

~~~~

  Run this, and you'll see that changing an element of either a or b
  also changes the corresponding element of the other.  The values of
  a and b are simply pointers, and \alert{they point to the same
    node}.

#### Low level list manipulation

### Low level list manipulation

  
   *  You can assign pointers: if a is a list, b = a makes b point
    to the same node a points to
~~~~
; b = a,   where a is a list
   load   R1,a[R0]    ; R1 = a
   store  R1,b[R0]    ; b := a
~~~~
   *  You can write expressions that create a new list, but don't
    modify any existing lists: \alert{a = a + b}
   *  You can modify an existing list structure
    
     *  a.copy()
     *  a.append(b)
     *  a.extend(b)
    
   *  All of these are implemented with while loops that traverse a
  

### b = a.copy()

Traverse a and make a new node for each node in a; link the new nodes
together to form the result.  The nodes in b have the same values as
the nodes in a, but they are distinct nodes

~~~~
   p := a
   b := nil
   while p /= nil do
       nn := newnode()
       *nn.value := *p.value
       *nn.next := nil
       *b.next := nn
       p := *p.next
~~~~

  (It's a little more complicated, you have to remember the beginning
  of b --- using a header node is helpful.)
  Now if you modify one of the lists (a, b) the other list is not affected
  
### List is represented as nodes


  \par\includegraphics[scale=0.5]{../figures/draw.io/list123.pdf}


### Appending to a list


  \par\includegraphics[scale=0.5]{../figures/draw.io/list123-append456.pdf}



 *  a = [1, 2, 3]
 *  b = [4, 5, 6]
 *  a.append(b)
 *  A new node is created: call newnode()
 *  The newnode has value=b and next=nil
 *  The last node in a had next=nil; that is changed to point to the
  new node
 *  Result: \alert{a =  [1, 2, 3, [4, 5, 6]]}



### Extending a list


  \par\includegraphics[scale=0.5]{../figures/draw.io/list123-extend456.pdf}



 *  a = [1, 2, 3]
 *  b = [4, 5, 6]
 *  a.extend(b)  
 *  The last node in a had next=nil
 *  That nil pointer was changed to point to b
 *  Result: \alert{a =  [1, 2, 3, 4, 5, 6]}


#### The + operator

  
   *  You can concatenate two lists a and b with \alert{a + b}
   *  The + operator does not modify either a or b
   *  It creates a new list, with copies of the nodes in a, followed
    by b
   *  Since it doesn't modify existing containers, you can use + on
    both
    
     *  mutable containers, e.g. lists
     *  immutable containers, e.g. string
    
  
  
#### The += operator

### The + operator}

   *  You can concatenate two lists a and b with \alert{a += b}
   *  The += operator does not copy a, it \alert{modifies} a by
    extending it
   *  Since += modifies existing containers, you cannot use += on
    immutable containers, e.g. strings
  

### List with + and +=

  
   *  You can write a + b where a and b are lists; if either is a
    list, both must be lists
   *  You can write a += b where
    
     *  a is a list
     *  b is either a list or an iterator
    
  


### Iterators

  
   *  A container class can have an \alert{iterator}
   *  There are built-in default iterators for lists and numbers
   *  You can define your own class and iterator
    
     *  You define a method called \texttt{\_\_iter\_\_} which
      creates a new iterator and initializes it
     *  An iterator provides a method called \texttt{\_\_next\_\_}
      and you can invoke this with \alert{xyz.next()}
     *  The implementation of next
    
  
  
### Defining an iterator for even numbers

~~~~
class EvensClass:
  def __iter__(self):
    self.state = 0
    return self

  def __next__(self):
    x = self.state
    self.state += 2
    return x

Evens = EvensClass()
EvenIterator = iter(Evens)
~~~~


### Using the iterator

~~~~
for i in EvenIterator:
    print (i)
    if i > 15:  # Try commenting this out: the iterator is unbounded
        break

print ('The for loop has finished, now call next directly')
print(next(EvenIterator))
~~~~

### Running the even iterator

~~~~
Defining iterator for even numbers
0
2
4
6
8
10
12
14
16
The for loop has finished, now call next directly
18
~~~~

### Extending a list with an iterator

You can use += to extend a list with an iterator
~~~~
a = [1, 2, 3, 4]
a += range(20,25)
print ('a = ', a)
~~~~

But you can't use + on a list and an iterator:
~~~~
a = [1, 2, 3, 4]b = [1, 2, 3, 4]
# b = b + range(20,25)  # This line gives syntax error
~~~~


#### for loops

### for loops in Algol and descendants

~~~~
sum := 0
for i := 0 to n-1 do sum := sum+i
print (sum)
~~~~

The translation needs 4 machine language instructions for the loop
control

~~~~
           sum := 0
           i := 0
avacodo    if i>= n then goto avacododone
           S
           i := i + 1
           goto avacado
avacododone
           print (sum)
~~~~


### for loops in Python


 *  A Python for loop traverses a sequence defined by an iterator
 *  If the iterator terminates, so does the for loop
 *  The iterator could also go on forever
 *  The iterator could give a sequence of numbers, or it could
  traverse a list, or some other container
 *  This is convenient but it hides what's actually going on


#### Revisiting the mysteries}

Focus on:
  
   *  What low level operations are being used: pointer assignment?
    copy?
   *  Know when you have two pointers to the same node
   *  Know when newnode has been called
  
~~~~
a = [1, 2]
b = a
a = a + [3, 4]
print ('a = ', a, '  b = ', b)
~~~~

Here, the results are final values:  a =  [1, 2, 3, 4]   b =  [1, 2]

~~~~
a = [1, 2]
b = a
a += [3, 4]
print ('a = ', a, '  b = ', b)
~~~~

Now the results are: a = [1, 2, 3, 4], b = [1, 2, 3, 4]

In the second example, \alert{a += b} modified the structure of a.
Since b points to the same node as a, this change also affects b.

### Compilers


 *  We have been writing algorithms in high level language notation
  and then translating it manually to assembly language
 *  A \alert{compiler} is a software application that performs this
  translation automatically
 *  A \alert{programming language} is a precisely defined high level
  language
 *  The compiler makes programming easier by allowing you to think
  about your algorithm more abstractly, without worrying about all the
  details of the machine


### Source and object


 *  The original high level language program is called the
  \alert{source code} --- it's what the programmer writes
 *  The final machine language program which the compiler produces
  is called the \alert{object code} --- it's what the machine executes



### {Compilation

 *  A compiler translates statements in a high level language into
assembly language

 *  In developing an assembly language program, it's best to begin
by writing high level pseudo-code (this becomes a comment) and then
translate it

 *  This approach helps keep the program readable, and reduces the
liklihood of getting confused

 *  Each kind of high level statement corresponds to a standard
  pattern in assembly language.  \emph{Follow these patterns!}

### How a compiler works


 *  Your high level source program is just a character string ---
  the computer cannot execute it directly
 *  The compiler reads the source program, checks its syntax, and
  analyses its structure
 *  Then it checks the types of all the variables and procedures
 *  Most advanced compilers translate the program to an intermediate
  ``goto form'', just as we are doing
 *  The program is finally translated to assembly language:
  \alert{``code generation''}
 *  The assembler translates the assembly language to machine
  language (the Sigma16 application contains an assembler)


### Major tasks in compilation


 *  \alert{Parsing} --- check the source program for correct syntax
  and work out the program structure (similar to ``diagramming
  sentences'' in English grammar)
 *  \alert{Type checking} --- work out the data type of each
  variable (integer, character, etc.) --- then
  
   *  Make sure the variables are used consistently
   *  Generate the right instructions for that data type
  
 *  \alert{Optimisation} --- analyse the program to find
  opportunities to rearrange the object code to make it faster
 *  \alert{Code generation} --- produce the actual machine
  instructions



### Parsing


 *  The \alert{syntax} of a language is its set of grammar rules
 *  If the source program contains a \alert{syntax error}, the
  compiler will not understand what you mean
  
   *  {\color{blue}if x$<$y then a = 1} --- ok, can be translated
   *  \alert{if x?y than b = 2} --- syntax errors! what does it
    mean?
  
 *  If there is a syntax error, \alert{you do not want the compiler
    to guess what the meaning is} --- that leads to unreliable
  software


### Types


 *  Type checking is one of the most important tasks the compiler
  performs
 *  There are many data types supported by a computer
  
   *  Binary integer
   *  Two's complement integer
   *  Floating point
   *  Character
   *  Instruction
  
 *  \alert{The computer hardware works on words, and the machine
    does not know what data type a word is.}
 *  It's essential to use the right instruction, according to the
  data type



### Integer and floating point


 *  An integer is a whole number:  $23$, $-47$
 *  A floating point number may have a fraction and exponent: $7.43
  \times 10^{28}$
 *  We have seen how an integer is represented: two's complement
 *  Floating point representation is different from two's complement
 *  Most computers have separate instructions for arithmetic on
  integer and floating point
  
   *  \alert{add}  R1,R2,R3  --- integer addition
   *  \alert{addf} R1,R2,R3  --- floating point addition
  
 *  \alert{The machine doesn't know what the bits in the registers
    mean} --- you must use the right instruction according to the data
  type



### Typechecking


 *  The compiler checks that each variable in the source program is
  used correctly
  
   *  If you add a number to a string, it's a type error
  
 *  Then it generates the correct instructions for the type
  
   *  For integer variables it uses \alert{add}
   *  For floating point variables it uses \alert{addf}
   *  \emph{These are different instructions!}
  
 *  This eliminates one of the commonest kinds of error in software



### Write programs at a high level

Every rule has some rare exceptions, but almost always these are good
principles:


 *  Write conditionals using if-then or if-then-else
 *  Write loops using the most appropriate construct
  
   *  Often, a while loop is best
   *  To traverse an array, a for loop is best
  
 *  Avoid goto statements (and if your language has break
  statements, avoid those)
 *  Use a straightforward style that's easy to read


\alert{Include the high level code in your program, as full line
  comments}

### Use patterns to translate to low level


 *  Low level algorithm contains just
  
   *   Assignment statements
   *  goto label
   *  if b then goto label
  
 *  Each high level construct is translated to low level using a
  fixed pattern


\alert{Include the low level code in your program, as full line
  comments --- after the high level code}


### Translate low level to assembly language


 *  Each low level statement should be a full line comment, followed
  by the instructions needed to implement that statement
  
   *  \alert{x := y + z} $\quad\Rightarrow\quad$\\
~~~~
; x := y + z
    load    R1,y[R0]    ; R1 := y
    load    R2,z[R0]    ; R2 := z
    add     R1,R1,R2    ; R1 := y + z
    store   R1,x[R0]    ; x := y + z
~~~~
   *  \alert{goto phase2} $\quad\Rightarrow\quad$\\
~~~~
; goto phase2
    jump    phase2[R0]  ; goto phase2
~~~~
   *  \alert{if $x<y$ then goto phase3} $\quad\Rightarrow\quad$
~~~~
; if x<y then goto phase3
    load    R1,x[R0]    ; R1 := x
    load    R2,y[R0]    ; R2 := y
    cmp     R1,R2       ; compare x, y
    jumpge  phase3[R0]  ; if x<y then goto phase3
~~~~
  



### Practical programming tip

\begin{enumerate}
 *  Write the high level algorithm
 *  \alert{Hand execute it to be sure it's correct}
 *  Type it into your file, as full line comments
 *  Translate it to low level form
 *  \alert{Hand execute it to be sure it's correct}
 *  Type it into your file, as full line comments
 *  Duplicate the low level code (copy and paste)
 *  Now go through the second copy of the low level code, and insert
  the assembly language after each statement
 *  \alert{Hand execute it to be sure it's correct}
 *  Run it on the emulator, in \alert{single step} mode, and check
  that the program does \alert{what you predicted in your hand
    execution}
\end{enumerate}

## Architecture

### Instructions

  
   *  You need to know what the basic instructions do and how to use
    them
    
     *  Memory and addresses: \alert{load store lea}
     *  Arithmetic: \alert{add sub mul div}
     *  Comparison: \alert{cmplt cmpeq cmpgt}
     *  Jumps: \alert{jump jumpt jumpf jal}
     *  System: \alert{trap R0,R0,R0} (just for halting, not for I/O)
    
   *  Instruction representation
    
     *  You should understand the \alert{concept}
     *  But you do not need to remember the details: the exam does
      not ask you to convert any instructions from assembly language
      to machine language
    
  


### Addresses and data structures
  
   *  Effective addresses: sum of displacement and register
   *  Accessing an element of an array
   *  Accessing an element of a record
   *  Pointers: the (*p) and (\&x) operators
   *  Linked list traversal
  

#### Compilation patterns

### High and low level programming constructs

  
   *  High level
    
     *  if then, if then else, case, while loops, for loops
    
   *  Low level
    
     *  assignment, goto, if then goto
    
  

### High and low level programming constructs

  
   *  The low level statements correspond to machine instructions
   *  Assignment statement
    
     *  Load the operands in the expression into registers
     *  Do the arithmetic
     *  Store the result into a variable in memory
     *  You can also keep variables in registers over a larger block
      of code
    
   *  goto label
    
     *  jump label[R0]
    
   *  if b then goto label
    
     *  Evaluate the boolean expression, put it in a register
     *  conditional jump: either jumpt or jumpf
    
  


### Compliation patterns

  
   *  Systematic pattern for translating each high level construct
    into low level statements
   *  Most high level constructs contain a Boolean expression
   *  Translate this into goto and if-then-goto statements that
    cause the right blocks of instructions to be executed
   *  Check that the translation is correct by hand executing with
    both values of the Boolean: True, False
   *  Case statements use a jump table
  

### Programming fundamentals

#### How do you learn programming


 *  The approach to learning programming has changed over the years
 *  First programming languages
  
   *  Learn the statements and what they do
   *  Statements are low level
  
 *  Large scale software
  
   *  Software becoming complex
   *  goto considered harmful
  
 *  Problem solving
  
   *  Programming languages have complex statements, control
    structures and data structures
   *  Teach ``problem solving''
   *  Use vague English and some examples to explain what the
    language constructs do
  



### Connections with other subjects

Similar debates occur in many subjects


 *  Natural language
  
   *  A popular idea: learning grammar impedes creativity
   *  Alternative view: knowing grammar \alert{enables} the ability
    to express your ideas
  
 *  Arts and crafts and music
  
   *  Should you learn how to use the tools of the trade?
   *  Or just pick up how to use them while ``expressing'' yourself?
  


### Basic list operations: extend and append

~~~~[commandchars=\\\{\}]
a = [1, 2]
a.extend([3,4])
b = [1, 2]
b.append([3,4])

\redtext{a =  [1, 2, 3, 4]}
\redtext{b =  [1, 2, [3, 4]]}
~~~~

### Effect of extend and append on data structures


  \includegraphics[scale=0.4]
  {../figures/png/survey-diagrams/list-print.png}



 *  Need to be able to read a ``box and arrow'' diagram and work out
  what the lists are
 *  a = [1, 2, 3, 4]
 *  b = [1, 2, [3, 4]]




%-----------------------------------------------------------------------------

### Make some lists}


  \includegraphics[scale=0.4]
  {../figures/png/survey-diagrams/list-ops-test-abc.png}




%-----------------------------------------------------------------------------

  ### XX changed a, YY changed c.\\
    Which is append, which extend?}


  \includegraphics[scale=0.35]
  {../figures/png/survey-diagrams/list-ops-test-xx-yy.png}




%-----------------------------------------------------------------------------
[fragile]

### List manipulation: abc foobarbaz

a = [1, 2]
b = a
c = [3, 4]
a = a + c
c.append(5)

\redtext{a = [1, 2, 3, 4]}
\redtext{b = [1, 2]
  \qquad\textrm{\emph{\textbf{Almost half answered [1,2,3,4]}}}}
\redtext{c = [3, 4, 5]}

 *  \emph{\textbf{In a = a+c, the  nodes in a are not changed.  A new list is
  created and a is made to point to that}}
 *  b still points to the nodes that comprised the \emph{original}
  value of a
 *  Here the lists are mutable, but you could also use a = a + c on
  \emph{immutable} data (like strings) because the + operator
  \emph{does not change the data}, it just creates a \emph{new} value

### List manipulation abc: initial values


  \includegraphics[scale=0.4]
  {../figures/png/survey-diagrams/q1-listmanipulation-diagram-abc.png}


### List manipulation abc: after + and append


  \includegraphics[scale=0.37]
  {../figures/png/survey-diagrams/q1-listmanipulation-diagram-plus-append.png}



### List manipulation: def

~~~~[commandchars=\\\{\}]
d = [1, 2]
e = d
f = [3, 4]
d.append(f)
f.append(5)

\redtext{d =  [1, 2, [3, 4, 5]]}
\redtext{e =  [1, 2, [3, 4, 5]]
  \qquad\textrm{\emph{\textbf{Almost half answered [1,2]}}}}
\redtext{f =  [3, 4, 5]}
~~~~


 *  append modifies the data structure, it doesn't produce a new
  list
 *  After the appends, e and f still point to the same nodes they
  did before, but those nodes now point to lists with changed data
 *  append can only be used on a mutable value such as a list, but
  not on an immutavle value such as a string


### List manipulation: def initial values


  \includegraphics[scale=0.4]
  {../figures/png/survey-diagrams/q2-listmanipulation-diagram-def.png}



### List manipulation: def after appends


  \includegraphics[scale=0.4]
  {../figures/png/survey-diagrams/q2-listmanipulation-diagram-df-append.png}



### List manipulation: ghi

~~~~[commandchars=\\\{\}]
g = [1, 2]
h = g
i = [3, 4]
g += i
i.append(5)

\redtext{g =  [1, 2, 3, 4]}
\redtext{h =  [1, 2, 3, 4]
  \qquad\textrm{\emph{\textbf{A majority answered [1,2]}}}}
\redtext{i =  [3, 4, 5]}
~~~~


 *  g += i modifies the representation of g (unlike g+i).
 *  g (and h) still point to the same node, but the list is changed
 *  The list that i points to is copied into the end of g, extending
  it, but these nodes are copies of the nodes in i
 *  i.append(5) modifies the representation of i, but not g (or h)


### List manipulation: ghi initial values


  \includegraphics[scale=0.4]
  {../figures/png/survey-diagrams/q3-listmanipulation-diagram-ghi.png}



### List manipulation: ghi after += and append


  \includegraphics[scale=0.38]
  {../figures/png/survey-diagrams/q3-listmanipulation-diagram-plus-append.png}




### For loop

~~~~[commandchars=\\\{\}]
student_marks = [[9,7,8], [2,5,1], [7,4,3], [9,7,6]]
for student in student_marks:
    for mark in student:
        if mark <= 5:
            break
print ('mark = ', mark)

\redtext{mark =  6}
~~~~
  

### Data structure used in for loop


  \includegraphics[scale=0.23]
  {../figures/png/survey-diagrams/q2-studentmarks-diagram-marks.png}



### A flowchart


  \includegraphics[scale=0.18]
  {../figures/png/survey-diagrams/q2-studentmarks-diagram-flowchart.png}



### If statement

~~~~[commandchars=\\\{\}]
a = [2,3,5,7,11]
b = 3
c = 2
result = 0

if a[b] == 5:
    result += 2
elif a[c+c] > 3 or a[b-c] < 3:
    result +=3
elif a[c-c] >= 2 and a[b-b] <= 2:
    result += 7
elif a[b+c] >= 11:
    result += 11
print ('If statement result = ', result)

\redtext{If statement result =  3}
~~~~
  

### Data structure for the if statement


  \includegraphics[scale=0.4]
  {../figures/png/survey-diagrams/q5-ifstatement-diagram-data-structures.png}


### Flowchart for the if statement


  \includegraphics[scale=0.13]
  {../figures/png/survey-diagrams/q5-ifstatement-diagram-flowchart.png}


### While loop

\hbox{
\begin{minipage}{10em}
~~~~[commandchars=\\\{\}]
a = 5
b = 3
while a >= b:
    print ("foo")
    a = a + 2
    b = b + 4
    print ("bar")
    a = a + 1
    print ("hello")
print ("world")
~~~~
\end{minipage}
\hspace{2em}
\begin{minipage}{10em}
~~~~[commandchars=\\\{\}]
\redtext{foo}
\redtext{bar}
\redtext{hello}
\redtext{foo}
\redtext{bar}
\redtext{hello}
\redtext{foo}
\redtext{bar}
\redtext{hello}
\redtext{world}
~~~~
\end{minipage}
}

\vspace{1em}

 *  Many got this wrong, and there were several different errors
 *  Some treated $a \geq b$ as if it meant $a > b$
 *  Some thought the while loop terminates as soon as $a \geq b$
  becomes true \alert{--- the boolean condition is checked at the top
    of the loop, not continuously as the loop runs}  


### Question: Continue statement


~~~~[commandchars=\\\{\}]
i = 1
j = 5
while i < j:
    i = i + 1
    if i == 3:
        \redtext{continue}
    print ('i = ', i)
\redtext{i =  2}
\redtext{i =  4}
\redtext{i =  5}
~~~~
}
\vspace{1em}

 *  Answers were all over the place
 *  That's ok \emph{if you're aware that you don't know what
    continue does}
 *  The danger is when you aren't aware
 *  Continue is dangerous because it's a goto statement that doesn't
  say explicitly where to go --- you need to know how to figure it out
 *  Continue should be used rarely if at all, just like goto



### Question: Break statement


~~~~[commandchars=\\\{\}]
i = 1
j = 5
while i < j:
    i = i + 1
    if i == 3:
        \redtext{break}
    print ('i = ', i)

\redtext{i =  2}
~~~~
}
\vspace{1em}

 *  Similar to continue: lots of answers, mostly wrong
 *  Again, that's ok \emph{if you're aware that you don't know what
    break does}
 *  Break is dangerous because it's a goto statement that doesn't
  say explicitly where to go --- you need to know how to figure it out
 *  Break should be used rarely if at all, just like goto

  
### A note about the break statement

  
   *  In Python, you can only break out of the innermost loop that
    contains the break
   *  If you are in several nested loops, and you want to break out
    of several of them, \emph{there is no good way to do this in
      Python}
   *  Break and continue should be used rarely if at all
   *  Break and continue are just goto statements, spelled differently
   *  The disadvantages of goto statements apply to break and
    continue, only more so
   *  (Break is commonly used in the C language, because the switch
    (case) statement in C doesn't work the way you normally want.)
  

### Results

  
   *  Some of the primitive operations on lists are widely
    misunderstood
   *  There are some misconceptions on what operators + and += mean
    when applied to lists
    
     *  Textbooks, web pages, and Stack Overflow also get this wrong
      quite often
    
   *  Some misunderstandings about how nested conditionals work
   *  The break and continue statements are goto statements where
    you don't say \emph{where} to go, and this leads to confusion
   *  \alert{Be sure that you know the exact meaning of the language
      constructs you're using.}
  

### Example: WriteValChar

### A simple procedure: WriteValChar


 *  The procedure takes an argument---a character---and prints it.
 *  To use the procedure: Load a character into R1
 *  This is a word, and it should be a valid printing Unicode
  character
 *  Call the WriteValChar procedure, and it will print this
  character


### Calling WriteValChar


 *  Suppose R5 contains p, a pointer to a node
 *  We want to print the value in that node
 *  We are in a procedure where the stack frame has size 1 (the main
  program)

~~~~
   load   R1,0[R5]              ; R1 := (*p).value (to be written)
   store  R14,1[R14]            ; point to current frame
   lea    R14,1[R14]            ; push stack frame
   jal    R13,WriteValChar[R0]  ; write character
~~~~

 *  If the call is made from a procedure with 23 words in the stack
  frame, then the store and lea instructions would use 23[R14]



### Definition of WriteValChar (1)

~~~~
WriteValChar
; Write a character in R1
; Structure of stack frame
;    6[R14]  origin of next frame
;    5[R14]  local variable c
;    4[R14]  save R3
;    3[R14]  save R2
;    2[R14]  save R1
;    1[R14]  return address
;    0[R14]  pointer to previous stack frame

; Create stack frame
    store   R13,1[R14]           ; save return address
    store   R1,2[R14]            ; save R1
    store   R2,3[R14]            ; save R2
    store   R3,4[R14]            ; save R3
~~~~
}

### Definition of WriteValChar (2)


~~~~
    store   R1,5[R14]        ; local c := R1 = char to write
    lea     R1,2[R0]         ; trap write code
    lea     R2,5[R14]        ; address of character to write
    lea     R3,1[R0]         ; one char
    trap    R1,R2,R3         ; trap write

    load    R1,2[R14]        ; restore R1
    load    R2,3[R14]        ; restore R2
    load    R3,4[R14]        ; restore R3
    load    R13,1[R14]       ; restore return address
    load    R14,0[R14]       ; pop stack frame
    jump    0[R13]           ; return
~~~~
}


### Static variables in a data segment


 *  The variables are not placed in memory right after the
  instructions
 *  The program makes a request to the operating system to
  allocate a block of memory for data (the static data segment)
 *  A register is dedicated to point to the static data segment
 *  Variables are accessed using the static data segment register



### Implementing static data segment


~~~~
     ... set up registers to request a
          data block of n words...

     trap  R1,R2,R3
     ... R3 is set to the address of the data block
     add   R12,R0,R3    ; R12 = address of static data segment

   ...
     load  R2,n[R12]    ; R2 := n (access static variable)

     segment            ; subsequent labels will access segment
n    data
x    data
~~~~
}

### Practical tip - editing

 *  Recommend that you use Notepad to edit program; copy and paste
  it into Sigma16 editor pane
 *  The assembly language program should contain ASCII characters
  (the characters on your keyboard)
 *  Don't edit your program using Word (it will use lots of
  non-ASCII characters)
 *  If you click Run to run a program at full speed, you need to
  click Refresh (otherwise the memory display won't be correct).
  Remember: Run, then Refresh.
 *  But it's recommended that you execute the program by clicking
  Step repeatedly, and each instruction check that it did what you
  intend.

## Linking}

### Subroutines

There are many programming language variations

 *  Reentrant code
 *  Recursion
 *  Coroutines


Architecture support

 *  Ability to jump to a location and save return address 
 *  Ability to save all the state of caller
 *  Support for efficient stack access



### Calling conventions


 *  Software comes from many authors, written in many languages 
 *  To be able to work, standard calling conventions are needed
 *  These are specified by the OS, and used by OS libraries


### Linking


 *  Large programs are written in many modules, compiled separately
 *  They need to communicate via \emph{external names}: names of
  subroutines, names of global variables.
 *  Compiling a source module (in any language) produces an object
  module (in a fixed notation specified by OS)
 *  Object module contains machine code, definitions of exported
  global names, and relocation information
 *  \emph{Linker} (part of OS) combines multiple object modules,
  replaces global names by their values, and (maybe) relocates code


### Interrupts --- an unrequested jump


 *  A jump (to the operating system) initiated by an event other
  than a jump instruction
 *  Provide a mechanism for a program to request OS services
 *  Prevent user program from executing prohibited instructions
 *  Foundation for implementation by OS of concurrent processes
  and threads


### Implementing interrupts


 *  The control algorithm implements interrupts
 *  At the beginning of the instruction fetch/execute loop, the
  control checks the interrupt request (an input to the processor)
  
   *  If 0 (no interrupt request) the control algorithm executes
    the next instruction normally
   *  If 1, the control algorithm saves the PC and updates the PC
    with the address of the interrupt handler.  The next
    instruction to execute will then be the beginning of the
    interrupt handler.
  


### Basic interrupt control

Control algorithm without interrupts:


~~~~
repeat forever
   ir := mem[pc], pc++;
   case ir_op of
     0 -> -- add instruction
     ...
~~~~
}

With interrupt\_request signal


~~~~
repeat forever
   case interrupt_request of
     1 ->
       savepc := pc, pc := handler_address
     0 -> -- normal instruction execution follows...
        ir := mem[pc], pc++;
        case ir_op of
           0 -> -- add instruction
     ...
~~~~
}

### Saving the PC


 *  A principle of interrupts is that \emph{the operating system
    must be able to resume the process that was interrupted}
 *  The essence of an interrupt is to assign a new value to the
  PC
 *  Therefore the PC must be saved before its old value is
  destroyed
 *  The hardware has to do this!  It is absolutely impossible to
  do it in software


### Where to save the PC?

There are several possibilities, many of which have been tried in
real computers


 *  Have a special register whose sole purpose is to save the PC
  during an interrupt.  The interrupt handler needs to copy that
  register someplace more permanent before another interrupt can
  occur!
 *  Store the PC into memory, at a fixed address.  Again, the
  interrupt handler would need to copy it to a permanent place
  before another interrupt.
 *  Push the PC into a stack in memory.  This requires either a
  fixed location for the stack, or a register pointing to the
  stack.  What happens if there is a stack overflow?



### Saving state


 *  All the registers that hold state belonging to the user
  program need to be saved.
 *  Two approaches to this:
  
   *  Require the interrupt handler to do it (i.e. use software
    instructions to save the state).  Flexible, requires
    instructions that can access all of the state.
   *  Save all state in the control algorithm (OS doesn't need to
    do it).  Simple, reliable.
  



### Disabling interrupts


 *  If any state-saving is done by software, the interrupt
  handler is \emph{vulnerable} for a number of clock cycles while
  it is executing the instructions to save state.
 *  If another interrupt occurs while the handler is vulnerable,
  the state of the first interrupted process will be lost.  At best
  the process has been destroyed; if the process is an OS service,
  the entire system may crash.
 *  To prevent this, interrupts are \emph{disabled} while the
  handler is vulnerable.
 *  The datapath contains a flip flop \emph{enable}, which is
  normally 0.
 *  The control algorithm performs an interrupt if \emph{and2
    intreq enable}
 *  When an interrupt occurs, the control sets enable to 0
 *  There is an instruction that sets enable to 1


### Missing interrupts


 *  If a new interrupt request is made while interrupts are
  disabled, the interrupt simply has to wait
 *  Some external events are \emph{real time} --- there is an
  absolute deadline by which they must be processed
  
   *  In some systems, a disk generates an interrupt request when
    the data is about to pass under the read head.  If an interrupt
    is delayed too long, the disk must rotate fully before the data
    can be accessed
   *  The time of day clock is implemented by counting timer
    interrupts; if one of these is missed, the clock will run slow
  
 *  It is the responsibility of the interrupt handler (OS) to
  save state quickly and enable interrupts
 *  Some operating systems (e.g. Linux) organise interrupt
  handlers in two parts
  
   *  A short part that saves state, and saves an indication in
    memory of what work needs to be done
   *  A longer part that does the actual work, but this can be
    delayed until other interrupts are processed
  



### Architecture must support OS

### Some historical examples

As computers developed, some architectures had to be abandoned because
they didn't support new ideas in programming and operating systems.


 *  \emph{Self-modifying code.}\\ Some early machines needed
  self-modifying code to iterate over arrays.  When the disadvantages
  were recognised, and index registers with effective address
  calculation were invented, those machines became obsolete.
 *  \emph{Inability to save full state.}\\ Early machines had a
  library of I/O routines, rather than a full operating system, and
  lacked the ability to save \emph{all} state on an interrupt.  This
  made an OS scheduler impossible.
 *  \emph{Small address size.}\\ Memory was expensive until the
  1970s, and machines used short addresses to keep code tight.  The
  PDP 10 computer had 18-bit addresses, and went from dominant to dead
  when memory hardware grew.



### More recent examples


 *  \emph{Dynamic address translation.}  Virtual memory is
  implemented partly in hardware, and partly in software.  The
  relevant parts of the architecture and operating system must be
  designed together.
 *  \emph{Metrics for virtual memory.} To be efficient, virtual
  memory needs information about usage patterns (``least recently
  used'', etc.).  This information is needed by the operating system
  but must be gatherd by the hardware.
 *  \emph{Primitives for mutual exclusion.}  It is theoretically
  possible to implement mutex in software, but to be efficient it
  needs hardware support (test and set, etc.).
 *  \emph{System virtualisation.} Virtual machines require the
  ability to emulate key parts of code while running other parts at
  full speed, without losing control.  This places a number of
  constraints on the architecture.




### Speed of cache memory


   *  Cache is a small fast memory that mirrors the most commonly
  used words of the primary memory.
   *  Cache may run at full processor speed.
   *  The datapath and control need to determine, for each memory
    access, whether the effective address refers to a word in the
    cache.  If so, the access is quick and the primary memory is
    not used.
   *  The test for cache residency must be \emph{fast}!  It
    happens on every memory access (more than once per instruction,
    on average).
   *  Extra registers are needed to give fast access to
    information about which addresses are in the cache.




### What's in the cache


 *  Since the cache contains recently used data, it does not
  contain a sequence of words at consecutive locations
 *  Therefore each location in the cache contains two parts:

 *  A word of data
 *  A tag: the actual address of this data in the primary memory

 *  This enables the machine to identify whether an arbitrary
  address refers to data that's already in the cache, and if so,
  where it is




### Cache lines



 *  In practice, we don't just keep individual memory locations
  (bytes or words) in the cache

 *  The cache is organised by ``large words'' called cache lines;
  e.g. 16 bytes at an address which is a multiple of 16.
 *  A memory access to an address $a, a+1, \ldots, a+15$ will
  actually refer to the same cache line (where $a$ is a multiple of
  16).
 *  Size of a cache line varies: making it bigger increases
  probability of cache hit but also increases penalty of a cache hit





### Searching the cache


 *  The cache has to be searched on each memory access, so this
  search must be fast!
 *  The quickest approach: \emph{direct mapped cache} requires
  each address to go into a specific cache entry.
 *  The most general approach: \emph{associative search}

 *  Each cache line has a dedicated comparitor that checks the
  memory address against the tag for this line
 *  The comparisons are done by hardware, in parallel

 *  There are also various compromises which require less
  hardware but offer less improvement in performance




### Fully associative cache


  
 *  Direct mapping is inflexible: it can cause a sequence of
  memory operations to induce a sequence of cache misses (similar
  to thrashing in virtual memory).
  
 *  An alternative approach is to allow a word of memory to be
  placed in any different cache location---this is called
  \emph{fully associative} cache.
  
 *  The hardware must search the tags of the set of possible
  cache locations corresponding to a memory location.




### Associative memory

Also called content-addressable memory.



 *  Each word contains several fields

 *  Data is not accessed by its address

 *  To fetch data, a field value is specified, and the memory
  returns the rest of the word that matches this field

 *  If it is possible for several matches to occur, the memory
  must resolve this.




### Searching in associative memory



 *  Each location has some logic as well as state

 *  On a memory access, the specified field value is broadcast

 *  In parallel, each location compares the broadcast field with
  its local field value, resulting in a Boolean \emph{match}

 *  A tree of or-gates can determine in logarithmic time whether
  a match exists.

 *  If there are multiple matches, a tree circuit can also
  determine a unique responder in logarithmic time.




### A compromise: Set associative cache


 *  Instead of allowing a block of memory to go into any location
  of the cache, restrict it to just a subset of the cache
  locations.
 *  You can think of direct mapped as a special case where the
  subset has one element, and fully associative as a special case
  where the subset is the entire set.
 *  This reduces the number of comparitors needed to check tags,
  and the size of the tree circuit to resolve matches.
 *  Many real machines use set associative cache.



### Reducing cache misses


 *  Increasing the cache size helps, but the fixed direct mapping
  can cause a sequence of memory operations to induce a sequence of
  cache misses (similar to thrashing in virtual memory).
 *  An alternative approach is to allow a word of memory to be
  placed in a set of different cache locations.
 *  The hardware must then do a search through the set of
  possible cache locations corresponding to a memory location.
 *  This requires searching the tags.  What matters is not the
  location of the cache entry, but the association between the tag
  and the value.




### Processor: after load x


  \includegraphics[scale=0.4]{figures/png/S16-processor-add-instr1.png}



### Processor: after load y


  \includegraphics[scale=0.4]{figures/png/S16-processor-add-instr2.png}




### Processor: after add


  \includegraphics[scale=0.4]{figures/png/S16-processor-add-instr3.png}



### Processor: after store


  \includegraphics[scale=0.4]{figures/png/S16-processor-add-instr4.png}


### Processor: after termination


  \includegraphics[scale=0.4]{figures/png/S16-processor-add-instr5.png}




## Data types


 *  A word of memory contains a string of bits
 *  The hardware has no idea what data type this string of bits
  has
 *  If you execute an instruction that treats the bits as an
  integer, then it's an integer \emph{during the execution of that
    instruction}
 *  The same holds for all other data types
 *  If the machine executes a word (i.e. the pc register points
  to that word), it will be interpreted as an instruction
 *  It is the responsibility of the programmer to use each word
  consistently
 *  Programming languages and compilers help by associating a
  \emph{type} with every data value
 *  A common error: accidentally executing a variable (e.g.
  jumping into the area of memory where your variables reside)



### Many kinds of procedure


 *  Many related programming constructs:
  
   *  Procedures
   *  Subroutines
   *  Functions
   *  Coroutines
   *  Methods
  
 *  They are \emph{called}, perform some computation, and
  \emph{return} to the caller.
 *  Many variations: how parameters are passed, how variables are
  allocated, whether there are restrictions on usage



### About those jumps (call and return)


 *  There is a minor issue that often isn't worth worrying about:
  
   *  It costs an extra jump to call, and an extra jump to return
   *  These jumps take a (very small) amount of execution time
  
 *  If a procedure is very small, it may be worthwhile plug in in
  its code wherever it's called --- this is a compiler optimisation
  called \emph{inlining}
 *  But usually the space you save by having just one copy of the
  procedure's instructions in memory is more important than the time
  required for the call and return


### Characters


 *  'a', 'b', and so on
 *  Some computers represent characters using ASCII --- an 8-bit
  code for each character
 *  Sigma16 uses Unicode
  
   *  Each character has a 16-bit code
   *  Each possible 16-bit word corresponds to some character
   *  Lots of characters are available: \"u \ss\ \"o $\alpha$
  
 *  There are tables giving the codes for all the characters


### Calling conventions


 *  The machine architecture doesn't force any particular way to
  pass arguments to a function, or to pass the result back to the
  caller
 *  The program just has to do this in a consistent way
 *  It's good practice to establish \alert{conventions} --- standard
  ways for functions and procedures to operate


### Programming tips

### How to program efficiently

If you don't want to spend forever getting your program working, but
prefer to go out for a pizza with your friends, here's how:

\begin{enumerate}
 *  Study the specification of the problem and understand what
  you're doing
 *  Develop a high level pseudocode algorithm
 *  Translate the high level algorithm into a low level form (the
  ``goto'' form)
 *  Work out the instructions needed to implement each low level
  statement
\end{enumerate}

### How to waste time

If you want to drag out a program, so it takes a \alert{really long
  time} to get it working:

\begin{enumerate}
 *  Write the instructions
 *  Add a comment to each instruction
 *  Then write the algorithm
\end{enumerate}


A tip!  Write the comments before the code!


## Using the instruction set


 *  You need to know what each instruction does, and how to use it
 *  A table of the instruction set helps to get an overview



### Part of the instruction set

Soon we will see some changes in this table, as well as some new
instructions.

~~~~
load   R1,x[R0]     ; R1 := x
lea    R1,123[R0]   ; R1 := 123
store  R1,x[R0]     ; x := R1
jumpf  R1,x[R0]     ; if R1=0 then goto x
jumpt  R1,x[R0]     ; if R1/=0 then goto x

add    R1,R2,R3     ; R1 := R2+R3
sub    R1,R2,R3     ; R1 := R2-R3
mul    R1,R2,R3     ; R1 := R2*R3
cmplt  R1,R2,R3     ; R1 := R2<R3
cmpeq  R1,R2,R3     ; R1 := R2=R3
cmpgt  R1,R2,R3     ; R1 := R2>R3
~~~~

### Closed source, open source, free


 *  \alert{Closed source} --- the software is proprietary, the
  company doesn't publish it (you can get the machine language code
  but not the high level language code)
 *  \alert{Open source} --- the source code is published, although
  the owner has control
 *  \alert{Free} --- You can make a copy of the code and modify it
 *  Much controversy!
  
   *  One view: a large company can ensure high quality, and needs
    to ensure it gets paid for its work
   *  Another view: ``with many eyes studying code, every bug is
    visible'' --- open source and free software can be more reliable
  



## Operating Systems


 *  An operating system provides \stress{services that enable
    application programs to run}
 *  An OS is organised in layers, like an onion
  
   *  Kernel: essential services without which no program can
    execute (e.g. calling conventions, memory protection, processes)
   *  Intermediate level: networking (e.g. TCP/IP)
   *  Higher level: optional services for convenience
    (e.g. graphical user interfaces, browsers, media players)
  

### Essential services


 *  Procedures
   
   *  Standard rules for which register to use as \stress{stack
      pointer}, which register to use for \stress{return address}
   *  These conventions are sometimes defined by the Operating
    System, sometimes by the compiler
  
 *  Preparing and starting programs
  
   *  \stress{Linking} separate modules into an executable program
   *  \stress{Launching} a program
  
 *  Services for a running program
  
   *  Input/Output and memory allocation
   *  Concurrency
   *  Virtual memory
  




### Requests to the OS


 *  Many operations cannot be performed directly by a user program.
 *  The program requests the operating system to perform them
 *  An OS request is performed by executing a \texttt{trap}
  instruction, such as \texttt{trap R1,R2,R3}
 *  The \stress{type of request} is a number, placed in R1, and
  \stress{operands} (if any) are in R2, R3
 *  The specific codes used to make a request are defined by the
  operating system, not by the hardware
 *  This is a major reason why compiled programs run only on one
  operating system




### The trap instruction


 *  trap R1,R2,R3
 *  It doesn't matter which registers you use; all that matters is
  the content of the first, second, and third operand
 *  R1 contains a code indicating which request 
 *  R2, R3 contain additional information depending on the type of
  request




### trap is like jump, but different


 *  \stress{trap} causes the machine to jump to the operating
  system, so it's similar to \stress{jump}
 *  The OS has a way to jump back to the user program later, so trap
  is analogous to \stress{jal} 
 *  But trap is different from jump/jal:
  
   *  A trap does not specify the address to jump to: this is
    defined by the hardware
   *  A trap also sets the computer into \stress{system state}; this
    is needed for system security (we'll discuss it later)
  





### Typical OS requests


 *  Terminate execution of the program
 *  Read from a file
 *  Write to a file
 *  Allocate a block of memory



### Memory allocation


 *  The operating system maintains a map of the entire memory,
  organised as a set of blocks
 *  A running program often needs to allocate a block of memory
  dynamically
 *  When a program requests memory, it asks for a \emph{size} of
  block that it needs, and the operating system returns with the
  \emph{address} of a block



### Adding a character to a string


 *  Use a variable $k$ that gives the current length of the string
  \textit{out}
 *  $k$ is also the index into \textit{out} where we can insert
  another character


~~~~
; out[k] = '*'
     load   R8,k[R0]          ; R8 = k
     load   R10,star[R0]      ; R10 = '*'
     store  R10,out[R8]       ; out[k] = '*'
; k = k + 1
     lea    R4,1[R0]          ; R4 = 1
     add    R8,R8,R4          ; R8 = k + 1
     store  R8,k[R0]          ; k = k + 1

star data   $002a             ; code for '*'
~~~~


### Memory allocation


 *  The operating system maintains a map of the entire memory,
  organised as a set of blocks
 *  A running program often needs to allocate a block of memory
  dynamically
 *  When a program requests memory, it asks for a \emph{size} of
  block that it needs, and the operating system returns with the
  \emph{address} of a block



### Part of the instruction set

~~~~
load   R1,x[R2]   R1 := mem[x+R2]
lea    R1,x[R2]   R1 := x+R2
store  R1,x[R2]   mem[x+R2] := R1
jump   x[R2]      pc := x+R2
jumpf  R1,x[R2]   if R1=0 then pc := x+R2
jumpt  R1,x[R2]   if R1/=0 then pc := x+R2
jal    R1,x[R2]   R1 := pc, pc := x+R2

add    R1,R2,R3   R1 := R2+R3
sub    R1,R2,R3   R1 := R2-R3
cmplt  R1,R2,R3   R1 := R2<R3
cmpeq  R1,R2,R3   R1 := R2=R3
cmpgt  R1,R2,R3   R1 := R2>R3
~~~~

## Pointers


 *  We often need a variable that contains the \emph{address of
    another variable}
 *  This is called a \alert{pointer}
 *  Example: to write a string with trap R1,R2,R3:
  
   *  R1 contains the code indicating a write request
   *  R2 contains a \alert{pointer to the string} (address of the
    first character in the string)
   *  R3 contains the length of the string
  


### Three key instructions: lea, load, store

These are the most important instructions dealing with memory and
memory addresses


 *  lea (load effective address) calculates the effective address
  and loads that into the destination register
  
   *  \stress{lea R1,x[R2] ; R1 = x+R2}
  
 *  load calulates the effective address, fetches the word from
  memory at that address, and loads it into the destination register
  
   *  \stress{load R1,x[R2] ; R1 =
      \important{mem[}x+R2\important{]}}
  
 *  store is the opposite of load: copies a word from the
  destination register into memory at the effective address
  
   *  \stress{store R1,x[R2] ; \important{mem[}x+R2\important{]} =
      R1}
  






### A computer is a digital circuit!}


 *  It isn't a magic box with a little man inside!
 *  A computer is a \alert{digital circuit} constructed from
  \alert{logic gates} and \alert{flip flops}



### Philosophical question: finding the ultimate cause}


 *  A computer does lots of different things
  
   *  Arithmetic
   *  Sequences of calculations
   *  Decisions
   *  Goto
  
 *  Yet it's just a digital circuit made of logic gates and flip
  flops, which \alert{cannot do any of these things}
 *  The interesting behaviours \alert{emerge} from the way the basic
  components are connected



### Some examples of emergent ``ultimate causes''}


 *  Arithmetic
  
   *  Full adder uses logic gates to calculate sum and carry
   *  Ripple carry adder transfers carry bits to get binary sum
  
 *  Sequences of calculations
  
   *  The pc register points to next instruction, and is incremented
    as each instruction is executed
  
 *  Decisions
  
   *  If statements are translated to \alert{if cond then goto label}
   *  Control algorithm uses a multiplexer to take different actions
    based on cond
  
 *  Goto
  
   *  Goto is just loading the pc with the effective address
  




### Summary


 *  Register circuits hold the state of a processor
 *  Combinational logic circuits perform calculations
 *  Multiplexers allow control over where data moves
 *  A control algorithm sets the multiplexers, determining what
  register changes will occur at the next clock tick
 *  Everything (address calculation, jumps, etc.) is implemented by
  loading the right value into a register
 *  The control algorithm is an \emph{interpreter} for the
  instruction set



#### Booleans


 *  A Boolean is a data type with two values: \alert{True} and
  \alert{False}
 *  A Boolean expression has the value True or False: \alert{$x <
    5$}
 *  Conditional control structures use a Boolean to decide what to
  do
 *  In assembly language, we will
  
   *  Calculate a Boolean value in a register
   *  Use it to control a ``conditional goto'' (called a
    \alert{conditional jump})
  


### Booleans in machine language


 *  A Boolean is represented as a word (16 bits)
 *  The canonical representation is False=0 and True=1
 (these are 16 bit integers).
 *  Comparison instructions produce Booleans in the canonical
  representation
 *  Conditional jumps treat \alert{0 as False}, and \alert{any
    non-zero integer as True}



### Example: program findmax}
  
  The program searches an array of natural numbers for the maximal
  element, and the loop terminates when a negative element is
  encountered.


~~~~
{-
Register usage:
  R0 = constant 0
  R1 = maximum value found so far
  R2 = loop index i
  R3 = constant -1, indicates end of list
  R4 = x[i]
  R5 = constant 1, for incrementing i
  R6 = temp Bool value
-}
~~~~
}

%-----------------------------------------------------------------------

{\scriptsize
~~~~
      load  R1,x[R0]     ; max := x[0]
      lea R2,1[R0]       ; i := 1
      lea R3,-1[R0]      ; R3 := -1
      lea R5,1[R0]       ; R5 := 1 (for counter)
loop  load  R4,x[R2]     ; R4 := x[i]
      cmpgt R6,R4,R3     ; R6 := (x[i] >= -1)
      jumpf R6,done      ; goto done if not
      cmpgt R6,R4,R1     ; R6 := (x[i] > max)
      jumpf R6,skip[R0]  ; goto skip if not
      add   R1,R4,R0     ; max := x[i]
skip  add   R2,R2,R5     ; i := i+1
      jump  loop[R0]     ; goto loop
done  store R1,max[R0]   ; save max
      trap  R0,R0,R0     ; terminate execution

x     data    2
      data   42
      data  224
      data   19
      data    4
      data   -1
max   data  $0000        ;  0
~~~~
}

### Trap

  
   *  The trap instruction has three register operands (it's an
    RRR instruction)
   *  Trap R2,R3,R5
   *  It requests the operating system to perform a service, such
    as input/output
   *  The first register contains a \emph{trap code} that tells
    the operating system what the program is requesting; the other
    registers may contain further data
   *  A trap code of 0 terminates the program
   *  You can stop a program by executing trap R0,R0,R0
  


## Addresses



 *  An address is specified in two parts: an \emph{index
    register} and a \emph{constant}.
 *  The constant is a 16 bit integer which constitutes a second
  word of the instruction.
 *  The machine calculates an \emph{effective address}, which is
  the sum of the constant and the index register


 *  lea  R1,const[R2]  \hspace{5em} R1 := const+R2
 *  load R1,const[R2]  \hspace{5em}R1 := mem[const+R2]
 *  store R1,const[R2] \hspace{5em}mem[const+R2] := R1




The lea instruction is ``load effective address''; the load and
store instructions use to effective address to fetch/update a word
in memory



### Accessing data
  
  The index-constant scheme offers several ways to access data
  \emph{using just one addressing mode}, which simplifies the
  hardware:

 
 *  \texttt{x[R0]} gives the address of a scalar $x$ (because R0
  contains 0)
 *  \texttt{\$0000[R4]} uses a variable address in the register
 *  \texttt{a[R2]} gives the element of the array, $a_i$ where
  $i$ is the index in R2.

   *  Unassessed exercise: Complete the truth table for the mux1
  circuit, by simulating it for all input combinations.  Try to
  describe, at a more abstract and intuitive level, how it works.





### The multiplexer circuit}

THIS ONE IS REDUNDANT, HAVE A BETTER tikz DIAGRAM


  \includegraphics[scale=0.8]{figures/logicworks/mux1clipped.pdf}


~~~~
mux1 c x y =
   or2  (and2 (inv c) x)
        (and2 c y)
~~~~


#### Characteristics of combinational circuits}

### Two kinds of circuit}

 *  Combinational circuits
  
   *  Consist entirely of logic gates
   *  No feedback loops
  
 *  Sequential circuits
  
   *  May contain logic gates and flip flops
   *  Feedback loops are allowed
  
### Hexadecimal Numbers}

## Hexadecimal Numbers


 *  Base-16 numbers
 *  Don't worry!  Hexadecimal numbers are used only when they are
  easier than binary!
 *  Used as a notation to make it easier to read and write bytes and
  words
 *  A small investment in learning Hex will save lots of time later
 *  Used routinely in \alert{machine language} and \alert{Internet
    protocols}

### Hexadecimal Digits


\footnotesize
\begin{tabular}{|c|c|c|}
  \hline
  \rowcolor{green!50}
  Hex Digit & Bit string & Decimal value \\
  \hline
  0 & 0000 & 0 \\
  1 & 0001 & 1 \\
  2 & 0010 & 2 \\
  3 & 0011 & 3 \\
  4 & 0100 & 4 \\
  5 & 0101 & 5 \\
  6 & 0110 & 6 \\
  7 & 0111 & 7 \\
  8 & 1000 & 8 \\
  9 & 1001 & 9 \\
  a & 1010 & 10 \\
  b & 1011 & 11 \\
  c & 1100 & 12 \\
  d & 1101 & 13 \\
  e & 1110 & 14 \\
  f & 1111 & 15 \\
  \hline
\end{tabular}


### Hexadecimal numbers (``hex numbers'')


 *  Hex is used to write bit strings more concisely
 *  The bit string needs to have a multiple of 4 bits
 *  Modern computers use ``nibbles'' (4 bits), bytes (8 bits), short
  words (16 bits), full words (32 bits), long words (64 bits), and
  sometimes 128-bit words
 *  Break up the word into groups of 4 bits
 *  Replace each group with the corresponding hex digit

  \begin{tabular}{|cccc|}
    \hline
    \rowcolor{yellow!30}
    0010 & 1101 & 0101 & 1100 \\
    \hline
    \rowcolor{green!20}
    2  &  d   &   5  &   c \\
    \hline
  \end{tabular}

### Hexadecimal is easier to read than bits


 *  Like binary, hex is for nonnegative integers
 *  But we can write out any word as hex digits
 *  Each hex digit corresponds to 4 bits: Hex is 4 times more
  concise
 *  Long strings of bits are hard to read: it's hard to keep your
  place


### Black boxes

 *  We can define a circuit as a black box by giving it a name,
  and specifying its ports (inputs and outputs)
 *  This is analogous to using abstraction in a programming
  language by defining a function for a commonly used computation
 *  It is a box, with inputs, outputs, and internal components
  and signals

~~~~
circuit_name input1 input2 = (output1,output2)
  where
    output1 = ...
    output2 = ...
    x = ... (internal signals...)
    y = ...
~~~~

## Jumping

All jumps refer to effective addresses.

 *  jumpf R1,const[R2] \\ If R1 false then goto mem[R2+const]
 *  jumpt R1,const[R2]  \\ If R1 true then goto mem[R2+const]

### Instruction set

~~~~
load   R1,x[R2]   R1 := mem[x+R2]
lea    R1,x[R2]   R1 := x+R2
store  R1,x[R2]   mem[x+R2] := R1
jumpf  R1,x[R2]   if R1=0 then pc := x+R2
jumpt  R1,x[R2]   if R1/=0 then pc := x+R2
jal    R1,x[R2]   R1 := pc, pc := x+R2

add    R1,R2,R3   R1 := R2+R3
sub    R1,R2,R3   R1 := R2-R3
cmplt  R1,R2,R3   R1 := R2<R3
cmpeq  R1,R2,R3   R1 := R2=R3
cmpgt  R1,R2,R3   R1 := R2>R3
~~~~

### What can we learn from this?

 *  To be a good programmer you need to
  
   *  Know your programming language
   *  Be meticulous
   *  \alert{Read} your code carefully
   *  Do thorough testing
  
 *  It was very helpful that Apple published the source code for
  this bug


### Closed source, open source, free

 *  \alert{Closed source} --- the software is proprietary, the
  company doesn't publish it (you can get the machine language code
  but not the high level language code)
 *  \alert{Open source} --- the source code is published, although
  the owner has control
 *  \alert{Free} --- You can make a copy of the code and modify it
 *  Much controversy!
  
   *  One view: a large company can ensure high quality, and needs
    to ensure it gets paid for its work
   *  Another view: ``with many eyes studying code, every bug is
    visible'' --- open source and free software can be more reliable

### Processor organisation}

### A computer is a digital circuit!

 *  It isn't a magic box with a little man inside!
 *  A computer is a \alert{digital circuit} constructed from
  \alert{logic gates} and \alert{flip flops}
  
   *  \alert{There is a full circuit design for Sigma16} --- the
    entire computer is implemented down to the level of inv, and2,
    dff, etc.
   *  Today we'll take a brief overview
   *  Some more detail in CS2
   *  Computer Architecture 4 covers the full system, along with
    advanced techniques: pipelining, superscalar, combinators,
    parallel scan, synchronisers, circuit parallelism, and more
  
### Processor organisation

 *  The RTM circuit (register transfer machine)

 *  Contains a register file, and adder, and a multiplexer
 *  It can execute a sequence of Sigma16 add instructions

 *  We can extend this circuit to handle the entire architecture!

### Control registers

 *  The circuit needs to \alert{keep track of the instructions}
 *  This is done using \alert{control registers}
  
   *  The \alert{instruction register (ir)} contains the instruction
    currently being executed
   *  The \alert{program counter register (pc)} contains the address
    of the next instruction
  
 *  You can see these registers in the emulator window

### The control algorithm

 *  Actions performed by the machine are \alert{register loads}
 *  The choice of \emph{which} action to perform is determinted by
  the \alert{control algorithm}
  
   *  This is described as an ordinary algorithm, using pseudocode
   *  It determines which values get loaded into which registers at
    each clock tick
   *  The algorithm is implemented using flip flops and logic gates
  
 *  The algorithm is an \alert{infinite loop}
  
   *  Each iteration fetches the next instruction, decodes it, and
    executes it
  
### Structure of the control algorithm

~~~~
repeat forever
  ir := mem [pc],  pc := pc+1
  case ir.op of
    0 -> reg [ir.d] := reg [ir.sa] + reg [ir.sb]    ; add
    1 -> reg [ir.d] := reg [ir.sa] - reg [ir.sb]    ; sub
       ...
    4 -> reg [ir.d] := reg [ir.sa] < reg [ir.sb]     ; cmplt
       ...
   15 -> case ir.sb of
               0 -> adr := mem [pc],  pc = pc + 1    ; lea
                       adr := adr + reg [ir.sa]
                       reg [ir.d] := adr
               1 -> adr := mem [pc],  pc = pc + 1    ; load
                       adr := adr + reg [ir.sa]
                       dat := mem [adr]
                       reg [ir.d] := dat
~~~~

### A closer look at lea

 *  Fetch the second word of the instruction (the displacement) and
  put it in adr; also increment the pc
 *  Calculate the effective address: displacement + index register
 *  Load the effective address into the destination register
\end{enumerate}

~~~~
        0 -> adr := mem [pc],  pc = pc + 1    ; lea
             adr := adr + reg [ir.sa]
             reg [ir.d] := adr
~~~~


### Lea and Load: similar but not identical!

 *  Load starts the same as lea: it calculates the effective address
 *  Then it \alert{fetches a word from memory at the effective
    address} and loads that into the destination register
 *  The difference is that lea just loads the effective address into
  the destination register

~~~~
        0 -> adr := mem [pc],  pc = pc + 1    ; lea
             adr := adr + reg [ir.sa]
             reg [ir.d] := adr

        1 -> adr := mem [pc],  pc = pc + 1    ; load
             adr := adr + reg [ir.sa]
             dat := mem [adr]
             reg [ir.d] := dat
~~~~

### A jump is just a load!

 *  Calculate the effective address
 *  Load the effective address into the pc
 *  The next instruction to be executed is whatever is at that
  address

~~~~
        3 -> adr := mem [pc],  pc = pc + 1    ; jump
             adr := adr + reg [ir.sa]
             pc := adr
~~~~

### Processor organisation

A computer is a digital circuit!

 *  It isn't a magic box with a little man inside!
 *  A computer is a \alert{digital circuit} constructed from
  \alert{logic gates} and \alert{flip flops}
  
   *  \alert{There is a full circuit design for Sigma16} --- the
    entire computer is implemented down to the level of inv, and2,
    dff, etc.
   *  Today we'll take a brief overview
   *  Some more detail in CS2
   *  Computer Architecture 4 covers the full system, along with
    advanced techniques: pipelining, superscalar, combinators,
    parallel scan, synchronisers, circuit parallelism, and more
  
### Processor organisation

 *  The RTM circuit (register transfer machine)

 *  Contains a register file, and adder, and a multiplexer
 *  It can execute a sequence of Sigma16 add instructions

 *  We can extend this circuit to handle the entire architecture!

### Control registers

 *  The circuit needs to \alert{keep track of the instructions}
 *  This is done using \alert{control registers}
  
   *  The \alert{instruction register (ir)} contains the instruction
    currently being executed
   *  The \alert{program counter register (pc)} contains the address
    of the next instruction
  
 *  You can see these registers in the emulator window

### The control algorithm

 *  Actions performed by the machine are \alert{register loads}
 *  The choice of \emph{which} action to perform is determinted by
  the \alert{control algorithm}
  
   *  This is described as an ordinary algorithm, using pseudocode
   *  It determines which values get loaded into which registers at
    each clock tick
   *  The algorithm is implemented using flip flops and logic gates
  
 *  The algorithm is an \alert{infinite loop}
  
   *  Each iteration fetches the next instruction, decodes it, and
    executes it
  
### Structure of the control algorithm

~~~~
repeat forever
  ir := mem [pc],  pc := pc+1
  case ir.op of
    0 -> reg [ir.d] := reg [ir.sa] + reg [ir.sb]    ; add
    1 -> reg [ir.d] := reg [ir.sa] - reg [ir.sb]    ; sub
       ...
    4 -> reg [ir.d] := reg [ir.sa] < reg [ir.sb]     ; cmplt
       ...
   15 -> case ir.sb of
               0 -> adr := mem [pc],  pc = pc + 1    ; lea
                       adr := adr + reg [ir.sa]
                       reg [ir.d] := adr
               1 -> adr := mem [pc],  pc = pc + 1    ; load
                       adr := adr + reg [ir.sa]
                       dat := mem [adr]
                       reg [ir.d] := dat
~~~~

### A closer look at lea

 *  Fetch the second word of the instruction (the displacement) and
  put it in adr; also increment the pc
 *  Calculate the effective address: displacement + index register
 *  Load the effective address into the destination register

~~~~
        0 -> adr := mem [pc],  pc = pc + 1    ; lea
             adr := adr + reg [ir.sa]
             reg [ir.d] := adr
~~~~


### Lea and Load: similar but not identical!

 *  Load starts the same as lea: it calculates the effective address
 *  Then it \alert{fetches a word from memory at the effective
    address} and loads that into the destination register
 *  The difference is that lea just loads the effective address into
  the destination register

~~~~
        0 -> adr := mem [pc],  pc = pc + 1    ; lea
             adr := adr + reg [ir.sa]
             reg [ir.d] := adr

        1 -> adr := mem [pc],  pc = pc + 1    ; load
             adr := adr + reg [ir.sa]
             dat := mem [adr]
             reg [ir.d] := dat
~~~~

### A jump is just a load!

 *  Calculate the effective address
 *  Load the effective address into the pc
 *  The next instruction to be executed is whatever is at that
  address

~~~~
        3 -> adr := mem [pc],  pc = pc + 1    ; jump
             adr := adr + reg [ir.sa]
             pc := adr
~~~~

### Programming Guidelines

 *  The program file should contain the following, in this order:
  \begin{enumerate}
   *  Full line comments that \alert{identify the program}: its
    name, author, date, brief description of what it does
   *  The \alert{algorithm in high level language notation}. You can
    use pseudocode, or an existing language.  It's recommended that
    you avoid very complex language constructs.
   *  The \alert{algorithm written in high level language notation,
    but in the ``goto'' form} (again as full line comments).
   *  The \alert{program written in assembly language}, with proper
    comments.
  \end{enumerate}

### Identify the program

~~~~
; Program Max (Sigma16 assembly language)
; Author: John O'Donnell, 2017

; Max: find the maximum element of an array
;
; The program is given
;   *  a natural number n, assume n>0
;   *  an n-element array x[0], x[1], ..., x[n-1]
;  It calculates
;   * max = the maximum element of x
~~~~

Write this first, and put it at the start of the file, as full ine
comments.  Say what the program is and what it does.

### High level algorithm as pseudocode

~~~~
; Since n>0, the array x contains at least one element,
; and a maximum element is guaranteed to exist.
;
;   max := x[0]
;   for i := 1 to n-1
;       if x[i] > max
;         then max := x[i]
~~~~
}

Put this after the program identification comments.

### Middle level: the ``goto form''

 *  The algorithm  is written in high level language notation
  
   *  It just uses variables, without specifying whether they are
      currently in registers or memory
    *  It doesn't need instructions like load, store, lea
    *  It can use expressions like \texttt{p + x[i]*y[i]} which
     would require several instructions to implement
  
It may use
  
   *  Assignment statements, e.g \texttt{ x := a + 3*(c-d)}
   *  goto statements, e.g. \texttt{goto calculateAverage}
   *  if - then - goto, e.g. \texttt{if i=n then goto done}
  
 *  It does not contain complex control constructs: while, for,
  if-then-else
 *  In an \emph{if} statement, only a goto is allowed after
  \emph{then}

### Translate high level code to low level ``goto form''

~~~~
;         i = 1
;         max = x[0]
; 
; loop:   if not (i<n) then goto done
;         if x[i] <= max then goto next
;         max = x[i]
; next:   i = i + 1
;         goto loop
; 
; done:   terminate
~~~~

\alert{It's easier to check that this low level is equivalent to both
  the high level algorithm and the assembly language, rather than
  translating all the way to assembly language in one giant step.}


### Specify how the registers are used

If you keep any variables or constants in registers, document this in
comments


~~~~
; Register usage
;   R1 = constant 1
;   R2 = n
;   R3 = i
;   R4 = max
~~~~
}

\alert{

 *   This makes it easier to check that you used the right register.
 *  A bug like \texttt{add R3,R3,R2 ; i := i+1} is easy to spot:
  just compare the instruction with the register usage.
 *  Without the register usage comments, you have to read a large
  part of the program to catch this one little bug.
}

### Lowest level: Commented assembly language


 *  The code should be organised in sections.
 *  Separate the sections with a blank line - makes it easier to
  read
 *  Each section corresponds to one statement in the goto-form
  algorithm
 *  That statement appears as a full line comment before the
  instructions
 *  Each instruction has a comment explaining what that instruction
  does, at the highest level possible (e.g. i := i + 1, not R3 := R3 +
  R7)



### Block of statements to initialise registers


~~~~
; Initialise

       lea   R1,1[R0]          ; R1 = constant 1
       load  R2,n[R0]          ; R2 = n
       lea   R3,1[R0]          ; R3 = i = 1
       load  R4,x[R0]          ; R4 = max = x[0]
~~~~
}

These instructions implement what the Register Usage comments say


### Assembly language code


~~~~
; Top of loop, determine whether to remain in loop
; if not (i<n) then goto done
loop
       cmp    R3,R2            ; compare i, n
       jumpge R5,done[R0]      ; if i>=n then goto done

; if x[i] <= max  then goto next
       load   R5,x[R3]         ; R5 = x[i]
       cmp    R5,R4            ; compare x[i], max
       jumple next[R0]         ; if x[i] <= max then goto next

; max := x[i]
       add   R4,R5,R0          ; max := x[i]
~~~~
}

### Assembly language, continued


~~~~
; Bottom of loop, increment loop index
;   i = i + 1
;   goto loop

next   add   R3,R3,R1          ; i = i + 1
       jump  loop[R0]          ; go to top of loop

; Store max and terminate
done   store R4,max[R0]        ; max = R4
       trap  R0,R0,R0          ; terminate
~~~~
}


### Data definitions


~~~~
; Data area
n        data   6
max      data   0
x        data  18
         data   3
         data  21
         data  -2
         data  40
         data  25
~~~~
}


### Static variables in a data segment


 *  The variables are not placed in memory right after the
  instructions
 *  The program makes a request to the operating system to
  allocate a block of memory for data (the static data segment)
 *  A register is dedicated to point to the static data segment
 *  Variables are accessed using the static data segment register


### Implementing static data segment


~~~~
     ... set up registers to request a
          data block of n words...

     trap  R1,R2,R3
     ... R3 is set to the address of the data block
     add   R12,R0,R3    ; R12 = address of static data segment

   ...
     load  R2,n[R12]    ; R2 := n (access static variable)

     segment            ; subsequent labels will access segment
n    data
x    data
~~~~
}


## A digital circuit for Sigma16


 *  There is a complete digital circuit for Sigma16
 *  It's too complicated for a schematic diagram
 *  It is fully specified in Hydra
 *  Executing the specification simulates the circuit
 *  {\redtext \emph{You can run Sigma16 machine language directly on
    the circuit}}


### Emergent behaviour


 *  A computer does lots of different things
  
   *  Arithmetic
   *  Sequences of calculations
   *  Decisions
   *  Goto
  
 *  Yet it's just a digital circuit made of logic gates and flip
  flops, which \alert{cannot do any of these things}
 *  The interesting behaviours \alert{emerge} from the way the basic
  components are connected



### Some examples of emergent ``ultimate causes''


 *  Arithmetic
  
   *  Full adder uses logic gates to calculate sum and carry
   *  Ripple carry adder transfers carry bits to get binary sum
  
 *  Sequences of calculations
  
   *  The pc register points to next instruction, and is incremented
    as each instruction is executed
  
 *  Decisions
  
   *  If statements are translated to \alert{if cond then goto label}
   *  Control algorithm uses a multiplexer to take different actions
    based on cond
  
 *  Goto
  
   *  Goto is just loading the pc with the effective address
  



## A multi-level systems programming language


 *  Our focus is on \emph{fundamental principles}, not on details of
  any specific programming language
 *  We'll describe algorithms using a language that
  
   *  Illustrates features from several real languages
   *  Avoids some of the irrelevant complicated features that appear
    only in one language
   *  Encourages a good, expressive programming style
   *  Can express algorithms at both a high level and a low level
   *  Supports \emph{systems programming} --- that means it can do
    things that ordinary programming languages can't do
  


### Features of Sigma (so far)


 *  Data values
  
   *  Integer constants, variables, expressions
   *  Boolean expressions
  
 *  Low level statements
  
   *  Assignment statement
   *  goto statement
   *  if bexp then goto statement
  
 *  High level statements
  
   *  if b then S1
   *  if b then S1 else S2
   *  while b do S1
  


## Assembly language

This section describes the syntax rules for assembly language

### Assembly language notation


 *  Each statement has four fields
  \begin{enumerate}
   *  label field: if present \alert{MUST} begin in first character
    of the line; may be omitted.
   *  operation field: \alert{MUST} be preceded by space, and
    followed by space.  May \alert{NOT} contain spaces.
   *  operand field: \alert{MUST} be preceded by space, and
    followed by space.  May \alert{NOT} contain spaces.
   *  comments.  Precede with semicolon ;
\end{enumerate}
 *  The assembler uses a very simple rule: it just looks for groups
  of non-space characters, separated by spaces


### Fields separated by spaces


 *  An assembly language statement has \alert{four fields, separated
    by space}
  
   *  label (optional) -- if present, must begin in leftmost
    character
   *  operation load, add, etc.
   *  operands: R1,R2,R3 or R1,x[R0]
   *  comments: ; x = 2 * (a+b)
  
 *  \alert{There cannot be any spaces inside a field}
  
   *  R1,R12,R5 is ok
   *  R1, R12,R5 is wrong
  


{\color{blue}
~~~~
loop   load   R1,count[R0]    ; R1 = count
       add    R1,R1,R2        ; R1 = R1 + 1
~~~~
}

The assember first breaks each statement into the four fields; then it
looks at the operation and operands.

### Correct form of operand field


 *  RRR
  
   *  Exactly three registers separated by commas
   *  Example: \alert{R8,R13,R0}
  
 *  RX
  
   *  Two operands: first is a register, second is an address
   *  Address is a name or constant followed by [register]
   *  Example: \alert{R12,array[R6]}
  


### Each of these statements is wrong!


~~~~
    add   R2, R8, R9     Spaces in the operand field
    store x[R0],R5       First operand must be register, second is address
  loop load R1,x[R0]     Space before the label
    jumpt R6,loop        Need register after address:  loop[R0]
    jal   R14, fcn[R0]   Space in operand field
~~~~
}

If you forget some detail, look at one of the example programs


### Writing constants


 *  In assembly language, you can write constants in either decimal
  or hexadecimal
  
   *  \alert{decimal: }  50
   *  \alert{hexadecimal:} \$0032
  


Examples:

~~~~
   lea   R1,40[R0]      ; R1 = 40
   lea   R2,$ffff[R0]   ; R2 = -1

x  data  25
y  data  $2c9e
~~~~

### Good style


 *  It isn't enough just to get the assembler to accept your program
  without error messages
 *  Your program should be \alert{clear and easy to read}
 *  This requires good style
 *  \alert{Good style saves time writing the program and getting it
    to work}
 *  A sloppy program looks unprofessional


### Comments


 *  In Sigma16, a semicolon \alert{;} indicates that the rest of the
  line is a comment
 *  You can have a full line comment: just put ; at the beginning
 *  You should use good comments in all programs, regardless of
  language
 *  But they are even more important in machine language, because
  the code needs more explanation
 *  At the beginning of the program, use comments to give the name
  of the program and to say what it does
 *  Use a comment on every instruction to explain what it's doing

### Indent your code consistently

Each field should be lined up vertically, like this:

~~~~
    load   R1,three[R0]  ; R1 = 3
    load   R2,x[R0]      ; R2 = x
    mul    R3,R1,R2      ; R3 = 3*x
    store  R3,y[R0]      ; y = 3*x
    trap   R0,R0,R0      ; stop the program
~~~~
}

Not like this:

~~~~
    load   R1,three[R0]     ; R1 = 3
  load  R2,x[R0] ; R2 = x
       mul R3,R1,R2           ; R3 = 3*x
 store         R3,y[R0]      ; y = 3*x
   trap  R0,R0,R0      ; stop the program
~~~~
}

{\color{blue}The exact number of spaces each field is indented isn't
  important; what's important is to \alert{make the program neat and
    readable.}}

### Use spaces, not tabs


 *  To indent your code, always use spaces
 *  Don't use tabs!
 *  In general, \alert{never use tabs} except in the (rare) cases
  they are actually required
  
   *  The tab character was introduced to try to mimic the tab key
    on old mechanical typewriters
   *  But \alert{software does not handle tab consistently}
   *  If you use tabs, your can look good in one application and a
    mess in another
  
 *  It's easy to indent with spaces, and it works everywhere!



## Programming techniques

Notes on how to use the instructions effectively

### Programming tip: Copying one register to another


 *  Here's a useful tip --- a standard programming technique
 *  Sometimes you want to copy a value from one register to another
  
   *   R3 := R12
  
 *  There's a standard way to do it:
  
   *  \texttt{add R3,R12,R0 ; R3 := R12}
  
 *  The idea is that R12 + 0 = R12!
 *  Why do it this way?  \emph{It's actually more efficient than
    providing a separate instruction just to copy the register!}


### Using load and store


 *  A common error is to confuse load and store
 *  The main points to remember:
  
   *  We need to keep variables in memory (most of the time) because
    memory is big --- there aren't enough registers to hold all your
    variables
   *  The computer hardware can do arithmetic on data in registers,
    but it cannot do arithmetic on data in memory
   *  Therefore, to do arithmetic on variables, you must
    \begin{enumerate}
     *  Copy the variables from memory to registers (\alert{load})
     *  Do the arithmetic in the registers (\alert{add},
      \alert{sub}, $\ldots$)
     *  Copy the result from registers back to memory
      (\alert{store})
    \end{enumerate}
  


### A useful convention


 *  The instruction set is designed to be regular, and to follow
  consistent conventions
  
   *  This makes programming easier
   *  It also helps with the hardware design!
  
 *  For most instructions, the operands follow the pattern of an
  assignment statement: information goes right to left
  
   *  Assignment statement: \alert{reg1 := reg2 + reg3}
   *  Add instruction: \alert{add R1,R2,R3}
   *  The two operands on the right (R2, R3) are added, and placed in the
    destination on the left (R1)
   *  Load instruction: \alert{load R1,x[R0]} means \alert{R1 := x}
  
 *  \alert{An exception: store}
  
   *  \alert{store R1,x[R0]} means x := R1: the information goes from
    left to right
   *  Why?  Doing it this way makes the digital circuit (the
    processor) a little bit faster
  



### Development by transformation}


 *  In typical ``real world'' situations a programmer just uses a
  high level language
 *  To produce assembly language, a typical programmer is tempted to
  hack out code randomly and debug it randomly
 *  We are insisting on a systematic approach
  
   *  Start with algorithm in a high level language
   *  Transform it to low level via systematic patterns
   *  Transform that to assembly language
   *  Test each stage by hand execution
   *  The worst bugs usually appear in the high to low transformation
  



### High level

~~~~
   sum := 0;
   p := &RecordArray;
   q := &RecordArrayEnd;
   while p < q do
     { *p.fieldA := *p.fieldB + *p.fieldC;
       sum := sum + *p.fieldA;
       p := p + RecordSize; }
~~~~


### Low level

~~~~
   sum := 0;
   p := &RecordArray;
   q := &RecordArrayEnd;
RecordLoop
   if (p<q) = False then goto recordLoopDone;
   *p.fieldA := *p.fieldB + *p.fieldC;
   sum := sum + *p.fieldA;
   p := p + RecordSize;
   goto recordLoop;
RecordLoopDone
~~~~


### Assembly

~~~~
    lea    R1,0[R0]                 ; sum := 0
    lea    R2,RecordArray[R0]       ; p := &RecordArray;
    lea    R3,RecordArrayEnd[R0]    ; q := &RecordArray;
    load   R4,RecordSize[R0]        ; R4 := RecordSize
RecordLoop
    cmplt  R5,R2,R3                 ; R5 := p<q
    jumpf  R5,RecordLoopDone[R0]    ; if (p<q) = False then goto RecordLoopDone
    load   R5,1[R2]                 ; R5 := *p.fieldB
    load   R6,2[R2]                 ; R6 := *p.fieldC
    add    R7,R5,R6                 ; R7 := *p.fieldB + *p.fieldC
    store  R7,0[R2]                 ; *p.fieldA := *p.fieldB + *p.fieldC
    add    R1,R1,R7                 ; sum := sum + *p.fieldA
    add    R2,R2,R4                 ; p := p + RecordSize
    jump   RecordLoop[R0]           ; goto RecordLoop
RecordLoopDone
~~~~


#### Basic types and statements}

### Basic expressions and statements


 *  Expressions
  
   *  integer and natural
   *  boolean
  
 *  assignments
  
   *  lhs := expression
  



### Exposing the architecture


 *  Low level language constructs expose the architecture
 *  The lowest level language constructs correspond directly to
  instructions
 *  Registers
  
   *  Register variables R0, R1, $\ldots$, R15
  
 *  Instructions
  
   *  load   \qquad \texttt{R1 := var}
   *  store  \qquad \texttt{var := R1}
   *  add    \qquad \texttt{R2 := R3 + R8}
   *  jump   \qquad \texttt{goto address}
   *  jumpt  \qquad \texttt{if R2 then goto address}
   *  jumpf  \qquad \texttt{if not R2 then goto address}
  
 *  Interface to operating system



### Relations between the levels


 *  We could have completely separate high and low level languages
 *  Hypothesis: it's better to allow mixing where the semantics is
  clear
 *  This allows you to
  
   *  Transform from high to low level in stages, rather than in one
    gigantic leap
   *  Focus on the low level for a particularly interesting part of
    the program, while keeping the boilerplate at a high level
  
 *  Disallow mixing levels where semantics is unclear
  
   *  R4 := R2 - (x*y)
  



#### Data types and data structures

### Data types


 *  Basic types
  
   *  integer, natural, pointer, boolean, character
  
 *  Compound types
  
   *  array, record, linked lists, trees
   *  Call stack, stack frame
   *  Static variables, local variables, heap variables
  



### Records

~~~~
program Records
 { x, y :
     record
       { fieldA : int;
         fieldB : int;
         fieldC : int; }
   x.fieldA := x.fieldB + x.fieldC;
   y.fieldA := y.fieldB + y.fieldC;
 }
~~~~


### Traverse array of records with indexing

~~~~
sum := 0;
for i := 0 to nrecords do
  { RecordArray[i].fieldA :=
       RecordArray[i].fieldB + RecordArray[i].fieldC;
    sum := RecordArray[i].fieldA; }
~~~~



### Traverse array of records with pointers

~~~~
sum := 0;
p := &RecordArray;
q := &RecordArrayEnd;
while p < q do
  { *p.fieldA := *p.fieldB + *p.fieldC;
    sum := sum + *p.fieldA;
    p := p + RecordSize; }
~~~~


### High level control structures

  
   *  if bexp then S
   *  if bexp then S else S
   *  if bexp then S elsif bexp then S elsif ... else S
   *  case exp of {S; S; ...}
   *  while bexp do S
   *  repeat S until bexp
   *  procedure P (var:type, ...)
   *  function F (var:type, ...) : type
  


### Low level control structures


 *  goto label
 *  if bexp then goto label
 *  if not bexp then goto label


### Compilation patterns


 *  Each programming construct can be translated according to a
  standard pattern
 *  It's useful to translate in two steps:
  
   *  First, translate complex statements to simple high level
    statements (go to label, if b then goto label)
   *  The ``goto form'' of the algorithm corresponds closely to
    machine instructions
   *  Then it's straightforward to complete the translation to
    assembly language
  
   *  Assignment statements --- loads, then arithmetic, then store
   *  goto label --- jump label[R0]
   *  if b then goto label --- jumpt R5,label[R0] where R5 contains
    b
   *  if not b then goto label --- jumpf R5,label[R0] where R5
    contains b
  
   *  This approach clarifies how the algorithm works
  


### if bexp then S

~~~~
if x<y
  then {statement 1;}
statement 2;
~~~~

\alert{Translates into}

~~~~
   R7 := (x < y)
   jumpf R7,skip[R0]
   instructions for statement 1
skip
   instructions for statement 2 
~~~~


### if bexp then S1 else S2

~~~~
if x<y
  then { S1 }
  else { S2 }
S3
~~~~

Compiled into:
~~~~
   R5 := (x<y)
   jumpf R5,else[R0]
; then part of the statement
   instructions for S1
   jump   done[R0]
; else part of the statement
else
   instructions for S2 
done
   instructions for statement S3
~~~~


### while b do S

~~~~
while i<n do
  { S1 }
S2
~~~~

Compiled into:
~~~~
loop
   R6 := (i<n)
   jumpf  R6,done[R0]
   ... instructions for the loop body S1 ...
   jump   loop[R0]
done
  instructions for S2
~~~~


### Implementing compilation patterns


 *  All the language constructs correspond to constructors in an
  algebraic data type
 *  Use pattern matching definitions, here's a sketch

~~~~
transform (IfThenElse b s1 s2) = do
  elseLabel <- newlabel
  doneLabel <- newlabel
  bcode <- transform b
  testJump <- ifnot bcode elseLabel
  thenCode <- transform s1
  elseCode <- transform s2
  return $
     bcode <+> testJump
       <+> thenCode <+> skipelse
       <+> elseLabel <+> elseCode
       <+> doneLabel
~~~~



### Some interesting points

%---------------------------------------------------------------------

### Opportunities


 *  Normally we think of a machine language as being limited in
  expressiveness
 *  But few programming languages expose some very important
  techniques that are available in a well-designed  machine language
  
   *  multiple word arithmetic
   *  jump tables
   *  arithmetic on data addresses
   *  arithmetic on instruction addresses
  



### Opportunity for multiple execution models


 *  There are several ways to handle procedure calls
  
   *  Call stack vs. heap
   *  Caller saves vs. callee saves
   *  General recursion vs. tail recursion
  
 *  Plan to explore some of these
  
   *  This requires transforming procedure calls into statements at
    the level of instructions
   *  Some support by compiler analysis is needed
  



### Tricky issues


 *  Syntax
  
   *  Explicit or implicit compound statements?
   *  \textbf{if} b \textbf{then} \{S; S;\} \textbf{else} \{S; S;\}
   *  \textbf{if} b \textbf{then} S; S \textbf{else} S; S
    \textbf{endif}
   *  Also affects scoping rules
   *  Could be interesting to allow/illustrate both styles
   *  Prefix operators: \texttt{p} or \texttt{(*p)} or \texttt{p->}
  
 *  Semantics

   *  Some low level statements have side effects
   *  Interrupts cause unexpected jumps
  
## Stacks

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


# Computer architecture

An *instruction set architecture* is a precise specification of all
aspects of a machine that are visible to a programmer.  It includes a
description of the registers, memory, data representations, and all
the instructions, but does not include components of the
implementation that are not visible to a machine language programmer.

## Overview of computer architecture

### Machine language and instructions

Very different from Python, Java, C, etc.  The designer of a machine
language has to `look both up and down':
  
* Looking up to higher levels of abstraction, the machine language
  must be powerful enough to provide the foundation for
  operating systems and programming languages.

* Looking down to the lower levels of implementation, the machine
  language must be simple enough so that a digital circuit can execute
  it.
  
Machine languages are designed to achieve high performance possible at
reasonable cost.  Their primary aim is not to make programming as easy
as possible

A machine language program consists of instructions.  An instruction
is analogous to a statement in a programming language.  However, each
instruction just performs a small fixed set of operations, while
programming language statements can be complex.  For example, the
assignment statement x := 2 * (a + b/c) involves three arithmitic
operations, and it would require at least three instructions to
express in machine language.

## Relationship between machine language and circuit

### The RTM instructions


 *  The RTM circuit can execute two instructions
  
   *  R2 := R1 + R0 ; add two registers and load result
   *  R1 := 8   ; load a constant
  
 *  We'll begin with the corresponding Sigma16 instructions


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

This program is experimental software, and is under development.

## Author

The architecture, software tools, and documentation were designed,
implemented, and written by John O'Donnell.  Contact:
john.t.odonnell9@gmail.com

## License

See the files Sigma16/LICENSE.txt and Sigma16/NOTICE.txt.

Copyright (c) 2019 John T. O'Donnell.  john.t.odonnell9@gmail.com
License: GNU GPL Version 3 or later.  Sigma16/LICENSE.txt,NOTICE.txt

This file is part of Sigma16.  Sigma16 is free software: you can
redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.
Sigma16 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.  You should have received a copy of the GNU General
Public License along with Sigma16.  If not, see
<https://www.gnu.org/licenses/>.

## Implementation

Sigma16 consists of several components:

* The Integrated Development Environment (IDE) is written in
  JavaScript, and normally uses a web browser to display the graphical
  user interface.

* The circuits that implement Sigma16 are defined using the Hydra
  hardware description language, which is an embedded domain specific
  language implemented in Haskell.

* The User Guide is written in markdown and prepared for a web browser
  using pandoc.

# Reference

## Index of notation

* ea

* mem[ea]

* reg[d]

* reg[a]

* reg[b]

* reg[d]

* lsb q
