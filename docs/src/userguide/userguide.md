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

The Sigma16 IDE (the main "app") is implemented in JavaScript and runs
in a web browser.  There are additional components to the system that
must be downloaded and run on a computer; these include a digital
circuit that implements the architecture and a high speed emulator.
You can run the IDE simply by clicking a link on the [Sigma16 Home
Page](https://jtod.github.io/home/Sigma16/).

This user guide is organised by topic, with chapters on the
architecture, the assembly language, the linker, and programming
techniques.  However, it's a good idea to begin with an overview of
how the whole system works, and to be able to write and run simple
programs, before delving into the details.  For a quick start, begin
with the tutorials, which show you how to enter and run a program and
how to use the programming environment.

# Tutorials

The following short tutorials introduce the system; full details
appear in later sections.  You can keep the tutorials visible in the
right panel while following along with the exercises in the main
panel.

## Hello, world!

To launch the program, visit the Sigma16 Home Page and click on the
link to run it.

The main window contains two main sections.  The largest area, on the
left side, is the *main working area*.  When the program launches,
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
sizes of the main working area and the user guide sections.

You can also open the User Guide in a separate browser tab or window.
The Welcome page contains a link to do this.

A good way to get started is to go through the entire process of
running a simple program called *Hellow, world!*.  For now, we focus
just on how to use the software tools to run the program; an
explanation of the Sigma16 architecture and what the statements mean
will come later.

The main working area has several pages, with buttons at the top to
switch between them:

* **Welcome** contains some introductory information, release notes,
   and links.
   
* **Examples** contains a collection of assembly language programs
   organized by topic.  The simplest examples just use the *Core* part
   of the architecture, while the *Advanced* examples use additional
   features.

* **Modules** shows a summary of all the files and modules you
  currently have open.  It also provides buttons allowing you to open
  files, close them, and select one to work on.

* **Editor** shows the selected module, where it can be edited.  You
  can assemble and execute the selected module.  To run a program,
  you'll load it into the Editor (there are several ways to do this),
  then assemble it and then run it.

* **Assembler** translates a program from assembly language to machine
   language, and shows the assembly isting as well as the object code.

* **Linker** combines a collection of object code modles into a single
  executable program.

* **Processor** shows the components of the architecture and executes
  machine language programs.

Let's begin by running a simple example program.

* Click **Editor**, then **Hello, world!**.  This will enter a small
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

To run the program slowly, click Step repeatedly.  To run the program
faster but without updating the display after each instruction, click
Run.  At any time you can click Pause to stop the processor, and you
can resume execution with either Step or Run.  Sometimes it's useful
to let the processor run at full speed until it reaches a particular
instruction, and then stop.  This can be done by setting a
*breakpoint* (described in the Programming section).

There are two independent views into the memory; this is convenient
for looking at the machine language code in one view and the data in
the other view.  (Despite the two views, there is just one memory!)
At this point the pc register contains 0, meaning that the next
instruction to be executed is the one in memory location 0.  The ir
and other registers also contain 0, but that is just the initial
value.

To exit the app, just close the browser window or tab.  This may
put up a dialogue box warning that any unsaved data may be lost and
asking you to confirm.

## Registers, constants, and arithmetic

The architecture has a **register file** which is an array of 16
registers, named R0, R1, R2, ..., R15.  The Register File is displayed
in a box on the Processor page.

A register is a circuit that can hold a number, and the elements of
the register file can be used to hold variable values.  They are
analogous to the registers in a calculator: think of each register as
a box that can hold a number, and think of the register name as a
variable name.

A computer program is a sequence of **instructions**.  Instructions
are similar to statements in a programming language, except that they
are simpler.

Sigma16 performs arithmetic on data in registers.  To do any
computation on some numbers, we first need to get those numbers into
registers.  To place a constant into a register, use the lea
instruction.  In the assembly language we use the **lea** instruction.
For example, to load 42 into register 3, write

~~~~
    lea   R2,42[R0]   ; R2 := 42
~~~~
    
This is a statement in **assembly language**, and it describes one
instruction.  The operation is **lea**, the operands are **R2,9[R0]**,
and the part after the semicolon is a comment.

The word *lea* is the *operation*, i.e. the name of the instruction.
The operandd field consists of two operands separated by a comma.  The
first operand (R2) is called the *destination*; this is the register
where the result will be placed.  The second operand is a constant
(42) followed by [R0].  When the computer executed this instruction,
it simply places the constant into the destination.  In a higher level
language, we could write *R2 := 42*.    

Most instructions follow a similar pattern, where the first operand is
the destination where the result is placed, and the subsequent
operands are the arguments to the computation.  This is the same
convention used in assignment statements in many programming
languages: the registers in sub R1,R2,R3 appear in the same order as
the variables in R1 := R2-R3.

For now, just ignore the **[R0]** part.  This has a purpose but that
won't become clear until later.  Also, the name "lea" may look odd,
but this also has a meaning that will become clear later.

All arithmetic operations take place in the registers, and there is a
separate instruction for each operation.  For example, the following
instruction will add the values in R8 and R1 and then put the result
into R4:

~~~~
   add   R4,R8,R1  ; R4 := R8 + R1
~~~~

Notice that the operand field doesn't use operators like := or +;
instead it just separates the registers with commas.  The first
operand (R4 in this example) is the *destination*, which is where the
result will be placed.  The last two operands (R8 and R1) are the
values that will be added.

To perform a calculation, need to get the data into registers (using
lea) and then perform the calculation (using arithmetic instructions).
The following program calculates 3+4:

~~~~
    lea   R5,3[R0]    ; R5 := 3
    lea   R8,4[R0]    ; R8 := 4
    add   R2,R5,R8    ; R2 := R5 + R8 = 3+4 = 7
~~~~

It's a good idea to use comments to explain the meaning of an
instruction.  For now, comments like "R4 := R8 + R1" will be used to
show what the instruction does. That's useful while learning what the
instructions do, but later on we will use comments to give more
meaningful information (for example, what do the values in the
registers mean, and why are we adding them?).

There are three more arithmetic instructions.  These follow the same
pattern as add: in each case, the arithmetic is performed on the last
two registers and the result is placed in the destination (the first
register):

~~~~
  add  R4,R11,R0   ; R4 := R11 + R0
  sub  R5,R2,R13   ; R5 := R2 - R13
  mul  R2,R10,R7   ; R2 := R10 * R7
  div  R5,R6,R12   ; R5 := R6 / R12, R15 := R6 rem R12
~~~~

The divide instruction is slightly different: it produces two results,
the quotient and the remainder.  The quotient is placed in the
destination, but the remainder is automatically placed into R15, even
though the instruction doesn't mention R15.  (What happens if you
write *div R15,R1,R2*?  In that case the quotient is placed into R15
and the remainder is discarded.)

Normally an arithmetic instruction will put a new value into the
destination register, but the operand registers are left unchanged.
However, what happens if one of the operands is the same as the
destination, for example *add R7,R7,R8*?

An arithmetic instruction proceeds in three phases: (1) obtain the
values in the operand registers; (2) perform the arithmetic on those
values; and (3) put the result into the destination, discarding
whatever value was previously there.  So consider this example:

~~~~
   lea   R7,20[R0]  ; R7 := 20
   lea   R8,30[R0]  ; R8 := 30
   add   R7,R7,R8   ; R7 := R7 + R8
~~~~

After the two lea instructions have executed, R7 contains 20 and R8
contains 30.  The add instruction does the following:

1. It fetches the values in R7 and R8, obtaining 20 and 30
2. It adds the values, obtaining the result 50
3. It puts the result 50 into the destination R7, discarding the
   previous value.
   
The final result is that R7 contains 50.

Constant data can be specified using either decimal or hexadecimal
notation.

* Decimal numbers are written as strings of digits, optionally
  preceded by a minus sign: 3,-19, 42.

* Hexadecimal numbers are always written as four hex digits, and in
  assembly language programs they are indicated by putting $ before
  the number.  Thus $00a5 and 0165 both represent the integer 165.

~~~~
   lea   R1,13[R0]     ; R1 =  13 (hex 000d)
   lea   R2,$002f[R0]  ; R2 := 47 (hex 002f)
   lea   R3,$0012[R0]  ; R3 := 18 (hex 0012)
   lea   R4,0012[R0]   ; R4 := 12 (hex 000c)
~~~~

The processor page shows numbers as hex without the leading $, but in
an assembly language program the $ is needed to avoid ambiguity.

Notice that Sigma uses := as the assignment operator; thus we write R7
:= R7 + R8 (and we don't write R7 = R7 + R8).  This is because an
assignment statement is profoundly different from an equation, and
mathematicians have long used the = operator to indicate equations.
It isn't just an academic or theoretical point; there have been plenty
of occasions where computer programmers get confused between
assignment and equality, and using the wrong operator doesn't help.

Why does assembly language use a notation like add R5,R2,R3 instead of
R5 := R2 + R3?  In short, every instruction will use a similar
notation: a keyword for the operation, followed by the operands
separated by commas.  This notation is also related closely to the way
instructions are represented in memory, which we'll see later

An arithmetic instruction performs just one operation.  Several
instructions are needed to evaluate a larger expression.  In general,
you'll need a separate instruction for every operator that appears in
an expression.

Example: calculate 3 + 4 * 5 and put the result into R10.  We have to
put the numbers into registers, using lea, and then perform the
arithmetic.  It doesn't matter which registers are used (as long as we
avoid R0 and R15).

~~~~
    lea  R1,3[R0]   ; R1 := 3
    lea  R2,4[R0]   ; R2 := 4
    lea  R3,5[R0]   ; R3 := 5
    mul  R2,R2,R3   ; R2 := R2*R3  = 4*5
    add  R10,R1,R2  ; R10 := R1 + R2 = 3 + 4*5 = 23
~~~~

This is nearly enough to constitute a complete program.  Only one more
thing is needed: a way to terminate the program when it finishes.
There is a special instruction to do this:

~~~~
   trap  R0,R0,R0   ; halt
~~~~

Try running this program.  You can go to the Editor and type it in,
but this program is part of the collection of examples built in to
Sigma16.  Here's how to run it:

* Go to the Examples page.  Click Core, then Simple, then ConstArith.  You
  should see the listing of the program.
* Click Copy example to editor
* Click Editor, and you should see the text of the program in the
  window.
* Go to the Assembler page.  Click Assemble.
* Go to the Processor page.  Click Boot, then Step repeatedly and
  watch the effect of each instruction by observing how the registers
  and memory are changed.

It's a good idea to step through the program slowly, rather than
running it to completion at full speed.  The emulator will show the
next instruction to be executed, highlighted in blue.  Think about
what the instruction should do; in particular what changes to the
registers will occur?  Then click Step and check to see if the right
thing happened.  Note that the emulator displays all values in
hexadecimal notation.  (Tip: this is a good way to debug programs!)

Generally you can use any register you like, and the choices of
registers in the previous examples are arbitrary.  Registers R1
through R14 are all the same.  However, two of the registers are
different:

* R0 contains the constant 0 and it will never change.  Any time an
  instruction uses R0, the value it gets will be 0.  It is legal for
  an instruction to attempt to modify R0 (for example, add R0,R3,R4 is
  legal) but after executing this instruction R0 still contains 0.
  The reason for this is that we frequently need to have access to a
  register containing 0.
  
* R15 is used for two specific purposes.  We have already seen the
  first: the divide instruction places the remainder into R15.  The
  second purpose is that R15 contains the *condition code*, which is a
  word that contains a number of bits that provide some information
  about an instruction.  For example, if an addition produces a result
  that is too large to fit in a register, a special flag indicating
  this is set in R15.  Many of the instructions, including all the
  arithmetic instructions, change the value of R15 as well as placing
  the result in the destination register.  For this reason, R15 cannot
  be used to hold a variable: its value would be destroyed almost
  immediately.
  
To summarise, Registers R1 through R14 are all identical and can be
used for variables.  R0 contains 0 and will never change.  R1 changes
very frequently and can be used to determine various error conditions
and other information about an instruction.

Example

* Suppose we have variables a, b, c, d
* R1=a, R2=b, R3=c, R4=d
* We wish to compute R5 = (a+b) * (c-d)

~~~~
    add   R6,R1,R2     ; R6 := a + b
    sub   R7,R3,R4     ; R7 := c - d
    mul   R5,R6,R7     ; R5 := (a+b) * (c-d)
~~~~

Summary.

* A lea instruction of the form *lea d,const[R0]* will put the
  constant into Rd.
* The general form of an arithmetic instruction is *op d,a,b*.  
  The meaning is *R_d := R_a  op  R_b*, and the fields are:

----  ------------------------------------------------
 op    operation: add, sub, mul,div
  d    destination register: where the result goes
  a    first operand register
  b    second operand register
----  ------------------------------------------------

## Keeping variables in memory

So far we have used registers in the register file to hold variables.
However, there are only 16 of these, and two have special purposes (R0
and R15).  That leaves only 14 registers, and most programs need more
than 14 variables.

The computer contains another subsystem called the *memory*.  This is
similar in some ways to the register file.  The memory contains a
sequence of *memory locations*, each of which can hold a word.  Each
location is identifed by an *address*, and the addresses count up from
0.  We will use the notation *mem[a]* to denote the memory location
with address a

* The register file is used to perform calculations.  In computing
  something like x := (2*a + 3*b) / (x-1), all the arithmetic will be
  done using the register file.  But there are only a few registers
  available.
  
* The memory is much larger: it contains 65,536 locations so it can
  hold all the variables in a program.  But the memory has a
  limitation: the computer cannot do arithmetic directly on data in
  the memory.  
  
A variable name refers to a word in memory.  Actually, the variable
name just stands for the address of the location which contains the
variable.  This allows you to refer to a variable by a name (x, sum,
count) rather than an address (003c, 0104, 00d7).

Since we need a lot of variables, they need to be kept in memory.  But
since we need to do arithmetic and arithmetic can be performed only on
data in registers, we adopt the following strategy:

* Keep data permanently in memory
* When you need to do arithmetic, copy a variable from memory to a
  register
* When finished, copy the result from a register back to memory

Two instructions are needed to do this:
* *load* copies a word from a memory location into a register.
  Suppose *xyz* is a variable in memory; then to copy its value into
  R2 we could write *load R2,xyz[R0]*
* *store* copies a word from a register into a memory location.  If R3
  contains the result of some calculations, and we want to put it back
  into memory in a varaible named result, we would write *store
  R3,result[R0]*
  
It's necessary to write [R0] after the variable name, similarly to
writing [R0] after the constant in a lea instruction.  Again, the
reason for that will be explained later but for now just treat it as
an arbitrary rule.

At this point we have enough instructions to write an assignment
statement in assembly language.  Typically we will first write an
algorithm using higher level language notation, and then translate it
into instructions.

The problem: translate *x := a+b+c* into assembly language.

Solution:

~~~~
   load   R1,a[R0]      ; R1 := a
   load   R2,b[R0]      ; R2 := b
   add    R3,R1,R2      ; R3 := a+b
   load   R4,c[R0]      ; R4 := c
   add    R5,R3,R4      ; R5 := (a+b) + c
   store  R5,x[R0]      ; x := a+b+c
~~~~

Why do we have registers and memory?  After all, this makes
programming a little more complicated.  You have to keep track of
which variables are currently in registers, and you have to use load
and store instructions to copy data between the registers and memory.
Wouldn't it be easier just to get rid of the distinction between
registers and memory, and do all the arithmetic on memory?

Yes, this would be simpler.  Furthermore, it's possible to design a
computer that way, and there have actually been real computers like
that.  However, this approach makes the computer very much slower.
With modern circuits, a computer without load and store instructions
(where you do arithmetic on memory locations) would run approximately
100 times slower.  So nearly all modern computers do arithmetic in
registers, and use instructions like load and store to copy data back
and forth between registers and memory.

The variables used in a program need to be defined and given an
initial value.  This is done with the *data* statement.  The variable
name comes first, and it must start at the beginning of the line (no
space before it).  Then comes the keyword *data*, followed by the
initial value, which may be written in either decimal or hexadecimal.

For example, to define variables x, y, z and give them initial values:

~~~~
x    data   34    ; x is a variable with initial value 34
y    data    9    ; y is initially 9
z    data    0    ; z is initially 0
abc  data  $02c6  ; specify initial value as hex
~~~~

The data statements should come *after* all the instructions in the
program.  This may look surprising: in some programming language you
have to declare your variables at the beginning, before using them.
There is a very good reason why we will put the instructions first,
and the data statements after; but again the reason will come later.

There is a simple example of a complete program that uses load, store,
and data statement.  The text is given below, but you don't need to
type it in.

* Go to the Examples page.  Click Core, then Simple, then Add.  You
  should see the listing of the program.
* Click Copy example to editor
* Click Editor, and you should see the text of the program in the
  window.
* Go to the Assembler page.  Click Assemble.
* Go to the Processor page.  Click Boot, then Step repeatedly and
  watch the effect of each instruction by observing how the registers
  and memory are changed.

~~~~
; Program Add.  See Sigma16/README.md in top folder
; A minimal program that adds two integer variables

; Execution starts at location 0, where the first instruction will be
; placed when the program is executed.

      load   R1,x[R0]   ; R1 := x
      load   R2,y[R0]   ; R2 := y
      add    R3,R1,R2   ; R3 := x + y
      store  R3,z[R0]   ; z := x + y
      trap   R0,R0,R0   ; terminate

; Expected result: z = 37 (0025)

; Static variables are placed in memory after the program

x     data  23
y     data  14
z     data   0
~~~~

## Assembly language

The programs shown here are written in *assembly language*.  The
machine itself executes programs in *machine language*, which is
covered later.  Assembly language is translated to machine language by
a program called an *assembler*.

The purpose of assembly language is to give the programmer absolute
control over the machine language code without having to remember lots
of numeric codes.  For example, it is easier to remember the name
"mul" for multiply than to remember the machine language code (which
happens to be 3).  Similarly, it's easier to remember the names of
variables (x, y, sum, total) than the numeric addresses of the memory
locations that hold these variables.

The syntax of assembly language is simple and rigid.  Every statement
must fit on one line of source code; you cannot have a statement that
spans several lines, and you cannot have several statements on one
line.

Sigma16 assembly language uses a small set of characters.  Any
character not on this list will generate an error message.  A Sigma16
program can *manipulate* any 16-bit character, but the source assembly
language code is restricted to this source character set.  There are
many characters that look similar but are actually distinct.  For
example, the minus sign, the hyphen, the en-dash, and the em-dash all
look similar -- you have to look really closely to see the difference
-- but Sigma16 assembly language uses the minus sign, and the hyphens
and dashes won't work.

* letters: _abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ
* digits: 0123456789
* separators: (space) ,;
* quotes: " '
* punctuation: ".$[]()+-*
* other: ?¬£`<=>!%^&{}#~@:|/\'

Word processors often substitute characters.  For example, when you
type a minus sign in a paragraph of English text, word processors may
replace the minus sign with a hyphen or dash, which is correct for
typeset English but incorrect for assembly language.  The Sigma16
editor will insert the correct characters, as will plain text editors.

Each statement has a rigid format that consists of up to four
*fields*.  The fields must be separated by one or more spaces, and a
field cannot contain a space.  Every field is optional, but if a field
is missing then the following fields must also be missing, except for
an optional comment.  The fields are:

* label (optional) -- If present, the label must begin in the first
  character of the line.  If a line starts with a space, then there is
  no label field.  A label has the same syntax as names or identifiers
  in many languages: it may contain letters, digits, underscores, and
  must begin with a letter.  Both upper and lower case letters are
  allowed, and they syntax is case sensitive (Loop and LOOP and loop
  are three different labels).

* mnemonic -- This is the name of the operation: load, lea, add, sub,
  etc.  The mnemonic must be preceded by white space, and it must be
  the name of a valid instruction or assembler directive.
  
* operands field -- the operands required by the type of statement.
  There are several formats possible for the operands field, depending
  on the instruction; these are detailed later.  For example, for the
  add instruction the operand field must consist of three registers,
  separated by commas (e.g. R1,R2,R3).  Spaces are not allowed in the
  operands field: R1,R2,R3 is fine but R1, R2, R3 is an error.

* comments -- anything that follows the operands field, or anything
  that appears after a semicolon, is a comment.  The semicolon is not
  required if the mnemonic and operands fields are present, but it is
  good practice to include it.

Here are some syntactically valid statements:
~~~~
loop   load   R1,count[R0]    ; R1 = count
       add    R1,R1,R2        ; R1 = R1 + 1
~~~~

Each of the following statements is wrong!

~~~~
   add   R2, R8, R9    ; spaces in the operand field
loop1  store x[R0],R5  ; wrong order: should be R5,x[R0]
    addemup            ; invalid mnemonic
 loop2  load R1,x[R0]  ; Space before the label
    load R6,x          ; should be x[R0]
~~~~

If you forget some detail, look at one of the example programs

When the assembler is translating a program, it begins by looking at
the spaces in order to split each statement into the four fields.
This happens before it looks at the operation and operands.  The
assembly listing uses colors to indicate the different fields.  If you
get a syntax error message, the first thing to check is that the
fields are what you intended.  For example if you meant to say

~~~~
  add R1,R2,R3  ; x := a + b
~~~~
  
but you have a spurious space, like this

~~~~
  add R1, R2,R3  ; x := a + b
~~~~
  
the assembler will decide that the mnemonic is add, the operands field
is "R1," and all the rest - "R2,R3 ; x := a + b" -- is a comment, and
the colors of the text in the assembly listing will show this clearly.

Writing constants

In assembly language, you can write constants in either decimal
or hexadecimal.
  
* decimal:   50
* hexadecimal: $0032

Examples:

~~~~
   lea   R1,40[R0]      ; R1 = 40
   lea   R2,$ffff[R0]   ; R2 = -1

x  data  25
y  data  $2c9e
~~~~

Correct form of operand field

* RRR: Exactly three registers separated by commas R8,R13,R0.
  
* RX:Two operands: first is a register, second is an address.  The
  address is a name or constant followed by [register]. Example:
  R12,array[R6]




It isn't enough just to get the assembler to accept your program
without error messages.  Your program should be clear and easy to
read.  This requires good style.  Good style saves time writing the
program and getting it to work A sloppy program looks unprofessional.
Here are a few tips.

*Write good comments.* You should use good comments in all programs,
regardless of language.  But comments are even more important in
machine language, because the code tends to need more explanation.  At
the beginning of the program, use comments to give the name of the
program and to say what it does.  Use full line comments to say in
general what's going on, and put a comment on every instruction to
explain what it's doing.

*Indent your code consistently.* Each field should be lined up
vertically, like this:

~~~~
    load   R1,three[R0]  ; R1 = 3
    load   R2,x[R0]      ; R2 = x
    mul    R3,R1,R2      ; R3 = 3*x
    store  R3,y[R0]      ; y = 3*x
    trap   R0,R0,R0      ; stop the program
~~~~

Not like this:

~~~~
    load   R1,three[R0]     ; R1 = 3
  load  R2,x[R0] ; R2 = x
       mul R3,R1,R2           ; R3 = 3*x
 store         R3,y[R0]      ; y = 3*x
   trap  R0,R0,R0      ; stop the program
~~~~

The exact number of spaces each field is indented isn't important;
what's important is to make the program neat and readable.

*Spaces, not tabs!* To indent your code, always use spaces -- avoid
tabs!  In general, never use tabs except in the (rare) cases they are
actually required.  The tab character was introduced long ago into
computer character sets to try to mimic the tab key on old mechanical
typewriters But software does not handle tab consistently.  If you use
tabs, your can look good in one application and like a mess in
another.  It's easy to indent with spaces, and it works everywhere!

## Files and modules

Whatever method you use to edit your programs, be sure to **save your
work to a file** from time to time.  If you don't do that, sooner or
later the system will crash and you'll lose your data.

A *module* is a section of a program; it may be the complete program
or just a part of it.  A module may be saved in a file or it may
simply be text in the editor buffer.  A program may consist of just
one module, or it can be split between several files (*not supported
yet, coming soon*).

The Editor page contains a text area called the *editor buffer*.  When
you launch Sigma16, there is one module whose text is empty and
displayed in the editor buffer.  You can type a program (to be
precise, a module) into the editor buffer.  When you switch to the
Assembler page, the Assemble button will translate the text in the
editor buffer to machine language, which you can execute on the
Processor page.

After entering a program in the editor buffer, you should save it to a
file.  Click *Save in downloads* and the text in the editor buffer
will be written to a file on your computer.  Depending on how the
system is configured, there may be a dialogue box asking you for a
file name, or a generic default file name may be used (for example,
"S16DownloadFile (2).txt" or something similar).  This file will be
saved in the default Downloads directory as configured in your
browser.

The reason there is limited control over the name of the saved file,
and the directory where it is placed, is that web browsers enforce
strict limitations on the ability of applications to access your file
system.  That's a very good feature of browsers -- you don't want a
random web page to start deleting or corrupting your files -- but it
does make it a little inconvenient to save your edited assembly
programs.

To create a new module without destroying the existing one, click
*New* in the editor page.  This will make a new module with empty text
and display that in the editor buffer, so any text you had there will
disappear.  However, that text isn't lost, it's just hidden, and to
get it back you just need to select the previous module.

The *Modules* page shows a list of all the modules and allows you to
select one to work on.  The modules are shown in small sections
separated by horizontal lines.  The modules are numbered starting from
0, so if there are n modules their numbers go from 0 to n-1.  For each
module, the module number is shown, followed by some buttons to
operate on that module, and some information about it.  The first few
lines of the module are shown.  If you follow good programming style,
where the first rew lines of each module identify what the program is,
you'll be able to see at a glance what each module is without visiting
it in the editor.

Several buttons appear for each module in the list.  At any time, one
of the modules is *selected*. Click the Select button for any module
to select that one.  The selected module number is highlighted in red,
and when you go to the Editor page the text of the selected module
appears in the editor buffer.  This means you can have several
programs open at the same time, and just switch from one to the other
using the Select buttons in the Modules page.

You can also get rid of a module by clicking its Close button.  This
will delete its text, so it may be a good idea to select it and
download it in the Editor before closing it.

So far we have just created new modules by clicking *New* (in either
the Editor page or the Modules page).  You can also read files on your
computer into Sigma16.  Click *Choose files* and a dialogue box will
pop up.  You can select one or more files, and these will now appear
in the list of modules.

If a module was created by reading it from a file, its entry in the
list contains an extra *Refresh* button.  Clicking this will reread
the file and you won't need to use the file chooser dialogue box
again.

Common workflows:

* Just type your program into the editor buffer, and download it
  frequently.
  
* Use an external editor to enter your program.  After editing it, go
  to the Editor page and click Clear, then copy the program from your
  external editor and paste it into the editor buffer.  If you do
  this, make sure that your external text editor doesn't change your
  characters.  For example, word processors often change the minus
  character (-) into an en-dash.  There are four different characters
  that look similar to a minus sign (minus, hyphen, en-dash, em-dash)
  and the assembly language only accepts the minus sign.  If you get
  bad characters, the assembler will give an error message.
  
* Use an external editor and save the file.  In the Editor page, click
  Choose files and select your source file.  Then, every time you edit
  the text in your external editor, save it and then click *Refresh*
  on the module in the Modules page.  This way you don't need to
  Download the editor buffer because your up-to-date text will be in
  the external editor (but of course you have to save the file there).

## Conditionals

Conditionals allow a program to decide which statements to execute
based on Boolean expressions.  One example is the if-then statement,
for example:

~~~~
if x<y
  then statement 1
statement 2
~~~~

A related form is the if-then-else statement:

~~~~
if x<y
  then statement 1
  else statement 2
statement 3
~~~~

Conditionals are implemented in assembly language in two steps.
First, a *comparison* instruction is used to produce a Boolean
(i.e. either False or True).  Secondly, a *conditional jump*
instruction allows a choice of what instruction to execute next.

In Sigma16, every type of value is represented as a 16-bit word,
including Booleans.  If a word contains zero (hex 0000) then it
represents False, and otherwise it represents True.

The cmplt instruction compares two integers and produces a Boolean
result.  Its name means "compare for less-than". Here is an example:

~~~~
   cmplt  R5,R2,R8   ; R5 := R2 < R8
~~~~

This instruction compares the two operands R2 and R8.  If R2 < R8 then
the result is True, and the destination R5 is set to 1 (hex 0001).
Otherwise, if R2 >= R8, then the destination is set to 0 (hex 0000).

There are two more similar instructions:
~~~~
   cmpeq  R5,R2,R8   ; R5 := R2 = R8
   cmpgt  R5,R2,R8   ; R5 := R2 > R8
~~~~

The comments are assignments which set the destination register, and
:= is the assignment operator.  The right hand side of each assignment
is a relational expression: i.e. a comparison between two integers.
For the cmpeq instruction, we are comparing for equality, and the
equality operator is =.  In the Sigma system, = does not mean
"assign", it means "equals".  (Some programming languages use = to
mean assignment and == to mean =, which goes against centuries of
standard usage in mathematics and can be confusing.)

The cmplt and cmpgt instructions perform integer (two's complement)
comparison:

~~~~
   lea    R1,-3[R0]  ; R1 := -3 (hex fffd)
   lea    R2,5[R0]   ; R2 := 5  (hex 0005)
   cmplt  R3,R1,R2   ; R3 := -3 < 5 = True = 1 (hex 0001)
~~~~

The results would be different for natural number (binary) comparison.
The word fffd represents 65,533 in binary, which is greater than 5.
However, if two words are equal, the binary numbers they represent are
equal and the two's complement numbers they represent are also equal
(but the binary value may be different from the two's complement
value).  Thus cmpeq can be used for integer comparison and also
natural comparison, whic cmplt and cmpgt only perform integer comparison.

Now that we can compare two numbers and put the Boolean result into a
register, that Boolean can control which instructions to execute
next.  This is done with a *conditional jump* instruction.  There are
two forms: *jumpf* (jump if False) and *jumpt* (jump if True):

~~~~
         cmp   R3,R1,R2           ; R3 := R1 < R2
         jumpf R3,wasFalse[R0]    ; if not R3 then goto wasNotLt
         ...                      ; execute this if R3=True              
         ...
wasFalse ...                      ; execute this if R3=False
~~~~

Both *jumpf* and *jumpt* have the same syntax as lea, load, and
store.  They take two operands: the first is a register and the second
is a memory address.  The jumpf instruction jumps to the address if
the register contains False, and otherwise just skips to the next
instruction.  The jumpt instruction is similar, but it jumps if the
register contains True.

Usually the address in a jump instruction -- the place to jump to --
is specified as a label (e.g. wasFalse) and that label appears in the
label field of some instruction.  You can place a label in the same
line as the instruction, or it can be on a line with nothing else, in
which case the label refers to the next instruction.  In the following
code, label1 is the address of the add instruction and label 2 is the
address of the sub instruction.

~~~~
label1   add  R2,R4,R13
label2
         sub  R15,R0,R1
~~~~

If-then constructs are translated into assembly language following two
similar fixed patterns.  Suppose Bexp is a Boolean in any register Rd

~~~~
if Bexp
  then statement 1
statement 2
~~~~

This is translated according to the following pattern:

~~~~
       Rd := Bexp              ; usually use cmplt,cmpeq,cmpgt
       jumpf Rd,skip[R0]       ; if Bexp False, skip statement 1
       instructions for statement 1  ; only if Bexp True
skip   instructions for statement 2  ; always do this statement
~~~~

Usually a compare instruction is used to put the value of Bexp into
the register, but it could also be loaded from a variable or
calculated using Boolean instructions.

Here is an example:

~~~~
a := 93
x := 35
y := 71
if y > x then a := 59
b := 104
~~~~

The corresponding assembly language is:

~~~~
; a := 93
      lea     R1,93[R0]   ; R1 := 93
      store   R1,a[R0]    ; a := 93

; x := 35
      lea     R1,35[R0]   ; R1 := 35
      store   R1,x[R0]    ; x := 35

; y := 71
      lea     R1,71[R0]   ; R1 := 71
      store   R1,x[R0]    ; x := 71

; if y > x
      load    R1,y[R0]    ; R1 := y
      load    R2,x[R0]    ; R2 := x
      cmpgt   R3,R1,R2    ; R3 := y > x
      jumpf   R3,skip[R0] ; if not y > x then goto skip

;  then a := 59
      lea    R1,59[R0]    ; R1 := 59
      store  R1,a[R0]     ; a := 59

; b := 104
skip  lea    R1,104[R0]   ; R1 := 104
      store  R1,b[R0]     ; b := 104
~~~~

Notice the use of jumpf: if the Boolean expression is False we want to
skip over the "then" part.

An if-then-else statement has a similar compilation pattern, but this
time there are two separate parts: the "then-part" and the
"else-part".  Depending on the value of the Boolean expression, one of
those parts should be executed and the other should be skipped over.

For if-then-else, and many other control constructs, we need an
*unconditional jump* which will always go to the specified address,
and which doesn't use a Boolean.  The jump instruction is similar to
jumpf and jumpt except that the register containing the Boolean is
omitted:

~~~~
   jump   somewhere[R0]    ; go to somewhere
~~~~

The general form is

~~~~
if Bexp
  then S1
  else S2
S3  
~~~~

The pattern for translating this to assembly language is:

~~~~
   Rd := Bexp               ; put Boolean into Rd
   jumpf Rd,elsePart[R0]    ; if not Bexp then skip then-part
   instructions for S1      ; then-part of the statement
   jump   afterward[R0]     ; skip over else-part
elsePart
   instructions for S2      ; else-part of the statement
afterward
   instructions for S3
~~~~

## Loops

Loops are implemented using compilation patterns based on comparisons
and jumps.  The fundamental form is the *while loop*.

~~~~
while Bexp do S1
S2
~~~~

The compilation pattern is:

~~~~
loop
   Rd := Bexp
   jumpf  Rd,loopDone[R0]
   instructions for S1
   jump   loop[R0]
loopDone
   instructions for S2
~~~~

Occasionally you may encounter an infinite loop, which is sometimes
expressed as a while loop:

~~~~
while true do S1
~~~~

This doesn't need a Boolean expression; it is simply compiled into:

~~~~
loop
   instructions for S1
   jump   loop[R0] 
~~~~

Infinite loops are rather rare, or at least they should be.  On
occasion they are exactly what is wanted.  For example, operating
systems contain a loop that looks for something useful to do, and then
does it, and this should be an infinite loop.

However, there is a common but poor programming style that uses
infinite loops with random break or goto statements to get out of the
loop.  This may be appropriate on occasion but generally it is bad
style.

So far we have seen several compilation patterns:

* if-then
* if-then-else
* while

Every high level programming construct has a compilation pattern, and
they are mostly built using comparisons and jumps.  In principle,
these patterns are straightforward to use.  However, there are two
issues that require a little care: uniqueness of labels and nested
statements.

Labels must be unique: the same one cannot be used twice in the same
program, and if it is, the assembler will give an error message.
This means that you cannot follow the compilation patterns blindly.
If you use "loop" as the label for a while loop, as in the pattern
above, you need a different label for your next while loop.

The best approach here is not to use labels like loop, loop1, loop2.
It's far better to think about the *purpose* of the construct in your
program and to use a label that reflects this purpose.

Another complication is that most programs contain *nested
statements*.  These are statements that contain smaller statements,
and the containment may go several levels deep.

~~~~
if b1
  then S1
       if b2 then S2 else S3
       S4
  else S5;
       while b3 do S6
S7
~~~~

There is an important principle to follow here: every time a statement
appears in a compilation pattern (we have been calling them S1, S2,
S3, etc.), it should be translated as a *block*.

A block is a sequence of instructions which *always* begins execution
at the first instruction, and *always* finishes at the end.  You
*never* jump into the middle of it, and it *never* jumps out of the
middle to some other place.
  
Every statement should be compiled into a block of code.  This block
may contain internal structure --- it may contain several smaller
blocks --- but to execute it you should always begin at the beginning
and it should always finish at the end.

In programming language theory, programming with blocks is often
considered to be good practice or good style.  But it is more than
just an issue of style.  If you always treat the statements inside
compilation patterns as blocks, the patterns will "just work", no
matter how deeply nested they are.  If you violate the block
structure, you will find the program extremely difficult to get to
work.

## Machine language

The actual bits representing an instruction (written in hex) (e.g
0d69) are *machine language*.  The actual hardware runs the machine
language --- it's just looking at the numbers.  The text notation with
names -- e.g. add R13,R6,R9 -- is called assembly language.  Assembly
language is for humans to read and write; machine language is for
machines to execute.  Both languages specify the program in complete
detail, down to the last bit

As a program is running, the memory contains all your program's data:
the variables, data structures, arrays, lists, etc.  *The memory also
contains the machine language program itself.* The program is stored
inside the computer's main memory, along with the data.  This concept
is called *the stored program computer*.
  
There is an alternative approach: a computer can be designed to have
one memory to hold the data, and a completely separate memory to hold
the program.  This approach is often used for special-purpose
computers (primarily micro-controllers), but experience has shown this
to be inferior for general purpose computers.

Instruction formats: different types of instruction

Sigma16 has several instruction formats
  
*  \alert{RRR} instructions use the \alert{registers}
*  \alert{RX} instructions use the \alert{memory}
*  \alert{EXP} instructions use \alert{registers and constant}
  
Each kind of instruction is called an instruction format.  All the
instructions with the same format are similar Each instruction format
has a standard representation in the memory.
 
The machine language program is in the memory So we need to represent
each instruction as a word An instruction format is a systematic way
to represent an instruction using a string of bits, on one or more
words.  Every instruction is either RRR, RX, or EXP
  
* An RRR instruction is represented in one word (recall that a word is
  16 bits).
* An RX instruction is represented in two words.
  
Fields of an instruction word

An instruction word has 16 bits.  There are four fields, each 4 bits.
We write the value in a field using hexadecimal.  hex digits: 0, 1, 2,
3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f.  These correspond to 0, 1, ...,
15

The names of the fields are:

* op -- holds the operation code
* d  -- usually holds the destination register
* a  -- usually holds the first source operand register
* b -- usually holds the second source operand register

Representing RRR instructions

Every RRR instruction consists of
  
* An operation (e.g. add)
* Three register operands: a destination and two operands
* The instruction performs the operation on the operands and
  puts the result in the destination
  
Example: add R3,R12,R5.  We need to specify \alert{which} RRR
instruction this is.  Is it add? sub? mul? another?  This is done with
an operation code --- a number that says what the operation is.  There
are about a dozen RRR instructions, so a 4-bit operation code
suffices.  We also need to specify three registers: destination and
two source operands.  There are 16 registers, so a particular one can
be specified by 4 bits.  Total requirements: 4 fields, each 4 bits ---
total 16 bits.  An RRR instruction exactly fills one word.

All RRR instructions have the same form, just the operation
differs
  
* add  R2,R2,R5     ; R2 = R2 + R5
* sub  R3,R1,R3     ; R3 = R1 - R3
* mul  R8,R6,R7     ; R8 = R6 * R7
  
In add R2,R5,R9 we call R5 the first operand, R9 the second operand,
and R2 the destination.  It's ok to use the same register as an
operand and destination!  Later we'll see some more RRR instructions,
obut they all have the same form as these do.

Here are the RRR operation codes:

   mnemonic   opcode
  ---------- --------
   add        0
   sub        1
   mul        2
   div        3
   trap       d

Don't memorise this table!  You just need to understand how it's used.

Example of RRR:

~~~~
add  R13,R6,R9
~~~~

* The opcode (operation code) is 0
* Destination register is 13 (hex d)
* Source operand registers are 6 and 9 (hex 6 and 9)
* So the instruction is  0d69

Representing RX instructions

Every RX instruction contains two operands:
  
* A \important{register}
* A \important{memory location}
  
We have seen several so far:
  
*  lea  R5,19[R0]  ; R5 = 19
*  load R1,x[R0]  ; R1 = x
*  store R3,z[R0] ; z = R3
*  jump  finished[R0] ; goto finished
  
* The first operand (e.g. R1 here) is called the destination
  register, just like for RRR instructions
* The second operand x[R0] specifies a memory address
* Each variable is kept in memory at a specific location which is
  identified by its address

The memory operand has two parts:
  
* The variable x is a name for the address where x is kept --- called
  the displacement.
* The R0 part is just a register, called the index register.

Format of RX instruction
~~~~
load R1,x[R0]
~~~~

There are two words in the machine language code.
The first word has 4 fields: op, d, a, b, where
  
* op contains f for every RX instruction
* d contains the register operand (in the example, 1)
* a contains the index register (in the example, 0)
* b contains a code indicating \emph{which} RX instruction this is (1
  means load)
  
The second word contains the *displacement*.  In the example, this is
the address of x.  Suppose x has memory address 0008.  Then the
machine code for load R1,x[R0] is:

~~~~
f101
0008
~~~~

Operation codes for RX instructions

Recall, for RRR the op field contains a number saying which RRR
instruction it is.  For RX, the op field always contains f.  So how
does the machine know which RX instruction it is?  Answer: there is a
secondary code in the b field.

   mnemonic    secondary opcode in b field
  ~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   lea         0
   load        1
   store       2

## A strange program

Consider ``Program Strange'' below.  This program doesn't compute
anything particularly useful.  It's rather strange and not a model for
good programming style, but it illustrates an extremely important
concept, which is discussed below.

You can find the program on the Examples page, in the Core section, or
you can copy it below and paste it into the Editor page.  Run the
program with different initial values of a variable *y*, as described
below.  For each value of *y*, first try executing the program
manually, with paper and pencil, and then run it on the emulator to
check whether your execution was correct.  Give the final values of
the registers, and think about what is going on as the program runs.
For each run, assume that all the registers contain 0 after the
program is booted, before it begins execution.

1. Run the program in its original form, with *y data 0*
2. Change the last line to *y data 1* and run it again
3. Now use *y data 256*
4. *y data 8192*
5. *y data -5424*

~~~~
; Strange: A Sigma16 program that is a bit strange    
        load   R1,y[R0]
        load   R2,x[R0]
        add    R2,R2,R1
        store  R2,x[R0]
        lea    R3,3[R0]
        lea    R4,4[R0]
x       add    R5,R3,R3
        add    R0,R0,R7
        trap   R0,R0,R0
y       data   0
~~~~

*Solution* -- it's best to try answering the questions on your own
first, and then to check by running the program on the emulator,
before reading the solution!

The program loads an *instruction* into a register, does arithmetic on
it by adding *y* to it, and stores the result back into memory.  This
phenomenon is called *self-modifying code*, and it exploits the fact
that instructions and data are held in the same memory (this is the
*stored program computer* concept).  The original instruction is *add
R5,R3,R3*, and its machine language code is 0533.

1. When y=0, the final values are: R1=0, R2=0533, R3=3, R4=4, R5=6.
   The only notable points are that the store instruction doesn't
   actually change the value of the word in memory (it was 0533 and
   0533 is being stored there), and the last add instruction doesn't
   change the value in R0 because R0 can never change; it is always 0.
   (Of course if R7=0 then the result of the addition is 0 anyway.)
  
2. When y=1, the final values are: R1=1, R2=0534, R3=3, R4=4, R5=7.
   Note that R5 is *not* 3+3=6.  When y=1 is added to the instruction,
   the result is 0534 which means *add R5,R3,R4*, so instead of adding
   R3+R3 it adds R3+R4.

3. When y=256, the final values are: R1=256=0100, R2=0633, R3=3, R4=4,
   R5=0, R6=6. The decimal number 256 is 0100 in hexadecimal.  When
   this is added to the instruction, the result is 0633, which means
   *add R6,R3,R3* so R3+R3 is loaded into R6, not into R5.
   
4. When y=8192, the final values are: R1=4096=2000, R2=2533, R3=3,
   R4=4, R5=9.  The decimal number 8192 is 2000 in hexadecimal, and
   when this is added to the instruction the result is 2533, which
   means *mul R5,R3,R3*.  It's no longer an *add* instruction, it's a
   *multiply* instruction that calculates R5 := R3*R3 = 9.
   
5. When y=-5424 the program goes into an infinite loop. R1=ead0 (the
   hexadecimal representation of -5424, R2=f003, R3=3, and R4=4.  What
   started out as the *add* instruction at x has been transformed into
   *jump 7[R0]*, comprising the word at *x* (f003) and the following
   word (which is 0007).  This jump instruction goes back to the first
   lea instruction, and the program runs for ever (lea, lea, jump).
   
There is a lot to say about the phenomenon of self-modifying code.

This program shows clearly that a computer does not execute assembly
language; it executes machine language.  Try running it on the Sigma16
application (single step each instruction).  You'll see that the
assembly language statement *add R5,R3,R3* is highlighted in red, but
that is just the GUI trying to be helpful.  What's important is that
the machine language instruction is fetched from memory and loaded
into ir (the instruction register), and that is not 0533.  The machine
decodes the contents of ir and does whatever that says to do; it isn't
aware of the assembly language statement.  Indeed, a machine doesn't
even understand the concept of assembly language --- everything is
just bits!
   
To follow exactly what is happening in the emulator, it's important to
look at the pc and ir registers.  These reflect what the machine is
doing.  The assembly language does not.

What is self-modifying code good for?  The answer lies in the early
history of electronic computers.  Early computers (late 1940s and
early 1950s) did not use an effective address (i.e.  displacement +
index) like Sigma16; the instructions simply specified the absolute
memory address of an operand.  This is ok for simple variables, but
how could they process arrays?

The solution was to use self modifying code.  In a loop that traverses
an array, there would be a load instruction using address 0.  In the
body of the loop, there would be instructions to calculate the address
of x[i] by loading the address of x and adding i; this is then stored
into the address field of the load instruction.  That instruction is
then executed, obtaining the value of x[i].  This technique became
obsolete in the early 1950s with the invention of index registers and
effective addresses.
   
The pioneers of computers considered the concept of the *stored
program computer* (i.e. the program and data are in the same memory)
to be fundamental and essential.  One of the most important reasons
was that it made arrays possible.  Now we consider the stored program
concept to be fundamental *for different reasons*.
   
Self modifying code is tricky, and difficult to debug.  It makes
programs hard to read: you can't rely on what the program says, but on
what its instructions will become in the future.  For these reasonas,
self modifying code is now considered to be bad programming practice.
   
If a program modifies itself, you can't have one copy of the program
in memory and allow it to be shared by several users.  For example,
it's common now to have a web browser open with several tabs.  Each
tab is served by an independent process (a separate running instance
of a program that updates the window showing the web page).  If you
have 5 tabs open, there are 5 processes, each running the same machine
language code, and there's only one copy of that in memory.  This
wouldn't work if the program modified itself!
   
Self modifying code leads to security holes: if a hacker has the
ability to change your machine language code in memory, they could
make your own program act against you.
   
Modern computers use a technique called *segmentation* that prevents a
program from modifying itself.  This leads to increased reliability
and security.
   
Some computers have a facility that allows you to gain the power of
self modifying code without actually modifying the code in memory.
The idea is to have an instruction *execute R1,x[R0]* which calculates
the logical or of the two operands and then executes the result; x is
the address of an instruction and R1 contains the modification to it.
The modified instruction is executed, but there is no change to the
machine code in memory.  This idea was used in the IBM 360 and its
successors.  However, as the design of effective addresses has become
more sophisticated, the execute instruction is rarely needed, and most
modern computers don't provide it.

# Architecture

Our focus is on fundamental concepts, ideas and principles.  Sigma16
illustrates the fundementals of computer systems but it avoids
unnecessary complexity.  For example, Sigma16 has just one word size
(16 bits) while most commercial machines provide a variety.  That
variety is useful for practical applications but it complicates many
of the details while not adding any new fundamental ideas.  Most
commercial computers that achieve success in the marketplace
eventually become encrusted with complications that help support
backward compatibility; this can lead to great complexity.

## Data representation and notation

Sigma16 is a 16-bit architecture, and every data value is a 16-bit
word.  Integers are represented in 16-bit two's complement notation.
The bits of a word are numbered from left to right, starting with 0.
Thus the leftmost (most significant) bit of a word is bit 0, and the
rightmost (least significant) is bit 15.

The value of a word can be written in several ways:

* An unsigned integer between 0 and 65,535 (2^16 - 1)
* A signed integer between -32,768 and 32,767 (-2^15 and 2^15 - 1)
* A 4-digit hexadecimal constant, where the digits are 0-9 a-f.
  Sometimes, when the context is clear, this is written as just the
  hex digits (e.g 3b2f).  In assembly language programs, hex constants
  are written with a preceding $ sign (e.g. $3b2f).  This is necessary
  to avoid ambiguity: 1234 is a decimal number and $1234 is a
  hexadecimal number.  In contexts where there is no ambiguiity, the $
  may be omitted: for example, the GUi shows register and memory
  contents as hexadecimal without the leading $.
  
Some machine operations act on individual bits in a word.  We will use
the notation word.n to indicate bit n in the word, where the bits are
numbered from 0 at the left (most significant) position up to 15 at
the rightmost (least significant) position.  For example the fourth bit
of R15 can be written as R15.3.

A *field* is a consecutiave sequence of bits within a word.  For
example, we will later define a field named *op* which consists of
bits 0-3 of a word; this means the leftmost four bits of the word.

## Overview of the subsystems

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

**R0 holds the constant 0**

One of the registers, R0, has a special property: it always contains
the constant 0.  It is legal to perform an instruction that attempts
to load some other value into R0, but the register will still contain
0 after executing such an instruction.  Such an instruction will
simply have no lasting effect.

**R15 is the condition code register**

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
a collection of several *fields*, with each field giving one specific
piece of information about the instruction.

The particular scheme for describing an instruction as a collection of
fields is called an *instruction format*.  Like most computers,
Sigma16 has a small number of instruction formats and a larger number of
instructions.  The key to understanding the interface between machine
language and digital circuit design is to master the instruction
formats.

The core architecture (the simplest part of the system) uses just two
instruction formats: the **RRR format** for instructions that perform
operations in the registers, and the **RX format** for instructions
that refer to a memory location.

The advanced parts of the architecture provide additional instructions
which are represented with the EXP format. The name EXP stands
simultaneously for *expansion* (because it provides for many
additional instructions) and *experimental* (because it allows for
experimentation with the design and implementation of new
instructions).

### Instruction fields

An instruction may consist of one word or two words, depending on the
instruction format.  These words are subdivided into 4-bit *fields*,
each with a unique name.

First word of the instruction:

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

Second word of the instruction (if there is a second word):

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

Some instruction formats combine two of the 4-bit fields to form a
larger field:

* The a and b fields may be combined to form an 8-bit field called ab
  (only for the EXP format)
* The g and h fields may be combined to form an 8-bit field called gh
* The e, f, g, h fields may be combined to form a 16-bit field called
  disp

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

The first word of every instruction contains the
following fields.

* op  (bits 0-3) opcode, determines instruction format
* d   (bits 4-7) 4-bit destination
* a   (bits 8-11) 4-bit operand
* b   (bits 12-15) 4-bit operand, or expanded opcode for RX
* ab  (bits 8-15)  8-bit expanded opcode for EXP

A second word is needed to represent RX and EXP2 formats.  There
are individual names for the individual 4-bit fields, as well as names
(disp, gh) for larger fields.

* e (bits 0-3) 4-bit operand
* f (bits 4-7) 4-bit operand
* g (bits 8-11) 4-bit operand
* h (bits 12-15) 4-bit operand
* gh (bits 8-15) 8-bit operand
* disp (bits 0-15) 16 bit operand "displacement"

There are two kinds of format: the machine instruction formats, and
the assembly language instruction statement formats.  There are three
machine instruction formats: RRR, RX, EXP2.  However, there is a
larger set of assembly language statement formats, because there are
special syntaxes for some instructions, and there are assembler
directives that aren't instructions at all.  The assembly language
formats are described later.

The core architecture has only two instruction formats: RRR and RX.

### RRR format

RRR instructions perform operations on data in registers,
but not referring to memory.  The representation is one word.

RRR instructions: op is the operation code which determines the
instruction; op must be between 0 and 13 (hex 0 to hex d).  The
destination register is Rd, the operands are Ra and Rb.

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

RX instructions that specify a memory location as well as a
register operand.  The representation is two words.

RX instructions are two words, where op=15, b contains the secondary
opcode which specifies which RX instruction it is, d is the
destination, a is the index register, and the second word is a 16 bit
constant called the displacement (often written disp for short).

~~~~
---------------------
| op |  d |  a |  b |
---------------------
|    displacement   |
---------------------
~~~~

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

### EXP2 format

An EXP2 instruction contains 14 (hex e) in the op field, and the a and
b fields are combined into a single 8-bit number that contains a
secondary opcode.  This means that the EXP format allows for 256
instructions.  This greatly expands the number of instructions that
can be accommodated, and it allows for experimental instructions for
research purposes.  (The name EXP stands simultaneously for both
EXPansion and EXPerimentation.)

An EXP2 instruction instruction is two words, with an 8-bit secondary
operation code in the ab field of the first word.  There is a 4-bit
operand in the d field of the first word, and four 4-bit fields in the
second word, named e, f, g, h.

~~~~
---------------------
| op |  d |    ab   |
---------------------
|  e |  f |  g |  h |
---------------------
~~~~

Some EXP2 instructions combine the g and h fields to provide an 8-bit
operand.

~~~~
---------------------
| op |  d |    ab   |
---------------------
|  e |  f |    gh   |
---------------------
~~~~

### Summary of instruction formats

The following table summarises the instruction formats.  The core of
the architecture needs only the first two (RRR and RX).  The more
advanced features require the EXP2 format.

Table: **Machine language instruction formats**

------------------------------------------------------------
 Format   Size   Opcode   Operands   Example
-------- ------ -------- ---------- --------------------
 RRR      1      op       d,a,b      add Rd,Ra,Rb
 
 RX       2      op,b     d,disp,a   load Rd,disp[Ra]
 
 EXP2     2      op,ab    d,e,f,g,h  extract Re,Rf,Rg,Rh
 
 EXP2     2      op,ab    d,e,f,gh   save Rd,Re,gh[Rf]
------------------------------------------------------------

### Assembly language statement formats

Assembly language statements generally correspond to the instruction
formats, but there is not an exact correspondence for several reasons:

* Sometimes an instruction is written in assembly language with a
  field omitted which exists in the machine language code but is
  ignored.  For example, the instruction *cmp R1,R2* generates an RRR
  instruction, but the third operand field is omitted because the
  instruction requires only one operand, not two.  The assembler sets
  the unused operand to 0, but the machine ignores it.  This is called
  a "don't care" field in the instruction.
  
* Sometimes two instructions look the same in assembly language but
  use different machine language instruction formats.  For example,
  *add R1,R2,R3* and *push R1,R2,R3* look similar, but *add* uses the
  RRR instruction format and *push* uses the EXP2 instruction format.
  The reason for this is that there are not enough bits in the op
  field to accommodate all the instructions with three register
  operands, so an *expanding opcode* is used.  Thus push is
  represented with op=14, indicating EXP format, and the EXP2 variant
  is used for this instruction.
  
* The 4-bit fields are sometimes used to denote a register from the
  register file (R3), or a control register (mask), or a constant .
  In assembly language the constants are written just as a number
  (e.g. shiftl R1,R2,5).  Control registers are written by name rather
  than their number in the control register file (e.g. getctl
  R3,mask).
  
* Some assembly language statements are *pseudoinstructions*.  These
  are special cases of more general instructions.  For example, *and*
  is a pseudoinstruction which generates a *logicw* instruction
  specialised to perform a logical and.

Table: **Assembly language statement formats**

-------------------------------------------------------
 Asm   Example                  ML formats
------- ---------------------  ---------------------------
 RRR     add     Rd,Ra,Rb       RRR
 RX      lea     Rd,disp[Ra]    RX

 RR      inv     Rd,Ra          RRR (b ignored), RREXP
 JX      jump    disp[Ra]       RX (b ignored)
 KX      jumpc0  d,disp[Ra]     RX (d is constant)

 RRK     shiftl  Rd,Ra,k        EXP2
 RRKK    extract Rd,Re,g,h      EXP2
 RRRKK   andfld  Rd,Re,Rf,g,h   EXP2
 RREXP   execute Re,Rf          EXP2
 RCEXP   getctl  Re,Cf          EXP2
-------------------------------------------------------

An EXP instruction may use the fields op, d, ab, e, f, g, h.  The g
and h fields can be combined into a single 8-bit field gh All EXP
instructions combine the a and b fields into a single 8-bit field
called ab.  Some EXP instructions combine the g and h fields into a
single 8-bit field called gh.  The EXP format has the following
variants.
  
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

Example: add R1,R2,R3 ; R1 := R2 + R3

The instruction add Rd,Ra,Rb has operands Ra and Rb and destination
Rd.  It fetches the operands Ra and Rb, calculates the sum Ra + Rb,
and loads the result into the destination Rd.  The effect is Rd :=
Ra + Rb.  For example, add R5,R12,R2 performs R5 := R12 + R3.

The add instruction is RRR format with opcode=0.  Given destination Rd
and operands Ra and Rb (where d, a, b are hex digits), add Rd,Ra,Rb is
reprseented by 0dab.

  Code    Assembly          Effect
  -----   ----------------  ------------------
  062c    add R6,R2,R12     ; R6 := R2 + R12
  0d13    add R13,R1,R3     ; R13 := R1 + R3
  
In addition to setting the destination register, the add instruction
sets several bits in the condition code R15 and may set a bit in the
req register.

---------  ---------------------
 R15.ccG    result > 0 (binary)
 R15.ccg    result > 0 (two's complement)
 R15.ccE    result = 0
 R15.ccl    result <tc 0 (two's complement)
 R15.ccV    overflow (binary)
 R15.CCv    overflow (two's complement)
 R15.CCc    carry output
---------  ---------------------

### sub

Example: sub R1,R2,R3 ; R1 := R2 - R3

This instruction is similar to add; the only difference is that it
calculates R2-R3 and places the result in R1.  The effect on the
condition code is the same as for add.

The instruction sub Rd,Ra,Rb has operands Ra and Rb and destination
Rd.  It fetches the operands Ra and Rb, calculates the difference Ra -
Rb, and loads the result into the destination Rd.  The effect is Rd :=
Ra - Rb.  For example, sub R5,R12,R2 performs R5 := R12 - R3.

The sub instruction is RRR format with opcode=1.

  Code    Assembly          Effect
  -----   ----------------  ------------------
  162c    sub R6,R2,R12     ; R6 := R2 - R12
  1d13    sub R13,R1,R3     ; R13 := R1 - R3
  
In addition to setting the destination register, the sub instruction
sets several bits in the condition code R15 and may set a bit in the
req register.

---------  ---------------------
 R15.ccG    result > 0 (binary)
 R15.ccg    result > 0 (two's complement)
 R15.ccE    result = 0
 R15.ccl    result < 0 (two's complement)
 R15.ccV    overflow (binary)
 R15.CCv    overflow (two's complement)
 R15.CCc    carry output
---------  ---------------------

### mul

Example: mul R1,R2,R3 ; R1 := R2 * R3

The multiply instruction mul Rd,Ra,Rb calculates the integer (two's
complement) product of the operands Ra and Rb, and places the result
in the destination register Rd.  The mul instruction does not produce
the natural (binary) product.

If the magnitude of the product is too large to be representable as a
16 bit two's complement integer, this is an overflow.  If overflow
occurs, the integer overflow bit is set in the condition code (F15)
and the integer overflow bit is also set in the interrupt request
register (req), and the lower order 16 bits of the product are loaded
into Rd.

---------  ---------------------
 R15.ccg    result > 0 (two's complement)
 R15.ccE    result = 0
 R15.ccl    result < 0 (two's complement)
 R15.CCv    overflow (two's complement)
 R15.CCc    carry output
---------  ---------------------

### div

Example: div R1,R2,R3 ; R1 := R2 / R3, R15 := R2 rem R3

Unlike the other arithmetic operations, the divide instruction div
Rd,Ra,Rb produces two results: the quotient Ra / Rb and the remainder
Ra rem Rb.  It loads the quotient into the destination register Rd,
and the remainder is loaded into R15.

If the destination register Rd is actually R15, then the quotient is
placed in R15, and the remainder is discarded.

The divide instruction doesn't set the condition code, since R15 is
used for the remainder.  Therefore there is no condition code bit to
indicate division by 0.  However, it is easy for a program to detect a
division by 0.

* (Explicit test for error) The program can compare the divisor with 0
  before or after executing the divide instruction, and jump to an
  error handler if the divisor is 0.  This is similar to testing the
  condition code after an add, sub, or mul instruction, but it does
  require two instructions: a compare followed by a conditional jump.
  For example:

~~~~
   div   R1,R2,R3       ; R1 := R2/R3, R15 := R2 rem R3
   cmpeq R4,R3,R0       ; Did we divide by 0?
   jumpt zeroDivide[R0] ; If yes, handle error
~~~~
  
* (Exception) The program can detect division by 0 using an interrupt.
  To do this, enable interrupts and enable the interrupt mask for
  division by 0.  See the section on Interrupts.  This approach does
  not require a compare or jump instruction for each division.

### addc

(Not yet implemented, coming soon)

The **binary add with carry** instruction *addc Rd,Re,Rf* calculates
the sum of the binary numbers in the operand registers Re and Rf as
well as the carry bit in the condition code.  The sum is loaded into
the destination register Rd and the carry output is set in the
condition code register.  Overflow is not possible with this
instruction.

## Accessing memory

A memory address is a 16-bit binary number.  Instructions don't
specify addresses directly; they specify an address with two
components: a *displacement* and an *index*, written as
"displacement[index]".  The displacement is a 16 bit constant, and in
assembly language it may be given as a decimal integer, a hexadecimal
word, or a label.  The index is a register.  For example,$0c45[R5] has a
displacement of 0c45 and an index of R5.

When an instruction executes, the machine takes the displacement and
index and calculates the *effective address*.  This is defined to be
the binary sum of the displacement and the curent value in the index
register.  In the example above, if R5 contains 3, then the effective
address of $0c45[R5] is $0c48.

If you just want to specify an address *a* in an instruction, this can
be written as "a[R0]".  Since R0 contains the constant 0, the
effective address is just *a*.

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

Example: save R2,R9,20[R14] is equivalent to

~~~~
  store  R2,20[R14]
  store  R3,21[R14]
  store  R4,22[R14]
  store  R5,23[R14]
  store  R6,24[R14]
  store  R7,25[R14]
  store  R8,26[R14]
  store  R9,27[R14]
~~~~
  
The **save** instruction stores a sequence of adjacent registers into
memory starting from the effective address. The index register (R14 in
this example) is not changed.  Save is equivalent to a fixed sequence
of store instructions; its purpose of save is to copy the state of
registers into memory during a procedure call or a context switch.

Typically, save is used as part of a procedure call and restore is
used as part of the return.

The instruction *save Re,Rf,gh[Ra]* stores the contents of Re, Re+1,
..., Rf into memory at consecutive locations beginning with
*mem[Ra+gh]*.

The instruction is EXP format, and the offset is limited to 8 bits,
because it is specified in the *gh* field, which is the rightmost 8
bits of the second word of the instruction.  The secondary opcode is
8, which is in the ab field of the first word of the instruction.

The first register to be saved is in the *e* field, and the last
register to be saved is in the *f* field.  The instruction always
stores at least one register.  If *e* and *f* are the same, for
example *save R5,R5,0[F14]* then that register (R5 in the example) is
stored.  If *e* > *f* then the register numbers wrap around  For
example, 

~~~~
   save    R11,R3,3[R5]
~~~~

is equivalent to
~~~~
   store   R11,3[R5]
   store   R12,4[R5]
   store   R13,5[R5]
   store   R14,6[R5]
   store   R15,7[R5]
   store   R0,8[R5]
   store   R1,9[R5]
   store   R2,10[R5]
   store   R3,11[R5]
~~~~

### restore

Example: restore R2,R9,20[R14] is equivalent to

~~~~
  load   R2,20[R14]
  load   R3,21[R14]
  load   R4,22[R14]
  load   R5,23[R14]
  load   R6,24[R14]
  load   R7,25[R14]
  load   R8,26[R14]
  load   R9,27[R14]
~~~~

The **restore** instruction copies a sequence of consecutive memory
locations starting from the effecive address into a sequence of
adjacent registers.  The index register (R14 in this example) is not
changed.  Restore is equivalent to a fixed sequence of load
instructions; its purpose of restore is to restore the state of
registers from memory after a procedure call or a context switch.

Typically, save is used as part of a procedure call and restore is
used as part of the return.

The instruction *restore Re,Rf,gh[Rd]* copies the contents of memory
at consecutive locations beginning with mem[gh+Rf] into registers Re,
Re+1, ..., Rf.

The instruction is EXP format, and the displacement is limited to 8
bits, because it is specified in the gh field (the rightmost 8 bits)
of the second word of the instruction.  The secondary opcode is 9,
which is in the ab field of the first word of the instruction. The
assembly language statement format is RRXEXP.

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

Push a word onto a stack.

Example: push R1,R2,R3 pushes value in R1 onto stack with R2 = current
top and R3 = last address in stack.

Push the word in R1 onto a stack with top R2 and limit R3.  If the
stack is full, nothing is stored into memory and an error is indicated
in the condition code and interrupt request registers; an interrupt
will occur if interrupts are enabled and the stack mask bit is set.

Push has the following semantics:

    if R2<R3
      then R2 := R2+1; mem[R2] := R1
      else R15.sovfl := 1, req.sovfl := 1
  
If R2 = R3 this means the stack completely fills the block, and there
is no space to store a new element.  In this case, the push
instruction does not store R1: it doesn't modify memory outside the
block, and it doesn't overwrite data in the stack.  Instead, the
instruction indicates a stack overflow by setting the sovfl (stack
overflow) bit in the condition code (R15), and it also sets the stack
fault bit in the interrupt request register.  If interrupts are
enabled and the stack fault bit is set in the interrupt mask register,
then an interrupt will occur after the push instruction completes.
There will be no interrupt if interrupts are disabled, or the stack
fault bit is not set in the mask register.

Push is an EXP2 format instruction comprising two words.

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

## Bit operations on words

The following instructions treat a word as a sequence of bits, and
operate on the individual bits.

### inv

The **invert** pseudoinstruction *inv Rd,Ra* inverts the bits in the
operand Ra and places the result in the destination Rd.  Thus bit i of
Rd is set to the logical negation (invert) of bit i of Ra.  The
operand Ra is not changed.

Inverting a bit means changing 0 to 1, and changing 1 to 0.

Example:
~~~~
   lea  R1,$00ff[R0]
   lea  R2,$0f0f[R0]
   inv  R3,R1         ; R3 := inv 00ff      = ff00
~~~~

The inv pseudoinstruction generates a logicw instruction with the
function operand set to perform a logical invert.

### and

The **logical and** pseudoinstruction *and Rd,Ra,Rb* calculates the
logical "and" (conjunction) of the corresponding bits in the operands
Ra and Rb, and places the result in the destination Rd.  Thus bit i of
Rd is the logical and of bit i in Ra and bit i in Rb.  The operand
registers are not changed.

The logical and of two bits is 1 if both bits are 1, and otherwise 0.

Example:
~~~~
   lea  R1,$00ff[R0]
   lea  R2,$0f0f[R0]
   and  R4,R1,R2      ; R4 := 00ff and 0f0f = 000f
~~~~

The and pseudoinstruction generates a logicw instruction with the
function operand set to perform a logical and.

### or

The **logical or** pseudoinstruction *or Rd,Ra,Rb* calculates the
logical "inclusive or" (disjunction) of the corresponding bits in the
operands Ra and Rb, and places the result in the destination Rd. Thus
bit i of Rd is the logical or of bit i in Ra and bit i in Rb.  The
operand registers are not changed.

The logical or of two bits is 1 if either or both of the bits are 1,
and 0 otherwise.

Example:
~~~~
   lea  R1,$00ff[R0]
   lea  R2,$0f0f[R0]
   or   R5,R1,R2      ; R5 := 00ff or  0f0f = 0fff
~~~~

The or pseudoinstruction generates a logicw instruction with the
function operand set to perform a logical or.

### xor

The **logical exclusive or** pseudoinstruction *xor Rd,Ra,Rb*
calculates the logical "exclusive or" of the bits in the operands Ra
and Rb, and places the result in the destination Rd.  Thus bit i of Rd
is the logical xor of bit i in Ra and bit i in Rb.  The operand
registers are not changed.

The logical exclusive or of two bits is 1 if one or the other of the
bits is 1, and 0 otherwise.  The exclusive or of two bits is the same
as the inclusive or if either bit is 0.  The only time it is different
is if both bits are 1: in that case exclusive or gives 0 but inclusive
or gives 1.

Example:
~~~~
   lea  R1,$00ff[R0]
   lea  R2,$0f0f[R0]
   xor  R3,R1,R2      ; R3 := 0ff0
~~~~

The xor pseudoinstruction generates a logicw instruction with the
function operand set to perform an xor.

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

## Bit operations on fields

Several instructions operate on some but not all of the bits in a
word.  All of these instructions can be implemented using combinations
of logic and shift instructions.  They are included in the
architecture for several reasons: (1) these operations are important
for writing interpreters and simulators, and they constutute important
abstractions; (2) they are easier to use and more readable than the
corresponding shifts; (3) they can be implemented efficiently in a
digital circuit and this implementation is an interesting design
problem.

Bits are indexed from the left starting with 0, so the leftmost (most
significant) bit has index 0, and the rightmost (least significant)
has index 15.  A bit index is specified as a 4-bit binary number *i*
such that 0 <= *i* <= 15.
  
A **bit field** is a string of consecutive bits in a word defined by
the sequence of its indices.  For example, W.[6,7,8] means the 3-bit
field comprising indices 6, 7, and 8 in the word W.

Bit field instructions do not enumerate all the bits in a field.
Instead, they specify the field with a pair of 4-bit binary numbers.
The notation W.{s,e} denotes the bit field in W with start index *s*
and end index *e*, where *s* and *e* are 4-bit binary numbers.
Therefore 0 <= *s*,*e* <= 15.  The number of bits in the field (its
size) is max (0, e-s+1).  If e-s+1 <= 0 then the field is empty; this
occurs if e <= s-1.  If *s*=0 and *e*=15 then the size of the field is
16 and it consists of the entire word.

Examples (W is an arbitrary word):

* W.{5,5} is the bit at index 5
* W.{0,0} is the leftmost (most significant) bit
* W.{15,15} is the rightmost (least significant) bit
* W.{0,15} is the field consisting of the entire word W
* W.{1,0} is the empty field containing no bits.  In general, W.{s,e}
  where e<s is the empty field
* ir.{0,4} is the opcode, i.e. the 4-bit field comprising the leftmost
  four bits of the instruction register.
* ir.{8,15} is the ab field of the instruction register, containing
  the expanded operation code for EXP format instructions.

(An alternative way to specify bit fields, not used in Sigma16, would
be to give the starting index and field size.  However, that approach
is less flexible, as the value of the specified field size would range
between 0 and 15, yet there are 16 possible field sizes.  This means
that, depending on precisely how the definition is written, either
empty fields or maximal fields would not be representable.)

### logicw

* General form: logicw Rd,Re,Rf,g
* Instruction format: EXP2
* Assembly format: RRRKEXP
* Rd := fcn g Re Rf

### logicb

* General form: logicb Rd,Re,Rf,g,h
* Rd.h := fcn g Re.h Rf.h
* Assembly format: RRRKKEXP

### extract

The extract instruction obtains the value of an arbitrary field (g,h)
from a source register Re and loads it right-adjusted into the
destination register Rd.

General form: *extract Rd,Re,g,h*
Instruction format: EXP2
Assembly format: RRKKEXP

Semantics:
* The size of the field is max (0, h-g+1)
* Rd.{15+g-h,15} := Re.{g,h}
* Rd.{0,15+g-h-1} := 0*

For example, extract R1,R2,3,5 extracts a field from R2 starting at
bit position 3 and ending with bit position 5.  This 3-bit field is
placed in R1 in the rightmost 3 bits (indices 13, 14, 15); the
leftmost bits (indices 0 through 12) are all set to 0.

The instruction is EXP2 format, where the d field gives the
destination register, the e field gives the source register, and the
field is specified by the g and h fields.  The assembly language
statement format is RRKKEXP.

The extract instruction is not essential: it can be performed by a
left shift followed by a right shift.  *extract Rd,Re,g,h* is
equivalent to

~~~~
   shiftl Rd,Re,g        ; clear bits to left of field
   shiftr Rd,Rd,15-h+g   ; clear bits to right of field
~~~~

For example *extract R1,R2,3,5* is equivalent to (but easier to
understand than) the following:

~~~~
   shiftl  Rd,Re,3
   shiftr  Rd,Rd,13
~~~~


### inject, injecti

The inject instruction obtains a right-adjusted field from a source
register Rf and places it into the specified field (g,h) of the value
in the source register Re; the result is placed into the destination
register Rd.  The injecti instruction is the same as inject, except
that the injected field is inverted.

General form: *inject Rd,Re,Rf,g,h*

General form: *injecti Rd,Re,Rf,g,h*

~~~~
   lea     R1,$ffff[R0]
   lea     R2,$0005[R0]
   inject  R4,R0,R2,4,7  ; R4 := 0500
   inject  R5,R1,R2,4,7  ; R5 := f5ff

   injecti R6,R0,R2,4,7  ; R6 := 0a00
   injecti R7,R1,R2,4,7  ; R6 := faff
~~~~

### field

(Not yet implemented, coming soon)

Pseudoinstruction

The field pseudoinstruction loads a word into the destination register
Rd; this word consists of 1 bits in the specified field (g,h) and 0 in
all other bit positions.  This provides a field mask that can be used
with logic instructions for a variety of purposes.

* General form: *field Rd,g,h*
* Pseudo-instruction:  *injecti Rd,R0,g,h*
* Assembler format: RKK

Semantics
* Rd.i = 1 for g <= i <- h
* Rd.i = 0 for i < g or i > h

Example:

~~~~
 field   R3,4,  ; R3 := 0fc0
~~~~

Using a field mask
* invert it to give negative mask
* and R1 with mask to clear bits outside the field
* and R1 with negative mask to clear only the field 
* xor R1 with mask to invert bits in the field

### execute

(Not yet implemented, coming soon)

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

### testset

(Not yet implemented, coming soon)

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

The instruction format is EXP2.  As there is no operand, the d field
is ignored and the second word of the instruction is omitted.  Thus
every rfi instruction has the same machine language representation:
e000.  The fields are op, d, ab; op=14 to indicate EXP format, d=0
because it is unused, and ab=0 because this is the secondary opcode of
rfi.

This instruction is privileged.  Since the instruction changes the
status register, it can be used to perform a context switch.

## Trap operations

### Halt

### Nonblocking read

### Write

## List of instructions

Table: **Instruction set**

-------------------------------------------------------
 Mne     AL     ML    Op   Effect
------- ----- ------ ---- --------------------
add      RRR   RRR    0      Rd := Ra + Rb

sub      RRR   RRR    1      Rd := r[a] - Rb

mul      RRR   RRR    2      Rd := Ra * Rb

div      RRR   RRR    3      Rd := Ra / Rb,
                             R15 := Ra rem Rb

cmp      RRR   RRR    4      R15 := Ra ? Rb

cmplt    RRR   RRR    5      Rd := Ra < Rb

cmpeq    RRR   RRR    6      Rd := Ra = Rb

cmpgt    RRR   RRR    7      Rd := Ra > Rb

skipf    RRR   RRK    8      if Rd.a=0 then pc := pc+b (Not yet implemented)

skipt    RRR   RRK    9      if Rd.a=1 then pc := pc+b (Not yet implemented)

trap     RRR   RRR    d       

lea      RX    RX     f,0    Rd := Ra+disp

load     RX    RX     f,1    Rd := mem[Ra+disp]

store    RX    RX     f,2    mem[Ra+disp] := Rd

jump     JX    RX     f,3    pc := Ra+disp

jumpc0   KX    RX     f,4    if R15.k=1 then pc := Ra+disp

jumpc1   KX    RX     f,5    if R15.k=0 then pc := Ra+disp

jumpf    RX    RX     f,6    if Rd=0 then pc := Ra+disp

jumpt    RX    RX     f,7    if Rd/=0 then pc := Ra+disp

jal      RX    RX     f,8    Rd := pc, pc := Ra+disp

testset  RX    RX     f,9    Rd := mem[Ra+disp], (Not yet implemented)
                             mem[Ra+disp] := 1
                             
rfi      NO    R      e,0    pc := ipc,
                             status := istatus
                             
execute  RR    RREXP  e,8                             (Not yet implemented)

getctl   RC    RCEXP  e,10   Rd := Rc

putctl   RC    RCEXP  e,11   Rc := Rd

push     RRR   RRREXP e,18

pop      RRR   RRREXP e,19

top      RRR   RRREXP e,1a

shiftl   RRK   RRKEXP e,20   Rd := Ra shl k

shiftr   RRK   RRKEXP e,21   Rd := Ra shr k

extract  RRKK  RRKKEXP e,38

save     RRX   RRXEXP  e,40  mem[Rb+ofs] := Re,
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

Humans write assembly language.  The program is text: red}add
R4,R2,R12.  It's easier to read, you don't need to remember all the
codes, and memory addresses are much easier to handle.
  
The computer executes machine language: The program is words
containing 16-bit numbers: 042c.  It's possible for a digital circuit
(the computer) to execute.  No names for instructions or variables:
everything is a number.
  
A human writes a machine-level program in assembly language.  A
software application called the *assembler* reads it in and translates
it to machine language.  When it sees an instruction mnemonic like add
or div, it replaces it with the operation code (0, 3, or whatever).
The assembler helps with variable names --- the machine language needs
addresses (numbers) and the assembler calculates them
  
*  You can use names (add, div) rather than numeric codes (0, 3)
*  You can use variable names (x, y, sum) rather than memory
  addresses (02c3, 18d2)
*  You write a program in assemply language
*  The \emph{assembler} translates it into machine language

Compilers and assemblers are similar in some ways: both of them
translate a program from one language to another.  The main difference
is that compilers translate between languages that are very different,
while assemblers translate between very similar languages.

Example: a sequence of RRR instructions

Assembly language
~~~~
    add    R3,R5,R1
    sub    R4,R2,R3
    mul    R1,R9,R10   
~~~~

Machine language
~~~~
    0351
    1423
    219a
~~~~




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


### module statement

Declare the name of the module.  The argument must be an indentifier.
The following statement says that this is the object code for module
named abc.

~~~~
module abc
~~~~

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

### org statement

The org statement sets the location counter to a specified address.
Currently the argument must be a a 4-digit hexadecimal constant, such
as 3b9f.

~~~~
org  0a04
~~~~

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

### import statment

~~~~
import mod3,sqr,addr,addr,...
~~~~

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


### equ statement

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

# Programming

(This section under development)

## Structure of a program

Simple ("static") variabls need to be declared with a data statement,
which also gives an initial value.

~~~~
x  data  23
~~~~

This means: allocate a word in memory for x and initialize it to 23.
The data statements should come after the trap instruction that
terminates the program

## How to perform commmon tasks

### Copying one register to another

Sometimes you want to copy a value from one register to another: R3 :=
R12.  There isn't an instruction specifically for this purpose,
because there is no need: just use the add instruction:

~~~~
add R3,R12,R0 ; R3 := R12
~~~~
  
Since R12 + 0 = R12, this copies the value in R12 into R3.  One might
think that this is less efficient than having a special instruction to
perform the copy, but it actually turns out to be more efficient to do
it this way!

## Compilation

There are two ways to handle variables:

The statement-by-statement style: Each statement is compiled
independently.  The pattern is: load, arithmetic, store.
Straightforward but inefficient.
  
The register-variable style: Keep variables in registers across a
group of statements.  Don't need as many loads and stores.  More
efficient.  You have to keep track of whether variables are in memory
or a register.  Use comments to show register usage.  Real compilers
use this style.  Use this style if you like the shorter code it
produces.
  
We'll translate the following program fragment to assembly
language, using each style:

~~~~
x = 50;
y = 2*z;
x = x+1+z;
~~~~

Statement-by-statement style

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

Register-variable style

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
                                
Comparison of the styles

Statement by statement.
* Each statement is compiled into a separate block of code.
* Each statement requires loads, computation, then stores.
* A variable may appear in several different registers.
* There may be a lot of redundant loading and storing.
* The object code corresponds straightforwardly to the source
  code, but it may be unnecessarily long.
  
Register variable
* The instructions corresponding to the statemnts are mixed
  together.
* Some statements are executed entirely in the registers.
* A variable is kept in the same register across many
  statments.
* The use of loads and stores is minimised.
* The object code is concise, but it's harder to see how it
  corresponds to the source code.
* It's possible to have a mixture of the styles: you don't have
  to follow one or the other all the time.

## Errors: avoiding, finding, and fixing

### Robust programming

*Use a systematic programming process

* Start with a high level algorithm
* Then translate that to the low level ("if b then goto label") form
* Translate the low level to assembly language, keeping the higher
  level versions as comments
  
*Use comments both to develop the program and to document it*

* Write the comments first, as you develop the program.  There should
  already be some good comments (e.g. the algorithm) before any
  instructions at all have been written.
* Don't fall into the trap of hacking out instructions and then adding
  comments later: this loses the benefits that documention offers as
  you're writing the code.
  
*How to write good comments*

* Keep the high level and low level algorithms as comments
* Comment each instruction
* Use the comments to explain what your program is doing, not to
  explain what an instruction does.
* Assume that the reader already knows the language, but not the
  details of your program.

### Error messages

### Runtime debugging

*What if an instruction doesn't do what you expected?*

* Execute the program to the point where the mysterious instruction is
  about to be executed, but has not yet executed.  (To do this, you
  can step through the program, or set a breakpoint.)
  
* Make sure you know what the instruction is supposed to do (check the
  User Guide).
  
* Looking at the state of the registers and memory, carefully predict
  what you expect the instruction to do.
  
* Execute the one instruction (click Step) and compare the state of
  the machine with your prediction.
  
* Make sure the instruction has not been modified in memory.  Compare
  the machine language produced by the assembler with the *current*
  contents of the word or words in memory where the instruction is
  located.

### Breakpoints

*(Note: the breakpoint system is not fully implemented yet; the
following describes a temporary breakpoint facility.)*

A breakpoint is the address of an instruction; when the machine is
about to execute that instruction (i.e. when the pc contains that
address) the emulator will halt execution, enabling the programmer to
examine the state of registers and memory.  To set a breakpoint, click
Breakpoint and enter the instruction address you want to stop at in
the dialogue box.  There are several control buttons.  Refresh means
"read the contents of the text in the box, which must be a $ followed
by a 4 hex digit address".  Whenever you change the text, you should
click Refresh.  The Enable button toggles the breakpoint on and off.
The Close button hides the Breakpoint dialogue box.  Here's an
example.  Suppose you want to stop execution of a program at address
00f6:

* Click Breakpoint
* Enter $00f6
* Click Refresh
* Click Enable
* Click Close
* Click Run

The execution will run until the pc becomes equal to 00f6
and will then stop.

Click Refresh, then Enable, then
Close.  Then click Run, and the emulator will run at full speed until
the pc reaches the specified value; then it will stop so you can
examine the state of the machine.


# Circuit

There is a complete digital circuit called M1 that implements the core
of the Sigma16 architecture (apart from the divide instruction).  This
circuit is specified completely down to the level of logic gates and
flip flops.  M1 uses sequential control and does not have instruction
level parallelism.  Its design is written in Hydra, a functional
computer hardware description language.  The circuit can be simulated,
and you can run Sigma16 programs on the circuit.

Follow these steps to run the Sigma16 circuit on the simulator:

* Download Hydra and save it in your workspace.  Suppose its location
  is /c/Users/me/a/b/c/Hydra.

* Open a shell and enter circuit/M1.  Run a test program,
  e.g. ArrayMax.hs, with the following command, which will execute the
  ArrayMax program on the digital circuit.  The simulation will
  produce detailed simulation output showing the values in key
  registers, flip flops, and signals during every clock cycle.

~~~~
ghci -i/c/Users/me/a/b/c/Hydra/src/haskell/ ArrayMaxRun
~~~~

The command can be simplified by defining a .ghci file, which will be
loaded automatically when you invoke ghci.  You can put the .ghci file
in your $HOME directory.  See the GHC User Guide for more details
about configuring ghci.  The .ghci file could contain a definition
similar to this:

~~~~
:set -i/c/Users/a/b/c/Hydra/src/haskell/
~~~~

# Installation

The application currently works with Chrome and Firefox, and
possible Edge.

The software is available on the Internet at the [Sigma16 home
page](https://jtod.github.io/home/Sigma16/), which contains a link to
the latest version, some previous versions, related documents, and
more information about the project.

## How to run Sigma16

There are a number of ways to run the software, but it's recommended
that you try the easiest way.

### The easiest way: just click a link

Run the app with two clicks:

  1. Visit the [Sigma16 homepage](https://jtod.github.io/home/Sigma16/)
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
	
**Version number**

The version number is needed in several places.  To keep it
consistent, there is only one primary place where it should be
specified manually: in the version property in app/package.json.

The makefile extracts the version number from that file, and (1)
defines a make variable; (2) writes the Sigma16/VERSION file with just
the version number, and (3) writes Sigma16/app/version.js which is
just defines the version number as a global constant.  

# About Sigma16

This program is experimental software, and is under development.
Sigma16 consists of several components:

* The Integrated Development Environment (IDE) is written in
  JavaScript, and normally uses a web browser to display the graphical
  user interface.  The assembler, linker, and emulator are also
  implemented in JavaScript and are closely integrated with the IDE.

* The circuits that implement Sigma16 are defined using the Hydra
  hardware description language, which is an embedded domain specific
  language implemented in Haskell.

* The User Guide is written in markdown and prepared for a web browser
  using pandoc.

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

### Copyright

Sigma16 is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Sigma16 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Sigma16.  If not, see <https://www.gnu.org/licenses/>.

### GPL3 License

GNU GENERAL PUBLIC LICENSE
Version 3, 29 June 2007

Copyright © 2007 Free Software Foundation, Inc. <https://fsf.org/>

Everyone is permitted to copy and distribute verbatim copies of this license document, but changing it is not allowed.

Preamble
The GNU General Public License is a free, copyleft license for software and other kinds of works.

The licenses for most software and other practical works are designed to take away your freedom to share and change the works. By contrast, the GNU General Public License is intended to guarantee your freedom to share and change all versions of a program--to make sure it remains free software for all its users. We, the Free Software Foundation, use the GNU General Public License for most of our software; it applies also to any other work released this way by its authors. You can apply it to your programs, too.

When we speak of free software, we are referring to freedom, not price. Our General Public Licenses are designed to make sure that you have the freedom to distribute copies of free software (and charge for them if you wish), that you receive source code or can get it if you want it, that you can change the software or use pieces of it in new free programs, and that you know you can do these things.

To protect your rights, we need to prevent others from denying you these rights or asking you to surrender the rights. Therefore, you have certain responsibilities if you distribute copies of the software, or if you modify it: responsibilities to respect the freedom of others.

For example, if you distribute copies of such a program, whether gratis or for a fee, you must pass on to the recipients the same freedoms that you received. You must make sure that they, too, receive or can get the source code. And you must show them these terms so they know their rights.

Developers that use the GNU GPL protect your rights with two steps: (1) assert copyright on the software, and (2) offer you this License giving you legal permission to copy, distribute and/or modify it.

For the developers' and authors' protection, the GPL clearly explains that there is no warranty for this free software. For both users' and authors' sake, the GPL requires that modified versions be marked as changed, so that their problems will not be attributed erroneously to authors of previous versions.

Some devices are designed to deny users access to install or run modified versions of the software inside them, although the manufacturer can do so. This is fundamentally incompatible with the aim of protecting users' freedom to change the software. The systematic pattern of such abuse occurs in the area of products for individuals to use, which is precisely where it is most unacceptable. Therefore, we have designed this version of the GPL to prohibit the practice for those products. If such problems arise substantially in other domains, we stand ready to extend this provision to those domains in future versions of the GPL, as needed to protect the freedom of users.

Finally, every program is threatened constantly by software patents. States should not allow patents to restrict development and use of software on general-purpose computers, but in those that do, we wish to avoid the special danger that patents applied to a free program could make it effectively proprietary. To prevent this, the GPL assures that patents cannot be used to render the program non-free.

The precise terms and conditions for copying, distribution and modification follow.

TERMS AND CONDITIONS
0. Definitions.
“This License” refers to version 3 of the GNU General Public License.

“Copyright” also means copyright-like laws that apply to other kinds of works, such as semiconductor masks.

“The Program” refers to any copyrightable work licensed under this License. Each licensee is addressed as “you”. “Licensees” and “recipients” may be individuals or organizations.

To “modify” a work means to copy from or adapt all or part of the work in a fashion requiring copyright permission, other than the making of an exact copy. The resulting work is called a “modified version” of the earlier work or a work “based on” the earlier work.

A “covered work” means either the unmodified Program or a work based on the Program.

To “propagate” a work means to do anything with it that, without permission, would make you directly or secondarily liable for infringement under applicable copyright law, except executing it on a computer or modifying a private copy. Propagation includes copying, distribution (with or without modification), making available to the public, and in some countries other activities as well.

To “convey” a work means any kind of propagation that enables other parties to make or receive copies. Mere interaction with a user through a computer network, with no transfer of a copy, is not conveying.

An interactive user interface displays “Appropriate Legal Notices” to the extent that it includes a convenient and prominently visible feature that (1) displays an appropriate copyright notice, and (2) tells the user that there is no warranty for the work (except to the extent that warranties are provided), that licensees may convey the work under this License, and how to view a copy of this License. If the interface presents a list of user commands or options, such as a menu, a prominent item in the list meets this criterion.

1. Source Code.
The “source code” for a work means the preferred form of the work for making modifications to it. “Object code” means any non-source form of a work.

A “Standard Interface” means an interface that either is an official standard defined by a recognized standards body, or, in the case of interfaces specified for a particular programming language, one that is widely used among developers working in that language.

The “System Libraries” of an executable work include anything, other than the work as a whole, that (a) is included in the normal form of packaging a Major Component, but which is not part of that Major Component, and (b) serves only to enable use of the work with that Major Component, or to implement a Standard Interface for which an implementation is available to the public in source code form. A “Major Component”, in this context, means a major essential component (kernel, window system, and so on) of the specific operating system (if any) on which the executable work runs, or a compiler used to produce the work, or an object code interpreter used to run it.

The “Corresponding Source” for a work in object code form means all the source code needed to generate, install, and (for an executable work) run the object code and to modify the work, including scripts to control those activities. However, it does not include the work's System Libraries, or general-purpose tools or generally available free programs which are used unmodified in performing those activities but which are not part of the work. For example, Corresponding Source includes interface definition files associated with source files for the work, and the source code for shared libraries and dynamically linked subprograms that the work is specifically designed to require, such as by intimate data communication or control flow between those subprograms and other parts of the work.

The Corresponding Source need not include anything that users can regenerate automatically from other parts of the Corresponding Source.

The Corresponding Source for a work in source code form is that same work.

2. Basic Permissions.
All rights granted under this License are granted for the term of copyright on the Program, and are irrevocable provided the stated conditions are met. This License explicitly affirms your unlimited permission to run the unmodified Program. The output from running a covered work is covered by this License only if the output, given its content, constitutes a covered work. This License acknowledges your rights of fair use or other equivalent, as provided by copyright law.

You may make, run and propagate covered works that you do not convey, without conditions so long as your license otherwise remains in force. You may convey covered works to others for the sole purpose of having them make modifications exclusively for you, or provide you with facilities for running those works, provided that you comply with the terms of this License in conveying all material for which you do not control copyright. Those thus making or running the covered works for you must do so exclusively on your behalf, under your direction and control, on terms that prohibit them from making any copies of your copyrighted material outside their relationship with you.

Conveying under any other circumstances is permitted solely under the conditions stated below. Sublicensing is not allowed; section 10 makes it unnecessary.

3. Protecting Users' Legal Rights From Anti-Circumvention Law.
No covered work shall be deemed part of an effective technological measure under any applicable law fulfilling obligations under article 11 of the WIPO copyright treaty adopted on 20 December 1996, or similar laws prohibiting or restricting circumvention of such measures.

When you convey a covered work, you waive any legal power to forbid circumvention of technological measures to the extent such circumvention is effected by exercising rights under this License with respect to the covered work, and you disclaim any intention to limit operation or modification of the work as a means of enforcing, against the work's users, your or third parties' legal rights to forbid circumvention of technological measures.

4. Conveying Verbatim Copies.
You may convey verbatim copies of the Program's source code as you receive it, in any medium, provided that you conspicuously and appropriately publish on each copy an appropriate copyright notice; keep intact all notices stating that this License and any non-permissive terms added in accord with section 7 apply to the code; keep intact all notices of the absence of any warranty; and give all recipients a copy of this License along with the Program.

You may charge any price or no price for each copy that you convey, and you may offer support or warranty protection for a fee.

5. Conveying Modified Source Versions.
You may convey a work based on the Program, or the modifications to produce it from the Program, in the form of source code under the terms of section 4, provided that you also meet all of these conditions:

a) The work must carry prominent notices stating that you modified it, and giving a relevant date.
b) The work must carry prominent notices stating that it is released under this License and any conditions added under section 7. This requirement modifies the requirement in section 4 to “keep intact all notices”.
c) You must license the entire work, as a whole, under this License to anyone who comes into possession of a copy. This License will therefore apply, along with any applicable section 7 additional terms, to the whole of the work, and all its parts, regardless of how they are packaged. This License gives no permission to license the work in any other way, but it does not invalidate such permission if you have separately received it.
d) If the work has interactive user interfaces, each must display Appropriate Legal Notices; however, if the Program has interactive interfaces that do not display Appropriate Legal Notices, your work need not make them do so.
A compilation of a covered work with other separate and independent works, which are not by their nature extensions of the covered work, and which are not combined with it such as to form a larger program, in or on a volume of a storage or distribution medium, is called an “aggregate” if the compilation and its resulting copyright are not used to limit the access or legal rights of the compilation's users beyond what the individual works permit. Inclusion of a covered work in an aggregate does not cause this License to apply to the other parts of the aggregate.

6. Conveying Non-Source Forms.
You may convey a covered work in object code form under the terms of sections 4 and 5, provided that you also convey the machine-readable Corresponding Source under the terms of this License, in one of these ways:

a) Convey the object code in, or embodied in, a physical product (including a physical distribution medium), accompanied by the Corresponding Source fixed on a durable physical medium customarily used for software interchange.
b) Convey the object code in, or embodied in, a physical product (including a physical distribution medium), accompanied by a written offer, valid for at least three years and valid for as long as you offer spare parts or customer support for that product model, to give anyone who possesses the object code either (1) a copy of the Corresponding Source for all the software in the product that is covered by this License, on a durable physical medium customarily used for software interchange, for a price no more than your reasonable cost of physically performing this conveying of source, or (2) access to copy the Corresponding Source from a network server at no charge.
c) Convey individual copies of the object code with a copy of the written offer to provide the Corresponding Source. This alternative is allowed only occasionally and noncommercially, and only if you received the object code with such an offer, in accord with subsection 6b.
d) Convey the object code by offering access from a designated place (gratis or for a charge), and offer equivalent access to the Corresponding Source in the same way through the same place at no further charge. You need not require recipients to copy the Corresponding Source along with the object code. If the place to copy the object code is a network server, the Corresponding Source may be on a different server (operated by you or a third party) that supports equivalent copying facilities, provided you maintain clear directions next to the object code saying where to find the Corresponding Source. Regardless of what server hosts the Corresponding Source, you remain obligated to ensure that it is available for as long as needed to satisfy these requirements.
e) Convey the object code using peer-to-peer transmission, provided you inform other peers where the object code and Corresponding Source of the work are being offered to the general public at no charge under subsection 6d.
A separable portion of the object code, whose source code is excluded from the Corresponding Source as a System Library, need not be included in conveying the object code work.

A “User Product” is either (1) a “consumer product”, which means any tangible personal property which is normally used for personal, family, or household purposes, or (2) anything designed or sold for incorporation into a dwelling. In determining whether a product is a consumer product, doubtful cases shall be resolved in favor of coverage. For a particular product received by a particular user, “normally used” refers to a typical or common use of that class of product, regardless of the status of the particular user or of the way in which the particular user actually uses, or expects or is expected to use, the product. A product is a consumer product regardless of whether the product has substantial commercial, industrial or non-consumer uses, unless such uses represent the only significant mode of use of the product.

“Installation Information” for a User Product means any methods, procedures, authorization keys, or other information required to install and execute modified versions of a covered work in that User Product from a modified version of its Corresponding Source. The information must suffice to ensure that the continued functioning of the modified object code is in no case prevented or interfered with solely because modification has been made.

If you convey an object code work under this section in, or with, or specifically for use in, a User Product, and the conveying occurs as part of a transaction in which the right of possession and use of the User Product is transferred to the recipient in perpetuity or for a fixed term (regardless of how the transaction is characterized), the Corresponding Source conveyed under this section must be accompanied by the Installation Information. But this requirement does not apply if neither you nor any third party retains the ability to install modified object code on the User Product (for example, the work has been installed in ROM).

The requirement to provide Installation Information does not include a requirement to continue to provide support service, warranty, or updates for a work that has been modified or installed by the recipient, or for the User Product in which it has been modified or installed. Access to a network may be denied when the modification itself materially and adversely affects the operation of the network or violates the rules and protocols for communication across the network.

Corresponding Source conveyed, and Installation Information provided, in accord with this section must be in a format that is publicly documented (and with an implementation available to the public in source code form), and must require no special password or key for unpacking, reading or copying.

7. Additional Terms.
“Additional permissions” are terms that supplement the terms of this License by making exceptions from one or more of its conditions. Additional permissions that are applicable to the entire Program shall be treated as though they were included in this License, to the extent that they are valid under applicable law. If additional permissions apply only to part of the Program, that part may be used separately under those permissions, but the entire Program remains governed by this License without regard to the additional permissions.

When you convey a copy of a covered work, you may at your option remove any additional permissions from that copy, or from any part of it. (Additional permissions may be written to require their own removal in certain cases when you modify the work.) You may place additional permissions on material, added by you to a covered work, for which you have or can give appropriate copyright permission.

Notwithstanding any other provision of this License, for material you add to a covered work, you may (if authorized by the copyright holders of that material) supplement the terms of this License with terms:

a) Disclaiming warranty or limiting liability differently from the terms of sections 15 and 16 of this License; or
b) Requiring preservation of specified reasonable legal notices or author attributions in that material or in the Appropriate Legal Notices displayed by works containing it; or
c) Prohibiting misrepresentation of the origin of that material, or requiring that modified versions of such material be marked in reasonable ways as different from the original version; or
d) Limiting the use for publicity purposes of names of licensors or authors of the material; or
e) Declining to grant rights under trademark law for use of some trade names, trademarks, or service marks; or
f) Requiring indemnification of licensors and authors of that material by anyone who conveys the material (or modified versions of it) with contractual assumptions of liability to the recipient, for any liability that these contractual assumptions directly impose on those licensors and authors.
All other non-permissive additional terms are considered “further restrictions” within the meaning of section 10. If the Program as you received it, or any part of it, contains a notice stating that it is governed by this License along with a term that is a further restriction, you may remove that term. If a license document contains a further restriction but permits relicensing or conveying under this License, you may add to a covered work material governed by the terms of that license document, provided that the further restriction does not survive such relicensing or conveying.

If you add terms to a covered work in accord with this section, you must place, in the relevant source files, a statement of the additional terms that apply to those files, or a notice indicating where to find the applicable terms.

Additional terms, permissive or non-permissive, may be stated in the form of a separately written license, or stated as exceptions; the above requirements apply either way.

8. Termination.
You may not propagate or modify a covered work except as expressly provided under this License. Any attempt otherwise to propagate or modify it is void, and will automatically terminate your rights under this License (including any patent licenses granted under the third paragraph of section 11).

However, if you cease all violation of this License, then your license from a particular copyright holder is reinstated (a) provisionally, unless and until the copyright holder explicitly and finally terminates your license, and (b) permanently, if the copyright holder fails to notify you of the violation by some reasonable means prior to 60 days after the cessation.

Moreover, your license from a particular copyright holder is reinstated permanently if the copyright holder notifies you of the violation by some reasonable means, this is the first time you have received notice of violation of this License (for any work) from that copyright holder, and you cure the violation prior to 30 days after your receipt of the notice.

Termination of your rights under this section does not terminate the licenses of parties who have received copies or rights from you under this License. If your rights have been terminated and not permanently reinstated, you do not qualify to receive new licenses for the same material under section 10.

9. Acceptance Not Required for Having Copies.
You are not required to accept this License in order to receive or run a copy of the Program. Ancillary propagation of a covered work occurring solely as a consequence of using peer-to-peer transmission to receive a copy likewise does not require acceptance. However, nothing other than this License grants you permission to propagate or modify any covered work. These actions infringe copyright if you do not accept this License. Therefore, by modifying or propagating a covered work, you indicate your acceptance of this License to do so.

10. Automatic Licensing of Downstream Recipients.
Each time you convey a covered work, the recipient automatically receives a license from the original licensors, to run, modify and propagate that work, subject to this License. You are not responsible for enforcing compliance by third parties with this License.

An “entity transaction” is a transaction transferring control of an organization, or substantially all assets of one, or subdividing an organization, or merging organizations. If propagation of a covered work results from an entity transaction, each party to that transaction who receives a copy of the work also receives whatever licenses to the work the party's predecessor in interest had or could give under the previous paragraph, plus a right to possession of the Corresponding Source of the work from the predecessor in interest, if the predecessor has it or can get it with reasonable efforts.

You may not impose any further restrictions on the exercise of the rights granted or affirmed under this License. For example, you may not impose a license fee, royalty, or other charge for exercise of rights granted under this License, and you may not initiate litigation (including a cross-claim or counterclaim in a lawsuit) alleging that any patent claim is infringed by making, using, selling, offering for sale, or importing the Program or any portion of it.

11. Patents.
A “contributor” is a copyright holder who authorizes use under this License of the Program or a work on which the Program is based. The work thus licensed is called the contributor's “contributor version”.

A contributor's “essential patent claims” are all patent claims owned or controlled by the contributor, whether already acquired or hereafter acquired, that would be infringed by some manner, permitted by this License, of making, using, or selling its contributor version, but do not include claims that would be infringed only as a consequence of further modification of the contributor version. For purposes of this definition, “control” includes the right to grant patent sublicenses in a manner consistent with the requirements of this License.

Each contributor grants you a non-exclusive, worldwide, royalty-free patent license under the contributor's essential patent claims, to make, use, sell, offer for sale, import and otherwise run, modify and propagate the contents of its contributor version.

In the following three paragraphs, a “patent license” is any express agreement or commitment, however denominated, not to enforce a patent (such as an express permission to practice a patent or covenant not to sue for patent infringement). To “grant” such a patent license to a party means to make such an agreement or commitment not to enforce a patent against the party.

If you convey a covered work, knowingly relying on a patent license, and the Corresponding Source of the work is not available for anyone to copy, free of charge and under the terms of this License, through a publicly available network server or other readily accessible means, then you must either (1) cause the Corresponding Source to be so available, or (2) arrange to deprive yourself of the benefit of the patent license for this particular work, or (3) arrange, in a manner consistent with the requirements of this License, to extend the patent license to downstream recipients. “Knowingly relying” means you have actual knowledge that, but for the patent license, your conveying the covered work in a country, or your recipient's use of the covered work in a country, would infringe one or more identifiable patents in that country that you have reason to believe are valid.

If, pursuant to or in connection with a single transaction or arrangement, you convey, or propagate by procuring conveyance of, a covered work, and grant a patent license to some of the parties receiving the covered work authorizing them to use, propagate, modify or convey a specific copy of the covered work, then the patent license you grant is automatically extended to all recipients of the covered work and works based on it.

A patent license is “discriminatory” if it does not include within the scope of its coverage, prohibits the exercise of, or is conditioned on the non-exercise of one or more of the rights that are specifically granted under this License. You may not convey a covered work if you are a party to an arrangement with a third party that is in the business of distributing software, under which you make payment to the third party based on the extent of your activity of conveying the work, and under which the third party grants, to any of the parties who would receive the covered work from you, a discriminatory patent license (a) in connection with copies of the covered work conveyed by you (or copies made from those copies), or (b) primarily for and in connection with specific products or compilations that contain the covered work, unless you entered into that arrangement, or that patent license was granted, prior to 28 March 2007.

Nothing in this License shall be construed as excluding or limiting any implied license or other defenses to infringement that may otherwise be available to you under applicable patent law.

12. No Surrender of Others' Freedom.
If conditions are imposed on you (whether by court order, agreement or otherwise) that contradict the conditions of this License, they do not excuse you from the conditions of this License. If you cannot convey a covered work so as to satisfy simultaneously your obligations under this License and any other pertinent obligations, then as a consequence you may not convey it at all. For example, if you agree to terms that obligate you to collect a royalty for further conveying from those to whom you convey the Program, the only way you could satisfy both those terms and this License would be to refrain entirely from conveying the Program.

13. Use with the GNU Affero General Public License.
Notwithstanding any other provision of this License, you have permission to link or combine any covered work with a work licensed under version 3 of the GNU Affero General Public License into a single combined work, and to convey the resulting work. The terms of this License will continue to apply to the part which is the covered work, but the special requirements of the GNU Affero General Public License, section 13, concerning interaction through a network will apply to the combination as such.

14. Revised Versions of this License.
The Free Software Foundation may publish revised and/or new versions of the GNU General Public License from time to time. Such new versions will be similar in spirit to the present version, but may differ in detail to address new problems or concerns.

Each version is given a distinguishing version number. If the Program specifies that a certain numbered version of the GNU General Public License “or any later version” applies to it, you have the option of following the terms and conditions either of that numbered version or of any later version published by the Free Software Foundation. If the Program does not specify a version number of the GNU General Public License, you may choose any version ever published by the Free Software Foundation.

If the Program specifies that a proxy can decide which future versions of the GNU General Public License can be used, that proxy's public statement of acceptance of a version permanently authorizes you to choose that version for the Program.

Later license versions may give you additional or different permissions. However, no additional obligations are imposed on any author or copyright holder as a result of your choosing to follow a later version.

15. Disclaimer of Warranty.
THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM “AS IS” WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

16. Limitation of Liability.
IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

17. Interpretation of Sections 15 and 16.
If the disclaimer of warranty and limitation of liability provided above cannot be given local legal effect according to their terms, reviewing courts shall apply local law that most closely approximates an absolute waiver of all civil liability in connection with the Program, unless a warranty or assumption of liability accompanies a copy of the Program in return for a fee.

END OF TERMS AND CONDITIONS

How to Apply These Terms to Your New Programs
If you develop a new program, and you want it to be of the greatest possible use to the public, the best way to achieve this is to make it free software which everyone can redistribute and change under these terms.

To do so, attach the following notices to the program. It is safest to attach them to the start of each source file to most effectively state the exclusion of warranty; and each file should have at least the “copyright” line and a pointer to where the full notice is found.

    <one line to give the program's name and a brief idea of what it does.>
    Copyright (C) <year>  <name of author>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
Also add information on how to contact you by electronic and paper mail.

If the program does terminal interaction, make it output a short notice like this when it starts in an interactive mode:

    <program>  Copyright (C) <year>  <name of author>
    This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.
    This is free software, and you are welcome to redistribute it
    under certain conditions; type `show c' for details.
The hypothetical commands `show w' and `show c' should show the appropriate parts of the General Public License. Of course, your program's commands might be different; for a GUI interface, you would use an “about box”.

You should also get your employer (if you work as a programmer) or school, if any, to sign a “copyright disclaimer” for the program, if necessary. For more information on this, and how to apply and follow the GNU GPL, see <https://www.gnu.org/licenses/>.

The GNU General Public License does not permit incorporating your program into proprietary programs. If your program is a subroutine library, you may consider it more useful to permit linking proprietary applications with the library. If this is what you want to do, use the GNU Lesser General Public License instead of this License. But first, please read <https://www.gnu.org/licenses/why-not-lgpl.html>.
