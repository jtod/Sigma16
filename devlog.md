# Development log for Sigma16

## Log

2019-07-11 Thursday
* Worked just on jtod.github.io page.  It's awkward to edit manually
  on the web interface (just one file at a time, and can't copy a
  directory into it).  I made a local git repository
  homepage/jtod.github.io and cloned the github page into that.  Then
  I edited this by copying the relevant files from the current Sigma16
  repository, and made an index.html and a directory Sigma16 with its
  own index.html, and then created a subdirectory for a testing
  version; that's where the files were copied into.  Then, visiting my
  github page in the browser, I got the home page (my index.html), and
  navigated to the Sigma16 directory and from there to the testing
  directory.  This launched and ran correctly.  So this is basically
  how to provide a release that can be run directly from the web.

2019-07-10 Wednesday
* Worked on the emulator; lea and add are implemented and working.

2019-07-09 Tuesday
* Installed node.js, npm, and electron, got an electron demo working,
  and got Sigma16 to run in a standalone app.  There are some problems
  with fonts etc but it now runs both in the browser and standalone.
  
2019-07-08 Monday
* Created branch modularize for changing the program from script files
  to modules, in order to avoid dumping all the definitions in all the
  files into one global namespace.

2019-07-07 Sunday
* Implemented a boot function that assumes the current module is valid
  and standalone.  This boots the example add program into the
  processor memory.  Started the emulator: it fetches the next
  instruction, increments the pc register, splits the instruction into
  fields, and dispatches on the opcode.  There is an rrr function that
  takes a function argument for the particular operation, and the xx
  and rx functions don't yet do a secondary dispatch on the secondary
  opcode.  This is all working but the memory highlighting isn't being
  cleared correctly.

2019-07-06 Saturday
* Added an object code generator to the assembler; it now generates
  the object code with 5 words per line on hexdata statements.  The
  assembler doesn't yet parse the hexdata statements.  Got a book from
  Amazon for Kindle (on Spectre laptop): CSS Secrets by Lea Verou;
  this looks excellent.

2019-07-05 Friday
* Working with git and github.  The Sigma16 directory has been
  reorganized and the files have been added to git and committed.
  There is a .gitignore file that ignores the development notes log
  file.
  
2019-07-04 Thursday
* Created web page on github, installed git, reorganized directory
  structure in Sigma16, renamed the directory to be just Sigma16
  rather than Sigma16-3.0.16 (which is the current version).  Plan to
  use git facilities for making releases and giving them version
  numbers.
  
2019-07-03
* Looking into using git and github for Sigma16; github could provide
  an online host for the browser versions.
  
2019-07-01 Monday
* Changed the assembler to use m.asmStmt etc, rather than just
  asmStmt, where m is the current module
  
2019-06-30 Sunday
* Yesterday and today resumed working on Sigma16.  Looked at
  introducing an array of modules; the aim is to provide a clean way
  to transmit the object code from the assembler to the linker and
  then to the emulator.

2019-06-07 Friday
* Changed version to 3.0.14.  Work on the GUI.  Try to separate the
  components of the doc window sizing: one function to set the size of
  the doc window, and another to handle detection of movement of the
  vertical divider.  Then it should be possible to initialize the size
  so that the editor buffer is properly sized (currently the editor
  buffer is narrow, and it snaps to fill the space when the vertical
  divider is moved and the doc section is resized).  Succeeded in
  implementing the two buttons for growing/shrinking the user guide
  section.  These work, although there is a little glitch when the
  direction (shrinking or growing) is changed.  It would be a good
  idea (maybe) to have the grow/shrink buttons operate repeatedly as
  long as the mouse is down.  But the approach now looks promising.

2019-06-04 Tuesday
* The assembler is mostly working; it generates RRR, RR, RX, JX, and
  data statements correctly, and produces a good assembly listing.
   
2019-06-04 Monday
* Adjustments to the assembler.  The opcode table (the instruction set
  spec definitions) need to have the right opcodes inserted.  Need to
  fix the displacements for RX instructions.
  
2019-06-03 Monday
* The assembler is largely working.  Data statements allow integers,
  including signed integers, and hex constants.  Code is generated for
  RRR, RR, RX, and JX.  The displacements are not correct, but the
  symbol table is ok.

2019-06-02 Sunday
* Progress on the assembler pass 2
  
2019-05-31 Friday
* Added parsing and fields in statement object for RR and JX formats,
  and made a test file containing correct and invalid statements for
  testing the assembler and error messages
  
2019-05-29 Wednesday
* Fixed up the object representation for an assembly language
  statement, including creating the object and printing it.  The
  assembler passes now take a statement object and modify it by
  filling in fields.

2019-05-20 Monday
* In version 3.0.12.  Work on parsing operands for the assembler.

2019-05-17 Friday
* Implemented the changes to memory accesses.  There are a couple of
  test functions which can be invoked from the console; this is a good
  approach to testing because the test functions can affect the GUI
  but they don't need to have buttons on the GUI.

2019-05-16 Thursday
* Began revision of the memory operations.  The idea is that store and
  fetch will push the address onto a stack of accessed addresses,
  instead of modifying the memory strings by highlighting the
  address.  Then, when all the memory accesses for an operation have
  finished, the highlighting and the joining of the memory strings for
  the displays will be performed.  This will be considerably faster
  than doing all the highlighting and joining for each operation; for
  example a store instruction will access three addresses and there's
  no need to do the expensive join three times.  Furthermore, it's
  possible that the store and fetch (with pushing the address) will be
  fast enough to be used for full-speed mode, where the stack of
  accessed addresses can simply be cleared after each instruction.
  
2019-05-14 Tuesday
* Add top level buttons for adjusting the width of the user guide
  section without needing to drag the vertical bar between the system
  and the guide
* Add a button in File page to insert an example into the editor text
  buffer

2019-05-09 Thursday
* The boot function takes hex constants from linker text area and
  loads them into the memory.  The font size isn't right.  Should make
  memory update collect a list of transactions, and then display them
  in one step, to avoid repeated joins to reconstruct the memory
  display for each operation.

2019-05-07 Tuesday
* Parsing fields and RRR operands

2019-05-06 Monday
* Parser handling the fields properly

2019-05-05 Sunday
* Plan to work on parser for the linker.  The linker statements will
  also be valid assembly language syntax.  First split a source line
  into fields, then parse the fields independently, rather than trying
  to handle the entire line with a regular expression.  Splitting into
  fields first will simplify the parsing and also improve error
  messages.

2019-05-03 Friday
* Resumed working on the program.  Started a function to parse
  statements for the linker, reading about regular expressions.  A
  couple of days ago I copied the draft program onto a private area in
  my web page.  It didn't work on my iPhone although parts of the gui
  did display.  This might be because the iPhone has an older browser;
  should try on a more recent iPad or Chrome Book.

2019-04-24 Wednesday
* Met with Martin Sobzky, Fionnuala, and Cordy.  Discussed using
  Sigma16 for operating system fundamentals, and compilation and
  languages.
  

2019-04-23 Tuesday
* Improved the layout of the processor status.  Now each section is a
  separate div, including the section label as well as the content,
  and the section has its own border.  Beforehand, the section label
  wasn't part of the section; the labels and the contents were laid
  out in the overall grid.  Now each section is a separate div, which
  is also a grid, and the sections are laid out in the top level
  processor status area div, which is also a grid.  The layout looks
  better now.  There are two problems: in the Input/Output section,
  the horizontal width is fixed, and it doesn't grow as more space
  becomes available.  Also, the I/O section is about 1 em too high if
  the input buffer text area is present (commenting that out prevents
  this problem).

2019-04-22 Monday
* Implemented an array of all registers, updated the put and get
  methods to indicate that the register has been highlighted, and
  implemented a function to clear all the register highlighting.  This
  is working for all registers, including the register file and the
  control registers.
  
2019-04-21 Sunday
* Confirmed that the program won't be able to control scrolling in the
  user guide because of the cross platform restrictions.  These are
  just files in the same directory, but it is apparantly impossible
  without setting up a web server on the local computer, which is
  completely out of the question.
  
2019-04-19 Friday

* Playing at KH today

2019-04-18 Thursday

* Took the cat to the vet; she doesn't have a chip

* I tried reading a text file into a text area, the same way the user
  guide is read into innerHTML, but the result is not the contents of
  the text file but rather a description of an object.  This is a
  confirmation of what I had concluded before.  Also, there is
  confirmation on Stack Overflow that you can't read a local file.
  
* There are several things I can do about accessing the example
  programs.  Two or three of them could be defined as string constants
  in the JavaScript program, so they would always be easily
  available.  The installation directory will contain an Examples
  folder, and I could explain in the File page that the user can
  navigate to the installation directory (or the Examples directory)
  and then open a file.  It would be up to the user to know where this
  directory is.  Finally, I could have links to the examples on the
  server, in case the user has an Internet connection and is running
  Sigma16 with web access.  Need to confirm whether this will actually
  work; it might not.

* Did just a little more with the memory access, and planned how to
  organize this.

2019-04-17 Wednesday

* We took in the cat in the morning, and had lunch with Fionnuala and
  Liam

* I implemented a first draft of the memory, using an array to hold
  the memory locations.  Experimented: it turns out to be faster to
  hold an additional array with the string representation of each
  element and to join these, rather than to generate the full string
  for each update.
  
2019-04-16 Tuesday

* Defined intToHex4 and intToBit, to show words and bits in the display

* Abstracted the code to generate the registers, and gave it a
  parameter to show the value.  This makes it possible to use the same
  function to generate a register holding a word and a register
  holding a bit.  I used this to define a number of the registers
  
2019-04-15 Monday

* Try the experiments itemized Sunday: removing the outer tags from
  the user guide, and inserting the guide into the gui.html, and see
  if either of these approaches enables the main JavaScript program to
  visit a particular place in the guide section of the gui.  But they
  both do not work.
  
* Improved the grid layout of the processor state.  All the names of
  the guide lines have been changed from hline1, hline2, etc., to
  symbolic names.  A new horizontal line was introduced for the
  headers of the instruction decode and emulator, and the spacing
  between the lines was changed from a fixed value (6em) to
  min-content.  This has closed up the layout, and later a little bit
  of spacing can be added to the actual register names, values, etc.
  
* Tried defining a global variable in Sigma16gui.js and printing it in
  a script in the user guide index.html, but this doesn't work: the
  variable is not in scope.  There seem to be entirely separate scopes
  for the main program and the innerHTML; perhaps this is related to
  restrictions on cross site scripting.
  
* Another idea is to use the appendChild method of the node, rather
  than setting innerHTML.  Would that make the scope of the user guide
  visible to the main program?
  
* It's going to take a lot of experimentation; I'll do this using the
  welcome text, which is just directly edited html, rather than the
  user guide, which is generated by pandoc.  
  
* I defined objects to represent the pc and ir registers, with values,
  elements to display the values in the gui, and methods to put, get,
  and refresh.  This is working for the pc and ir.  The variables to
  hold the elements are defined but not initialized in the datatypes
  js file, and they are given values in the gui file on the
  window.onload event handler; this ensures that the elements exist
  before they are assigned to the variables.  The objects and methods
  are working.  Next step is to write a function that will generate
  one of these, as the definition for the pc and ir are almost
  identical, varying only in the element they use to display the value.  

2019-04-14 Sunday

* Improved the presentation of the subsystems in the processor state.
  Everything is now in a grid, and aligned fairly well, although the
  register groups have too much vertical space (perhaps adjust the
  grid container and use auto sizing).  A monspace font is now used,
  and the headings are centered and bold.
  
* Tried unsuccessfully to make a JavaScript function that could scroll
  the user guide to a particular point.  This isn't working, and there
  is nothing easily findable on the web to show how.  It seems that
  the anchors in the innerHTML are not visible to the main program.
  Several ideas to try: (1) remove the outer elements: the doctype,
  the html, the head, and the body tags; (2) try inserting the entire
  text of the user guide directly into the main gui.html file and see
  if this works (it would be inelegant but could be automated by the
  makefile).  

2019-04-13 Saturday

* Reading about fonts.  It's necessary to select suitable fonts for
  displaying the registers as well as the programs, and it will also
  be essential to be able to calculate exactly how high these are, in
  order to implement scrolling in the memory and program displays.
  
* Putting in a rudimentary Input/Output section.  This will also go
  into the Processor State grid.

2019-04-12 Friday

* Looked into mechanisms to allow highlighting in the editor text
  area.  This is a problem because text areas cannot be highlighted
  without tricky programming.  I found and bookmarked a page
  explaining how to do it by overlaying a text area with an html, so
  you can type into the textarea but will see the overlying html,
  which can contain formatting.

* Created a div for the assembler listing.  This needs to be html, not
  a text area, for syntax highlighting and formatting error messages
  etc.  At first this didn't display with the right dimensions, but
  eventually the CSS worked.  Then I created similar divs for the
  processor listing and the linker.
  
* The html areas (now in linker, assembler, processor) overflow the
  text below the bottom of the area.  Need to get it to use scrollbars
  and avoid overflowing.  Setting overflow: scroll; in the css file
  doesn't work.  But this was working with the welcome pane, so it
  shouldn't be necessary to set this with JavaScript.  AssemberText
  has the right dimensions, but not the html inside it.  After
  defining a div with id = AsmTextHtml, and defining height and width
  to be 100%, and setting overflow: scroll, it works.  Do the same for
  the linker and processor areas for Html display.  It's better to set
  overflow: auto; this shows the scrollbar only when the content would
  overflow, and it does this independently for x and y directions.
  
* I've been working with a small program consisting of html, css, and
  JavaScript, and working up from full frame, adding functionality
  piece by piece.  Now that the basic layout is working (with some
  issues still), I've reinstated the file reading and downloading code
  from version 3.0.3, and this is working.

* Putting in grids for the processor pane, to show the control
  registers.  The registers, instruction decode, and register file are
  displaying, and a rudimentary memory display.
  

2019-04-11 Thursday 3.0.6

* Got the html for the Welcome pane to size properly.  This was done
  by looking at the DOM for this, and putting in suitable CSS
  entries.  Also, I simplified the welcome html; it doesn't start with
  doctype but just with <html>.  I did the same with the User Guide,
  and now both of these are rendering properly and sizing correctly,
  as the window is resized or the barrier between the left and right
  middle sections is dragged.

* I also put in a dummy row of buttons for each pane, to get a
  placeholder for each of these.
  
* One problem is that the dragging to adjust the left/right middle is
  slow, and now it only works dragging to the left.  When dragging to
  the right, it doesn't take effect until the mouse reaches the right
  side of the html.  This is the same behavior I saw earlier.

2019-04-10 Wednesday

* Yesterday I got separate programs with (a) vertical resizing of the
  editor text area and (b) the slider to adjust space between left and
  right parts of middle section.  Today I got these to work together.
  
* There are several glitches in the layout.  I fixed one of these.
  The editor text area was narrow, not filling the available
  horizontal space, and not adjusting when the middle slider is used.
  But I put in a new statement in the left/right adjustment event
  handler, and set the style.width for the text area to be the same as
  the left box width.  And this works!  Now the editor text area fills
  the horizontal space, even after the entire browser window is
  resized, or when the left/right slider is moved.
  
* Issues.  When the editor text area gets a scroll bar, it overlaps
  the left/right drag barrier.  Probably need to make the text area
  slightly narrower.  The editor text area height calculation isn't
  quite right.  When the Welcome pane and User Guide get html, it
  doesn't size correctly, although when plain text is used in those
  places it is sized correctly.
  
* Today I got a Yard-o-led ball point pen, pencil, and rollerball pen.

2019-04-09 Tuesday

* Planning to continue working on the text vertical resizing problem,
  and will also resume reading about JavaScript and start a separate
  thread developing the low level arithmetic operations.  Here are
  some possible ways forward on the text vertical resizing problem:
  
  - A flex box with a column should work but doesn't.  Should the
    flex-direction be changed to flex flow?  Any other fixes to this
    approach?
	
  - Maybe a grid would work.  Can I define a grid and anchor the top
    grid line to the top edge of the middle left area, anchor the
    bottom grid line to the bottom edge, let some grid items have a
    fixed vertical size, and let the remaining grid items grow to fill
    the space?
	
  - I could use the approach from Version 2 of Sigma16: calculate the
    height of the text area by subtracting the height of the fixed
    objects from the height of the viewport, and then setting it
    explicitly.
	

2019-04-08 Monday

* I made a fresh minimal file with html and css, and started working
  systematically from the top level objects down.  I got it to the
  point that the middle section looks good, with a middle left
  section, a middle right one, and a middle barrier between them which
  later can be used as the resize handle.  But then, when I put a row
  of buttons and a row of text in the middle left section, the text
  just would not resize to fill the vertical space available.  I spent
  a lot of time fiddling with variations on the css, reading the book
  on css, and looking up on Stack Overflow, which was useless as usual.

2019-04-07 Sunday

* Didn't get much done; preparing for the Book Harvest.  Read a little
  of the documentation for css.

2019-04-06 Saturday

* Got file reader to work, and also saving a file by downloading it.
  The saved file goes into the Downloads folder, and it isn't possible
  to navigate to where you prefer to save it.  Saved version 3.0.3
  with the working file read/write, and starting version 3.0.4
  
2019-04-05 Friday

* Got the middle panel, containing the gui and the user manual, to
  resize the two sections.  This is essentially the flexexperiment
  program, adapted into the full Sigma16 gui.  It isn't working
  perfectly but it's good progress.
  
2019-04-04 Thursday

* Got flexexperiment to work; this has a div with two horizontal
  sections with short height (suitable for buttons etc) and with a
  text area below.  The text area fills all the remaining space in the
  window, and it resizes properly as the browser window is resized.  A
  big problem in the 2.4 version of Sigma16 has been getting the text
  areas to resize, and I had to resort to a kludge with JavaScript
  code to try to measure the available space and then to set a
  property.  It would be much better to get the resizing all to happen
  automatically using flex.  I think the key point here is that the
  parent containers (html and body) need to have height set to 100%.

* Started this file, from an older todo.md file, and changed its name
  to log.md.

2019-03-23 Saturday

* During the morning, Cordy and I talked a couple of times about the
  JavaScript Sigma16, and I gradually started thinking it might make
  sense for me to do this.  The user interface is already built using
  HTML5 and CSS and some JavaScript, and it would actually be easier
  to do it entirely with those tools.  I could probably rewrite the
  emulator and assembler easily in JS.  During the afternoon and
  evening I read about JavaScript, and got several books on Kindle.

2019-03-22 Friday.  Start of the JavaScript rewrite of Sigma16.

* Cordy suggested writing a JavaScript implementation of Sigma16.  She
  was primarily concerned with what would happen if ThreepennyGUI
  becomes unsupported at some time in the future.  I had already been
  thinking that the GUI needs redesign and reimplementation, and it
  may be better to do this directly using html, css, and JavaScript
  rather than doing it indirectly through Haskell and Threepenny.
  Much of the current Haskell pro,gram deals with the user interface,
  and that perhaps could be done more transparently using JavaScript.
  It should be possible to write the emulator and assembler also in
  JavaScript, and this would mean that Sigma16 could run in a browser,
  with no need to install Haskell, or Stack, or to download a binary.

## Problems

* The Middle Section (the gui and the user manual) doesn't have the
  correct vertical layout.  This may be because of setting the height
  to 100%.  Perhaps use flex, or use it correctly.
  
* When more space is given to the user manual, by sliding the boundary
  to the left, the user manual doesn't grow.  It just slides to the
  left and leaves empty space to its right.
  
* Resizing the boundary between the gui and the user manual.  The
  resizing is working as of 2019-04-05 but when the mouse is moved to
  the left, there is no effect until the mouse reaches the right side
  of the user manual, and then it jumps to the right by the width of
  the manual.  I had at first thought it was slow moving to the right,
  but that isn't the case: it doesn't trigger moving until the mouse
  reaches a specific point.

## To do

### processor

memory display goes up to ffff, then 0000 again

### files

https://gitlab.haskell.org/ghc/ghc/issues/9752

Student saved rtf file containing formatting codes, and got the
message 

FC Write: save as
Writing 51731 bytes to C:\Users\heath_000\Documents\OrderedListsExercise
Error while saving file: C:\Users\heath_000\Documents\OrderedListsExercise: commitBuffer; invalid argument (invalid character)
