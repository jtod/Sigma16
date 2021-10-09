Quick start:  How to run a Sigma16 program on the M1 CPU circuit
See user guide in docs directory

  $ ghci
  ghci> :load Run
  ghci> :main programs/Add
  M1> run
  M1> quit
  ghci> :quit

Some useful commands

  ghci:main programs/ArrayMax
  M1> break reset    set breakpoint
  M1> run            run until reset=1
  M1> (enter)        run just one  clock cycle
  M1> regs           print the contents of the register file
  M1> mem 0 20       print memory from address 0 to 20
  
See the M1 User Guide in the docs directory
