------------------------------------------------------------------------
-- The Sigma16_M1 Architecture
-- John O'Donnell
-- February 2010
------------------------------------------------------------------------


-- *********************************************************************
-- Tutorial 7 Part 1
-- Add a new control signal to the system, named ctl_new_signal
-- Changes to the file are labelled with this string:
--     $$$ Exercise 7.1
-- *********************************************************************

{- This module M1, a digital circuit that implements a subset of the
Sigma16 instruction set architecture. To run the circuit, use the
RunM1 simulation driver along with a main module that defines a
machine language program.  See ArrayMax.hs for an example. -}

module Tut7_1_M1 where  -- $$$ Exercise 7.1

import Hydra
import Hydra.StandardCircuits

------------------------------------------------------------------------
{- Instruction set architecture of Sigma16

RRR instructions
________________________________________________________________________

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

Table: **Instructions represented in RRR format**
________________________________________________________________________

RX instructions
________________________________________________________________________

 op   b   format  mnemonic   operands   action
---- --- -------- ---------- ---------- ---------------------------------
 f    0     RX    lea        Rd,x[Ra]   Rd := x+Ra
 f    1     RX    load       Rd,x[Ra]   Rd := mem[x+Ra]
 f    2     RX    store      Rd,x[Ra]   mem[x+Ra] := Rd
 f    3     RX    jump       x[Ra]      pc := x+Ra
 f    4     RX    jumpf      Rd,x[Ra]   if Rd==0 then pc := x+Ra
 f    5     RX    jumpt      Rd,x[Ra]   if Rd/=0 then pc := x+Ra
 f    6     RX    jal        Rd,x[Ra]   Rd := pc, pc := x+Ra

Table: **Instructions represented by RX and X formats**
________________________________________________________________________
-}


------------------------------------------------------------------------
--			Arithmetic/Logic Unit
------------------------------------------------------------------------

{- The ALU performs addition, subtraction, negation, increment, and
comparision for <, =, and >.  It receives four control signals
(a,b,c,d) that determine which function it will compute.  These are
shown in the following table, where don't-care values are left blank.

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

The control algorithm defines all control signals, regardless of what
operation is being performed.  The signal values for the ALU are:

  alu_add = 0000
  alu_sub = 0100
  alu_neg = 1000
  alu_inc = 1100
  alu_lt  = 1101
  alu_eq  = 1110
  alu_gt  = 1111
-}

alu n (a,b,c,d) x y = (cout,z)
  where
    wzero = fanout n zero
    wone = boolword n one
    negating = xor2 a b
    comparing = and3 a b (or2 c d)
    x' = mux2w (a,b) x x wzero x
    y' = mux2w (a,b) y (winv y) (winv x) (mux1w (or2 c d) wone y)
    xy = bitslice2 x' y'
    (cout,sum) = rippleAdd negating xy
    (lt,eq,gt) = rippleCmp xy
    lt_tc = mux2 (xy!!0) lt zero one lt
    eq_tc = eq
    gt_tc = mux2 (xy!!0) gt one zero gt
    comp_bool = mux2 (c,d) zero lt_tc eq_tc gt_tc
    comp_word = boolword n comp_bool
    z = mux1w comparing sum comp_word


------------------------------------------------------------------------
--			   Control signals
------------------------------------------------------------------------

{- The behaviour of the datapath is determined by the values of the
control signals.  A naming convention is used for these signals.  Each
control signal has a name with three parts, separated by _.  The first
part is ctl (to indicate that it's a control signal).  The second part
is the name of the subsystem in the datapath which is being
controlled, and the third part is the specific operation being
commanded by the signal.

  ctl_rf_ld    Load  register file (if 0, remain unchanged)
  ctl_rf_pc    Input to register file is pc (if 0, check ctl_rf_alu)
  ctl_rf_alu   Input to register file is ALU output r (if 0, use m)
  ctl_rf_sd    Use ir_d as source a address (if 0, use ir_sa)
  ctl_alu_a    4-bit alu operation code (see section on the ALU)
  ctl_alu_b      "
  ctl_alu_c      "
  ctl_alu_d      "
  ctl_ir_ld    Load ir register (if 0, remain unchanged)
  ctl_pc_ld    Load pc register (if 0, remain unchanged)
  ctl_pc_ad    Input to pc is ad (if 0, r)
  ctl_adr_ld    Load ad register (if 0, remain unchanged)
  ctl_adr_alu   Obtain ad input from alu (if 0, from memory data input)
  ctl_ma_pc    Transmit pc on memory address bus (if 0, transmit addr)
  ctl_x_pc     Transmit pc on x (if 0, transmit reg[sa])
  ctl_y_ad     Transmit ad on y (if 0, transmit reg[sb])
  ctl_sto      Memory store (if 0, fetch)
  ctl_new_signal  -- $$$ Exercise 7.1
-}


------------------------------------------------------------------------
--			       Datapath
------------------------------------------------------------------------

{- The datapath contains the registers, computational systems, and
interconnections.  It has two inputs: a set of control signals
provided by the control unit, and a data word from the either the
memory system or the DMA input controller. -}

datapath control memdat = (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p)
  where

-- Give names to the individual control signals

      (ctl_rf_ld,  ctl_rf_pc,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
       ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad,  ctl_adr_ld,  ctl_adr_alu,
       ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto,
       ctl_new_signal)  -- $$$ Exercise 7.1
         = control

-- Specify the size parameters

      n = 16    -- word size
      k =  4    -- the register file contains 2^k registers 

-- Registers

      (a,b) = regfile n k ctl_rf_ld ir_d rf_sa rf_sb p
      ir = reg n ctl_ir_ld memdat
      pc = reg n ctl_pc_ld q
      ad = reg n ctl_adr_ld (mux1w ctl_adr_alu memdat r)

-- ALU

      (ovfl,r) = alu n ctl_alu_op x y

-- Internal processor signals

      x = mux1w ctl_x_pc a pc         -- alu input 1
      y = mux1w ctl_y_ad b ad         -- alu input 2
      rf_sa = mux1w ctl_rf_sd ir_sa ir_d  -- a = reg[rf_sa]
      rf_sb = ir_sb                       -- b = reg[rf_sb]
      p  = mux1w ctl_rf_pc            -- data input to register file
             (mux1w ctl_rf_alu memdat r)
             pc
      q = mux1w ctl_pc_ad r ad        -- input to pc
      ma = mux1w ctl_ma_pc ad pc      -- memory address - to memory
      md = a                          -- memory data - to memory for store
      cond = orw a                    -- boolean for conditional jumps

-- Instruction fields

      ir_op = field ir  0 4           -- instruction opcode
      ir_d  = field ir  4 4           -- instruction destination register
      ir_sa = field ir  8 4           -- instruction source a register
      ir_sb = field ir 12 4           -- instruction source b register


------------------------------------------------------------------------
--			  Control Algorithm
------------------------------------------------------------------------

{-

repeat forever
  st_instr_fet:
    ir := mem[pc], pc++;
       {ctl_ma_pc, ctl_ir_ld, ctl_x_pc, ctl_alu=alu_inc, ctl_pc_ld}
  st_dispatch:
  case ir_op of

    0 -> -- add instruction
        st_add:
          reg[ir_d] := reg[ir_sa] + reg[ir_sb]
             {ctl_alu_abcd=0000, ctl_rf_alu, ctl_rf_ld}

    1 -> -- sub instruction
        st_sub:
          reg[ir_d] := reg[ir_sa] - reg[ir_sb]
             {ctl_alu_abcd=0100, ctl_rf_alu, ctl_rf_ld}

    2 -> -- mul instruction
        -- unimplemented

    3 -> -- div instruction
        -- unimplemented

    4 -> -- cmplt instruction
        st_cmplt:
          reg[ir_d] := reg[ir_sa] < reg[ir_sb]
            assert [ctl_alu_abcd=1101, ctl_rf_alu, ctl_rf_ld]

    5 -> -- cmpeq instruction
        st_cmpeq:
          reg[ir_d] := reg[ir_sa] = reg[ir_sb]
            assert [ctl_alu_abcd=1110, ctl_rf_alu, ctl_rf_ld]

    6 -> -- cmpgt instruction
        st_cmpgt:
          reg[ir_d] := reg[ir_sa] > reg[ir_sb]
            assert [ctl_alu_abcd=1111, ctl_rf_alu, ctl_rf_ld]

    7 -> -- inv instruction
        -- unimplemented

    8 -> -- and instruction
        -- unimplemented

    9 -> -- or instruction
        -- unimplemented

    10 -> -- xor instruction
        -- unimplemented

    11 -> -- shiftl instruction
        -- unimplemented

    12 -> -- shiftr instruction
        -- unimplemented

    13 -> -- trap instruction
        st_trap0:
          -- The trap instruction is not implemented, but the
          -- simulation driver can detect that a trap has been
          -- executed by observing when the control algorithm
          -- enters this state.  The simulation driver can then
          -- terminate the simulation.

    14 -> -- expand to XX format
        -- This code allows expansion to a two-word format with
        -- room for many more opcode values
        -- unimplemented; there are currently no XX instructions

    15 -> -- expand to RX format

      case ir_sb of
        0 -> -- lea instruction
            st_lea0:
              ad := mem[pc], pc++;
              assert [ctl_ma_pc, ctl_adr_ld, ctl_x_pc,
                      ctl_alu_abcd=1100, ctl_pc_ld]
            st_lea1:
              reg[ir_d] := reg[ir_sa] + ad
                assert [ctl_y_ad, ctl_alu=alu_add, ctl_rf_alu,
                        ctl_rf_ld]

        1 -> -- load instruction
            st_load0:
              ad := mem[pc], pc++;
                assert [ctl_ma_pc, ctl_adr_ld, ctl_x_pc,
                        ctl_alu_abcd=1100, ctl_pc_ld]
            st_load1:
              ad := reg[ir_sa] + ad
                assert [set ctl_y_ad, ctl_alu_abcd=0000,
                        set ctl_adr_ld, ctl_adr_alu]
            st_load2:
              reg[ir_d] := mem[ad]
                assert [ctl_rf_ld]

        2 -> -- store instruction
            st_store0:
              ad := mem[pc], pc++;
                assert [ctl_ma_pc, ctl_adr_ld, ctl_x_pc,
                        ctl_alu_abcd=1100, ctl_pc_ld]
            st_store1:
              ad := reg[ir_sa] + ad
                assert [set ctl_y_ad, ctl_alu_abcd=0000,
                        set ctl_adr_ld, ctl_adr_alu]
            st_store2:
              mem[addr] := reg[ir_d]
                assert [ctl_rf_sd, ctl_sto]

        3 -> --  jump instruction
            st_jump0:
              ad := mem[pc], pc++;
                assert [ctl_ma_opc, ctl_adr_ld, ctl_x_pc,
                        ctl_alu=alu_inc, ctl_pc_ld]
            st_jump1:
              ad := reg[ir_sa] + ad, pc := reg[ir_sa] + ad
                assert [ctl_y_ad, ctl_alu=alu_add, ctl_adr_ld,
                        ctl_adr_alu, ctl_pc_ld]

        4 -> --  jumpf instruction
            st_jumpf0:
              ad := mem[pc], pc++;
                assert [ctl_ma_pc, ctl_adr_ld, ctl_x_pc,
                        ctl_alu_abcd=1100, ctl_pc_ld, ctl_rf_sd]
              case reg[ir_sa] = 0 of
                False: -- nothing to do
                True:
                  st_jumpf1:
                    pc := reg[ir_sa] + ad
                      assert [ctl_y_ad, ctl_alu_abcd=0000, ctl_pc_ld]

        5 -> -- jumpt instruction
            st_jumpt0:
              ad := mem[pc], pc++;
                assert [ctl_ma_pc, ctl_adr_ld, ctl_x_pc,
                        ctl_alu_abcd=1100, ctl_pc_ld, ctl_rf_sd]
              case reg[ir_sa] = 0 of
                False:
                  st_jumpt1:
                    pc := reg[ir_sa] + ad
                      assert [ctl_y_ad, ctl_alu_abcd=0000, ctl_pc_ld]
                True: -- nothing to do

        6 -> -- jal instruction
            st_jal0:
              ad := mem[pc], pc++;
                assert [ctl_ma_pc, ctl_adr_ld, ctl_x_pc,
                        ctl_alu_abcd=1100, ctl_pc_ld]
            st_jal1:
              reg[ir_d] := pc, ad := reg[ir_sa] + ad, pc := reg[ir_sa] + ad
                assert [ctl_rf_ld, ctl_rf_pc, ctl_y_ad, ctl_alu_abcd=0000,
                        ctl_adr_ld, ctl_adr_alu, ctl_pc_ld, ctl_pc_ad]

        7 -> -- nop
        8 -> -- nop
        9 -> -- nop
        10 -> -- nop
        11 -> -- nop
        12 -> -- nop
        13 -> -- nop
        14 -> -- nop
        15 -> -- nop
-}


------------------------------------------------------------------------
--			   Control circuit
------------------------------------------------------------------------

control
  :: Clocked a
  => a -> [a] -> a
  -> ([a], a, (a,a,a,a,(a,a,a,a),a,a,a,a,a,a,a,a,a,a)) -- $$$ Exercise 7.1
                                                       -- added another a

control reset ir cond = (state,start,ctl_signals)
  where

      ir_op = field ir  0 4       -- instruction opcode
      ir_d  = field ir  4 4       -- instruction destination register
      ir_sa = field ir  8 4       -- instruction source a register
      ir_sb = field ir 12 4       -- instruction source b register

      start = orw
        [reset,st_nop,st_load2,st_lea1,st_add,st_sub,st_neg,
         st_mul0,st_store2,st_cmpeq,st_cmplt,st_cmpgt,
         st_jumpt1, and2 st_jumpt0 (inv cond),
         st_jumpf1, and2 st_jumpf0 cond,
         st_jump1, st_jal1, st_trap1]

      st_instr_fet = dff start
      st_dispatch  = dff st_instr_fet

      pRRR = demux4w ir_op st_dispatch
      pXX  = demux4w ir_sb (pRRR!!14)
      pRX  = demux4w ir_sb (pRRR!!15)

      st_lea0 = dff (pRX!!0)
      st_lea1 = dff st_lea0

      st_load0  = dff (pRX!!1)
      st_load1  = dff st_load0
      st_load2  = dff st_load1

      st_store0 = dff (pRX!!2)
      st_store1 = dff st_store0
      st_store2 = dff st_store1

      st_jump0  = dff (pRX!!3)
      st_jump1  = dff st_jump0

      st_jumpf0 = dff (pRX!!4)
      st_jumpf1 = dff (and2 st_jumpf0 (inv cond))

      st_jumpt0 = dff (pRX!!5)
      st_jumpt1 = dff (and2 st_jumpt0 cond)

      st_jal0   = dff (pRX!!6)
      st_jal1   = dff st_jal0

      st_add    = dff (pRRR!!0)
      st_sub    = dff (pRRR!!1)
      st_mul0   = dff (pRRR!!2)
      st_div0   = dff (pRRR!!3)
      st_cmplt  = dff (pRRR!!4)
      st_cmpeq  = dff (pRRR!!5)
      st_cmpgt  = dff (pRRR!!6)
      st_trap0  = dff (pRRR!!13)
      st_trap1  = dff st_trap0

      st_nop    = dff (pRRR!!0) -- deprecated
      st_neg    = zero -- deprecated

      state =
        [st_instr_fet, st_dispatch, st_add, st_sub,
         st_mul0,
         st_cmpeq, st_cmplt, st_cmpgt, st_trap0, st_trap1,
         st_lea0, st_lea1, st_load0, st_load1, st_load2,
         st_store0, st_store1, st_store2, st_jump0, st_jump1,
         st_jumpf0, st_jumpf1, st_jumpt0, st_jumpt1,
         st_jal0, st_jal1]

      ctl_rf_ld   = orw [st_load2,st_lea1,st_add,st_sub,
                           st_neg,st_cmpeq,st_cmplt,st_cmpgt,st_jal1]
      ctl_rf_pc   = orw [st_jal1]
      ctl_rf_alu  = orw [st_lea1,st_add,st_sub,st_neg,st_cmpeq,
                           st_cmplt,st_cmpgt]
      ctl_rf_sd   = orw [st_store2,st_jumpf0]
      ctl_alu_a   = orw [st_instr_fet,st_load0,st_store0,st_lea0,
                         st_cmpeq,st_cmplt,st_cmpgt,st_jumpf0,st_jal0]
      ctl_alu_b   = orw [st_instr_fet,st_load0,st_store0,st_lea0,
                         st_sub,st_cmpeq,
                         st_cmplt,st_cmpgt,st_jumpf0,st_jal0]
      ctl_alu_c   = orw [st_cmpeq,st_cmpgt]
      ctl_alu_d   = orw [st_cmpeq,st_cmplt,st_cmpgt]
      ctl_ir_ld   = orw [st_instr_fet]
      ctl_pc_ld   = orw [st_instr_fet,st_load0,st_lea0,st_store0,
                           st_jumpt0,st_jumpt1,st_jumpf0,st_jumpf1,
                           st_jump0,st_jump1,st_jal0,st_jal1]
      ctl_pc_ad   = orw [st_jal1]
      ctl_adr_ld  = orw [st_load0,st_load1,st_lea0,st_store0,
                         st_store1,st_jumpt0,st_jumpf0,st_jump0,st_jump1,
                         st_jal0,st_jal1]
      ctl_adr_alu = orw [st_load1,st_store1,st_jump1,st_jal1]
      ctl_ma_pc   = orw [st_instr_fet,st_load0,st_lea0,st_store0,
                           st_jumpt0,st_jumpf0,st_jump0,st_jal0]
      ctl_x_pc    = orw [st_instr_fet,st_load0,st_lea0,st_store0,
                           st_jumpt0,st_jumpf0,st_jump0,st_jal0]
      ctl_y_ad    = orw [st_load1,st_store1,st_lea1,st_jumpt1,
                         st_jumpf1,st_jump1,st_jal1]
      ctl_sto     = orw [st_store2]
      ctl_alu_op  = (ctl_alu_a,ctl_alu_b,ctl_alu_c,ctl_alu_d)
      ctl_new_signal = orw [] -- $$$ Exercise 7.1

      ctl_signals =
        (ctl_rf_ld,  ctl_rf_pc,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
         ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad,  ctl_adr_ld,  ctl_adr_alu,
         ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto,
         ctl_new_signal) -- $$$ Exercise 7.1


------------------------------------------------------------------------
-- System
------------------------------------------------------------------------

m1_system reset dma dma_a dma_d =
  let n = 16
      (ctl_state, ctl_start, ctl_signals) = control reset ir cond
      ctl_sto = get_ctl_sto ctl_signals
      datapath_outputs = datapath ctl_signals m_out
      (ma,md,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) = datapath_outputs
      m_sto = or2 dma ctl_sto
      m_addr = mux1w dma ma dma_a
      m_real_addr = field m_addr (n-8) 8
      m_data = mux1w dma md dma_d
      m_out = memw n 8 m_sto m_real_addr m_data
  in (ctl_state, ctl_start, ctl_signals,
      datapath_outputs,
      m_sto, m_addr, m_real_addr, m_data, m_out)

{- get_ctl_sto is a projection that extracts the ctl_sto field from
the entire control word. -}

get_ctl_sto (ctl_rf_ld,  ctl_rf_pc, ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
   ctl_ir_ld,  ctl_pc_ld,  ctl_pc_ad, ctl_adr_ld, ctl_adr_alu,
   ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto,
   ctl_new_signal) -- $$$ Exercise 7.1
     = ctl_sto
