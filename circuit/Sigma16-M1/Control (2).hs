{-# LANGUAGE NamedFieldPuns #-}
-- allows concise definition of record of control signals

module Control where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import ControlSignals

----------------------------------------------------------------------
--			  Control Algorithm
----------------------------------------------------------------------

{- This is the high level control algorithm, written using assignment
statements to describe the effect that will take place at the end of a
clock cycle.  Each statement is decorated with the list of control
signals that must be asserted during the clock cycle to make the
datapath perform the operation.  Some of the Sigma16 instructions are
unimplemented, but the key ones are all defined.

repeat forever
  st_instr_fet:
    ir := mem[pc], pc++;
       {ctl_ma_pc, ctl_ir_ld, ctl_x_pc, ctl_alu=alu_inc, ctl_pc_ld}
  st_dispatch:
  case ir_op of

    0 -> -- add instruction
        st_add:
          reg[ir_d] := reg[ir_sa] + reg[ir_sb]
             assert [ctl_alu_abcd=0000, ctl_rf_alu, ctl_rf_ld]

    1 -> -- sub instruction
        st_sub:
          reg[ir_d] := reg[ir_sa] - reg[ir_sb]
             assert [ctl_alu_abcd=0100, ctl_rf_alu, ctl_rf_ld]

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

    7 -> -- cmp instruction
        -- unimplemented

    8 -> -- inv instruction
        -- unimplemented

    9 -> -- and instruction
        -- unimplemented

    10 -> -- or instruction
        -- unimplemented

    11 -> -- xor instruction
        -- unimplemented

    12 -> -- test instruction
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
        -- further instructions using registers

      case ir_sb of
        xxx -> -- shiftl instruction
         -- unimplemented

        xxx -> -- shiftr instruction
        -- unimplemented

    15 -> -- expand to RX format
        -- This code allows expansion to a two-word format with
        -- an address operand.  The instruction is determined by a
        -- secondary opcode in the b field of the ir

      case ir_sb of
        0 -> -- lea instruction
            st_lea0:
              ad := mem[pc], pc++;
              assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc,
                      ctl_alu_abcd=1100, ctl_pc_ld]
            st_lea1:
              reg[ir_d] := reg[ir_sa] + ad
                assert [ctl_y_ad, ctl_alu=alu_add, ctl_rf_alu,
                        ctl_rf_ld]

        1 -> -- load instruction
            st_load0:
              ad := mem[pc], pc++;
                assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc,
                        ctl_alu_abcd=1100, ctl_pc_ld]
            st_load1:
              ad := reg[ir_sa] + ad
                assert [set ctl_y_ad, ctl_alu_abcd=0000,
                        set ctl_ad_ld, ctl_ad_alu]
            st_load2:
              reg[ir_d] := mem[ad]
                assert [ctl_rf_ld]

        2 -> -- store instruction
            st_store0:
              ad := mem[pc], pc++;
                assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc,
                        ctl_alu_abcd=1100, ctl_pc_ld]
            st_store1:
              ad := reg[ir_sa] + ad
                assert [ctl_y_ad, ctl_alu_abcd=0000,
                        set ctl_ad_ld, ctl_ad_alu]
            st_store2:
              mem[addr] := reg[ir_d]
                assert [ctl_rf_sd, ctl_sto]

        3 -> --  jump instruction
            st_jump0:
              ad := mem[pc], pc++;
                assert [ctl_ma_opc, ctl_ad_ld, ctl_x_pc,
                        ctl_alu=alu_inc, ctl_pc_ld]
            st_jump1:
              ad := reg[ir_sa] + ad, pc := reg[ir_sa] + ad
                assert [ctl_y_ad, ctl_alu=alu_add, ctl_ad_ld,
                        ctl_ad_alu, ctl_pc_ld]

        4 -> --  jumpf instruction
            st_jumpf0:
              ad := mem[pc], pc++;
                assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc,
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
                assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc,
                        ctl_alu_abcd=1100, ctl_pc_ld, ctl_rf_sd]
              case reg[ir_sa] = 0 of
                False:
                  st_jumpt1:
                    pc := reg[ir_sa] + ad
                      assert [ctl_y_ad, ctl_alu_abcd=0000, ctl_pc_ld]
                True: -- nothing to do

        6 -> -- jumpc0 instruction

        7 -> -- jumpc1 instruction

*** CHANGED *** ???
        8 -> -- jal instruction
            st_jal0:
              ad := mem[pc], pc++;
                assert [ctl_ma_pc, ctl_ad_ld, ctl_x_pc,
                        ctl_alu_abcd=1100, ctl_pc_ld]
            st_jal1:
              reg[ir_d] := pc, ad := reg[ir_sa] + ad,
              pc := reg[ir_sa] + ad
              assert [ctl_rf_ld, ctl_rf_pc, ctl_y_ad,
                      ctl_alu_abcd=0000,
                      ctl_ad_ld, ctl_ad_alu, ctl_pc_ld, ctl_pc_ad]

        9 -> -- nop
        10 -> -- nop
        11 -> -- nop
        12 -> -- nop
        13 -> -- nop
        14 -> -- nop
        15 -> -- nop
-}


----------------------------------------------------------------------
--			   Control circuit
----------------------------------------------------------------------

control
  :: CBit a
  => a -> [a] -> a
  -> (CtlState a, a, CtlSig a)

control reset ir cond = (ctlstate,start,ctlsigs)
  where

      ir_op = field ir  0 4       -- instruction opcode
      ir_d  = field ir  4 4       -- instruction destination register
      ir_sa = field ir  8 4       -- instruction source a register
      ir_sb = field ir 12 4       -- instruction source b register

      start = orw
        [reset,st_load2,st_lea1,st_add,st_sub,
         st_mul0,st_store2,st_cmpeq,st_cmplt,st_cmpgt,
         st_jumpt1, and2 st_jumpt0 (inv cond),
         st_jumpf1, and2 st_jumpf0 cond,
         st_jump1, st_jal1, st_trap0]

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

      ctl_rf_ld   = orw [st_load2,st_lea1,st_add,st_sub,
                           st_cmpeq,st_cmplt,st_cmpgt,st_jal1]
      ctl_rf_pc   = orw [st_jal1]
      ctl_rf_alu  = orw [st_lea1,st_add,st_sub,st_cmpeq,
                           st_cmplt,st_cmpgt]
      ctl_rf_sd   = orw [st_store2,st_jumpf0]
      ctl_alu_a   = orw [st_instr_fet,st_load0,st_store0,st_lea0,
                         st_cmpeq,st_cmplt,st_cmpgt,st_jumpf0,st_jal0]
      ctl_alu_b   = orw [st_instr_fet,st_load0,st_store0,st_lea0,
                         st_sub,st_cmpeq,
                         st_cmplt,st_cmpgt,st_jumpf0,st_jal0]
      ctl_alu_c   = orw [st_cmpeq,st_cmpgt]
      ctl_alu_d   = orw [st_cmpeq,st_cmplt,st_cmpgt]  -- ????? not cmpeq
      ctl_ir_ld   = orw [st_instr_fet]
      ctl_pc_ld   = orw [st_instr_fet,st_load0,st_lea0,st_store0,
                           st_jumpt0,st_jumpt1,st_jumpf0,st_jumpf1,
                           st_jump0,st_jump1,st_jal0,st_jal1]
      ctl_pc_ad   = orw [st_jal1]
      ctl_ad_ld   = orw [st_load0,st_load1,st_lea0,st_store0,
                         st_store1,st_jumpt0,st_jumpf0,st_jump0,st_jump1,
                         st_jal0,st_jal1]
      ctl_ad_alu  = orw [st_load1,st_store1,st_jump1,st_jal1]
      ctl_ma_pc   = orw [st_instr_fet,st_load0,st_lea0,st_store0,
                           st_jumpt0,st_jumpf0,st_jump0,st_jal0]
      ctl_x_pc    = orw [st_instr_fet,st_load0,st_lea0,st_store0,
                           st_jumpt0,st_jumpf0,st_jump0,st_jal0]
      ctl_y_ad    = orw [st_load1,st_store1,st_lea1,st_jumpt1,
                         st_jumpf1,st_jump1,st_jal1]
      ctl_sto     = orw [st_store2]

      ctlsigs = CtlSig
        {ctl_alu_a,  ctl_alu_b,  ctl_alu_c,  ctl_alu_d,
         ctl_x_pc,   ctl_y_ad,   ctl_rf_ld,  ctl_rf_pc,
         ctl_rf_alu, ctl_rf_sd,  ctl_ir_ld,  ctl_pc_ld,
         ctl_pc_ad,  ctl_ad_ld,  ctl_ad_alu, ctl_ma_pc,
         ctl_sto}

      ctlstate = CtlState
        {st_instr_fet,
         st_dispatch,
         st_add,
         st_sub,
         st_mul0,
         st_cmpeq,
         st_cmplt,
         st_cmpgt,
         st_trap0,
         st_lea0, st_lea1,
         st_load0, st_load1, st_load2,
         st_store0, st_store1, st_store2,
         st_jump0, st_jump1,
         st_jumpf0, st_jumpf1,
         st_jumpt0, st_jumpt1,
         st_jal0, st_jal1}

