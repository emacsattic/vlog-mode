
//^ [register address]
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
`define HSCTL    32'h2010_0000  // HS    control
`define VSCTL    32'h2010_0004  // VS    control
`define GMCTL    32'h2010_0008  // Gamma control
`define HSCFG1   32'h2010_000C  // HS    config_1
`define HSCFG2   32'h2010_0010  // HS    config_2
`define VSCFG1   32'h2010_0014  // VS    config_1
`define VSCFG2   32'h2010_0018  // VS    config_2
`define DMASTS   32'h2010_001C  // DMA   states
`define DDCCTL   32'h2010_0020  // DDC   control
`define HS_YUV   32'h2010_0024  // Coefs for YVU in HS
`define VS_Y     32'h2010_0064  // Coefs for Y   in VS
`define VS_UV    32'h2010_00A4  // Coefs for UV  in VS

module m_vpp_regman (
    //^ [ports]
    // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    vpp_clk,
    vpp_rst_b,                                  /**/ /* test */

    // OCP interface
    i_lrbiu_mcmd,                               // [3]
    i_lrbiu_maddr,                              // [32]
    i_lrbiu_mdata,                              // [32]
    o_lrbiu_scmdaccept,                         // [1]
    o_lrbiu_sdata,                              // [32]

    // HSCTL, VSCTL, GMCTL
    o_hs_work,                                  // [1] bypass HS
    o_vs_work,                                  // [1] bypass VS
    o_gm_work);                                 // [1] bypass Gamma

input             vpp_clk;
input             vpp_rst_b;                    //
input     [1:0]   i_lrbiu_mcmd;                 //
input     [31:0]  i_lrbiu_maddr;
input     [31:0]  i_lrbiu_mdata;                /*dsf*/
output            o_lrbiu_scmdaccept;           //
output    [31:0]  o_lrbiu_sdata;
output    [31:0]  sdfaf;
output            o_hs_work;
output            o_vs_work;
output            o_gm_work;

// data output mux
reg       [23:0]  vs_coefuv_data_o;
reg       [23:0]  vs_coefuv_data_r;
reg       [31:0]  vs_coefuv_data_r;             //sdtd

reg       [2:0]   my_reg;                       // reg comments...
parameter [22:0]  param       = value;          // parameter comments...
//        10      18        28  32          44
wire      [1:0]   my_wire     = my_reg;

parameter [4:1]   test1,
                  test2,
                  test3;

wire      [para5-2:0] test_wire;

parameter         para1       = 1;              //
parameter         para2       = 2;
parameter         para3       = 3,
                  para4       = 4,
                  para5       = 5;

// (vlog-signal-get-signal-width "test_wire")
assign  haha = test_wire;

assign  framey      = (({i_reg_framey, i_reg_frameuv} == 2'b10) &&
                       ~i_reg_prog) ? 1'b0 : i_reg_framey;

m_vpp_ff_ce #(1,1'b0) u_ff_actv_delay1 (
    .vpp_clk            (vpp_clk),          // comm
    .vpp_rst_b          (vpp_rst_b),
    .i_ce               (1'b1),
    .i_in1              (i_reg_actv),
    .o_out1             (actv_1));

//// auto sense //
always @(s3)
  if (s1)
     d1 = p1;
  else
     d1 = p2;

parameter         IDLE        = 3'b000;         // 0 - IDLE
parameter         Y1          = 3'b100;         // 4 - Y1
parameter         U           = 3'b101;         // 5 - U
parameter         Y2          = 3'b110;         // 6 - Y2
parameter         V           = 3'b111;         // 7 - V
parameter         WAITD       = 3'b011;         // 3 - WAITD
parameter         PICBEG      = 3'b010;         // 2 - PICBEG
parameter         PICEND      = 3'b001;         // 1 - PICEND

// auto sense //
always @(state or i_HTRANS or BUSY or slave_ready or S_IDLE)
begin
   next = state;
   case (state)
       S_FIRST:
         begin
            if ((i_HTRANS == BUSY)&&slave_ready)
            begin
               next = S_IDLE;
            end
         end
       
       S_ERROR:
         begin
            next = S_IDLE;
         end
       default:
         begin
            next = S_IDLE;
         end
   endcase // case(state)
end

//// auto sense //
always @ (state_r   or bei3_uv or       wait_count_r or      line_done_1)
begin
  case (state_r)
      PICBEG:
        if (stepen_r)
        begin
           fsm_ns = IDLE;
        end
        else
           fsm_ns = PICBEG;

      IDLE: if
        (i_lbc_status && ~pic_end_r)
           fsm_ns = Y1;
      else
         fsm_ns = IDLE;

      Y1:
        if (~i_u_hs_full)
           fsm_ns_en = U;
        else
           fsm_ns = Y1;

      U:
        if (i_y_fifo_vacancy > 2'b1)
           fsm_ns = Y2;
        else
           fsm_ns = U;

      Y2:
        if (~i_v_hs_full)
           fsm_ns = V;
        else
           fsm_ns = Y2;

      V:
        if ((v_rd_addr_r == fw_src_uv) && (curr_line_r == fh_dst))
           fsm_ns = PICEND;
        else
           if (i_y_fifo_vacancy > 2'b1)
              if ((v_rd_addr_r == bei1_uv) ||
                  (v_rd_addr_r == bei2_uv) ||
                  (v_rd_addr_r == bei3_uv) ||
                  (v_rd_addr_r == fw_src_uv))
                 fsm_ns = WAITD;
              else
                 fsm_ns = Y1;
           else
              fsm_ns = V;
      WAITD:
        if ((fw_src_uv > bei3_uv ? (wait_count_r >= 3'h2) : (wait_count_r == 3'h3)) &&
            i_lbc_status && (i_y_fifo_vacancy > 2'b1))
           fsm_ns = Y1;
        else
           fsm_ns = WAITD;

      PICEND:
        if (line_done_1)
           fsm_ns = IDLE;
        else
           fsm_ns = PICEND;

      default: fsm_ns = IDLE;
  endcase
end

// (setq vlog-indent-align-else-to-if nil)
always @(posedge vpp_clk)
  if (~vpp_rst_b)
     current_state_r<= 3'b0;
  else if (i_reg_update)
     current_state_r<= 3'b0;
  else if (((tpval_hsctly && ((current_state_r== 3'b0) || (current_state_r== 3'h4)) && mix_val && i_reg_hsctl && ~prethrowy && ~throwy)
            || (tpval_hsctly_rev && ((current_state_r== 3'b0) || (current_state_r== 3'h4)) && mix_val && ~i_reg_hsctl)) && ~hold_r)
     current_state_r<= 3'b1;
  else if (((tpval_hsctly && (current_state_r== 3'b1) && mix_val && i_reg_hsctl && ~prethrowy && ~throwy)
            || (tpval_hsctly_rev && (current_state_r== 3'b1) && mix_val && ~i_reg_hsctl)) && ~hold_r)
     current_state_r<= 3'h2;
  else if (((tpval_hsctlv && (current_state_r== 3'h2) && mix_val && i_reg_hsctl && ~prethrowv && ~throwv)
            || (tpval_hsctlv_rev && (current_state_r== 3'h2) && mix_val && ~i_reg_hsctl)) && ~hold_r)
     current_state_r<= 3'h3;
  else if (((tpval_hsctlu && (current_state_r== 3'h3) && mix_val && i_reg_hsctl && ~prethrowu && ~throwu)
            || (tpval_hsctlu_rev && (current_state_r== 3'h3) && mix_val && ~i_reg_hsctl)) && ~hold_r)
     current_state_r<= 3'h4;

always @ (i_vs_coefuv_addr or coef_vs_uv_0_r or coef_vs_uv_1_r or
          coef_vs_uv_2_r or coef_vs_uv_3_r or coef_vs_uv_4_r or
          coef_vs_uv_5_r or coef_vs_uv_6_r or coef_vs_uv_7_r)
begin
   case (i_vs_coefuv_addr)
     3'b000 : vs_coefuv_data_o = coef_vs_uv_0_r[23:0];
     3'b001 :
        vs_coefuv_data_o = coef_vs_uv_1_r[23:0];
     3'b010 : vs_coefuv_data_o = coef_vs_uv_2_r[23:0];
     3'b011 : vs_coefuv_data_o = coef_vs_uv_3_r[23:0];
     3'b100 :
        vs_coefuv_data_o = coef_vs_uv_4_r[23:0];
     3'b101 : vs_coefuv_data_o = coef_vs_uv_5_r[23:0];
     3'b110 : vs_coefuv_data_o = coef_vs_uv_6_r[23:0];
     3'b111 : vs_coefuv_data_o = coef_vs_uv_7_r[23:0];
     default: vs_coefuv_data_o = 24'b0;
   endcase
end

// @Todo
always @(posedge vpp_clk)
  if (~vpp_rst_b)
     vs_coefuv_data_r <= 24'b0;
  else
     vs_coefuv_data_r <= vs_coefuv_data_o;

assign  o_vs_coefuv_data = vs_coefuv_data_r;
//$ [VS UV coef registers] ----------------------------------------------------

endmodule // m_vpp_regman

module m_vpp_cosim_top;

reg               clk;
reg               reset_b;
wire              testbench_stop;



initial
fork
   clk     = 1'b0;
   reset_b = 1'b0;
   #10 reset_b = 1'b1;
join

always #5 clk = ~clk;

always @(posedge clk)
  if (u_vpp_rtl.i_vsync == 1'b1)
  begin
     $timeformat(-9, 3, "ns", 10);
     $display("pp activated, current simulation time is %t\n", $time);
  end

assign test = test1 ? test2 : test4;

// (setq verilog-indent-begin-after-if t)
// (setq verilog-indent-begin-after-if nil)
/*
 (vlog-in-star-comments-p)
 (vlog-in-parens-p)
 */
always @(posedge clk)
  if (u_vpp_rtl.u_vpp_regman.update_r) begin
     test <= (heihei && haha);
  end
  else begin
     if (xxsefsdf) begin
        sfsdf;
        sdfsdfs;
        sdfsdf;
     end else if (oioisdofafweu) begin
        end
  end

always @(posedge clk)
  if (u_vpp_rtl.u_vpp_regman.update_r)
  begin
     $fdisplay (logfile, "{%d}_h\t%d ->%d\t(%d)\t{%d}_v\t%d =>\t%d\t(%d)\t%d\t%d\t[%d,\t%d]\t[%d,\t%d]",
                u_vpp_rtl.u_vpp_regman.hs_work_r,
                u_vpp_rtl.u_vpp_regman.hsfw_r,
                u_vpp_rtl.u_vpp_regman.hdfw_r,
                u_vpp_rtl.u_vpp_regman.hsstep_r,
                u_vpp_rtl.u_vpp_regman.vs_work_r,
                u_vpp_rtl.u_vpp_regman.vsfh_r,
                u_vpp_rtl.u_vpp_regman.vdfh_r,
                u_vpp_rtl.u_vpp_regman.vsstep_r,
                u_vpp_rtl.u_vpp_regman.framey_r,
                u_vpp_rtl.u_vpp_regman.frameuv_r,
                u_vpp_rtl.u_vpp_regman.offodd_r,
                u_vpp_rtl.u_vpp_regman.offeven_r,
                u_vpp_rtl.u_vpp_regman.offy_r,
                u_vpp_rtl.u_vpp_regman.offuv_r);
     $fflush (logfile);
  end


always @(negedge internalClock) //start of clock transition region
begin
   initialInputState = in;
   inputStayedStable = 1;
   if ((ce_activeLow == 1 && clockEnable==1'b0) ||  (ce_activeLow ==0 && clockEnable==1'b1))  //if ClockEnable is active then let the Output change
   begin
      if (clock === 1'hx)   //if clock didn't transition directly to 0 or 1 (did 0 or 1 -> x instead)
      begin
         inClockingRegion = 1;  //enable stability checker (flip-flop goes clocks an x if input changes during clocking region)
         if (undelayedOut !== in)  //if flip-flop is changing state,
         begin
            if ( sc_activeLow == 1)
            begin
               case ({set, clear2[2:0]})                      //SC are active low
                   2'b00: begin undelayedOut <= {n{1'b1}}; end  //Set output
                   2'b01: begin undelayedOut <= {n{1'b1}}; end  //Set output
                   2'b10: begin undelayedOut <= {n{1'b0}}; end  //Clear output
                   2'b11: begin undelayedOut = 'hx; end   // begin transitioning to new value
               endcase
            end
            else
            begin
               case ({set, clear})                      //SC are active High
                   2'b11: begin undelayedOut <= { n{1'b1} } ; end  //Set output
                   2'b10: begin undelayedOut <= { n{1'b1} } ; end  //Set output
                   2'b01: begin undelayedOut <= { n{1'b0} }; end  //Clear output
                   2'b00: begin undelayedOut = 'hx; end   // begin transitioning to new value
               endcase
            end
         end

         @(clock) //end of clock transition region
         inClockingRegion = 0;
      end
      if (inputStayedStable)
      begin
         if ( sc_activeLow == 1)
         begin
            case ({set, clear})                      //SC are active low
                2'b00: begin undelayedOut <= {n{1'b1}}; end  //Set output
                2'b01: begin undelayedOut <= {n{1'b1}}; end  //Set output
                2'b10: begin undelayedOut <= {n{1'b0}}; end  //Clear output
                2'b11: begin undelayedOut <= initialInputState; end    //Q = D
            endcase
         end
         else
         begin
            case ({set, clear})                      //SC are active High
                2'b11: begin undelayedOut <= { n{1'b1} } ; end  //Set output
                2'b10: begin undelayedOut <= { n{1'b1} } ; end  //Set output
                2'b01: begin undelayedOut <= { n{1'b0} }; end  //Clear output
                2'b00: begin undelayedOut <= initialInputState; end    //Q = D
            endcase
         end
      end
      //else if clockEnable is not active then do nothing
   end
end

always @ (...)
  case (expr_1)
      BRANCH_1:
        if (expr_2) begin
           xxx = yyy;
        end else
           xxx = !DUDU;
      default:
        xxx = 1'b0;
  endcase

// todo: test is a test todo

// psl assert testing

// synopsys what what what ...

DW02_mult #(9,12) u_mul1 (
    .A                  ({1'b0,w_src1}),
    .B                  (w_coef1      ),
    .TC                 (1'b1         ),
    .PRODUCT            (w_addend1    ));

DW02_mult #(9,12) u_mul2 (
    .A                  ({1'b0,w_src2}),
    .B                  (w_coef2      ),
    .TC                 (1'b1         ),
    .PRODUCT            (w_addend2    ));

endmodule // m_vpp_cosim_top
/*heihei*/
// (vlog-skip-blank-and-useless-backward (point-min))
// (vlog-skip-blank-and-useless-forward (point-max))

`ifdef
/*heihei*/
