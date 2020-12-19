module top(input clk,reset,
           output wire [31:0]writedata,dataadr,
			  output wire memwrite);

wire [31:0]pc,instr,readdata;

mips mips(clk,reset,pc,instr,memwrite,dataadr,writedata,readdata);
imem imem(pc[7:2],instr);
dmem dmem(clk,memwrite,dataadr,writedata,readdata);
endmodule

module adder(input [31:0]a,b,
				 output wire [31:0]y);
assign y=a+b;
endmodule

module datapath(input clk,reset,
                input memtoreg,pcsrc,
					 input alusrc,regdst,
					 input regwrite,jump,
					 input [2:0]alucontrol,
					 output wire zero,
					 output wire[31:0]pc,
					 input [31:0]instr,
					 output wire [31:0]aluout,writedata,
					 input [31:0]readdata);
					 
wire [4:0]writereg;
wire [31:0]pcnext,pcnextbr,pcplus4,pcbranch,signimm,signimmsh,srca,srcb,result;

flopr#(32) pcreg(clk,reset,pcnext,pc);
adder pcadd1(pc,32'b100,pcplus4);
s12 immsh(signimm,signimmsh);
adder pcadd2(pcplus4,signimmsh,pcbranch);
mux2#(32) pcbrmux(pcplus4,pcbranch,pcsrc,pcnextbr);
mux2#(32) pcmux(pcnextbr,{pcplus4[31:28],instr[25:0],2'b00},jump,pcnext);

regfile rf(clk,regwrite,instr[25:21],instr[20:16],writereg,result,srca,writedata);
mux2#(5) wrmux(instr[20:16],instr[15:11],regdst,writereg);
mux2#(32) resmux(aluout,readdata,memtoreg,result);
signext se(instr[15:0],signimm);

mux2#(32) srcbmux(writedata,signimm,alusrc,srcb);
alu alu(srca,srcb,alucontrol,aluout,zero);

endmodule 

module alu(srca,srcb,alucontrol,aluout,zero);
input [31:0]srca,srcb;
input [2:0]alucontrol;
output reg [31:0]aluout;
output wire zero;
always@(*)
case(alucontrol)
   3'b000:aluout=srca&srcb;
	3'b001:aluout=srca|srcb;
	3'b010:aluout=srca+srcb;
	3'b100:aluout=srca&(~srcb);
	3'b101:aluout=srca|(~srcb);
	3'b110:aluout=srca+(~srcb)+1'b1;
	3'b111:if(srca<srcb) aluout=1;
          else aluout=0;	
endcase
assign zero=(aluout==0)?1:0;	
endmodule

module aludec(input [5:0]funct,
              input [1:0]aluop,
				  output reg [2:0]alucontrol);
always@(*)
case(aluop)
     2'b00:alucontrol<=3'b010;
	  2'b01:alucontrol<=3'b110;
	  default:case(funct)
	      6'b100000:alucontrol<=3'b010;
			6'b100010:alucontrol<=3'b110;
			6'b100100:alucontrol<=3'b000;
			6'b100101:alucontrol<=3'b001;
			6'b101010:alucontrol<=3'b111;
			default:  alucontrol<=3'bxxx;
		endcase
endcase
endmodule

module controller(input [5:0]op,funct,
                  input zero,
						output wire memtoreg,memwrite,
						output wire pcsrc,alusrc,
						output wire regdst,regwrite,
						output wire jump,
						output wire [2:0]alucontrol);

wire [1:0]aluop;
wire branch;

maindec md(op,memtoreg,memwrite,branch,alusrc,regdst,regwrite,jump,aluop);
aludec ad(funct,aluop,alucontrol);
assign pcsrc=branch&zero;
endmodule 

module dmem(input clk,we,
            input [31:0]a,wd,
				output wire [31:0]rd);

reg [31:0]ram[63:0];

assign rd=ram[a[31:2]];
always@(posedge clk)
 if(we) ram[a[31:2]]<=wd;

 endmodule

module flopr#(parameter width=8)
             (input clk,reset,
				  input [width-1:0]d,
				  output reg [width-1:0]q);

always@(posedge clk,posedge reset)
if(reset)q<=0;
else q<=d;

endmodule

module imem(input [5:0]a,
            output wire [31:0]rd);
reg [31:0]ram[63:0];
initial
$readmemh("C:/intelFPGA_lite/18.1/single_mips/memfile.dat",ram);
assign rd=ram[a];
endmodule

module maindec(input [5:0]op,
               output wire memtoreg,memwrite,
					output wire branch,alusrc,
					output wire regdst,regwrite,
					output wire jump,
					output wire [1:0]aluop);

reg [8:0]controls;

assign {regwrite,regdst,alusrc,branch,memwrite,memtoreg,jump,aluop}=controls;
always@(*)
case(op)
   6'b000000:controls<=9'b110000010;
	6'b100011:controls<=9'b101001000;
	6'b101011:controls<=9'b001010000;
	6'b000100:controls<=9'b000100001;
	6'b001000:controls<=9'b101000000;
	6'b000010:controls<=9'b000000100;
	default:  controls<=9'bxxxxxxxxx;
endcase

endmodule

module mips(input clk,reset,
            output wire [31:0]pc,
				input [31:0]instr,
				output wire memwrite,
				output wire [31:0]aluout,writedata,
				input [31:0]readdata);

wire [2:0]alucontrol;
wire memtoreg,alusrc,regdst,regwrite,jump,pcsrc,zero;

controller c(instr[31:26],instr[5:0],zero,memtoreg,memwrite,pcsrc,alusrc,regdst,regwrite,jump,alucontrol);
datapath dp(clk,reset,memtoreg,pcsrc,alusrc,regdst,regwrite,jump,alucontrol,zero,pc,instr,aluout,writedata,readdata);

endmodule

module mux2#(parameter width=8)
            (input [width-1:0]d0,d1,
				 input s,
				 output wire [width-1:0]y);
assign y=s?d1:d0;
endmodule

module regfile(input clk,
               input we3,
               input [4:0]ra1,ra2,wa3,
					input [31:0]wd3,
					output wire [31:0]rd1,rd2);
reg [31:0]rf[31:0];

integer i;
initial
for (i=0; i<32; i=i+1) rf[i] <= 0;

always@(posedge clk)
if(we3)rf[wa3]<=wd3;

assign rd1=(ra1!=0)?rf[ra1]:0;
assign rd2=(ra2!=0)?rf[ra2]:0;

endmodule 

module s12(input [31:0]a,
           output wire [31:0]y);
assign y={a[29:0],2'b00};
endmodule

module signext(input [15:0]a,
               output wire [31:0]y);
assign y={{16{a[15]}},a};
endmodule

module testbench();
  reg clk;
  reg reset;
  wire [31:0] writedata, dataadr;
  wire memwrite;
  // instantiate device to be tested
  top dut (clk, reset, writedata, dataadr, memwrite);
  // initialize test
  initial
    begin 
      reset <= 1; # 22; reset <= 0;
    end
  // generate clock to sequence tests
  always
    begin
      clk <= 1; # 5; clk <= 0; # 5;
    end
  // check results
  always @ (negedge clk)
    begin
      $display ("writedata=",writedata,"   dataadr=",dataadr);
      if (memwrite) begin
        if (dataadr === 84 & writedata === 7) begin
          $display ("Simulation succeeded");
          $stop;
        end else if (dataadr !== 80) begin
          $display ("Simulation failed");
          $stop;
        end
      end
    end
endmodule
