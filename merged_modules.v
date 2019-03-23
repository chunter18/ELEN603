module router (
	input clock, reset_n, 
	input [7:0] frame_n, 
	input [7:0] valid_n, 
	input [7:0] di, output [7:0]dout, 
	output [7:0] valido_n, [7:0] frameo_n);

// interconnect wires that you will need  , try to make the names meaningful 

	wire [3:0] addr_from_input [7:0];
	wire [31:0] payload_from_input [7:0];
	wire [31:0] payload_from_fifo [7:0];
	wire [31:0] payload_from_mux [7:0];
	wire [7:0] fifo_empty ;
	wire [7:0] vld_from_input ;
	wire [7:0] req_to_arb[7:0] ;
	wire [7:0] request_from_fifo ;
	wire [7:0] clear ;
	wire [7:0] grant [7:0];
	wire [2:0] grant_index [7:0];

// Let's use generate statements (will discuss this in class next week) to instantiate the 8-slices
genvar i;
generate 
for (i=0;i<8;i=i+1) begin :inst
portin portin (.clock(clock),.reset_n(reset_n),.frame_n(frame_n[i]), .valid_n(valid_n[i]), .di(di[i]),
.addr(addr_from_input[i]),.payload(payload_from_input[i]),.vld(vld_from_input[i]), .clear(clear[i]) );

DW_arb_rr #(8,1,2) arb(.clk(clock),.rst_n(reset_n),.init_n(1'b1),.enable(1'b1),
.request(req_to_arb[i]),.mask(8'b0000_0000),.granted(),.grant(grant[i]),.grant_index(grant_index[i])) ;

DW_fifo_s1_df #(32, 8, 0, 0) fifo (.clk(clock), .rst_n(reset_n), .push_req_n(!grant[i]), .pop_req_n(!request_from_fifo[i]), 
.diag_n(1'b1), .ae_level(3'h0), .af_thresh(3'h0), .data_in(payload_from_mux[i]), .empty(fifo_empty[i]), 
.almost_empty(), .half_full(), .almost_full(), .full(), .error(), .data_out(payload_from_fifo[i]) );

portout portout (.clock(clock), .reset_n(reset_n) , .din(payload_from_fifo[i]), .vld(!fifo_empty[i]), 
.frame_n(frameo_n[i]),.valid_n(valido_n[i]),.dout(dout[i]), .pop( request_from_fifo[i]) );

assign req_to_arb[i] = vld_from_input &

{addr_from_input[7]==i,addr_from_input[6]==i, addr_from_input[5]==i, addr_from_input[4]==i,addr_from_input[3]==i, addr_from_input[2]==i, addr_from_input[1]==i, addr_from_input[0]==i};


//.clear(grant[0])
assign clear[i] =  grant[0][i] | grant[1][i] | grant[2][i] | grant[3][i] |
		   grant[4][i] | grant[5][i] | grant[6][i] | grant[7][i]; 
// Student should create this expression 
end
endgenerate

// Switching Fabric is Here

// Student should create the switching fabric logic here, it could be as simple as 8 8-to-1 MUXES

mux8to1 mux1(.Out(payload_from_mux[0]), .Sel(grant_index[0]), 
.In1(payload_from_input[0]), .In2(payload_from_input[1]), 
.In3(payload_from_input[2]), .In4(payload_from_input[3]), 
.In5(payload_from_input[4]), .In6(payload_from_input[5]), 
.In7(payload_from_input[6]), .In8(payload_from_input[7]));

mux8to1 mux2(.Out(payload_from_mux[1]), .Sel(grant_index[1]), 
.In1(payload_from_input[0]), .In2(payload_from_input[1]), 
.In3(payload_from_input[2]), .In4(payload_from_input[3]), 
.In5(payload_from_input[4]), .In6(payload_from_input[5]), 
.In7(payload_from_input[6]), .In8(payload_from_input[7]));

mux8to1 mux3(.Out(payload_from_mux[2]), .Sel(grant_index[2]), 
.In1(payload_from_input[0]), .In2(payload_from_input[1]), 
.In3(payload_from_input[2]), .In4(payload_from_input[3]), 
.In5(payload_from_input[4]), .In6(payload_from_input[5]), 
.In7(payload_from_input[6]), .In8(payload_from_input[7]));

mux8to1 mux4(.Out(payload_from_mux[3]), .Sel(grant_index[3]), 
.In1(payload_from_input[0]), .In2(payload_from_input[1]), 
.In3(payload_from_input[2]), .In4(payload_from_input[3]), 
.In5(payload_from_input[4]), .In6(payload_from_input[5]), 
.In7(payload_from_input[6]), .In8(payload_from_input[7]));

mux8to1 mux5(.Out(payload_from_mux[4]), .Sel(grant_index[4]), 
.In1(payload_from_input[0]), .In2(payload_from_input[1]), 
.In3(payload_from_input[2]), .In4(payload_from_input[3]), 
.In5(payload_from_input[4]), .In6(payload_from_input[5]), 
.In7(payload_from_input[6]), .In8(payload_from_input[7]));

mux8to1 mux6(.Out(payload_from_mux[5]), .Sel(grant_index[5]), 
.In1(payload_from_input[0]), .In2(payload_from_input[1]), 
.In3(payload_from_input[2]), .In4(payload_from_input[3]), 
.In5(payload_from_input[4]), .In6(payload_from_input[5]), 
.In7(payload_from_input[6]), .In8(payload_from_input[7]));

mux8to1 mux7(.Out(payload_from_mux[6]), .Sel(grant_index[6]), 
.In1(payload_from_input[0]), .In2(payload_from_input[1]), 
.In3(payload_from_input[2]), .In4(payload_from_input[3]), 
.In5(payload_from_input[4]), .In6(payload_from_input[5]), 
.In7(payload_from_input[6]), .In8(payload_from_input[7]));

mux8to1 mux8(.Out(payload_from_mux[7]), .Sel(grant_index[7]), 
.In1(payload_from_input[0]), .In2(payload_from_input[1]), 
.In3(payload_from_input[2]), .In4(payload_from_input[3]), 
.In5(payload_from_input[4]), .In6(payload_from_input[5]), 
.In7(payload_from_input[6]), .In8(payload_from_input[7]));


// <!Switching Fabric is Here>


endmodule
module portin (input clock, reset_n,frame_n,valid_n,di,
               output reg [3:0] addr, output reg [31:0] payload, output reg vld, input clear);
               
    reg [4:0] count;
    reg vld_int;
    always@(posedge clock, negedge reset_n)
       if(~reset_n)
       begin
           count <= 0;
           vld_int <=1'b0;
           //flag <= 1'b0;
       end
       
       else if(clear)
       begin
            vld_int <= 1'b0;
            count <= 0;
            //flag <= 1'b0;
       end
       
       //else if(flag == 1'b1)    //flag is high, so valid will have been high for 1 cycle
       //begin
       //     flag <= 1'b0; //reset the flag
       //     vld <= 1'b0; //vld goes back to low - only high for one cycle
       //end
    
       else
       begin
           if(~frame_n & valid_n) //only frame_n low,  addr region
           begin
               addr[count] <= di;
               //count <= count + 1;
               if(count == 3) //4th bit coming in is a dont care
                    count <= 0; //reset count for addr bits
	       else
		    count <= count + 1;
           end
           
           else if(~frame_n & ~valid_n) //both low, payload region for first 31 bits
           begin
               payload[count] <= di;
               count <= count + 1; 
           end
           
           else if(frame_n & ~valid_n) //last bit of payload where fram_n is high but valid still low
           begin
                payload[count] <= di;
                count <= count + 1; //overflows back to 0
                vld_int <= 1'b1;
           end
       end

       
    assign vld = vld_int & !clear;       
    //always@(posedge vld) //validd just got set, flag that it went high
    //    flag <= 1'b1;          
               
endmodule
module portout(input clock, reset_n, input [31:0] din, input vld,
                 output reg  frame_n,valid_n,dout, output reg pop);
                 
    //connect something to empty line of fifo   
    reg [4:0] count;
    reg [31:0] temp;   
    reg state;    
    always@(posedge clock, negedge reset_n)
        if(~reset_n)
        begin
            temp <= 0;
            count <= 0;
            pop <= 1;
            frame_n <= 1;
            valid_n <= 1;
            state <= 0;
        end
        //no clear condition
        else if (vld & ~state)
        begin
            temp <= din;
            count <= 0;
            state <= 1; //set a flag to indicate we need to serialize
            pop <= 0;  
        end
        
        else if(state)
        begin
            frame_n <= 0;
            valid_n <= 0;
            dout <= temp[count];
            count <= count + 1;
            if(count == 31) //finished serailizing
            begin //reset control outputs
                state <= 0;
                frame_n <= 1;
                valid_n <= 1;
                pop <= 1;
            end
        end
       
endmodule
module mux8to1( output reg [31:0] Out, 
		input [2:0] Sel,
		input [31:0] In1, In2, In3, In4,
		input [31:0] In5, In6, In7, In8); 


	//Check the state of the input lines 

	always @ (In1 or In2 or In3 or In4 or In5 or In6 or In7 or In8 or Sel) 
	begin 
		case (Sel) 
			3'b000 : Out = In1; 
			3'b001 : Out = In2; 
			3'b010 : Out = In3; 
			3'b011 : Out = In4; 
			3'b100 : Out = In5; 
			3'b101 : Out = In6; 
			3'b110 : Out = In7; 
			3'b111 : Out = In8; 
			default : Out = 8'bx; //If input is undefined then output is undefined 
		endcase 

	end  

endmodule
