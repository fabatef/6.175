import TestBenchTemplates::*;
import Multipliers::*;

// Example testbenches
(* synthesize *)
module mkTbDumb();
    function Bit#(16) test_function( Bit#(8) a, Bit#(8) b ) = multiply_unsigned( a, b );
    Empty tb <- mkTbMulFunction(test_function, multiply_unsigned, True);
    return tb;
endmodule

(* synthesize *)
module mkTbFoldedMultiplier();
    Multiplier#(8) dut <- mkFoldedMultiplier();
    Empty tb <- mkTbMulModule(dut, multiply_signed, True);
    return tb;
endmodule

(* synthesize *)
module mkTbSignedVsUnsigned();
    function Bit#(16) test_function( Bit#(8) a, Bit#(8) b ) = multiply_signed( a, b );
    Empty tb <- mkTbMulFunction(test_function, multiply_unsigned, True);
    return tb;
endmodule

(* synthesize *)
module mkTbEx3();
    function Bit#(16) test_function( Bit#(8) a, Bit#(8) b ) = multiply_by_adding( a, b );
    Empty tb <- mkTbMulFunction(test_function, multiply_unsigned, True);
    //Empty tb <- mkTbMulFunction(test_function, multiply_signed, True);
    return tb;
endmodule

(* synthesize *)
module mkTbEx5();
    Multiplier#(8) test_module <- mkFoldedMultiplier();
    Empty tb <- mkTbMulModule(test_module, multiply_by_adding, True);
    return tb;
endmodule

(* synthesize *)
module mkTbEx7a();
    Multiplier#(2) test_module <- mkBoothMultiplier();
    Empty tb <- mkTbMulModule(test_module, multiply_signed, True);
    return tb;
endmodule

(* synthesize *)
module mkTbEx7b();
    Multiplier#(8) test_module <- mkBoothMultiplier();
    Empty tb <- mkTbMulModule(test_module, multiply_signed, True);
    return tb;
endmodule

(* synthesize *)
module mkTbEx9a();
    Multiplier#(2) test_module <- mkBoothMultiplierRadix4();
    Empty tb <- mkTbMulModule(test_module, multiply_signed, True);
    return tb;
endmodule

(* synthesize *)
module mkTbEx9b();
    Multiplier#(8) test_module <- mkBoothMultiplierRadix4();
    Empty tb <- mkTbMulModule(test_module, multiply_signed, True);
    return tb;
endmodule

