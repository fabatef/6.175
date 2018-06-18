// Reference functions that use Bluespec's '*' operator
function Bit#(TAdd#(n,n)) multiply_unsigned( Bit#(n) a, Bit#(n) b );
    UInt#(n) a_uint = unpack(a);
    UInt#(n) b_uint = unpack(b);
    UInt#(TAdd#(n,n)) product_uint = zeroExtend(a_uint) * zeroExtend(b_uint);
    return pack( product_uint );
endfunction

function Bit#(TAdd#(n,n)) multiply_signed( Bit#(n) a, Bit#(n) b );
    Int#(n) a_int = unpack(a);
    Int#(n) b_int = unpack(b);
    Int#(TAdd#(n,n)) product_int = signExtend(a_int) * signExtend(b_int);
    return pack( product_int );
endfunction



// Multiplication by repeated addition
function Bit#(TAdd#(n,n)) multiply_by_adding( Bit#(n) a, Bit#(n) b );

    Bit#(n) prod = 0;
    Bit#(n) tp = 0;

    for (Integer i = 0; i < valueOf(n); i = i + 1) begin
        Bit#(n) m = (a[i] == 0) ? 0 : b;
        //Bit#(TAdd#(n,1)) sum = signExtend(m) + signExtend(tp); nope
        Bit#(TAdd#(n,1)) sum = zeroExtend(m) + zeroExtend(tp); 
        prod[i] = sum[0];
        tp = truncateLSB(sum);
    end
    return {tp, prod};
endfunction



// Multiplier Interface
interface Multiplier#( numeric type n );
    method Bool start_ready();
    method Action start( Bit#(n) a, Bit#(n) b );
    method Bool result_ready();
    method ActionValue#(Bit#(TAdd#(n,n))) result();
endinterface



// Folded multiplier by repeated addition
module mkFoldedMultiplier( Multiplier#(n) )
	provisos(Add#(1, a__, n)); // make sure n >= 1
    
    // You can use these registers or create your own if you want
    Reg#(Bit#(n)) a <- mkRegU();
    Reg#(Bit#(n)) b <- mkRegU();
    Reg#(Bit#(n)) prod <- mkRegU();
    Reg#(Bit#(n)) tp <- mkRegU();
    Reg#(Bit#(TAdd#(TLog#(n),1))) i <- mkReg( fromInteger(valueOf(n)+1) );
    Integer n_val = valueOf(n);

    rule mulStep ( i < fromInteger(n_val));
        Bit#(n) m = (a[0] == 0) ? 0 : b;
        a <= a >> 1;
        Bit#(TAdd#(n,1)) sum = zeroExtend(m) + zeroExtend(tp); 
        prod <= {sum[0], prod[(n_val-1):1]};
        tp <= truncateLSB(sum);
        i <= i + 1;
    endrule

    method Bool start_ready();
        return i ==  fromInteger(n_val + 1);
    endmethod

    method Action start( Bit#(n) aIn, Bit#(n) bIn ) if (i ==  fromInteger(n_val + 1));
        a <= aIn; b <= bIn; i <= 0;
        tp <= 0; prod <= 0;
    endmethod

    method Bool result_ready();
        return i == fromInteger(n_val);
    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result if (i == fromInteger(n_val)) ;
        i <= i + 1;
        return {tp, prod};
    endmethod
endmodule



function Bit#(n) arth_shift(Bit#(n) a, Integer n, Bool right_shift);
    Int#(n) a_int = unpack(a);
    Bit#(n) out = 0;
    if (right_shift) begin
        out = pack(a_int >> n);
    end else begin //left shift
        out = pack(a_int <<n); end
    return out;
endfunction



// Booth Multiplier
module mkBoothMultiplier( Multiplier#(n) )
	provisos(Add#(2, a__, n)); // make sure n >= 2

    Reg#(Bit#(TAdd#(TAdd#(n,n),1))) m_neg <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),1))) m_pos <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),1))) p <- mkRegU;
    Reg#(Bit#(TAdd#(TLog#(n),1))) i <- mkReg( fromInteger(valueOf(n)+1) );
    Integer n_val = valueOf(n);

    rule mul_step( i < fromInteger(n_val) );
        let pr = p[1:0];
        Bit#(TAdd#(TAdd#(n,n),1)) temp = p;

        if ( pr == 2'b01 ) begin temp = p + m_pos; end 
        if ( pr == 2'b10 ) begin temp = p + m_neg; end

        p <= arth_shift(temp, 1,True); 
        i <= i + 1;       
    endrule

    method Bool start_ready();
        return i == fromInteger(n_val + 1);
    endmethod

    method Action start( Bit#(n) m, Bit#(n) r ) if (i == fromInteger(n_val + 1));
        i <= 0;
        m_pos <= {m, 0};
        m_neg <= {(-m), 0};
        p <= {0, r, 1'b0};
    endmethod

    method Bool result_ready();
        return i == fromInteger(n_val);
    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result() if (i == fromInteger(n_val));
        i <= i + 1;
        return truncateLSB(p);
    endmethod
endmodule



// Radix-4 Booth Multiplier
module mkBoothMultiplierRadix4( Multiplier#(n) )
	provisos(Mul#(a__, 2, n), Add#(1, b__, a__)); // make sure n >= 2 and n is even

    Reg#(Bit#(TAdd#(TAdd#(n,n),2))) m_neg <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),2))) m_pos <- mkRegU;
    Reg#(Bit#(TAdd#(TAdd#(n,n),2))) p <- mkRegU;
    Reg#(Bit#(TAdd#(TLog#(n),1))) i <- mkReg( fromInteger(valueOf(n)/2+1) );
    Integer n_val = valueOf(n);

    rule mul_step( i < fromInteger(n_val)/2);
        let pr = p[2:0];
        Bit#(TAdd#(TAdd#(n,n),2)) temp = p;

        if ( pr == 3'b001 ) begin temp = p + m_pos; end //0+
        if ( pr == 3'b010 ) begin temp = p + m_pos; end //0+
        if ( pr == 3'b011 ) begin temp = p + arth_shift(m_pos, 1, False); end //+0
        if ( pr == 3'b100 ) begin temp = p + arth_shift(m_neg, 1, False); end //-0
        if ( pr == 3'b101 ) begin temp = p + m_neg; end //0-
        if ( pr == 3'b110 ) begin temp = p + m_neg; end //0-

        p <= arth_shift(temp, 2, True); 
        i <= i + 1;       
    endrule

    method Bool start_ready();
        return i == fromInteger(n_val/2 + 1);
    endmethod

    method Action start( Bit#(n) m, Bit#(n) r ) if (i == fromInteger(n_val/2 + 1));
        i <= 0;
        m_pos <= {msb(m), m, 0};
        m_neg <= {msb(-m), (-m), 0};
        p <= {0, r, 1'b0};
    endmethod

    method Bool result_ready();
        return i == fromInteger(n_val/2);
    endmethod

    method ActionValue#(Bit#(TAdd#(n,n))) result() if (i == fromInteger(n_val/2));
        i <= i + 1;
        return p [(2*n_val):1];
    endmethod
endmodule
