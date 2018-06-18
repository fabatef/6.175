import Ehr::*;
import Vector::*;

//////////////////
// Fifo interface 

interface Fifo#(numeric type n, type t);
    method Bool notFull;
    method Action enq(t x);
    method Bool notEmpty;
    method Action deq;
    method t first;
    method Action clear;
endinterface

/////////////////
// Conflict FIFO

module mkMyConflictFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     data     <- replicateM(mkRegU());
    Reg#(Bit#(TLog#(n)))    enqP     <- mkReg(0);
    Reg#(Bit#(TLog#(n)))    deqP     <- mkReg(0);
    Reg#(Bool)              empty    <- mkReg(True);
    Reg#(Bool)              full     <- mkReg(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    method Bool notFull();
        return !full;
    endmethod

    method Action enq (t x) if (!full);
        data[enqP] <= x;
        let temp = enqP + 1;
        if (temp > max_index) begin temp = 0; end // why doesn't it loop back when the bit limit is reached?
        if (temp == deqP) begin full <= True; end 
        empty <= False; 
        enqP <= temp; 
    endmethod

    method Bool notEmpty();
        return !empty;
    endmethod

    method Action deq() if (!empty);
        let temp = deqP + 1;
        if (temp > max_index) begin temp = 0; end
        if (temp == enqP) begin empty <= True; end
        full <= False;
        deqP <= temp; 
    endmethod

    method t first() if (!empty);
        return data[deqP];
    endmethod

    method Action clear();
        deqP <= 0; enqP <= 0; empty <= True; full <= False;
    endmethod

endmodule

/////////////////
// Pipeline FIFO

// Intended schedule:
//      {notEmpty, first, deq} < {notFull, enq} < clear
module mkMyPipelineFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     data     <- replicateM(mkRegU());
    Ehr#(3, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(3, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(3, Bool)           empty    <- mkEhr(True);
    Ehr#(3, Bool)           full     <- mkEhr(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    method Bool notFull();
        return !full[1];
    endmethod

    method Action enq (t x) if (!full[1]);
        data[enqP[1]] <= x;
        let temp = enqP[1] + 1;
        if (temp > max_index) begin temp = 0; end 
        if (temp == deqP[1]) begin full[1] <= True; end 
        empty[1] <= False; 
        enqP[1] <= temp; 
    endmethod

    method Bool notEmpty();
        return !empty[0];
    endmethod

    method Action deq() if (!empty[0]);
        let temp = deqP[0] + 1;
        if (temp > max_index) begin temp = 0; end
        if (temp == enqP[0]) begin empty[0] <= True; end
        full[0] <= False;
        deqP[0] <= temp; 
    endmethod

    method t first() if (!empty[0]);
        return data[deqP[0]];
    endmethod

    method Action clear();
        deqP[2] <= 0; enqP[2] <= 0; empty[2] <= True; full[2] <= False;
    endmethod


endmodule

//////////////
// Bypass FIFO

// Intended schedule:
//      {notFull, enq} < {notEmpty, first, deq} < clear
module mkMyBypassFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Ehr#(2, t))     data     <- replicateM(mkEhrU());
    Ehr#(3, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(3, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(3, Bool)           empty    <- mkEhr(True);
    Ehr#(3, Bool)           full     <- mkEhr(False);

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    method Bool notFull();
        return !full[0];
    endmethod

    method Action enq (t x) if (!full[0]);
        data[enqP[0]][0] <= x;
        let temp = enqP[0] + 1;
        if (temp > max_index) begin temp = 0; end 
        if (temp == deqP[0]) begin full[0] <= True; end 
        empty[0] <= False; 
        enqP[0] <= temp; 
    endmethod

    method Bool notEmpty();
        return !empty[1];
    endmethod

    method Action deq() if (!empty[1]);
        let temp = deqP[1] + 1;
        if (temp > max_index) begin temp = 0; end
        if (temp == enqP[1]) begin empty[1] <= True; end
        full[1] <= False;
        deqP[1] <= temp; 
    endmethod

    method t first() if (!empty[1]);
        return data[deqP[1]][1];
    endmethod

    method Action clear();
        deqP[2] <= 0; enqP[2] <= 0; empty[2] <= True; full[2] <= False;
    endmethod
endmodule

//////////////////////
// Conflict-free fifo

// Intended schedule:
//      {notFull, enq} CF {notEmpty, first, deq}
//      {notFull, enq, notEmpty, first, deq} < clear
module mkMyCFFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
    // n is size of fifo
    // t is data type of fifo
    Vector#(n, Reg#(t))     data     <- replicateM(mkRegU());
    //To enforce {enq, deq} < clear, I replaced the Regs with Ehrs
    Ehr#(2, Bit#(TLog#(n))) enqP     <- mkEhr(0);
    Ehr#(2, Bit#(TLog#(n))) deqP     <- mkEhr(0);
    Ehr#(2, Bool)           empty    <- mkEhr(True);
    Ehr#(2, Bool)           full     <- mkEhr(False);

    Ehr#(n, Bool)        req_deq     <- mkEhr(False);
    Ehr#(n, Bool)       req_clear    <- mkEhr(False);
    Ehr#(n, Maybe#(t))   req_enq     <- mkEhr(tagged Invalid); 

    // useful value
    Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

    (*no_implicit_conditions, fire_when_enabled*)
    rule canonicalizie;

        let temp_e = enqP[0] + 1;
        if (temp_e > max_index) begin temp_e = 0; end 


        let temp_d = deqP[0] + 1;
        if (temp_d > max_index) begin temp_d = 0; end

            
        if ((!full[0] && isValid(req_enq[1])) && (!empty[0] && req_deq[1])) begin //enq and deq
            data[enqP[0]] <= fromMaybe(?, req_enq[1]);
            empty[0] <= False;
            full[0] <= False;
            enqP[0] <= temp_e; deqP[0] <= temp_d; 
        end else if (!empty[0] && req_deq[1]) begin //just deq
            if (temp_d == enqP[0]) begin empty[0] <= True; end
            full[0] <= False;
            deqP[0] <= temp_d; 
        end else if (!full[0] && isValid(req_enq[1])) begin //just enq
            data[enqP[0]] <= fromMaybe(?, req_enq[1]);
            if (temp_e == deqP[0]) begin  full[0] <= True; end
            empty[0] <= False;
            enqP[0] <= temp_e;
        end

        if (req_clear[1]) begin
            deqP[1] <= 0; enqP[1] <= 0; empty[1] <= True; full[1] <= False; 
        end


        //reset requests 
        req_clear[1] <= False;
        req_deq[1]   <= False;
        req_enq[1]   <= tagged Invalid;

    endrule

    method Bool notFull();
        return !full[0];
    endmethod

    method Action enq (t x) if (!full[0]);
        req_enq[0] <= tagged Valid (x);
    endmethod

    method Bool notEmpty();
        return !empty[0];
    endmethod

    method Action deq() if (!empty[0]);
        req_deq[0] <= True;

    endmethod

    method t first() if (!empty[0]);
        return data[deqP[0]];
    endmethod

    method Action clear();
        req_clear[0] <= True;
    endmethod

endmodule

