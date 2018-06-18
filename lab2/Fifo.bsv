import Ehr::*;
import Vector::*;
import FIFO::*;

interface Fifo#(numeric type n, type t);
    method Action enq(t x);
    method Action deq;
    method t first;
    method Bool notEmpty;
endinterface


module mkFifo(Fifo#(3,t)) provisos (Bits#(t,tSz));
   // define your own 3-elements fifo here. 
   Reg #(Maybe #(t)) da <- mkReg (tagged Invalid);
   Reg #(Maybe #(t)) db <- mkReg (tagged Invalid);
   Reg #(Maybe #(t)) dc <- mkReg (tagged Invalid);

   // Enq if there's at least one spot open... so, dc is invalid. 
   method Action enq(t x) if (!isValid (dc));
        if (!isValid (da)) begin da <= tagged Valid (x); end
        else if (!isValid (db)) begin db <= tagged Valid (x); end
        else begin dc <= tagged Valid (x); end
   endmethod

   //Deq if there's a valid data at da
   method Action deq() if (isValid (da));
        if (isValid (dc)) begin da <= db; db <= dc; dc <= tagged Invalid; end
        else if (isValid (db)) begin da <= db; db <= tagged Invalid; end
        else begin da <= tagged Invalid; end
   endmethod

   //First if there's a valid data at da
   method t first() if (isValid (da));
        return fromMaybe(?, da);
   endmethod

   //Check if fifo's empty
   method Bool notEmpty();
        return isValid(da);
   endmethod

endmodule


// Two elements conflict-free fifo given as black box
module mkCFFifo( Fifo#(2, t) ) provisos (Bits#(t, tSz));
    Ehr#(2, t) da <- mkEhr(?);
    Ehr#(2, Bool) va <- mkEhr(False);
    Ehr#(2, t) db <- mkEhr(?);
    Ehr#(2, Bool) vb <- mkEhr(False);

    rule canonicalize;
        if( vb[1] && !va[1] ) begin
            da[1] <= db[1];
            va[1] <= True;
            vb[1] <= False;
        end
    endrule

    method Action enq(t x) if(!vb[0]);
        db[0] <= x;
        vb[0] <= True;
    endmethod

    method Action deq() if(va[0]);
        va[0] <= False;
    endmethod

    method t first if (va[0]);
        return da[0];
    endmethod

    method Bool notEmpty();
        return va[0];
    endmethod
endmodule

module mkCF3Fifo(Fifo#(3,t)) provisos (Bits#(t, tSz));
    FIFO#(t) bsfif <-  mkSizedFIFO(3);
    method Action enq( t x);
        bsfif.enq(x);
    endmethod

    method Action deq();
        bsfif.deq();
    endmethod

    method t first();
        return bsfif.first();
    endmethod

    method Bool notEmpty();
        return True;
    endmethod

endmodule
