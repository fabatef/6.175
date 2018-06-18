import Vector::*;
import CacheTypes::*;
import MessageFifo::*;
import Types::*;

module mkMessageRouter(
        Vector#(CoreNum, MessageGet) c2r, //contains msgs to be sent out to ppp
        Vector#(CoreNum, MessagePut) r2c, //where msgs from the ppp go
        MessageGet m2r, //contains msgs be sent out to L1 Dcaches
        MessagePut r2m, //where msgs from L1 Dcaches go
        Empty ifc 
	);
	
	Reg#(int) counter <- mkReg(0);

    // TODO: optimize round robin strategy
    rule incrementCounter;
        counter <= (counter + 1) % fromInteger(valueOf(CoreNum));
    endrule

    //sending messages from the parent (m2r) to the correct L1 D cache (r2c)
    rule pppToL1;
    	CacheMemMessage msg = m2r.first; m2r.deq;
    	if (msg matches tagged Req .r)  r2c[r.child].enq_req(r); //downgrade request
    	if (msg matches tagged Resp .r) r2c[r.child].enq_resp(r); //upgrade response
    endrule

    //sending messages from L1 D caches (c2r) to the parent (r2m). 
    rule l1ToPpp;
        Bool found_resp = False;

        for(Integer i = 0; i < valueOf(CoreNum); i += 1) begin
            if (c2r[i].first matches tagged Resp .r) begin
                r2m.enq_resp(r);
                c2r[i].deq;
                found_resp = True;
                break;
            end
        end

        if (!found_resp) begin
            CacheMemMessage msg = c2r[counter].first; c2r[counter].deq;
            if (msg matches tagged Req .r) r2m.enq_req(r); //upgrade request
        //if (msg matches tagged Resp .r) r2m.enq_resp(r); //downgrade response
        end
    endrule

endmodule

