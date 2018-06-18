import ProcTypes::*;


`ifdef WITHOUTCACHE
import WithoutCache::*;
`endif
`ifdef WITHCACHE
import WithCache::*;
`endif

import Ifc::*;
import ProcTypes::*;
import Types::*;
import Ehr::*;
import MemTypes::*;
import GetPut::*;
import SimMem::*;
import Fifo::*;
import MemUtil::*;
import Memory::*;
import ClientServer::*;
import GetPut::*;

interface ConnectalWrapper;
   interface ConnectalProcRequest connectProc;
  interface ConnectalMemoryInitialization initProc;
endinterface

module [Module] mkConnectalWrapper#(ConnectalProcIndication ind)(ConnectalWrapper);
   let debug = True;
   Reg#(Bool) memDone <- mkReg(False);  


   Fifo#(2, DDR3_Req)  ddr3ReqFifo <- mkCFFifo();
   Fifo#(2, DDR3_Resp) ddr3RespFifo <- mkCFFifo();

   DDR3_Client ddrclient = toGPClient( ddr3ReqFifo, ddr3RespFifo );
   mkSimMem(ddrclient);

   Proc m <- mkProc(ddr3ReqFifo,ddr3RespFifo);

   rule relayMessage;
    	let mess <- m.cpuToHost();
        ind.sendMessage(pack(mess));	
   endrule

   interface ConnectalProcRequest connectProc;
      method Action hostToCpu(Bit#(32) startpc) if (memDone);
        $display("Received software req to start pc\n");
        $fflush(stdout);
        m.hostToCpu(unpack(startpc));
      endmethod
   endinterface
  interface ConnectalMemoryInitialization initProc;
	method Action done();
		$display("Done memory initialization");
// Make sure that no request is in flight
        memDone <= True;
	endmethod

	method Action request(Bit#(32) addr, Bit#(32) data);
		ind.wroteWord(0);
        let res = toDDR3Req(MemReq{op:St, addr:addr , data:data});
        if(debug) $display("data",fshow(res.data));
        if(debug) $display("addr",fshow(res.address));
        if(debug) $display("byteen",fshow(res.byteen));
        if(debug) $display("writeen",fshow(res.write));
        ddr3ReqFifo.enq(res);
	endmethod 
  endinterface
endmodule
