import CacheTypes::*;
import CacheTypes::*;
import Fifo::*;

// TODO: implement message FIFO

module mkMessageFifo(MessageFifo#(n));


	Fifo#(n, CacheMemResp) respFIFO <- mkCFFifo;
	Fifo#(n, CacheMemReq) reqFIFO <- mkCFFifo;


	method Action enq_resp( CacheMemResp d );
		respFIFO.enq(d);
	endmethod

	method Action enq_req( CacheMemReq d );
		reqFIFO.enq(d);
	endmethod

	method Bool hasResp = respFIFO.notEmpty;
	method Bool hasReq = reqFIFO.notEmpty;

	method Bool notEmpty = respFIFO.notEmpty || reqFIFO.notEmpty;

	//same as deq
	method CacheMemMessage first = respFIFO.notEmpty ? tagged Resp respFIFO.first : tagged Req reqFIFO.first;

	//as long as respFIFO has stuff in it, deq from there
	method Action deq;
		if(respFIFO.notEmpty) respFIFO.deq; else reqFIFO.deq;
	endmethod
endmodule
