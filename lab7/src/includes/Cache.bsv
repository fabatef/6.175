
import CacheTypes::*;
import MemUtil::*;
import Fifo::*;
import Vector::*;
import Types::*;
import MemTypes::*;

module mkTranslator(WideMem wMem, Cache ifc);

	
	function CacheWordSelect getOffset(Addr addr) = truncate(addr >> 2);
	Fifo#(2, MemReq) ldReqs <- mkCFFifo; //storage for load requests

	//MemReq --> WideMemReq. enq load mem operations
	method Action req(MemReq r);
		if (r.op == Ld) ldReqs.enq(r);
		wMem.req(toWideMemReq(r));
	endmethod

	//WideMemResp (cacheline)--> MemResp(data). Select the data with the offset
	method ActionValue#(MemResp) resp;
		let ldReq = ldReqs.first; ldReqs.deq; 
		let cacheLine <- wMem.resp; let wOffset = getOffset(ldReq.addr); 
		return cacheLine[wOffset];
	endmethod 
endmodule


typedef enum { Ready, StartMiss, SendFillReq, WaitFillResp } ReqStatus deriving ( Bits, Eq );

//direct-mapped, write-miss allocate, writeback
module mkCache(WideMem wMem, Cache ifc);
    
    //cache memory
    Vector#(CacheRows, Reg#(CacheLine)) dataArray <- replicateM(mkRegU);
    Vector#(CacheRows, Reg#(Maybe#(CacheTag))) tagArray <- replicateM(mkReg(tagged Invalid));
    Vector#(CacheRows, Reg#( Bool)) dirtyArray <- replicateM(mkReg(False));
    
    Fifo#(1, Data) hitQ <- mkBypassFifo;
    Reg#(MemReq) missReq <- mkRegU;
    Reg#(ReqStatus) mshr <- mkReg(Ready);

    //tag|index|offset(4 bits)|00; 
    function CacheIndex  getIndex (Addr addr) = truncate(addr >> 6);
    function CacheWordSelect getOffset (Addr addr) = truncate(addr >> 2);
    function CacheTag getTag (Addr addr) = truncateLSB(addr);


    //initiate writeback to DRAM if the cacheline's valid and dirty
    rule startMiss(mshr == StartMiss);
        
        let idx = getIndex(missReq.addr);
        let tag = tagArray[idx];
        let dirty = dirtyArray[idx];

        if(isValid(tag) && dirty) begin
            let addr = {fromMaybe(?, tag), idx, 6'b0}; 
            let data = dataArray[idx];
            wMem.req(WideMemReq {write_en: '1, addr: addr, data: data});
        end

        mshr <= SendFillReq;   
    endrule
    
    //send a read request to DRAM via wideMem to fetch the block containing the request 
    rule sendFillReq(mshr == SendFillReq);

        WideMemReq wMemReq = toWideMemReq(missReq);
        Bit#(CacheLineWords) write_en = 0; wMemReq.write_en = write_en;
        wMem.req(wMemReq);

        mshr <= WaitFillResp;
    endrule
    
    //update the cache with DRAM's response, and put load response in hitQ
    rule waitFillResp(mshr == WaitFillResp);
        
        let idx = getIndex(missReq.addr);
        let tag = getTag(missReq.addr);
        let wOffset = getOffset(missReq.addr);
        let data <- wMem.resp;

        tagArray[idx] <= tagged Valid tag;

        if(missReq.op == Ld) begin 
        	dirtyArray[idx] <= False;
        	dataArray[idx] <= data;
        	hitQ.enq(data[wOffset]); 
        end
        else begin //store request (write miss allocation)
        	data[wOffset] = missReq.data; 
        	dirtyArray[idx] <= True;
        	dataArray[idx] <= data;
        end     
        
        mshr <= Ready;
        
    endrule
    
    method Action req(MemReq r) if (mshr == Ready);
        
        let idx = getIndex(r.addr); 
        let wOffset = getOffset(r.addr);
        let currTag = tagArray[idx]; let tag = getTag(r.addr);
        let hit = isValid(currTag) ? fromMaybe(?,currTag) == tag : False;

        if (hit) begin
        	let cacheLine = dataArray[idx];
        	if(r.op == Ld) hitQ.enq(cacheLine[wOffset]); //fetch the word from the cacheline and enq
        	else begin //store request
        	    cacheLine[wOffset] = r.data; //overwrite the new word on the line
        	    dataArray[idx] <= cacheLine; //update data array
        		dirtyArray[idx] <= True; //cacheline is updated so set dirty bit true
        	end
        end
        else begin //initiates cache update for store misses too
        	missReq <= r;
        	mshr <= StartMiss;
        end
    endmethod
    
    method ActionValue#(Data) resp;
        hitQ.deq;
        return hitQ.first;
    endmethod
    
endmodule