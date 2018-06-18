// Six stage BHT

import Types::*;
import ProcTypes::*;
import MemTypes::*;
import MemInit::*;
import RFile::*;
import FPGAMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Fifo::*;
import Ehr::*;
import Btb::*;
import Scoreboard::*;
import Bht::*;

import GetPut::*;
import ClientServer::*;
import Memory::*;
import CacheTypes::*;
import WideMemInit::*;
import MemUtil::*;
import Vector::*;
import Cache::*;

// Data structure for Fetch to Decode stage
typedef struct {
    Addr pc;
    Addr predPc;
    Bool eEpoch;
    Bool dEpoch;
} Fetch2Decode deriving (Bits, Eq);

// Data structure for Decode to RegRead stage
typedef struct {
	Addr pc;
	Addr predPc;
    Bool eEpoch;
	DecodedInst dInst;
} Decode2RegRead deriving (Bits, Eq);

// Data structure for RegRead to Execute stage
typedef struct {
	Addr pc;
	Addr predPc;
	DecodedInst dInst;
	Data rVal1;
    Data rVal2;
    Data csrVal;
    Bool eEpoch;
} RegRead2Exec deriving (Bits, Eq);

// Data structure for Execute to WriteBack

typedef struct {
	Maybe#(RIndx) dst;
	Data data;
	Addr addr;
	IType iType;
	Maybe#(CsrIndx) csr;
} Exec2WriteBack deriving (Bits, Eq);

// redirect msg from Execute stage
typedef struct {
	Addr pc;
	Addr nextPc;
} ExeRedirect deriving (Bits, Eq);

//(* synthesize *)
module mkCore#(CoreID id)(
	WideMem iMem, 
	RefDMem refDMem, // debug: reference data mem
	Core ifc
);
    Ehr#(3, Addr) pcReg <- mkEhr(?);
    RFile            rf <- mkRFile;
	Scoreboard#(6)   sb <- mkBypassScoreboard; 
    CsrFile        csrf <- mkCsrFile;
    Btb#(6)         btb <- mkBtb; // 64-entry BTB
    DirectionPred#(6) bht <- mkBHT;


    WideMem wideMemWrapper <- mkWideMemFromDDR3(ddr3ReqFifo, ddr3RespFifo);

   //   Bool memReady = iMem.init.done && dMem.init.done;
 	//WideMemInitIfc memInit <- mkWideMemInitDDR3(ddr3ReqFifo);
 	Bool memReady = True;

    Vector#(2, WideMem) wideMems <- mkSplitWideMem (memReady && csrf.started, wideMemWrapper);

    Cache iMem <- mkCache(wideMems[1]);
    Cache dMem <- mkCache(wideMems[0]);



	// global epochs for redirection from Execute and Decode Stage
	Ehr#(3, Bool) exeEpoch <- mkEhr(False);
	Ehr#(2, Bool) decEpoch <- mkEhr(False);
	// FIFOs between stages
	Fifo#(2, Fetch2Decode)           f2d <- mkBypassFifo;
    Fifo#(2, Decode2RegRead)         d2r <- mkBypassFifo;
    Fifo#(2, RegRead2Exec)           r2e <- mkBypassFifo;
    Fifo#(2, Maybe#(Exec2WriteBack)) e2m <- mkBypassFifo; 
    Fifo#(2, Maybe#(Exec2WriteBack)) m2w <- mkBypassFifo;

	// mem req id
	MemReqIDGen memReqIDGen <- mkMemReqIDGen;

    // I mem
    ICache iCache <- mkICache( iMem );

	// D cache
	MessageFifo#(2) toParentQ <- mkMessageFifo; 
	MessageFifo#(2) fromParentQ <- mkMessageFifo; 
    DCache dCache <- mkDCache(
		id,
		toMessageGet(fromParentQ),
		toMessagePut(toParentQ),
		refDMem
	);


    //added J iType for JAL redirection 
    function Bool isBranch(IType iType) = (iType == J || iType == Br );

    //fetch pc and send fetch bundle to decode only if there is no stall
	rule doFetch(csrf.started);

		//request instruction fetch
		iCache.req(pcReg[0]);

		//make next PC prediction
		let predPc = btb.predPc(pcReg[0]);

		//line up next PC 
		pcReg[0] <= predPc;


		//make the f2d 
		Fetch2Decode f2d_bundle = Fetch2Decode {
			pc:     pcReg[0],
			predPc: predPc,
			eEpoch: exeEpoch[0],
			dEpoch: decEpoch[0]
		};

		//enq the f2d bundle for the Decode stage
		f2d.enq(f2d_bundle);

		$display("Fetch: PC = %x ", pcReg[0]);

	endrule

	//only do decode and send send decode bundle to regRead if there is not stall issued
	rule doDecode(csrf.started);

		//get f2d's bundle
		let d = f2d.first;

		//retrive requested instruction
		let inst <- iCache.resp;

		//decode the instruction
		let dInst = decode(inst);

		//if the bundle's  eEpoch doesn't match the global one, kill wrong path instruction.
		if (d.eEpoch != exeEpoch[1]) begin
			$display("Decode: PC = %x has wrong global execEpoch. Killed. dec->reg Fifo is notEmpty: %b", d.pc, d2r.notEmpty);
			
			f2d.deq;
		end
		else begin
			//if the bundle's dEpoch doesn't match the global one, kill wrong path instruction
			if (d.dEpoch != decEpoch[1]) begin
				$display("Decode: PC = %x has wrong global decEpoch. Killed. dec->reg Fifo is notEmpty: %b", d.pc, d2r.notEmpty);
				f2d.deq;
			end
			else begin
				//get bht prediction for branch instruction
				//d.pc + fromMaybe(?, dInst.imm) -- targetPC for J insts, and BR insts if taken
				let ppcDP = isBranch(dInst.iType) ? bht.ppcDP(d.pc, d.pc + fromMaybe(?, dInst.imm)) : d.predPc;


				//if BHT's prediction and predPC of current instruction don't match, flip dEpoch and redirect
				if (ppcDP != d.predPc) begin
					decEpoch[1] <= !decEpoch[1]; //flip
					pcReg[1] <= ppcDP; //redirect
					$display("Decode: for PC = %x with PPC = %x, BHT has triggered redirection to PC = %x", d.pc, d.predPc, ppcDP);
				end			

				//normal action

				//make the d2r bundle 
				Decode2RegRead d2r_bundle = Decode2RegRead {
					pc: d.pc,
					predPc: ppcDP,
					eEpoch: d.eEpoch,
					dInst:dInst
				};

				$display("Decode: PC = %x, inst = %x, expanded =", d.pc, inst, showInst(inst));

				//enq d2r bundle for the RegRead stage
				d2r.enq(d2r_bundle);

				//deq used f2d bundle
				f2d.deq;
			end
		end
	endrule

	rule doRegRead(csrf.started);

		//get d2r bundle
		let regR = d2r.first;

		//reg read
		Data rVal1 = rf.rd1(fromMaybe(?, regR.dInst.src1));
		Data rVal2 = rf.rd2(fromMaybe(?, regR.dInst.src2));
		Data csrVal = csrf.rd(fromMaybe(?, regR.dInst.csr));

		// make r2e bundle
		RegRead2Exec r2e_bundle = RegRead2Exec {
			pc: regR.pc,
			predPc: regR.predPc,
			dInst: regR.dInst,
			rVal1: rVal1,
			rVal2: rVal2,
			csrVal: csrVal,
			eEpoch: regR.eEpoch
		};

		//stall condition to prevent RAW hazard
		Bool stall = sb.search1(regR.dInst.src1) || sb.search2(regR.dInst.src2);

		//no stall needed
		if(!stall) begin

			// enque r2e bundle for exec stage
			r2e.enq(r2e_bundle);

			//update the scoreboard
			sb.insert(regR.dInst.dst);

			//deq used d2r bundle
			d2r.deq;
			$display("RegRead: no stall, enqued PC= %x for execution", regR.pc);

		end
		else begin

			$display("RegRead Stalled: PC = %x. Nothing enqued", regR.pc);
		end
	endrule

	rule doExecute(csrf.started);

		//get r2e bundle
		let e = r2e.first;

		//if global epoch and current bundle's epoch don't match, poision instruction
		if(e.eEpoch != exeEpoch[2]) begin

			//e2m bundle... invalid
			Maybe#(Exec2WriteBack) e2m_bundle = tagged Invalid;

			//enq e2m bundle for mem stage
			e2m.enq(e2m_bundle);
			$display("Execute: enqueing poisioned instruction at PC= %x", e.pc);
		end
		else begin
			// execute
			let eInst = exec(e.dInst, e.rVal1, e.rVal2, e.pc, e.predPc, e.csrVal);  
			
			//check unsupported instruction. Exiting
			if(eInst.iType == Unsupported) begin
				$fwrite(stderr, "ERROR: Executing unsupported instruction at pc: %x. Exiting\n", e.pc);
				$finish;
			end

			// check misprediction and redirect
			if(eInst.mispredict) begin 
				exeEpoch[2] <= !exeEpoch[2];
				pcReg[2] <= eInst.addr; // correct PC
				btb.update(e.pc, eInst.addr); // train BTB
				$display("Execute: handling misprediction. PC= %x, PPC= %x, CorrectNextPC= %x", e.pc, e.predPc, eInst.addr);
			end

			//if the executed instruction is a branch instruction, train the BHT with the branching data
			if (isBranch(eInst.iType)) bht.update(e.pc, eInst.brTaken);

			//make e2m bundle
			Maybe#(Exec2WriteBack) mem_bundle = tagged Valid (Exec2WriteBack{
				dst: eInst.dst,
				data: eInst.data,
				addr: eInst.addr,
				iType: eInst.iType,
				csr: eInst.csr
			});

			//enq mem_bundle for the Mem stage
			e2m.enq(mem_bundle);
			$display("Execute: enqued executed instruction at PC= %x for mem stage", e.pc);
		end

		//deq used r2e bundle 
		r2e.deq;

	endrule

	rule doMem(csrf.started);

		//get e2m bundle and do memory requests if it's valid
		let do_m = e2m.first;
		if ( isValid(do_m)) begin
			let eInst = fromMaybe(?, do_m);			
			// memory
			if(eInst.iType == Ld) begin
                let rid <- memReqIDGen.getID;
                let r = MemReq{op: Ld, addr: eInst.addr, data: ?, rid: rid};
                dCache.req(r);
                $display("%0t: core %d: Memory: issue mem req for Ld, dCache.req addr = %x, MemReq = ",$time, id, eInst.addr, fshow(r), "\n");
           	end else if(eInst.iType == St) begin
                let rid <- memReqIDGen.getID;
                let r = MemReq{op: St, addr: eInst.addr, data: eInst.data, rid: rid};
                dCache.req(r);
                $display("%0t: core %d: Memory: issue mem req for St, dCache.req addr = %x, MemReq =", $time, id, eInst.addr, fshow(r), "\n");
           	end else if(eInst.iType == Lr) begin
                let rid <- memReqIDGen.getID;
                let r = MemReq{op: Lr, addr: eInst.addr, data: ?, rid: rid};
                dCache.req(r);
                $display("%0t: core %d: Memory: issue mem req for Lr, dCache.req addr = %x, MemReq = ",$time, id, eInst.addr, fshow(r), "\n");
            end else if(eInst.iType == Sc) begin
                let rid <- memReqIDGen.getID;
                let r = MemReq{op: Sc, addr: eInst.addr, data: eInst.data, rid: rid};
                dCache.req(r);
                $display("%0t: core %d: Memory: issue mem req for store Sc, dCache.req addr = %x, MemReq =", $time, id, eInst.addr, fshow(r), "\n");
            end
		end
		else begin
			$display("Memory: got poisioned instruction. ignored");
		end

		$display("Memory: fired");
		//no state modified, just enq the e2m bundle over to WB stage
		m2w.enq(do_m);
		//deq used e2m bundle
		e2m.deq;
	endrule

	rule doWriteBack(csrf.started);

		//get m2w bundle and do writeback if valid
		let do_wb = m2w.first;
		if ( isValid(do_wb)) begin
			let wb = fromMaybe(?, do_wb);

			//if the instruction is a load, update wb.data with response from Dmem
			if (wb.iType == Ld || wb.iType == Lr  || wb.iType == Sc) begin
				wb.data <- dMem.resp;
			end

			// write back to reg file
			if(isValid(wb.dst)) begin
				rf.wr(fromMaybe(?, wb.dst), wb.data);
			end

			csrf.wr(wb.iType == Csrw ? wb.csr : Invalid, wb.data);

			$display("WriteBack: done");

		end 
		else begin
			$display("WriteBack: got poisioned instruction. ignored");
		end

		// remove dst reg from scoreboard
		sb.remove;
		//deq used m2w bundle
		m2w.deq;
	endrule

    interface MessageGet toParent = toMessageGet(toParentQ);
    interface MessagePut fromParent = toMessagePut(fromParentQ);
        

    method ActionValue#(CpuToHostData) cpuToHost if(csrf.started);
        let ret <- csrf.cpuToHost;
        return ret;
    endmethod

    method Bool cpuToHostValid = csrf.cpuToHostValid;

    method Action hostToCpu(Bit#(32) startpc) if ( !csrf.started);
	    $display("Start cpu");
        csrf.start; 
        pcReg[0] <= startpc;
    endmethod

endmodule



