// Six stage

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

// Data structure for Fetch to Decode stage
typedef struct {
    Addr pc;
    Addr predPc;
    Bool epoch;
} Fetch2Decode deriving (Bits, Eq);

// Data structure for Decode to RegRead stage
typedef struct {
	Addr pc;
	Addr predPc;
    Bool epoch;
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
    Bool epoch;
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

(* synthesize *)
module mkProc(Proc);
    Ehr#(2, Addr) pcReg <- mkEhr(?);
    RFile            rf <- mkRFile;
	Scoreboard#(6)   sb <- mkBypassScoreboard; 
	FPGAMemory     iMem <- mkFPGAMemory;
    FPGAMemory     dMem <- mkFPGAMemory;
    CsrFile        csrf <- mkCsrFile;
    Btb#(6)         btb <- mkBtb; // 64-entry BTB

	// global epoch for redirection from Execute stage
	Ehr#(2, Bool) exeEpoch <- mkEhr(False);


	// FIFOs between stages
	Fifo#(2, Fetch2Decode)           f2d <- mkBypassFifo;
    Fifo#(2, Decode2RegRead)         d2r <- mkBypassFifo;
    Fifo#(2, RegRead2Exec)           r2e <- mkBypassFifo;
    Fifo#(2, Maybe#(Exec2WriteBack)) e2m <- mkBypassFifo; //mem just needs the contents, won't change any state
    Fifo#(2, Maybe#(Exec2WriteBack)) m2w <- mkBypassFifo;

    Bool memReady = iMem.init.done && dMem.init.done;

    //fetch pc and send fetch bundle to decode only if there is no stall
	rule doFetch(csrf.started);

		//request instruction fetch
		iMem.req(MemReq{op: Ld, addr: pcReg[0], data: ?});

		//make next PC prediction
		let predPc = btb.predPc(pcReg[0]);

		//line up next PC 
		pcReg[0] <= predPc;


		//make the f2d 
		Fetch2Decode f2d_bundle = Fetch2Decode {
			pc: pcReg[0],
			predPc: predPc,
			epoch: exeEpoch[0]
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
		let inst <- iMem.resp;

		$display("Decode: PC = %x, inst = %x, expanded =", d.pc, inst, showInst(inst));

		//decode the instruction
		let dInst = decode(inst);

		//make the d2r bundle 
		Decode2RegRead d2r_bundle = Decode2RegRead {
			pc: d.pc,
			predPc: d.predPc,
			epoch: d.epoch,
			dInst:dInst
		};

		//enq d2r bundle for the RegRead stage
		d2r.enq(d2r_bundle);

		//deq used f2d bundle
		f2d.deq;
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
			epoch: regR.epoch
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
		if(e.epoch != exeEpoch[1]) begin

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
				
				exeEpoch[1] <= !exeEpoch[1]; // flip epoch
				pcReg[1] <= eInst.addr; // correct PC
				btb.update(e.pc, eInst.addr); // train BTB
				$display("Execute: handling misprediction. PC= %x, PPC= %x", e.pc, e.predPc);

			end

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
			let m = fromMaybe(?, do_m);			
			// memory
			if(m.iType == Ld) begin
				$display("Memory: requesting dmem for load ");
				dMem.req(MemReq{op: Ld, addr: m.addr, data: ?});
			end else if(m.iType == St) begin
				$display("Memory: requesting dmem for store ");
				dMem.req(MemReq{op: St, addr: m.addr, data: m.data});
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
			if (wb.iType == Ld) begin
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

    method ActionValue#(CpuToHostData) cpuToHost if(csrf.started);
        let ret <- csrf.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if ( !csrf.started && memReady );
	$display("Start cpu");
        csrf.start(0); // only 1 core, id = 0
        pcReg[0] <= startpc;
    endmethod

	interface iMemInit = iMem.init;
    interface dMemInit = dMem.init;
endmodule

