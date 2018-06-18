// FourCycle.bsv
//
// This is a four cycle implementation of the RISC-V processor.

import Types::*;
import ProcTypes::*;
import MemTypes::*;
import MemInit::*;
import RFile::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Vector::*;
import Fifo::*;
import Ehr::*;
import GetPut::*;
import FPGAMemory::*;
typedef enum {
	Fetch,
	Decode,
	Execute,
	WriteBack
} Stage deriving(Bits, Eq, FShow);

(* synthesize *)
module mkProc(Proc);
    Reg#(Addr)    pc <- mkRegU;
    RFile         rf <- mkRFile;
    FPGAMemory mem <- mkFPGAMemory;
	let dummyInit     <- mkDummyMemInit;
    CsrFile       csrf <- mkCsrFile;

    Bool memReady = mem.init.done && dummyInit.done;

    Reg#(Stage) state <- mkReg(Fetch);
    Reg#(DecodedInst) dInst <- mkRegU;
    Reg#(ExecInst) eInst <- mkRegU;

    rule doFetch (csrf.started && state == Fetch);
        mem.req(MemReq{op: Ld, addr: pc, data: ?});
        state <= Decode;
    endrule

    rule doDecode (csrf.started && state == Decode);
        Data inst <- mem.resp;
        $display("pc: %h inst: (%h) expanded: ", pc, inst, showInst(inst));
        $fflush(stdout);
        dInst <= decode(inst);
        state <= Execute;
    endrule

    rule doExecute (csrf.started && state == Execute);
        // read general purpose register values 
        Data rVal1 = rf.rd1(fromMaybe(?, dInst.src1));
        Data rVal2 = rf.rd2(fromMaybe(?, dInst.src2));
        // read CSR values (for CSRR inst)
        Data csrVal = csrf.rd(fromMaybe(?, dInst.csr));
        // execute
        let _eInst = exec(dInst, rVal1, rVal2, pc, ?, csrVal);
        // memory
        if (_eInst.iType == Ld) begin
            mem.req(MemReq{op: Ld, addr: _eInst.addr, data: ?});
        end else if(_eInst.iType == St) begin
            mem.req(MemReq{op: St, addr: _eInst.addr, data: _eInst.data});
        end

        if (_eInst.iType == Unsupported) begin
            $fwrite(stderr, "ERROR: Executing unsupported instruction at pc: %x. Exiting\n", pc);
            $finish;
        end
        eInst <= _eInst;
        state <= WriteBack;
    endrule

    rule doWriteBack (csrf.started && state == WriteBack);
        let data = eInst.data;
        // retrieve load result 
        if (eInst.iType == Ld) begin
            data <- mem.resp;
        end
        // write back to reg file
        if (isValid(eInst.dst)) begin
            rf.wr(fromMaybe(?, eInst.dst), data);
        end
        // update the pc depending on whether the branch is taken or not
        pc <= eInst.brTaken ? eInst.addr : pc + 4;
        // CSR write for sending data to host & stats
        csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, data);
        state <= Fetch;
    endrule

    method ActionValue#(CpuToHostData) cpuToHost;
        let ret <- csrf.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if ( !csrf.started && memReady );
        csrf.start(0); // only 1 core, id = 0
        pc <= startpc;
    endmethod

	interface iMemInit = dummyInit;
    interface dMemInit = mem.init;
endmodule

