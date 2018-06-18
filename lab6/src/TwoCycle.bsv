// TwoCycle.bsv
//
// This is a two cycle implementation of the RISC-V processor.

import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import MemInit::*;
import RFile::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Vector::*;
import Fifo::*;
import Ehr::*;

typedef enum {
	Fetch,
	Execute
} Stage deriving(Bits, Eq, FShow);

(* synthesize *)
module mkProc(Proc);
    Reg#(Addr) pc <- mkRegU;
    RFile      rf <- mkRFile;
    DMemory   mem <- mkDMemory;
	let dummyInit <- mkDummyMemInit;
    CsrFile  csrf <- mkCsrFile;
    //Ehr#(3, Addr) pc <- mkRegU;

    Bool memReady = mem.init.done && dummyInit.done;

	// TODO: complete implementation of this processor

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

