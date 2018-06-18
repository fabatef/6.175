import CacheTypes::*;
import Vector::*;
import FShow::*;
import MemTypes::*;
import Types::*;
import ProcTypes::*;
import Fifo::*;
import Ehr::*;
import RefTypes::*;

// TODO: implement blocking D cache

typedef enum { Ready, StartMiss, SendFillReq, WaitFillResp } ReqStatus deriving ( Bits, Eq );

module mkDCache#(CoreID id)(
		MessageGet fromMem, 
		MessagePut toMem,
		RefDMem refDMem, // debug: reference data mem
		DCache ifc
	);
    
    //cache memory
    Vector#(CacheRows, Reg#(CacheLine)) dataArray <- replicateM(mkRegU);
    Vector#(CacheRows, Reg#(Maybe#(CacheTag))) tagArray <- replicateM(mkReg(tagged Invalid));
    Vector#(CacheRows, Reg#(Bool)) dirtyArray <- replicateM(mkReg(False));
    Vector#(CacheRows, Reg#(MSI)) state <- replicateM(mkReg(I));

    
    Fifo#(1, Data) hitQ <- mkBypassFifo;
    Reg#(MemReq) missReq <- mkRegU;
    Reg#(ReqStatus) mshr <- mkReg(Ready);