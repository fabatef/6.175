import Vector::*;
import Complex::*;

import FftCommon::*;
import Fifo::*;
import FIFOF::*;

interface Fft;
    method Action enq(Vector#(FftPoints, ComplexData) in);
    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
endinterface


(* synthesize *)
module mkFftCombinational(Fft);
    FIFOF#(Vector#(FftPoints, ComplexData)) inFifo <- mkFIFOF;
    FIFOF#(Vector#(FftPoints, ComplexData)) outFifo <- mkFIFOF;
    Vector#(NumStages, Vector#(BflysPerStage, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));

    function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
        Vector#(FftPoints, ComplexData) stage_temp, stage_out;
        for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)  begin
            FftIdx idx = i * 4;
            Vector#(4, ComplexData) x;
            Vector#(4, ComplexData) twid;
            for (FftIdx j = 0; j < 4; j = j + 1 ) begin
                x[j] = stage_in[idx+j];
                twid[j] = getTwiddle(stage, idx+j);
            end
            let y = bfly[stage][i].bfly4(twid, x);

            for(FftIdx j = 0; j < 4; j = j + 1 ) begin
                stage_temp[idx+j] = y[j];
            end
        end

        stage_out = permute(stage_temp);

        return stage_out;
    endfunction
  
    rule doFft;
            inFifo.deq;
            Vector#(4, Vector#(FftPoints, ComplexData)) stage_data;
            stage_data[0] = inFifo.first;
      
            for (StageIdx stage = 0; stage < 3; stage = stage + 1) begin
                stage_data[stage+1] = stage_f(stage, stage_data[stage]);
            end
            outFifo.enq(stage_data[3]);
    endrule
    
    method Action enq(Vector#(FftPoints, ComplexData) in);
        inFifo.enq(in);
    endmethod
  
    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
        outFifo.deq;
        return outFifo.first;
    endmethod
endmodule

(* synthesize *)
module mkFftInelasticPipeline(Fft);
    FIFOF#(Vector#(FftPoints, ComplexData)) inFifo <- mkFIFOF;
    FIFOF#(Vector#(FftPoints, ComplexData)) outFifo <- mkFIFOF;
    Vector#(3, Vector#(16, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));

    Reg #(Maybe #( Vector#(FftPoints, ComplexData))) sReg1 <- mkRegU;
    Reg #(Maybe #( Vector#(FftPoints, ComplexData))) sReg2 <- mkRegU;


    function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
        Vector#(FftPoints, ComplexData) stage_temp, stage_out;
        for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)  begin
            FftIdx idx = i * 4;
            Vector#(4, ComplexData) x;
            Vector#(4, ComplexData) twid;
            for (FftIdx j = 0; j < 4; j = j + 1 ) begin
                x[j] = stage_in[idx+j];
                twid[j] = getTwiddle(stage, idx+j);
            end
            let y = bfly[stage][i].bfly4(twid, x);

            for(FftIdx j = 0; j < 4; j = j + 1 ) begin
                stage_temp[idx+j] = y[j];
            end
        end

        stage_out = permute(stage_temp);

        return stage_out;
    endfunction

    rule doFft;

        // At stage 0, doing the first bfly + permute stage. 
        //If inFifo is empty and there are outputs on reg1 and reg2...which there will be on the last stage
        // you're not pushing them out.
        if (inFifo.notEmpty) begin
            sReg1 <= tagged Valid (stage_f(0, inFifo.first)); inFifo.deq; end
        else begin sReg1 <= tagged Invalid; end

        //if the fifo is empty and sReg1 is valid, push its value into sreg2

        // At stage 1, doing the second bfly + permute
        sReg2 <= (isValid (sReg1) ? tagged Valid stage_f(1, fromMaybe (?, sReg1)) : tagged Invalid);

        // Last stage
        if (isValid (sReg2)) begin
            outFifo.enq(stage_f(2, fromMaybe(?, sReg2))); end
    endrule

    method Action enq(Vector#(FftPoints, ComplexData) in);
        inFifo.enq(in);
    endmethod
  
    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
        outFifo.deq;
        return outFifo.first;
    endmethod
endmodule

(* synthesize *)
module mkFftElasticPipeline(Fft);
    //FIFOF#(Vector#(FftPoints, ComplexData)) inFifo <- mkFIFOF;
    //FIFOF#(Vector#(FftPoints, ComplexData)) outFifo <- mkFIFOF;
    Fifo#(3, Vector#(FftPoints, ComplexData)) inFifo <- mkFifo;
    Fifo#(3, Vector#(FftPoints, ComplexData)) outFifo <- mkFifo;

    Vector#(3, Vector#(16, Bfly4)) bfly <- replicateM(replicateM(mkBfly4));

    Fifo#(3, Vector#(FftPoints, ComplexData)) fifo1 <- mkFifo;
    Fifo#(3, Vector#(FftPoints, ComplexData)) fifo2 <- mkFifo;
    //FIFOF#(Vector#(FftPoints, ComplexData)) fifo1 <- mkFIFOF;
    //FIFOF#(Vector#(FftPoints, ComplexData)) fifo2 <- mkFIFOF;


    function Vector#(FftPoints, ComplexData) stage_f(StageIdx stage, Vector#(FftPoints, ComplexData) stage_in);
        Vector#(FftPoints, ComplexData) stage_temp, stage_out;
        for (FftIdx i = 0; i < fromInteger(valueOf(BflysPerStage)); i = i + 1)  begin
            FftIdx idx = i * 4;
            Vector#(4, ComplexData) x;
            Vector#(4, ComplexData) twid;
            for (FftIdx j = 0; j < 4; j = j + 1 ) begin
                x[j] = stage_in[idx+j];
                twid[j] = getTwiddle(stage, idx+j);
            end
            let y = bfly[stage][i].bfly4(twid, x);

            for(FftIdx j = 0; j < 4; j = j + 1 ) begin
                stage_temp[idx+j] = y[j];
            end
        end

        stage_out = permute(stage_temp);

        return stage_out;
    endfunction

    //TODO: Implement the rest of this module
    // You should use more than one rule

    rule stage0;
        fifo1.enq(stage_f(0, inFifo.first)); inFifo.deq;
    endrule

    rule stage1;
        fifo2.enq(stage_f(1, fifo1.first)); fifo1.deq;  
    endrule

    rule stage2;
        outFifo.enq(stage_f(2, fifo2.first)); fifo2.deq;
    endrule

    method Action enq(Vector#(FftPoints, ComplexData) in);
        inFifo.enq(in);
    endmethod
  
    method ActionValue#(Vector#(FftPoints, ComplexData)) deq;
        outFifo.deq;
        return outFifo.first;
    endmethod
endmodule

