#!/bin/bash

## putting aws-fpga in home directory so it only has to build once
test -d connectal || git clone https://github.com/cambridgehackers/connectal
test -d fpgamake || git clone https://github.com/cambridgehackers/fpgamake
curl http://www.dabeaz.com/ply/ply-3.9.tar.gz | tar -zxf -
cd connectal/scripts/
ln -sf ../../ply-3.9/ply .
cd ../..
curl http://plath.csail.mit.edu:8000/program.tar.gz | tar -zxf -
sed -i 's/python script/python2.7 script/g' connectal/Makefile
