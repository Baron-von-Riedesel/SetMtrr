@echo off
cd Release
jwasm.exe -nologo -pe -Fl ..\SetMtrr.asm
pestub -n -x SetMtrr.exe \hx\bin\loadpe.bin
cd ..
