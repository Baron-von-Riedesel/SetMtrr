
# NMake makefile, creates SetMtrr.EXE
# tools used: JWasm, pestub

NAME=SetMtrr
OUTDIR=Release

ALL: $(OUTDIR) $(OUTDIR)\$(NAME).exe

$(OUTDIR):
	@mkdir $(OUTDIR)

$(OUTDIR)\$(NAME).exe: $(NAME).asm Makefile
	@cd $(OUTDIR)
	@jwasm.exe -nologo -pe -Fl ..\$(NAME).asm
	@pestub -n -x $(NAME).exe \hx\bin\loadpe.bin
	@cd ..

clean:
	@del $(OUTDIR)\*.exe

