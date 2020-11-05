
;*** use IDT to jump in ring 0 and get/set msr registers
;*** requires read/write access to IDT, read access to GDT

	.586
	.MODEL FLAT,stdcall
	option casemap:none

	.nolist
	.nocref
	include vesa32.inc
	include dpmi.inc
	.list
	.cref

@pe_file_flags = 10eh	;jwasm format -pe flags, telling jwasm to create relocations

MSR_MTRRCAPS equ 0FEh
MSR_MTRR_VAR equ 200h
MSR_MTRR_FIX64 equ 250h
MSR_MTRR_FIX16 equ 258h
MSR_MTRR_FIX4  equ 268h
MSR_MTRRDEFTYPE equ 02FFh
MSR_SYSCFG equ 0C0010010h	; AMD only
MSR_TOP_MEM equ 0C001001Ah	; AMD only
MSR_TOP_MEM2 equ 0C001001Dh	; AMD only

MTRR_VALID equ 800h	;flag in variable MTRR telling that the region is valid

;--- flags in MTRR default reg (MSR #2FF)
MTRR_ENABLED equ 800h
MTRR_FIXED_ENABLED equ 400h
MTRR_FIXED_SUPP equ 100h	;flag in MTRR cap msr

CStr macro text:vararg
local sym
	.const
sym db text,0
	.code
	exitm <offset sym>
endm

REGS	struct
_eax	dd ?
_edx	dd ?
_ecx	dd ?
_ebx	dd ?
REGS	ends

BINT	equ 5	;use this idt entry

	.data

dwLDT	dd 0
dqLDT0	dq 0
bUsage	db 0
bStatus  db 0
bSetVesa db 0
bSetFixed db 0
;bDisableMtrr db 0
bSysCfg  db 0

regions label word
	dw 3*4 dup (0)
endregions label byte

	.CODE

readmsr proc
	rdmsr
	iretd
	align 4
readmsr endp

writemsr proc
	wrmsr
	iretd
	align 4
writemsr endp

;--- call a ring 0 proc

r0proc proc uses esi edi ebx lpfnProc:dword, dwCodeSel:dword, regs:ptr REGS

local	idt:fword

	sidt idt
	mov edi,dword ptr idt+2
	add edi,BINT*8
	mov ecx, lpfnProc

	cli
	push [edi+0]
	push [edi+4]

	mov [edi+0],cx
	shr ecx,16
	mov [edi+6],cx
if 0
	and byte ptr [edi+4],09Fh	;reset priviledge level to r0 in int gate
else
	mov word ptr [edi+4],0EE00h	;386 interrupt gate
endif
	mov eax, dwCodeSel
	mov [edi+2],ax
	mov esi, regs

	mov eax, [esi].REGS._eax
	mov ecx, [esi].REGS._ecx
	mov edx, [esi].REGS._edx
	mov ebx, [esi].REGS._ebx

	int BINT

	mov [esi].REGS._eax, eax
	mov [esi].REGS._ecx, ecx
	mov [esi].REGS._edx, edx 
	mov [esi].REGS._ebx, ebx 

	pop [edi+4]
	pop [edi+0]

	sti
	ret
	align 4
r0proc endp

	include printf.inc

	.586

;--- get VESA information ( int 10h, ax=4F00h )

GetVesaInfo_ proc public uses edi esi ebx pVesaInfo:ptr VESAINFO

local	dosmemsel:dword
local	linDosMem:dword
local	rcptr:dword
local	rmcs:RMCS

	xor eax,eax
	mov dosmemsel,eax
	mov ax,0100h		;alloc DOS memory
	mov bx,20h			;256+256 bytes = sizeof VESAINFO
	int 31h
	jc svmx_er
	mov dosmemsel,edx
	mov rmcs.rES,ax
	mov rmcs.rAX,4F00h
	mov rmcs.rDI,0
	mov rmcs.rSSSP,0
						;clear the VESA info buffer
	movzx eax,ax
	shl eax,4
	mov linDosMem, eax

	mov edi,eax
	mov ecx,200h/4
	xor eax,eax
	rep stosd

	mov ebx, linDosMem
;	mov eax,"ASEV"
	mov eax,"2EBV"
	mov [ebx],eax
	push ebx
	lea edi,rmcs
	mov bx,0010h
	mov cx,0000h
	mov ax,0300h
	int 31h
	pop ebx
	jc svmx_er
	cmp rmcs.rAX,004Fh
	jnz svmx_er
	mov esi,linDosMem 
	mov edi, pVesaInfo
	mov ecx,sizeof VESAINFO
	rep movsb
	mov eax,1
	jmp svmx_ex
svmx_er:
	xor eax,eax
svmx_ex:
	mov edx,dosmemsel
	and edx,edx
	jz @F
	push eax
	mov ax,0101h
	int 31h
	pop eax
@@:
	ret
	align 4
GetVesaInfo_ endp

;--- copied from VESA32.ASM, but without phys2lin translation of LFB

GetVesaLFBAddr proc public uses ebx esi edi dwMode:dword

local	linmem:dword
local	dosmemsel:dword
local	rmcs:RMCS

	xor eax,eax
	mov dosmemsel,eax
	mov ax,0100h				  ;alloc DOS memory
	mov bx,10h					  ;256 bytes (sizeof SVGAINFO)
	int 31h
	jc getvesainfo_er
	mov dosmemsel,edx

;--- get svga info ( int 10h, ax=4F01h )

	mov rmcs.rSSSP,0
	mov rmcs.rDI,0
	mov rmcs.rES,ax
	mov rmcs.rAX,4F01h
	mov ecx,dwMode
	and ch,03Fh 				  ;
	mov rmcs.rCX,cx
	movzx eax,ax
	shl eax,4
	mov linmem,eax
	mov edi,eax
	mov ecx,sizeof SVGAINFO/4
	xor eax,eax
	rep stosd
	lea edi,rmcs
	push es
	push ss
	pop es
	mov bx,0010h
	mov cx,0000h
	mov ax,0300h
	int 31h
	pop es
	jc getvesainfo_er
	cmp rmcs.rAX,004Fh
	jnz getvesainfo_er

	mov edi,linmem
	test [edi].SVGAINFO.ModeAttributes, VESAATTR_LFB_SUPPORTED
	jz noLFB
	mov eax,[edi].SVGAINFO.PhysBasePtr
	and eax, eax
	jz noLFB
	jmp getvesainfo_ex
noLFB:
getvesainfo_er:
	xor eax,eax
getvesainfo_ex:
	mov edx,dosmemsel
	and edx,edx
	jz @F
	push eax
	mov ax,0101h
	int 31h
	pop eax
@@:
	ret
	align 4
GetVesaLFBAddr endp

;--- set VESA LFB to WC
;--- scan variable MTRRs and see if there is already one for LFB

SetVesa proc dwCaps:dword, pRegs:ptr REGS, r0cs:dword

local dwPhysBase:dword
local dwLFBSize:dword
local vesainfo:VESAINFO

	invoke GetVesaLFBAddr, 101h
	.if (!eax)
		invoke printf, CStr("cannot get VESA LFB information",10)
		jmp exit
	.endif
	mov dwPhysBase, eax
	invoke GetVesaInfo_, addr vesainfo
	.if (!eax)
		invoke printf, CStr("cannot get VESA video memory size",10)
		jmp exit
	.endif
	movzx eax, vesainfo.TotalMemory	;in 64 kB blocks
	shl eax, 16
	mov dwLFBSize, eax

	movzx ebx, byte ptr [dwCaps]	;get number of variable MTRRs
	xor esi, esi
	mov edi, pRegs
	.while (ebx)
		.if ([edi+sizeof REGS].REGS._eax & MTRR_VALID)
			mov ecx, [edi].REGS._eax
			mov edx, [edi].REGS._edx
			mov ax,cx
			and cx, 0F000h
			.if ((ecx == dwPhysBase) && (edx == 0) && al == 1 ) ;phys base and WC set?
				jmp found
			.endif
			.if ecx <= dwPhysBase && edx == 0 && al == 0
				call getupperlimit
				.if eax > dwPhysBase
					invoke printf, CStr("WARNING: region with type UC overlaps VESA LFB region",10)
				.endif
			.endif
if 0
			invoke printf, CStr("msr %X+%X: %X%08X %X%08X "),
				[edi].REGS._ecx, [EDI+sizeof REGS].REGS._ecx,
				[edi].REGS._edx, [edi].REGS._eax,
				[edi+sizeof REGS].REGS._edx, [edi+sizeof REGS].REGS._eax
endif
		.elseif (!esi)
			mov esi, edi
		.endif
		add edi, 2 * sizeof REGS
		dec ebx
	.endw
	.if (!esi)
		invoke printf, CStr("all MTRRs are in use, VESA LFB is not among them",10)
		jmp exit
	.endif
	mov edi, esi
found:
	.if ([edi+sizeof REGS].REGS._eax & MTRR_VALID)
		mov ecx, 0
		sub ecx, dwLFBSize
		or ch,08
		mov al, byte ptr [edi].REGS._eax
		.if ((al == 1) && (ecx == [edi+sizeof REGS].REGS._eax))
			mov eax, [edi].REGS._ecx
			mov edx, [edi+sizeof REGS].REGS._ecx
			invoke printf, CStr("MSRs %X and %X already set to speed VESA LFB access",10), eax, edx
			jmp exit
		.endif
	.endif

;--- no MTRR for LFB has been found, write one

	.if (dwCaps & 400h)	;WC supported?
		mov eax, dwPhysBase
		mov al, 01				;set WC type
		mov [edi].REGS._eax, eax
		mov [edi].REGS._edx, 0
		mov eax, 0
		sub eax, dwLFBSize
		or ah,8					;valid entry
		mov [edi+sizeof REGS].REGS._eax, eax
		mov [edi+sizeof REGS].REGS._edx, 0Fh 
		invoke r0proc, offset writemsr, r0cs, edi
		add edi, sizeof REGS
		invoke r0proc, offset writemsr, r0cs, edi

		mov eax, [edi-sizeof REGS].REGS._ecx
		mov edx, [edi].REGS._ecx
		mov ecx, dwLFBSize
		add ecx, dwPhysBase
		dec ecx
		invoke printf, CStr("SetMtrr: region %X-%X changed to WC [MSRs %X-%X modified]",10), dwPhysBase, ecx, eax, edx
	.else
		invoke printf, CStr("WC memory type not supported, no MSR modified",10)
	.endif
exit:
	ret
getupperlimit:
	mov eax,[edi+sizeof REGS].REGS._eax
	mov edx,[edi+sizeof REGS].REGS._edx
	test ah,8
	jz notvalid
	and ax,0F000h
	xor eax,-1
	xor edx,-1
	mov ecx,[edi].REGS._eax
	and cx,0F000h
	add eax,ecx
	adc edx,[edi].REGS._edx
	retn
notvalid:
	xor eax,eax
	retn
	align 4
SetVesa endp

;--- get cmdline parameters

GetCmdLine proc uses ebx esi edi

local	rmcs:RMCS

	mov rmcs.rSSSP,0
	mov rmcs.rAX,5100h
	mov rmcs.rFlags,3202h
	lea edi,rmcs
	mov bx,0021h
	xor ecx,ecx
	mov ax,0300h
	int 31h
	movzx ebx,rmcs.rBX
	shl ebx,4

	lea esi, [ebx+0080h]
	lodsb
	mov cl, al
	mov edi, offset regions
	.while (cl)
		lodsb
		dec cl
		.if ((al == '/') || (al == '-'))
			.if (cl)
				lodsb
				dec cl
				or al,20h
				.if ((al == '?') || (al == 'h'))
					mov bUsage, 1
				.elseif (al == 's')
					mov bSetVesa,1
;				.elseif (al == 'd')
;					mov bDisableMtrr,1
				.elseif (((al == 'b') || (al == 'c') || (al == 'p') || (al == 't') || (al == 'u')) && (byte ptr [esi] == '='))
					mov bSetFixed,1
					mov bl,al
					inc esi
					dec cl
					call gethex
					.if ((!CARRY?) && (eax < 10000h))
						.if (byte ptr [esi] == '-')
							push eax
							inc esi
							dec cl
							call gethex
							pop edx
							.if ((!CARRY?) && (eax < 10000h) && (eax > edx))
								.if (edi < offset endregions)
									mov [edi+0],dx
									mov [edi+2],ax
									.if (bl == 'b')
										mov word ptr [edi+4],"WB"
									.elseif (bl == 'c')
										mov word ptr [edi+4],"WC"
									.elseif (bl == 'p')
										mov word ptr [edi+4],"WP"
									.elseif (bl == 't')
										mov word ptr [edi+4],"WT"
									.elseif (bl == 'u')
										mov word ptr [edi+4],"UC"
									.endif
									add edi, 3*2
								.endif
							.endif
						.endif
					.endif
				.endif
			.endif
		.else
			.if al != ' ' && al != 9
				jmp error
			.endif
		.endif
	.endw
	mov eax,1
	ret
error:
	xor eax, eax
	ret
gethex:
	mov ch,0
	xor edx, edx
	.while (cl)
		mov al,[esi]
		cmp al,'0'
		jb donehex
		cmp al,'9'
		jbe @F
		or al,20h
		cmp al,'a'
		jb donehex
		cmp al,'g'
		jnc donehex
		sub al,27h
@@:
		sub al,'0'
		movzx eax,al
		shl edx, 4
		add edx, eax
		inc esi
		inc ch
		dec cl
	.endw
donehex:
	mov eax, edx
	cmp ch,1
	retn
	align 4
GetCmdLine endp

;--- get a ring 0 code selector
;--- search in GDT, then LDT

GetR0CS proc uses ebx

local	dwBase:dword
local	dfOldExc0E:fword
local	gdt:fword

	mov ebx,cs
	mov ax,0006
	int 31h
	mov word ptr dwBase+0,dx
	mov word ptr dwBase+2,cx

;--- first search in GDT

	mov ax,0202h
	mov bl,0Eh
	int 31h
	mov dword ptr dfOldExc0E+0,edx
	mov word ptr dfOldExc0E+4,cx
	mov ecx,cs
	mov edx,myexc0E
	mov ax,0203h
	int 31h

	sgdt gdt
	mov edx,dword ptr gdt+2
	movzx ecx,word ptr gdt
	inc ecx
	shr ecx, 3
	mov ebx, 0
	.while (ecx)
		mov ah,[edx+ebx*8+7]
		mov al,[edx+ebx*8+4]
		shl eax,16
		mov ax,[edx+ebx*8+2]
		.if (eax == dwBase)
			mov ax,[edx+ebx*8+5]
			and ah,0EFh
			.break .if (ax == 0CF9Bh)
		.endif
		inc ebx
		dec ecx
	.endw
	.if (!ecx)
		xor ebx, ebx
		sldt bx
		xor eax, eax
		and ebx, ebx
		jz exit				;no LDT defined, giving up
		and bl,0F8h
		mov ah,[edx+ebx+7]
		mov al,[edx+ebx+4]
		shl eax,16
		mov ax,[edx+ebx+2]	;get base of LDT
		mov edx, [eax+0]
		mov ecx, [eax+4]
		mov dword ptr dqLDT0+0, edx
		mov dword ptr dqLDT0+4, ecx
		mov word ptr [eax+0],-1	;limit [00-15]
		mov ecx, dwBase
		mov word ptr [eax+2],cx	;base [00-15]
		shr ecx, 16
		mov byte ptr [eax+4],cl	;base [16-23]
		mov word ptr [eax+5],0CF9Bh	;attr + limit [16-19]
		mov byte ptr [eax+7],ch	;base [24-31]
		mov dwLDT, eax
		mov eax, 4						;use first entry in LDT
		jmp exit
	.endif
	shl ebx,3
	mov eax, ebx
exit:
	push eax
	mov edx,dword ptr dfOldExc0E+0
	mov cx,word ptr dfOldExc0E+4
	mov bl,0Eh
	mov ax,0203h
	int 31h
	pop eax
	ret
myexc0E:
	xor eax, eax
	mov dword ptr [esp+3*4],offset exit
	retf
	align 4

GetR0CS endp

SetFixedMtrr proc uses esi edi ebx r0cs:dword

local	dwCB:dword
local	dwOfs:dword
local	regs:REGS

	mov esi, offset regions
	.while ((esi < offset endregions) && (word ptr [esi+4]))
		call getcb
		mov dwCB, edx
		movzx eax,word ptr [esi+0]
		movzx edi,word ptr [esi+2]

		push eax
		movzx ecx,word ptr [esi+4]
		xchg cl,ch
		push ecx
		invoke printf, CStr("SetFixedMtrr: %X-%X %s",10), eax, edi, esp
		add esp,4
		pop eax

		shr eax, 8			;C000 -> C0
		shr edi, 8
		.if (eax < 080h)		;--- fix64k 0000-9FFF?
			invoke printf, CStr("fixed MTRRs below 8000h cannot be set yet",10)
		.elseif (eax < 0C0h)	;--- fix16k A000-BFFF?
			sub eax, 080h   ;80,84 .. B8,BC -> 00,04 .. 38,3C
			sub edi, 080h
			shr eax, 2		;00,04 .. 38,3C -> 00,01 .. 0e,0f
			shr edi, 2
			sub edi, eax
			inc edi			;edi=16k pages to modify
			mov ecx, eax
			shr eax, 3		;00-07 -> 0, 08-0F -> 1
			mov ebx, MSR_MTRR_FIX16
			add ebx, eax
			mov [regs]._ecx, ebx
			lea ebx,regs
			mov eax,ecx
			and eax,4
			add ebx,eax

			mov eax,0FFh
			and ecx,3
			shl ecx, 3		;0,1,2,3 -> 0,8,16,24
			shl eax, cl

			.while (edi)
				push eax
				invoke r0proc, offset readmsr, r0cs, addr regs
				pop eax
				.while (edi && eax)
					mov edx,dwCB
					and edx, eax
					not eax
					and [ebx],eax
					or [ebx],edx
					not eax
					shl eax, 8
					dec edi
				.endw
				add ebx,4
				mov eax,0ffh
				.while (edi && eax)
					mov edx,dwCB
					and edx, eax
					not eax
					and [ebx],eax
					or [ebx],edx
					not eax
					shl eax, 8
					dec edi
				.endw
				invoke r0proc, offset writemsr, r0cs, addr regs
				inc regs._ecx
				mov eax,0ffh
				lea ebx,regs
			.endw
		.else				;--- fix4k C000-FFFF?
			sub eax, 0C0h	;eax = 0..3F
			sub edi, 0C0h
			sub edi, eax
			inc edi			;pages to modify
			mov ecx, eax
			shr eax, 3		;8 4K pages for 1 mtrr
			mov ebx, MSR_MTRR_FIX4	;regs 268h-26Fh
			add ebx, eax
			mov [regs]._ecx, ebx
			lea ebx,regs
			mov eax,ecx
			and eax,4
			add ebx,eax

			mov eax,0FFh
			and ecx,3
			shl ecx,3		;0,1,2,3 -> 0,8,16,24
			shl eax, cl

			.while (edi)
				push eax
				invoke r0proc, offset readmsr, r0cs, addr regs
				pop eax
				.while (edi && eax)
					mov edx,dwCB
					and edx, eax
					not eax
					and [ebx],eax
					or [ebx],edx
					not eax
					shl eax, 8
					dec edi
				.endw
				add ebx,4
				mov eax,0ffh
				.while (edi && eax)
					mov edx,dwCB
					and edx, eax
					not eax
					and [ebx],eax
					or [ebx],edx
					not eax
					shl eax, 8
					dec edi
				.endw
				invoke r0proc, offset writemsr, r0cs, addr regs
				inc regs._ecx
				mov eax,0FFh
				lea ebx,regs
			.endw
		.endif
		add esi, 3*2
	.endw
	ret
getcb:
	mov dx,[esi+4]
	.if (dx == "UC")
		mov edx, 00000000h
	.elseif (dx == "WC")
		mov edx, 01010101h
	.elseif (dx == "WP")
		mov edx, 05050505h
	.elseif (dx == "WT")
		mov edx, 04040404h
	.elseif (dx == "WB")
		mov edx, 06060606h
	.endif
	retn
	align 4

SetFixedMtrr endp


main proc c

local	r0cs:dword
local	dwSysCfg:DWORD
local	dwCaps:DWORD
local	regs[80]:REGS

	invoke GetCmdLine
	.if (eax == 0 || bUsage)
;		invoke printf, CStr("usage: SetMtrr <WC|UC>",13,10)
		invoke printf, CStr("SetMtrr v1.5 (C) Japheth 2005-2020",10)
		invoke printf, CStr("usage: SetMtrr [options]",10)
		invoke printf, CStr(" options: -? display this help",10)
		invoke printf, CStr("          -s set VESA LFB to WC, may speedup VESA memory write access.",10)
;		invoke printf, CStr("          -d disable MTRRs.",10)
		invoke printf, CStr("          -b=ssss-eeee set type of memory range to WB (write back [06])",10)
		invoke printf, CStr("          -c=ssss-eeee set type of memory range to WC (write combine [01])",10)
		invoke printf, CStr("          -p=ssss-eeee set type of memory range to WP (write protect [05])",10)
		invoke printf, CStr("          -t=ssss-eeee set type of memory range to WT (write thru [04])",10)
		invoke printf, CStr("          -u=ssss-eeee set type of memory range to UC (uncachable [00])",10)
		invoke printf, CStr("          (",3Ch,"ssss",3Eh," and ",3Ch,"eeee",3Eh," are segment addresses >= 8000 and <= FFFF)",10)
		invoke printf, CStr("Without option SetMtrr will display current status of MTRRs.",10)
		jmp exit
	.endif

;--- if no "set" option is given, display status
	.if bSetVesa == 0 && bSetFixed == 0
		mov bStatus, 1
	.endif

;--- running on NT?

	mov ax,3306h
	int 21h
	.if (bx == 3205h)
		invoke printf, CStr("SetMtrr will not run on NT platforms",10)
		jmp exit
	.endif

;--- MTRRs supported?

	pushfd
	push 200000h
	popfd
	pushfd
	pop eax
	popfd
	test eax,200000h	;CPUID supported?
	jz nomtrr

	mov eax,1
	xor edx, edx
	cpuid
	.if (!(edx & 1000h))
nomtrr:
		invoke printf, CStr("SetMtrr: MTRRs not supported",10)
		jmp exit
	.endif

;--- Only AMD knows SYSCFG msr
	xor eax,eax
	cpuid
	.if ebx == "htuA" && edx == "itne" && ecx == "DMAc"	;EBX-EDX-ECX == "AuthenticAMD"?
		mov bSysCfg,1
	.endif

;--- find a ring 0 code selector

	invoke GetR0CS
	.if (!eax)
		invoke printf, CStr("cannot get a ring 0 code selector",10)
		jmp exit
	.endif
	mov r0cs, eax

;--- read the SYSCFG (system configuration register) (#C0010010) - AMD only
	.if bSysCfg
		lea edi, regs
		mov [edi].REGS._eax, 0
		mov [edi].REGS._ecx, MSR_SYSCFG
		invoke r0proc, offset readmsr, r0cs, edi
		mov eax, [edi].REGS._eax
		mov dwSysCfg, eax
		.if (bStatus)
			mov eax,CStr("disabled")
			bt dwSysCfg, 20
			jnc @F
			mov eax,CStr("enabled")
@@:
			mov ecx,CStr("disabled")
			bt dwSysCfg, 21
			jnc @F
			mov ecx,CStr("enabled")
@@:
			mov edx,CStr("MTRRdefType")
			bt dwSysCfg, 22
			jnc @F
			mov edx,CStr("WB")
@@:
			invoke printf, CStr("SYSCFG(#C0010010): %X (TOP_MEM %s, TOP_MEM2 %s, Memory 4GB-TOP_MEM2: %s)",10), dwSysCfg, eax, ecx, edx
			.if (dwSysCfg & 100000h)
				mov [edi].REGS._ecx, MSR_TOP_MEM
				invoke r0proc, offset readmsr, r0cs, edi
				invoke printf, CStr("TOP_MEM(#C001001A): %X%08X",10), [edi].REGS._edx, [edi].REGS._eax
			.endif
			.if (dwSysCfg & 200000h)
				mov [edi].REGS._ecx, MSR_TOP_MEM2
				invoke r0proc, offset readmsr, r0cs, edi
				invoke printf, CStr("TOP_MEM2(#C001001D): %X%08X",10), [edi].REGS._edx, [edi].REGS._eax
			.endif
		.endif
	.endif

;--- read the MTRRCAPS register (#FE)
;--- contains the number of MTRRs in low byte

	lea edi, regs
	mov [edi].REGS._eax, 0
	mov [edi].REGS._ecx, MSR_MTRRCAPS
	invoke r0proc, offset readmsr, r0cs, edi
	mov eax, [edi].REGS._eax
	mov dwCaps, eax
	movzx esi, al
	.if (bStatus)
		mov ecx, CStr("is")
		test ah,4			;bit 10: WC supported
		jnz @F
		mov ecx, CStr("is NOT")
@@:
		invoke printf, CStr("MTRRCAPS(#FE): %X (WC %s supported, %u variable MTRRs)",10), eax, ecx, esi
	.endif

;--- read all variable MTRRs

	mov ebx, MSR_MTRR_VAR
	lea edi, regs
	shl esi, 1
	.while (esi)
		mov [edi].REGS._eax, 0
		mov [edi].REGS._edx, 0
		mov [edi].REGS._ebx, 0
		mov [edi].REGS._ecx, ebx
		invoke r0proc, offset readmsr, r0cs, edi
		add edi, sizeof REGS
		inc ebx
		dec esi
	.endw

;--- display status (-s)?
	.if (bStatus)
		lea edi, regs
		movzx esi, byte ptr [dwCaps]
		mov ebx,MSR_MTRR_VAR
		.while (esi)

			invoke printf, CStr("#%X: %08X.%08X  %08X.%08X"),
				ebx, [edi].REGS._edx, [edi].REGS._eax,
				[edi+sizeof REGS].REGS._edx, [edi+sizeof REGS].REGS._eax
			.if ([edi+sizeof REGS].REGS._eax & MTRR_VALID)
				mov eax,[edi+sizeof REGS].REGS._eax
				mov edx,[edi+sizeof REGS].REGS._edx
				push ebx
				push esi
				mov ecx,[edi].REGS._eax
				mov ebx,[edi].REGS._edx
				call gettype
				and cx,0F000h
				and ax,0F000h
				xor eax,-1
				xor edx,-1
				add eax,ecx
				adc edx,ebx
				and edx,0Fh
				invoke printf, CStr(" (%X%08X-%X%08X, %s)"), ebx, ecx, edx, eax, esi
				pop esi
				pop ebx
			.endif
			invoke printf, CStr(10)
			add edi, sizeof REGS * 2
			add ebx, 2
			dec esi
		.endw

;--- display the fixed range registers as well (#250, #258-259, #268-26F)
;--- also display the default type MTRR_DEF_TYPE (#2FF)

		.if (dwCaps & MTRR_FIXED_SUPP)	;fixed MTRRs supported?
			lea edi, regs
			mov [edi].REGS._ecx,MSR_MTRR_FIX64
			invoke r0proc, offset readmsr, r0cs, edi
			invoke printf, CStr(10,"#250: %08X.%08X (FIX64K: 00000-7FFFF)",10), [edi].REGS._edx, [edi].REGS._eax
			mov [edi].REGS._ecx,MSR_MTRR_FIX16
			invoke r0proc, offset readmsr, r0cs, edi
			invoke printf, CStr("#258: %08X.%08X (FIX16K: 80000-9FFFF)",10), [edi].REGS._edx, [edi].REGS._eax
			mov [edi].REGS._ecx,MSR_MTRR_FIX16+1
			invoke r0proc, offset readmsr, r0cs, edi
			invoke printf, CStr("#259: %08X.%08X (FIX16K: A0000-BFFFF)",10), [edi].REGS._edx, [edi].REGS._eax
			mov ebx, MSR_MTRR_FIX4
			mov esi,8
			mov edi,0C0000h
			.while (esi)
				mov regs._ecx,ebx
				invoke r0proc, offset readmsr, r0cs, addr regs
				lea eax,[edi+7FFFh]
				invoke printf, CStr("#%X: %08X.%08X (FIX4K: %X-%X)",10), ebx, regs._edx, regs._eax, edi, eax
				add edi,8000h
				inc ebx
				dec esi
			.endw
		.else
			invoke printf, CStr(10,"SetMtrr: Fixed MTRRs not supported",10)
		.endif
if 0
		.if ([dwCaps] & 10000h)	;PAT available?
			mov regs._ecx,277h
			invoke r0proc, offset readmsr, r0cs, addr regs
			invoke printf, CStr(10,"#277: %08X.%08X",10), regs._edx, regs._eax
		.endif
endif
		;MTRR default
		mov regs._ecx,MSR_MTRRDEFTYPE
		invoke r0proc, offset readmsr, r0cs, addr regs
		mov cl,byte ptr regs._eax
		call gettype
		.if (regs._eax & MTRR_ENABLED)
			mov edx,CStr("MTRRs enabled, Fixed MTRRs ")
			.if (regs._eax & MTRR_FIXED_ENABLED)
				mov ecx,CStr("enabled")
			.else
				mov ecx,CStr("disabled")
			.endif
		.else
			mov edx,CStr("MTRRs disabled")
			mov ecx,CStr(0)
		.endif
		invoke printf, CStr(10,"MTRRDefType(#2FF): %08X.%08X (%s%s, default type=%s)",10), regs._edx, regs._eax, edx, ecx, esi
		jmp exit
	.endif

	.if bSetFixed
		.if (dwCaps & MTRR_FIXED_SUPP)	;fixed MTRRs supported?
			invoke SetFixedMtrr, r0cs
		.else
			invoke printf, CStr(10,"SetMtrr: Fixed MTRRs not supported",10)
		.endif
	.endif

	.if bSetVesa
		invoke SetVesa, dwCaps, addr regs, r0cs
	.endif
if 0
	.if bDisableMtrr
		mov regs._ecx,MSR_MTRRDEFTYPE
		invoke r0proc, offset readmsr, r0cs, addr regs
		btr regs._eax, 11
		invoke r0proc, offset writemsr, r0cs, addr regs
	.endif
endif
exit:
;--- restore first entry in LDT if it was used
	.if (dwLDT)
		mov ebx, dwLDT
		mov edx, dword ptr dqLDT0+0
		mov ecx, dword ptr dqLDT0+4
		mov dword ptr [ebx+0], edx
		mov dword ptr [ebx+4], ecx
	.endif
	xor eax,eax
	ret
gettype:
	.if (cl == 0)
		mov esi, CStr("UC")
	.elseif (cl == 1)
		mov esi, CStr("WC")
	.elseif (cl == 4)
		mov esi, CStr("WT")
	.elseif (cl == 5)
		mov esi, CStr("WP")
	.elseif (cl == 6)
		mov esi, CStr("WB")
	.else
		mov esi, CStr("??")
	.endif
	retn

main endp

mainCRTStartup proc c

	call main
	mov ah,4ch
	int 21h

mainCRTStartup endp

	END mainCRTStartup

