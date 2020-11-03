
VESAINFO struct
VESASignature db "VBE2"              ;0
VESAVersion   dw ?                   ;4
OEMStringPtr  dd ?                   ;6
Capabilities  dd ?                   ;10
VideoModePtr  dd ?                   ;14
TotalMemory   dw ?                   ;18
xxxx          dw ?                   ;20 since Version 2.0
OEMSoftwareRev dd ?                  ;22
OEMVendorNamePtr dd ?                ;26
OEMProductRevPtr dd ?                ;30
VIReserved    db 222 dup (?)         ;34
VIReserved2   db 256 dup (?)
VESAINFO ends

;--- ModeAttributes flags
VESAATTR_LFB_SUPPORTED	equ 080h	;LFB supported

SVGAINFO struct
ModeAttributes dw ?
WinAAttributes db ?
WinBAttributes db ?
WinGranularity dw ?
WinSize        dw ?
WinASegment    dw ?
WinBSegment    dw ?
WinFuncPtr     dd ?
BytesPerScanLine dw ?
;---------------------- rest is optional info (since Version 1.2)
XResolution    dw ?
YResolution    dw ?
XCharSize      db ?
YCharSize      db ?
NumberOfPlanes db ?
BitsPerPixel   db ?
NumberOfBanks  db ?
MemoryModel    db ?				;
BankSize       db ?
NumberOfImagePages db ?
Reserved       db ?
RedMaskSize        db ?
RedFieldPosition   db ?
GreenMaskSize      db ?
GreenFieldPosition db ?
BlueMaskSize       db ?
BlueFieldPosition  db ?
RsvdMaskSize       db ?
RsvdFieldPosition  db ?
DirectColorModeInfo db ?
PhysBasePtr        dd ?         ;since Version 2.0
OffScreenMemOffset dd ?
OffScreenMemSize   dw ?
Reserved2          db 206 dup (?)
SVGAINFO ends

