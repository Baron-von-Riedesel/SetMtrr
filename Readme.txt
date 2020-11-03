
  About SetMtrr

  SetMtrr is a tool to set a MTRR for VESA LFB to memory type 01 (WC).
  This may increase video memory write speed on DOS for P3s and P4s.
  In a Windows DOS box it is usually already done by the video driver.
  For more technical information please read the intel manuals.
  
  This tool requires cpu ring 0 access, so it cannot run on WinXP/2k/NT or
  DOSEMU, and it isn't needed there either.
  
  Thanks to RayeR for this hint!

  
  History
  
  11/2020 v1.4
    - setting MTRRs requires option -s; without option just current
      status is displayed. Option -i removed.
  07/2007 v1.3
    - -b=ssss-eeee option added.
  07/2007 v1.2
    - -i option added.
  01/2007 v1.1
    - check for support of CPUID opcode added
    - exception 0E trapped in ring 0 protected-mode when accessing GDT/LDT/IDT
    - skipped code to move binary in extended memory
  12/2005 v1.0 initial

  Japheth
