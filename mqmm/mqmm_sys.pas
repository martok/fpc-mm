unit mqmm_sys;

{$mode objfpc}{$H+}

interface

function SysOSAlloc(size: ptruint): pointer;
procedure SysOSFree(p: pointer; size: ptruint);

implementation

{$DEFINE SYSOS_WIN_HEAP}
{.$DEFINE SYSOS_WIN_VIRTUALALLOC}


{$IFDEF SYSOS_WIN_HEAP}
{ memory functions }
function GetProcessHeap : THandle;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetProcessHeap';
function HeapAlloc(hHeap : THandle; dwFlags : DWord; dwBytes : SIZE_T) : pointer;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'HeapAlloc';
function HeapFree(hHeap : THandle; dwFlags : dword; lpMem: pointer) : boolean;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'HeapFree';
{$IFDEF SYSTEMDEBUG}
function WinAPIHeapSize(hHeap : THandle; dwFlags : DWord; ptr : Pointer) : SIZE_T;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'HeapSize';
{$ENDIF}


  function SysOSAlloc(size: ptruint): pointer;
  var
    p : pointer;
  begin
    p := HeapAlloc(GetProcessHeap, 0, size);
  {$ifdef DUMPGROW}
    Writeln('new heap part at $',hexstr(p), ' size = ',WinAPIHeapSize(GetProcessHeap()));
  {$endif}
    SysOSAlloc := p;
  end;

  procedure SysOSFree(p: pointer; size: ptruint);
  begin
    HeapFree(GetProcessHeap, 0, p);
  end;

{$ENDIF}

{$IFDEF SYSOS_WIN_VIRTUALALLOC}
type
     LPVOID  = pointer;
     WINBOOL = longbool;
function VirtualAlloc(lpAddress:LPVOID; dwSize:PTRUINT; flAllocationType:DWORD; flProtect:DWORD):LPVOID; {$ifdef wince}cdecl{$else}stdcall{$endif}; external 'kernel32' name 'VirtualAlloc';
function VirtualFree(lpAddress:LPVOID; dwSize:PTRUINT; dwFreeType:DWORD):WINBOOL; {$ifdef wince}cdecl{$else}stdcall{$endif}; external 'kernel32' name 'VirtualFree';
const
     MEM_COMMIT = 4096;
     MEM_FREE = 65536;
     MEM_RESERVE = 8192;
     MEM_IMAGE = 16777216;
     MEM_MAPPED = 262144;
     MEM_PRIVATE = 131072;
     MEM_DECOMMIT = 16384;
     MEM_RELEASE = 32768;
     MEM_TOP_DOWN = 1048576;
     PAGE_READONLY = 2;
     PAGE_READWRITE = 4;
     PAGE_WRITECOPY = 8;
     PAGE_EXECUTE = 16;
     PAGE_EXECUTE_READ = 32;
     PAGE_EXECUTE_READWRITE = 64;
     PAGE_EXECUTE_WRITECOPY = 128;
     PAGE_GUARD = 256;
     PAGE_NOACCESS = 1;
     PAGE_NOCACHE = 512;

 function SysOSAlloc(size: ptruint): pointer;
 var
   p : pointer;
 begin
   p:= VirtualAlloc(nil, size, MEM_COMMIT, PAGE_READWRITE);
   SysOSAlloc := p;
 end;

 procedure SysOSFree(p: pointer; size: ptruint);
 begin
   VirtualFree(p, 0, MEM_RELEASE);
 end;
{$ENDIF}
end.

