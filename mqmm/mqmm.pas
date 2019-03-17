unit mqmm;

{$mode objfpc}
{$PointerMath off}
{$TypedAddress on}

interface

{$warn 4055 off}
{$warn 4056 off}
{$warn 3177 off}

{$modeswitch advancedrecords}

{$Optimization REGVAR}
{$Optimization USEEBP}
{$Optimization USERBP}


type
  TBitmapFastType = NativeUInt;
  PBitmapFastType = ^TBitmapFastType;
  TBitmapWidth = 0..(512 div BitSizeOf(TBitmapFastType) * BitSizeOf(TBitmapFastType))-1;
  PWideBitmap = ^TWideBitmap;
  TWideBitmap = bitpacked array[TBitmapWidth] of Boolean;
  PHeapSlice = ^THeapSlice;
  PPoolLargeAllocation = ^TPoolLargeAllocation;
  PPoolSmallAllocation = ^TPoolSmallAllocation;
  PAllocationPool = ^TAllocationPool;
  PUserBlock = ^TUserBlock;
  TAllocationType = (atSmall, atLarge);

  THeapSlice = record
    Size: PtrUInt;
  end;

  TPoolSmallAllocation = record
    Next, Prev: PPoolSmallAllocation;
    PrevWithFree: PPoolSmallAllocation;
    AllocationTypeSmall: TAllocationType;
    FreeUsage: TBitmapWidth;
    UsageBitmap: TWideBitmap;
    OwnerPool: PAllocationPool;
  end;

  TPoolLargeAllocation = record
    Next, Prev: PPoolLargeAllocation;
  end;


  TAllocationPool = record
    AllocSize: NativeUInt;
    UsableSize: NativeUInt;
    Lock: LongBool;

    case AllocationType: TAllocationType of
      atSmall: (
        MaxUsage: TBitmapWidth;
        FirstSmallAllocation,
        LastSmallAllocation: PPoolSmallAllocation;
        FreeSmallAllocation: PPoolSmallAllocation;
      );
      atLarge: (
        FirstLargeAllocation,
        LastLargeAllocation: PPoolLargeAllocation;
      );
  end;
  TUserBlock = record
    // for Small Pool blocks, this points to the PPoolSmallAllocation
    OwnerPool: PAllocationPool;
    UserSize: PtrUInt;
  end;

const
   __ofsdif = PtrUInt(@(PAllocationPool(nil)^).AllocationType) - PtrUInt(@(PPoolSmallAllocation(nil)^).AllocationTypeSmall);
   {$IF __ofsdif<>0}{$ERROR 'Address of AllocationType must match'}{$IFEND}


var
  AllocationPools: array[0..15] of TAllocationPool = (
    (AllocSize: 16),
    (AllocSize: 24),
    (AllocSize: 32),
    (AllocSize: 48),
    (AllocSize: 64),
    (AllocSize: 72),
    (AllocSize: 128),
    (AllocSize: 192),
    (AllocSize: 256),
    (AllocSize: 384),
    (AllocSize: 512),
    (AllocSize: 768),
    (AllocSize: 1024),
    (AllocSize: 1536),
    (AllocSize: 2048),
    (AllocSize: 0)
  );
  LastSmallPool: PAllocationPool;

const
  PageSize = 4096;
  MaxSmallAllocationPoolSize = 64*PageSize;

  LargeBlockFastGrowGranularity = 2*PageSize;
  LargeBlockFastGrowLimit = PageSize*1024;

implementation

uses
  mqmm_sys;

{ TMemBlock }

var
  TotalAllocd: integer = 0;
  TotalAlloc: PtrUInt = 0;

function NewHeapSlice(size:PtrUInt): PHeapSlice;
begin
  Result:= SysOSAlloc(Size + sizeof(THeapSlice));
  InterlockedIncrement(TotalAllocd);
  InterlockedExchangeAdd(TotalAlloc, Size);
  //WriteLn(ThreadID, ' alloc ', HexStr(Result), ' size ', size + sizeof(THeapSlice), ' st ', TotalAllocd, ' ', TotalAlloc);
  Result^.Size:= Size;
end;

procedure DelHeapSlice(hs: PHeapSlice);
var
  sz: PtrUInt;
begin
  //WriteLn(ThreadID, ' free  ', HexStr(hs), ' size ', hs^.Size + sizeof(THeapSlice), ' st ', TotalAllocd, ' ', TotalAlloc);
  InterlockedExchangeAdd(TotalAlloc, -hs^.Size);
  sz:= hs^.Size + sizeof(THeapSlice);
  //Fillchar(hs^, sz, $dd);
  SysOSFree(hs, sz);
  InterlockedDecrement(TotalAllocd);
end;

function PoolFromSize(size: ptruint): PAllocationPool;
begin
  Result:= @AllocationPools[0];
  while Result^.AllocSize > 0 do begin
    if size <= Result^.UsableSize then
      Exit
    else
      inc(Result);
  end;
end;

{.$DEFINE BITMAP_NAIVE}
{.$DEFINE BITMAP_POS_LOOP}
{.$DEFINE BITMAP_TWIDDLE_32bit}
function Bitmap_FindFirstUnset(var bmp: TWideBitmap): TBitmapWidth;// inline;
// Assume: at least one bit is unset
{$IFDEF BITMAP_NAIVE}
begin
  Result:= 0;
  while (Result <= high(bmp)) and bmp[Result] do
    inc(Result);
{$ELSE}
// Assume: WideBitmap is a multiple of TBitmapFastType
var
  p: PBitmapFastType;
  x, lowestClear: TBitmapFastType;
  maskUpToBit: TBitmapFastType;
  c: TBitmapFastType;
begin
  Result:= 0;
  p:= Pointer(@bmp);
  while Result < high(TBitmapWidth) do begin
    x:= p^;
    if (not x) <> 0 then begin
      lowestClear:= not x and (x + 1);                           // __BLCIC(x)
{$IFDEF BITMAP_POS_LOOP}
      while lowestClear and 1 = 0 do begin
        inc(Result);
        lowestClear:= lowestClear shr 1;
      end;
{$ELSE}
      // c:= __TZCNT(lowestClear)
{$IFDEF BITMAP_TWIDDLE_32bit}
      {$IF SizeOF(TBitmapFastType) <> 4}{$ERROR 'TBitmapFastType of incorrect type for platform'}{$IFEND}
      c:= BitSizeOf(TBitmapFastType) - 1;
      if (lowestClear and $0000FFFF <> 0) then c -= 16;
      if (lowestClear and $00FF00FF <> 0) then c -= 8;
      if (lowestClear and $0F0F0F0F <> 0) then c -= 4;
      if (lowestClear and $33333333 <> 0) then c -= 2;
      if (lowestClear and $55555555 <> 0) then c -= 1;
{$ELSE}
      maskUpToBit:= lowestClear xor (lowestClear - 1);           // __BLSMSK(lowestClear)
      c:= PopCnt(maskUpToBit) - 1;
{$ENDIF}
      inc(Result, c);
{$ENDIF}
      Exit;
    end;
    inc(p);
    inc(Result, BitSizeOf(TBitmapFastType));
  end;
{$ENDIF}
end;

procedure Bitmap_Set(var bmp: TWideBitmap; Index: TBitmapWidth); //inline;
begin
  bmp[Index]:= True;
end;

procedure Bitmap_Clear(var bmp: TWideBitmap; Index: TBitmapWidth);// inline;
begin
  bmp[Index]:= False;
end;

procedure Bitmap_ClearAll(var bmp: TWideBitmap);// inline;
begin
  FillChar(bmp, sizeof(bmp), $00);
end;


Function QuickGetMem(Size : ptruint) : Pointer; forward;
Function QuickFreeMem(P : pointer) : ptruint; forward;
Function QuickReAllocMem(var p:pointer;Size:ptruint):Pointer; forward;
Function QuickMemSize (p:pointer): ptruint; forward;


procedure Freechain_Remove(pool: PAllocationPool; alloc: PPoolSmallAllocation);
var
  ap: PPoolSmallAllocation;
begin
  if alloc = pool^.FreeSmallAllocation then
    pool^.FreeSmallAllocation:= alloc^.PrevWithFree
  else begin
    ap:= pool^.FreeSmallAllocation;
    while Assigned(ap) do begin
      if ap^.PrevWithFree = alloc then begin
        ap^.PrevWithFree:= alloc^.PrevWithFree;
        break;
      end;
      ap:= ap^.PrevWithFree;
    end;
  end;
end;

function AllocSmall(Pool: PAllocationPool; Size: ptruint): Pointer;
var
  alloc, nextAlloc: PPoolSmallAllocation;
  userhdr: PUserBlock;
  hs: PHeapSlice;
  b: TBitmapWidth;
begin
  alloc:= pool^.FreeSmallAllocation;
  while Assigned(alloc) do begin
    if alloc^.FreeUsage <> 0 then begin
      b:= Bitmap_FindFirstUnset(alloc^.UsageBitmap);
      userhdr:= PUserBlock(PtrUInt(alloc) + sizeof(TPoolSmallAllocation) + b * pool^.AllocSize);
      userhdr^.OwnerPool:= PAllocationPool(alloc);
      userhdr^.UserSize:= Size;
      Bitmap_Set(alloc^.UsageBitmap, b);
      Dec(alloc^.FreeUsage);
      if alloc^.FreeUsage = 0 then
        Freechain_Remove(Pool, alloc);
      Result:= Pointer(PtrUInt(userhdr) + sizeof(TUserBlock));
      Exit;
    end;
    alloc:= alloc^.PrevWithFree;
  end;
  hs:= NewHeapSlice(Size + Sizeof(TUserBlock) + Sizeof(TPoolSmallAllocation) + PtrUInt(pool^.MaxUsage + 1) * Pool^.AllocSize);
  //WriteLn(ThreadID, ' new page in pool ', pool^.AllocSize);
  nextAlloc:= PPoolSmallAllocation(PtrUInt(hs) + sizeof(THeapSlice));
  nextAlloc^.Prev:= pool^.LastSmallAllocation;
  nextAlloc^.Next:= nil;
  nextAlloc^.PrevWithFree:= nil; // can only be nil, otherwise we would have used that
  pool^.FreeSmallAllocation:= nextAlloc;
  if Assigned(pool^.LastSmallAllocation) then
    pool^.LastSmallAllocation^.Next:= nextAlloc
  else
    pool^.FirstSmallAllocation:= nextAlloc;
  pool^.LastSmallAllocation:= nextAlloc;
  nextAlloc^.AllocationTypeSmall:= atSmall;
  nextAlloc^.OwnerPool:= Pool;
  nextAlloc^.FreeUsage:= Pool^.MaxUsage - 1;
  Bitmap_ClearAll(nextAlloc^.UsageBitmap);
  Bitmap_Set(nextAlloc^.UsageBitmap, 0);
  userhdr:= PUserBlock(PtrUInt(nextAlloc) + sizeof(TPoolSmallAllocation));
  userhdr^.OwnerPool:= PAllocationPool(nextAlloc);
  userhdr^.UserSize:= Size;
  Result:= Pointer(Ptruint(userhdr) + sizeof(TUserBlock));
end;

function AllocLarge(Pool: PAllocationPool; Size: ptruint): Pointer;
var
  hs: PHeapSlice;
  nextAlloc: PPoolLargeAllocation;
  userhdr: PUserBlock;
begin
  hs:= NewHeapSlice(Size + Sizeof(TUserBlock) + Sizeof(TPoolLargeAllocation));
  nextAlloc:= PPoolLargeAllocation(PtrUInt(hs) + sizeof(THeapSlice));
  nextAlloc^.Prev:= pool^.LastLargeAllocation;
  nextAlloc^.Next:= nil;
  if Assigned(pool^.LastLargeAllocation) then
    pool^.LastLargeAllocation^.Next:= nextAlloc
  else
    pool^.FirstLargeAllocation:= nextAlloc;
  pool^.LastLargeAllocation:= nextAlloc;
  userhdr:= PUserBlock(PtrUInt(nextAlloc) + sizeof(TPoolLargeAllocation));
  userhdr^.OwnerPool:= pool;
  userhdr^.UserSize:= Size;
  Result:= Pointer(Ptruint(userhdr) + sizeof(TUserBlock));
end;

Function QuickGetMem(Size : ptruint) : Pointer;
var
  pool: PAllocationPool;
begin
  Result:= nil;
  if Size = 0 then exit;
  pool:= PoolFromSize(Size);
  case pool^.AllocationType of
    atSmall: begin
      if (Size>8) and (Size * 2 < pool^.UsableSize) then
        WriteLn(ThreadID, ' allocation in wasteful pool, want: ',Size,' using: ', pool^.UsableSize,' (', pool^.AllocSize, ')');
      Result:= AllocSmall(pool, Size);
    end;
    atLarge: begin
      Result:= AllocLarge(pool, Size);
    end;
  end;
end;

Function QuickFreeMem(P : pointer) : ptruint;
var
  userhdr: PUserBlock;
  pool: PAllocationPool;
  delAlloc: PPoolLargeAllocation;
  mb: PHeapSlice;
  b: integer;
  alloc: PPoolSmallAllocation;
  rel, poollast: PtrUInt;
begin
  Result:= 0;
  if p = nil then exit;
  userhdr:= PUserBlock(PtrUInt(p) - SizeOf(TUserBlock));
  pool:= userhdr^.OwnerPool;
  case pool^.AllocationType of
    atSmall: begin
      alloc:= PPoolSmallAllocation(pool);
      pool:= alloc^.OwnerPool;
      poollast:= pool^.MaxUsage * Pool^.AllocSize;
      rel:= PtrUInt(userhdr) - PtrUInt(alloc) - Sizeof(TPoolSmallAllocation);
      if rel <= poollast then begin
        b:= rel div pool^.AllocSize;
        Inc(Alloc^.FreeUsage);
        Bitmap_Clear(alloc^.UsageBitmap, b);
        if alloc^.FreeUsage = pool^.MaxUsage then begin
          // remove references in free chain
          FreeChain_Remove(pool, alloc);

          if pool^.LastSmallAllocation = alloc then
            pool^.LastSmallAllocation:= alloc^.Prev
          else
            alloc^.Next^.Prev:= alloc^.Prev;
          if pool^.FirstSmallAllocation = alloc then
            pool^.FirstSmallAllocation:= alloc^.Next
          else
            alloc^.Prev^.Next:= alloc^.Next;
          mb:= PHeapSlice(Ptruint(alloc) - sizeof(THeapSlice));
          //WriteLn(ThreadID, ' drop page in pool ', pool^.AllocSize);
          DelHeapSlice(mb);
        end else
        if alloc^.FreeUsage = 1 then begin
          // if we just became free, add to free chain
          alloc^.PrevWithFree:= pool^.FreeSmallAllocation;
          pool^.FreeSmallAllocation:= alloc;
        end;
      end;
    end;
    atLarge: begin
      delAlloc:= PPoolLargeAllocation(PtrUInt(userhdr) - sizeof(TPoolLargeAllocation));
      if pool^.LastLargeAllocation = delAlloc then
        pool^.LastLargeAllocation:= delAlloc^.Prev
      else
        delAlloc^.Next^.Prev:= delAlloc^.Prev;
      if pool^.FirstLargeAllocation = delAlloc then
        pool^.FirstLargeAllocation:= delAlloc^.Next
      else
        delAlloc^.Prev^.Next:= delAlloc^.Next;
      mb:= PHeapSlice(Ptruint(delAlloc) - sizeof(THeapSlice));
      DelHeapSlice(mb);
    end;
  end;
end;

Function QuickFreeMemSize(p:pointer;Size:ptruint):ptruint;
begin
  if size<=0 then
    exit;
  if p = nil then exit;
  //if (size <> QuickMemSize(p)) then
  //  runerror(204);
  Result:= QuickFreeMem(p);
end;

procedure DoReallocMemory(var p:pointer;OldSize, NewSize:ptruint);
var
  newmem: Pointer;
begin
  newmem:= QuickGetMem(NewSize);
  if OldSize > NewSize then
    Move(p^, newmem^, NewSize)
  else
    Move(p^, newmem^, OldSize);
  QuickFreeMem(p);
  p:= newmem;
end;

procedure GrowMemory(var p:pointer;OldSize, NewSize:ptruint);
var
  userhdr: PUserBlock;
  pool: PAllocationPool;
  hs: PHeapSlice;
  alloc: PPoolLargeAllocation;
  Reallocsize: PtrUInt;
  allocs: PPoolSmallAllocation;
begin
  userhdr:= PUserBlock(PtrUint(p) - sizeof(TUserBlock));
  pool:= userhdr^.OwnerPool;
  case pool^.AllocationType of
    atSmall: begin
      allocs:= PPoolSmallAllocation(pool);
      pool:= allocs^.OwnerPool;
      if NewSize > pool^.UsableSize then begin
        DoReallocMemory(p, OldSize, NewSize);
        Exit;
      end;
    end;
    atLarge: begin
      alloc:= PPoolLargeAllocation(PtrUInt(userhdr) - sizeof(TPoolLargeAllocation));
      hs:= PHeapSlice(PtrUInt(alloc) - sizeof(THeapSlice));
      if NewSize + SizeOf(TPoolLargeAllocation) + sizeof(TUserBlock) > hs^.Size then begin
        if NewSize < LargeBlockFastGrowLimit then
          Reallocsize:= ((NewSize * 2) div LargeBlockFastGrowGranularity + 1) * LargeBlockFastGrowGranularity
        else
          Reallocsize:= (NewSize div LargeBlockFastGrowLimit +1) * LargeBlockFastGrowLimit;
        DoReallocMemory(p, OldSize, Reallocsize);
        userhdr:= PUserBlock(PtrUint(p) - sizeof(TUserBlock));
      end;
    end;
  end;
  userhdr^.UserSize:= NewSize;
end;

procedure ShrinkMemory(var p:pointer;OldSize, NewSize:ptruint);
var
  userhdr: PUserBlock;
  pool: PAllocationPool;
  alloc: PPoolSmallAllocation;
begin
  userhdr:= PUserBlock(PtrUint(p) - sizeof(TUserBlock));
  pool:= userhdr^.OwnerPool;
  case pool^.AllocationType of
    atSmall: begin
      alloc:= PPoolSmallAllocation(pool);
      pool:= alloc^.OwnerPool;
      if (pool<>@AllocationPools[0]) and (NewSize * 2 < pool^.UsableSize) then begin
        DoReallocMemory(p, OldSize, NewSize);
        Exit;
      end;
    end;
    atLarge: begin
      if (NewSize * 2 < OldSize) or (NewSize < LastSmallPool^.UsableSize) then begin
        DoReallocMemory(p, OldSize, NewSize);
        Exit;
      end;
    end;
  end;
  userhdr^.UserSize:= NewSize;
end;

Function QuickReAllocMem(var p:pointer;Size:ptruint):Pointer;
var
  ms: PtrUInt;
begin
  if size = 0 then begin
    QuickFreeMem(p);
    p:= nil;
    Exit(p);
  end;
  if p = nil then begin
    p:= QuickGetMem(Size);
    Exit(p);
  end;

  ms:= QuickMemSize(p);
  if Size > ms then begin
    // grow
    GrowMemory(p, ms, Size);
    Result:= p;
  end else begin
    // shrink or same
    ShrinkMemory(p, ms, Size);
    Result:= p;
  end;
end;

Function QuickMemSize (p:pointer): ptruint;
var
  userhdr: PUserBlock;
begin
  userhdr:= PUserBlock(PtrUInt(p) - SizeOf(TUserBlock));
  Result:= userhdr^.UserSize;
  //pool:= userhdr^.OwnerPool;
  //case pool^.AllocationType of
  //  atSmall: begin
  //    Result:= pool^.AllocSize - sizeof(TUserBlock);
  //  end;
  //  atLarge: begin
  //    thisAlloc:= PPoolLargeAllocation(PtrUInt(userhdr) - sizeof(TPoolLargeAllocation));
  //    hs:= PHeapSlice(Ptruint(thisAlloc) - sizeof(THeapSlice));
  //    Result:= hs^.Size - Sizeof(TPoolLargeAllocation) - sizeof(TUserBlock);
  //  end;
  //end;
end;

function QuickGetHeapStatus:THeapStatus;
var res: THeapStatus;
begin
  fillchar(res,sizeof(res),0);
  QuickGetHeapStatus:=res;
end;

function QuickGetFPCHeapStatus:TFPCHeapStatus;
begin
  fillchar(QuickGetFPCHeapStatus,sizeof(QuickGetFPCHeapStatus),0);
end;

procedure InitAllocPools;
var
  ap: PAllocationPool;
  i, max: Integer;
begin
  ap:= @AllocationPools[0];
  for i:= 0 to high(AllocationPools) do begin
    if ap^.AllocSize <> 0 then begin
      ap^.UsableSize:= ap^.AllocSize - SizeOf(TUserBlock);
      ap^.AllocationType:= atSmall;
      LastSmallPool:= ap;
      max:= MaxSmallAllocationPoolSize div ap^.AllocSize;
      if max > high(TBitmapWidth) then
        max:= high(TBitmapWidth);
      ap^.MaxUsage:= max;
    end
    else
      ap^.AllocationType:= atLarge;
    inc(ap);
  end;
end;

Const
 QMemoryManager : TMemoryManager =
    (
      NeedLock : false;
      GetMem : @QuickGetMem;
      FreeMem : @QuickFreeMem;
      FreememSize : @QuickFreeMemSize;
      AllocMem : @SysAllocMem;
      ReallocMem : @QuickReAllocMem;
      MemSize : @QuickMemSize;
      InitThread : nil;
      DoneThread : nil;
      RelocateHeap : nil;
      GetHeapStatus : @QuickGetHeapStatus;
      GetFPCHeapStatus: @QuickGetFPCHeapStatus;
    );

Var
  OldMemoryManager : TMemoryManager;

Initialization
  InitAllocPools;
  GetMemoryManager (OldMemoryManager);
  SetMemoryManager (QMemoryManager);

Finalization
  SetMemoryManager (OldMemoryManager);
end.
