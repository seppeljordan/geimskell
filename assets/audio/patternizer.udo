  opcode StrayLen2, k, S
Stray xin
Sep1		sprintf	"%c", 32
Sep2		sprintf	"%c", 9
klen		strlenk		Stray
kcount	=	0
kwarsep	 = 1
kndx = 0
 if klen == 0 kgoto end
loop:
Snext		strsubk		Stray, kndx, kndx+1
ksep1p		strcmpk		Snext, Sep1
ksep2p		strcmpk		Snext, Sep2
 if ksep1p == 0 || ksep2p == 0 then
kwarsep	=		1
 else 
  if kwarsep == 1 then
kcount		=		kcount + 1
kwarsep	 = 0 
  endif 
 endif	
		loop_lt	kndx, 1, klen, loop 
end: 		xout		kcount
  endop

  opcode StrayGetNum, k, Skjj
;returns kelindex in Stray. this element must be a number
Str, kelindx, isepA, isepB xin
;;DEFINE THE SEPERATORS
isep1     =         (isepA == -1 ? 32 : isepA)
isep2     =         (isepA == -1 && isepB == -1 ? 9 : (isepB == -1 ? isep1 : isepB))
Sep1      sprintf   "%c", isep1
Sep2      sprintf   "%c", isep2
;;INITIALIZE SOME PARAMETERS
Stray     strcpyk   Str ;make sure to update in performance
klen      strlenk   Stray
kstartsel =         -1; startindex for searched element
kendsel   =         -1; endindex for searched element
kel       =         0; actual number of element while searching
kwarleer  =         1
kndx      =         0
 if klen == 0 kgoto end ;don't go into the loop if Stray is empty
loop:
Snext     strsubk   Stray, kndx, kndx+1; next sign
ksep1p    strcmpk   Snext, Sep1; returns 0 if Snext is sep1
ksep2p    strcmpk   Snext, Sep2; 0 if Snext is sep2
;;NEXT SIGN IS NOT SEP1 NOR SEP2
if ksep1p != 0 && ksep2p != 0 then
 if kwarleer == 1 then; first character after a seperator 
  if kel == kelindx then; if searched element index
kstartsel =         kndx; set it
kwarleer  =         0
  else 			;if not searched element index
kel       =         kel+1; increase it
kwarleer  =         0; log that it's not a seperator 
  endif 
 endif 
;;NEXT SIGN IS SEP1 OR SEP2
else 
 if kstartsel > -1 then; if this is first selector after searched element
kendsel   =         kndx; set iendsel
          kgoto     end ;break
 else	
kwarleer  =         1
 endif 
endif
          loop_lt   kndx, 1, klen, loop 
end: 		
Snum      strsubk   Stray, kstartsel, kendsel
Snum      init      "nan"
knum      strtodk   Snum
          xout      knum
  endop 
      


opcode patternizer, kkk,iiS 
; Give it a string with numbers and it outputs trigger 1 or no-trigger 0
; Example ktrigger patternizer 4, 120, "0 1 2 3"
; Made by Hlödver Sigurdsson 2016
iTimeSignature, iBPM, Spattern xin
  kOffTrigger init -1
  kPatLen StrayLen2 Spattern
  kPatMax StrayGetNum Spattern, kPatLen - 1
  krate_counter timek
  iOneSecond =  kr
  iBeatsPerSecond = iBPM / 60
  iTicksPerBeat = iOneSecond / iBeatsPerSecond
  if iTimeSignature != 0 then
  kBeatCounts = (ceil(kPatMax) >= iTimeSignature ? ceil((kPatMax+0.00001)/iTimeSignature)*iTimeSignature : iTimeSignature)
  endif
  kPatternLength = (iTimeSignature < 1 ? ceil(kPatMax+0.00001) * iTicksPerBeat : kBeatCounts * iTicksPerBeat)
  kIndex init 0
  kNextEvent StrayGetNum Spattern, kIndex % kPatLen
  kLastEvent StrayGetNum Spattern, (kPatLen - 1)
    if int(krate_counter % kPatternLength) == int(iTicksPerBeat * kLastEvent) then
       kOffTrigger += 1
    endif
    if int(krate_counter % kPatternLength) == int(iTicksPerBeat * kNextEvent) then
      kTrigger = 1
      kIndex += 1
    else
      kTrigger = 0
    endif
xout kTrigger, kOffTrigger, kIndex
endop

opcode reviver, 0, i
instrnum xin
kActive active instrnum
if kActive < 1 then
schedkwhen 1, 0, 0, instrnum, 0, 10000
endif
endop
