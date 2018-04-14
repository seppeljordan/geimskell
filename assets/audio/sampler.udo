;; From Iain McCurdy
opcode pitchshifter, a, akkkkii ; individual buffer feedback
  ainL,iratio,ifeedback,iDelay,iSmooth,imaxdelay,iwfn xin
  setksmps 1
  
  kPortTime linseg 0,0.001,1
  kratio  portk iratio, kPortTime*iSmooth 
  kDelay  portk iDelay, kPortTime*iSmooth 
  aDelay  interp kDelay
  
  arate  = (kratio-1)/kDelay  ;SUBTRACT 1/1 SPEED
  
  aphase1  phasor -arate    ;MOVING PHASE 1-0
  aphase2  phasor -arate, .5   ;MOVING PHASE 1-0 - PHASE OFFSET BY 180 DEGREES (.5 RADIANS)
  
  agate1  tablei aphase1, iwfn, 1, 0, 1  ;
  agate2  tablei aphase2, iwfn, 1, 0, 1  ;
  
  abuf1  delayr imaxdelay   ;DECLARE DELAY BUFFER
  adelsig1 deltap3 aphase1 * aDelay  ;VARIABLE TAP
  aGatedSig1 = adelsig1 * agate1
  delayw ainL + (aGatedSig1*ifeedback) ;WRITE AUDIO TO THE BEGINNING OF THE DELAY BUFFER, MIX IN FEEDBACK SIGNAL - PROPORTION DEFINED BY gkFB
  
  abuf2  delayr imaxdelay   ;DECLARE DELAY BUFFER
  adelsig2 deltap3 aphase2 * aDelay  ;VARIABLE TAP
  aGatedSig2 = adelsig2 * agate2
  delayw ainL + (aGatedSig2*ifeedback) ;WRITE AUDIO TO THE BEGINNING OF THE DELAY BUFFER, MIX IN FEEDBACK SIGNAL - PROPORTION DEFINED BY gkFB

  aGatedMixL = (aGatedSig1 + aGatedSig2) * 0.5
  xout aGatedMixL
endop

giTriangle2 ftgen 0,0,16384,20,3
opcode sampler,aa,iiii 
  iamp_in, ifreq_in, isample_in, ibase_freq xin
  iamp = ampdbfs(iamp_in)
  ifreq = ifreq_in
  isample = isample_in
  isamplefreq = ibase_freq
  ilen ftlen isample
  isr ftsr isample
  p3 = ilen/isr
  ichannels = ftchnls(isample)

  if (ichannels == 1) then
    ; idel = 2/ifreq
    ; kdel port idel,0.01,0.1
    aL loscil iamp, 1, isample, 1, 0
    ; pitchshifter
    aL pitchshifter aL,ifreq/isamplefreq,0.001,0.1,1,4, giTriangle2
    aR = aL
  elseif (ichannels == 2) then
    aL, a0 loscil iamp, 1, isample, 1, 0
    aL pitchshifter aL,ifreq/isamplefreq,0.001,0.1,1,4, giTriangle2
    aR = aL
    ; aL PitchShifter aL, ifreq/isamplefreq, 0.01,giSaw
    ; aR PitchShifter aR, ifreq/isamplefreq, 0.01,giSaw
  else
    aL = 0
    aR = 0
  endif   
  aenv  linseg 0, 0.0005, 1, p3 - 0.0395, 1, 0.02, 0, 0.01, 0
  aL =  aL*aenv/2
  aR =  aR*aenv/2
  xout aL, aR
endop
