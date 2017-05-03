<CsoundSynthesizer>
<CsOptions>
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1.0

giHandle OSCinit 7770

;; alwayson "ListenForShots"

instr ListenForShots
  kf1 init 0
  kf2 init 0
  nxtmsg:
  kk  OSClisten giHandle, "/shoot", "f", kf1
    if (kk == 0) goto ex
    printk 0,kf1
    event "i", "Shoot", 0, 1
    kgoto nxtmsg
  ex:
endin

instr Shoot
  idur = 0.1
  p3 = idur + 0.3
  aSig pinker
  aSig moogladder2 aSig, max:k(expon(sr/2, idur,0.1),0), 2
  aDeClick linseg 1, idur, 1,0.1,0.1,0.1,0
  aSig *= aDeClick*0.00001
  outs aSig, aSig
endin


</CsInstruments>
<CsScore>
i "ListenForShots" 0 3600
e 0 1


</CsScore>
</CsoundSynthesizer>
