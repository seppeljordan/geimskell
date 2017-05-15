<CsoundSynthesizer>
<CsOptions>
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1.0

seed 0

giHandle OSCinit 7770

giDrone ftgen 0, 0, 256, 9,  1,1,0,   1.732050807568877,.5773502691896259,0,   2.449489742783178,.408248290463863,0,   3.162277660168379,.3162277660168379,0,   3.872983346207417,.2581988897471611,0,   4.58257569495584,.2182178902359924,0,   5.291502622129182,.1889822365046136,0, 6,.1666666666666667,0,   6.70820393249937,.1490711984999859,0,   7.416198487095663,.1348399724926484,0,   8.124038404635961,.1230914909793327,0,   9.539392014169456,.1048284836721918,0,  10.2469507659596,.0975900072948533,0,  10.95445115010332,.0912870929175277,0,   11.6619037896906,.0857492925712544,0
;; alwayson "ListenForShots"

instr ListenForShots
  kf1 init 0
  nxtmsg:
  kk  OSClisten giHandle, "/shoot", "f", kf1
    if (kk == 0) goto ex
    printk 0,kf1
    event "i", "Shoot", 0, 1
    kgoto nxtmsg
  ex:
endin


instr ListenForExplosions
  kf1 init 0
  nxtmsg:
  kk  OSClisten giHandle, "/explosion", "f", kf1
    if (kk == 0) goto ex
    printk 0,kf1
    event "i", "Explosion", 0, 1
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

instr Explosion
  setksmps 1
  aFeedback init 0
  aSig1 foscil 0.2+aFeedback, 20,10,line(0,2,1000),20, giDrone
  aFeedback = aSig1
  aSig1 limit aSig1, 0, 0.3
  aEnv expon 1, 0.1,0.01 
  aSig1 *= aEnv
  aSigL, aSigR pan2 aSig1, rnd(0.5)-0.25 ;; Add panning!
  p3 = 2
  ;;aReverbL, aReverbR freeverb aSigL, aSigR, 0.98, 0.2, 0.001
  aReverb reverb aSig1, 2
  aSigOut butterlp aReverb, expon(1000,2,200)
  aSigOut butterhp aReverb, 100
  outs aSigOut/2, aSigOut/2
endin

</CsInstruments>
<CsScore>
i "ListenForShots"      0 [3600*24*7]
i "ListenForExplosions" 0 [3600*24*7]
;;e 0 1
;;i "Explosion" 0 1

</CsScore>
</CsoundSynthesizer>
