<CsoundSynthesizer>
<CsOptions>
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1.0

seed 0

#include "patternizer.udo"

giHandle OSCinit 7770

gaRvbL init 0
gaRvbR init 0
giDrone ftgen 0, 0, 256, 9,  1,1,0,   1.732050807568877,.5773502691896259,0,   2.449489742783178,.408248290463863,0,   3.162277660168379,.3162277660168379,0,   3.872983346207417,.2581988897471611,0,   4.58257569495584,.2182178902359924,0,   5.291502622129182,.1889822365046136,0, 6,.1666666666666667,0,   6.70820393249937,.1490711984999859,0,   7.416198487095663,.1348399724926484,0,   8.124038404635961,.1230914909793327,0,   9.539392014169456,.1048284836721918,0,  10.2469507659596,.0975900072948533,0,  10.95445115010332,.0912870929175277,0,   11.6619037896906,.0857492925712544,0
giParabola ftgen 0, 0, 131072, 19, 0.5, 1, 180, 1
;;alwayson "REVERB"

giPad_tbl_len = 2^18
giPad_base_freq = 23.3
giPad ftgen 0, 0, giPad_tbl_len, "padsynth", giPad_base_freq, 50, 1.1, 2, 1, 0.2, 0.3,0.2,0.1 

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
    event "i", "expolosion", 0, 1
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

opcode Flanger_stereo,aa,aakkkkk ;MADE BY IAIN MCCURDY
  aL,aR,krate,kdepth,kdelay,kfback,kmix xin
  adlt interp kdelay
  amod oscili kdepth, krate, giParabola 
  adlt sum adlt, amod ;static delay time and modulating delay time are summed
  adelsigL flanger aL, adlt, kfback , 1.2 ;flanger signal created
  adelsigL dcblock adelsigL
  adelsigR flanger aR, adlt, kfback , 1.2 ;flanger signal created
  adelsigR dcblock adelsigR
  aL sum aL*(1-kmix), adelsigL*kmix ;create dry/wet mix
  aR sum aR*(1-kmix), adelsigR*kmix
  xout aL,aR ;send audio back to caller instrument
endop


instr PAD
  giPad_tbl_len = 2^18
  giPad_base_freq = 23.3
  ifreq_constant = (sr/giPad_tbl_len/giPad_base_freq)
  asig1 poscil 0.1, ifreq_constant*p4/2, giPad, rnd(1)
  asig2 poscil 0.1, ifreq_constant*p4*1.01/2, giPad, rnd(1)
  asig sum asig1, asig2
  asig linen asig, p3*0.1,p3,p3*0.1
  aL, aR Flanger_stereo asig, asig, 5.5, 0.001, 0.01, 0.001, 1
  gaRvbL += aL*0.7
  gaRvbR += aR*0.7
  aL *= 0.3
  aR *= 0.3 
  outs aL, aR
endin

opcode rand_nth,i,i[]
  iarr[] xin
  ilen lenarray iarr
  irnd random 0, ilen
  irnd = floor(irnd)
  xout iarr[irnd]
endop


instr song1
  ;; Tenor
  kDurs1[]  fillarray 6,6,6,3,3,6,6
  kFreqs1[] fillarray 7.09,7.04,7.09,7.08,7.09,7.00,7.11,7.09
  kTrigger1, kOffTrigger1, kIndex1 patternizer 4, 120, "8 9.5 13 14"
  schedkwhen kTrigger1, 0, 0, "PAD", 0, kDurs1[kIndex1%lenarray(kDurs1)],cpspch(kFreqs1[kIndex1%lenarray(kFreqs1)])
  ;; Bass
  kDurs2[]  fillarray 4, 4, 1, 1, 1
  kFreqs2[] fillarray 7.00, 7,01, 6.09, 7.00,7.04
  kTrigger2, kOffTrigger2, kIndex2 patternizer 4, 120, "0 4 8 9 10 11"
  schedkwhen kTrigger2, 0, 0, "PAD", 0, kDurs2[kIndex2%lenarray(kDurs2)],cpspch(kFreqs2[kIndex2%lenarray(kFreqs2)]) 
endin

/*
instr DELETEME
  ibpm = 120
  ibps = int(kr/ibpm*60)
  ipatterns[] array giPattern1, giPattern2,giPattern3
  ktime timeinstk
  kLastTime init 0
  k_m_indx init 0
  k_p_indx init 0
  next:
    ipattern rand_nth ipatterns
    kNextBeat tab k_p_indx, ipattern
    kNextNote tab k_m_indx, giMelody1
    if kLastTime+kNextBeat*ibps <= ktime then
      kLastTime = ktime 
      event "i", "PAD", 0, kNextBeat, cpspch(kNextNote)
    k_m_indx = (tab:k(k_m_indx+1,giMelody1) == 0 ? 0 : k_m_indx + 1)
    k_p_indx = (tab:k(k_p_indx+1,ipattern) == 0 ? 0 : k_p_indx + 1)
      reinit next
    endif
endin
*/

instr REVERB
  aL, aR reverbsc gaRvbL, gaRvbR, 0.7, 700
  outs aL, aR
  clear gaRvbL, gaRvbR
endin





</CsInstruments>
<CsScore>
;; i "ListenForShots"      0 [3600*24*7]
;; i "ListenForExplosions" 0 [3600*24*7]
;;e 0 1
;;i "Explosion" 0 1
i "REVERB" 0 360
i "song1" 0 360
;;i "debug" 0 1
</CsScore>
</CsoundSynthesizer>
