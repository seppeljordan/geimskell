<CsoundSynthesizer>
<CsOptions>
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1.0

instr testOSC
  print 6
  OSCsend 1, "localhost", 7770, "/shoot", "f",66.6
  
endin


</CsInstruments>
<CsScore>
i "testOSC" 2 1
</CsScore>
</CsoundSynthesizer>
