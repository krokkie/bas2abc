10 P=1
20 CLS:SCREEN 0,0,0:DEF SEG=&H40:POKE &H17,(PEEK(&H17) OR 96)
30 REM Psalm,Skrif,Ander
50 LOCATE 1,1:PRINT "ษอออออออออออออออออออออออออออออออออออออออออออป
60 PRINT "บ          Ander Musiek Liedere             บ
70 PRINT "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
80 PRINT "บ A. Apostoliese Geloofsbelydenis           บ
90 PRINT "บ B. Dit is die dag deur God geskenk        บ
100 PRINT "บ C. Suid-Sotho gesang 79                   บ
110 PRINT "บ D. Suid-Sotho gesang 195                  บ
120 PRINT "บ E. Gesang 210                             บ
130 PRINT "บ F. Greenleaves                            บ
140 PRINT"บ G. Haas Das se lied                       บ
150 PRINT"บ H. Hansie Slim                            บ
160 PRINT"บ I. Lavender's Blue                        บ
170 PRINT"บ J. Ludwig (Fur Elise)                     บ
180 PRINT"บ K. Johannes Calvyn Skoollied              บ
190 PRINT"บ L. Die Stem van Suid-Afrika               บ
200 PRINT"บ M. Tweede beryming van die Stem           บ
210 PRINT"บ N. Verjaarsdag liedjie                    บ
220 PRINT"บ O. Willtell                               บ
230 PRINT"บ P. Uitgaan                                บ
240 PRINT"ศอออออออออออออออออออออออออออออออออออออออออออผ
250 IF P=1 THEN COLOR 0,7:LOCATE 4,3:PRINT "A. Apostoliese Geloofsbelydenis":COLOR 7,0
260 IF P=2 THEN COLOR 0,7:LOCATE 5,3:PRINT "B. Dit is die dag deur God geskenk":COLOR 7,0
270 IF P=3 THEN COLOR 0,7:LOCATE 6,3:PRINT "C. Suid-Sotho gesang 79":COLOR 7,0
280 IF P=4 THEN COLOR 0,7:LOCATE 7,3:PRINT "D. Suid-Sotho gesang 195":COLOR 7,0
290 IF P=5 THEN COLOR 0,7:LOCATE 8,3:PRINT "E. Gesang 210":COLOR 7,0
300 IF P=6 THEN COLOR 0,7:LOCATE 9,3:PRINT "F. Greenleaves":COLOR 7,0
310 IF P=7 THEN COLOR 0,7:LOCATE 10,3:PRINT "G. Haas Das se lied":COLOR 7,0
320 IF P=8 THEN COLOR 0,7:LOCATE 11,3:PRINT "H. Hansie Slim":COLOR 7,0
330 IF P=9 THEN COLOR 0,7:LOCATE 12,3:PRINT "I. Lavender's Blue":COLOR 7,0
340 IF P=10 THEN COLOR 0,7:LOCATE 13,3:PRINT "J. Ludwig (Fur Elise)":COLOR 7,0
350 IF P=11 THEN COLOR 0,7:LOCATE 14,3:PRINT "K. Johannes Calvyn Skoollied":COLOR 7,0
360 IF P=12 THEN COLOR 0,7:LOCATE 15,3:PRINT "L. Die Stem van Suid-Afrika":COLOR 7,0
370 IF P=13 THEN COLOR 0,7:LOCATE 16,3:PRINT "M. Tweede beryming van die Stem":COLOR 7,0
380 IF P=14 THEN COLOR 0,7:LOCATE 17,3:PRINT "N. Verjaarsdag liedjie":COLOR 7,0
390 IF P=15 THEN COLOR 0,7:LOCATE 18,3:PRINT "O. Willtell":COLOR 7,0
400 IF P=16 THEN COLOR 0,7:LOCATE 19,3:PRINT "P. Uitgaan":COLOR 7,0
410 U$=INKEY$:IF U$="" THEN 410
420 IF U$="8" AND P=1 THEN P=16:GOTO 50
430 IF U$="2" AND P=16 THEN P=1:GOTO 50
440 IF U$="8" THEN P=P-1:GOTO 50
450 IF U$="2" THEN P=P+1:GOTO 50
460 IF U$=CHR$(13) THEN 480
461 IF U$="A" THEN P=1:GOTO 250
462 IF U$="B" THEN P=2:GOTO 250
463 IF U$="C" THEN P=3:GOTO 250
464 IF U$="D" THEN P=4:GOTO 250
465 IF U$="E" THEN P=5:goTO 250
466 IF U$="F" THEN P=6:GOTO 250
467 IF U$="G" THEN P=7:GOTO 250
468 IF U$="H" THEN P=8:GOTO 250
469 IF U$="I" THEN P=9:GOTO 250
470 IF U$="J" THEN P=10:GOTO 250
471 IF U$="K" THEN P=11:GOTO 250
472 IF U$="L" THEN P=12:GOTO 250
473 IF U$="M" THEN P=13:GOTO 250
474 IF U$="N" THEN P=14:GOTO 250
475 IF U$="O" THEN P=15:GOTO 250
476 IF U$="P" THEN P=16:GOTO 250
479 GOTO 410
480 IF P=1 THEN 640
490 IF P=2 THEN PLAY "o3 l8 t90 mn c o2 b a b g a b o3 c p8 c c o2 g g e g f e p8 e a a g b o3 c o2 a g p8 o3 c o2 b a g f e d c":GOTO 20
500 IF P=3 THEN 750
510 IF P=4 THEN 800
520 IF P=5 THEN 850
530 IF P=6 THEN 890
540 IF P=7 THEN 940
550 IF P=8 THEN 1040
560 IF P=9 THEN 1080
570 IF P=10 THEN 1110
580 IF P=11 THEN 1640
590 IF P=12 THEN 1710
600 IF P=13 THEN 1780
610 IF P=14 THEN 1840
620 IF P=15 THEN 1860
630 IF P=16 THEN CLS:DEF SEG=&H40:POKE &H17,(PEEK(&H17) AND 20):SYSTEM
640 REM Apostoliese Geloofsbeleidenis
650 PLAY "t100 mn l8 o2 g g g l4 o3 c o2 g o3 c o2 g p4 l8 o3 c c l4 d d c p8"
660 PLAY "l8 c o2 l4 a l8 a a l4 f l8 b- a l4 g f p4 l8 o2 a a o3 l4 d o2 a o3 d o2 a p4"
670 PLAY "p8 l8 o3 d l4 e l8 e e d d d d l4 o2 b b  l8 g g o3 c o2 b a g l4 a l8 a a l4 b g l8 o3 c o2 b a g l4 a a g g p4"
680 PLAY "g l8 a- a- a- a-  g g g g l4 f f p4 f l8 e- e- e- e- d- d- d- d- c c l4 c p4"
690 PLAY "c l8 c c c c o1 l4 b b o2 c p4 l8 c c d- d- l4 e- l8 f f g g l4 a- b- o3 c p4"
700 PLAY "l8 o3 c c d d d d l4 e l8 e e l4 c l8 c c o2 a a a a l4 f l8 f f l4 b- l8 a b- l4 g f p4"
710 PLAY "l8 o2 f f l4 f l8 f f l4 b- l8 a b- l4 o3 c l8 c c l4 o2 b- b- a l8 a a l4 g f p8"
720 PLAY "l8 o2 f l4 b- l8 f f l4 b- l8 f f l4 b- p4 p4 l8 o2 a a o3 l4 d l8 d d c c c c o2 b- b- b- b- l4 a l8 o2 a a f f b- a l4 g g f l8 f f l4 b- l8 f f l4 b- b- p4"
730 PLAY "l4  o2 a o3 d l8 d d l4 c c o2 b b p4 l8  o2 a g l2 o3 e l4 d d l2 c c"
740 GOTO 20
750 REM Johan Bijker 15 Januarie  Suid-Sotho gesang 79
760 PLAY "t60 o2 mn l16 b b a b l8 o3 c o2 b b a l4 g p8 l16 a a a o3 c l8 d c o2 b g l4 a p8"
770 PLAY "o3 l16 d d o2 b o3 d l8 e d d c o2 l4 b p8 l16 e o3 c o2 b a l8 g g a o3 d. l4 d p8"
780 PLAY "l16 d c o2 b a l8 g g l4 a g"
790 GOTO 20
800 REM SUID SOTHO gesang 195 Gerhard bijker
810 PLAY "t60 mn l8 o2 f+ l16 f+ l8 d a g l16 a g f+ e l8 d p64"
820 PLAY "e. l16 e l8 d a l4 a f+ p64 l8 b. l16 b l8 a a g l16 a g f+ e l8 d p64"
830 PLAY "e. l16 e l8 o1 a l16 o2 g f+ l4 e d p64 l8 e. l16 e l8 o1 a l16 o2 g f+ l4 e d"
840 GOTO 20
850 REM gesang 210 G D Bijker
860 PLAY "t80 mn l4 o2 f l8 g f e- f b- o3 c l4 o2 b- p8 l8 b- f g b- o3 c d d l4 c p8"
870 PLAY "l8 c d o2 b- g f a b- o3 l4 c p8 o2 l8 f b- g e- g g f l4 f"
880 GOTO 20
890 REM grEenleeves
900 PLAY "o3a o4l2cl4d e.l8fl4e l2dl4o3b g.l8al4b o4l2co3l4a"
910 PLAY "a.l8g#l4a l2bl4g# o3l2el4a o4l2cl4d e.l8fl4e l2dl4o3b g.l8al4b o4l4c.o3l8bl4a"
920 PLAY "l4g#.l8f#l4g# l2a. l2a. o4l2g. l4g.l8f#l4e l2do3l4b g.l8al4b l2o4co3l4a"
930 PLAY "a.l8g#l4a l2bl4g# l2e. o4l2g. l4g.l8f#l4e l2do3l4b g.l8al4b o4l4c.o3l8bl4a g#.l8f#l4g# l2a. a.":GOTO 50
940 REM Haas Das se lied by Johan Bijker
950 PLAY "t100 mn o2 l16 a b-"
960 PLAY "l8 o3 c o2 a l4 o3 c l16 o2 b- o3 c l8 o2 b- l4 g l8 o3 c. l16  c d l8 c o2 b-"
970 PLAY "l4 a. l16 a b- o3 c o2 a o3 l4 c o2 l16 b- o3 c l8 o2 b- l4 g"
980 PLAY "l8 o3 c p16 o2 l16 c d d l8 e l4 f. p16 l16 o3 d d l8 d l4 d"
990 PLAY "l16 c c l8 c l4 c o2 l8 c c g c l4 a. p16 l 16 o3 d d l8 d l4 d l16 c c l8 c l4 c l8 c. o2 l16 b- l8 a g"
1000 PLAY "l4 f. l16 a b- l8 o3 c o2 a l4 o3 c l16 o2 b- o3 c l8 o2 b- l4 g l8 o3 c. l16  c d l8 c o2 b-"
1010 PLAY "l4 a. l16 a b- o3 c o2 a o3 l4 c o2 l16 b- o3 c l8 o2 b- l4 g l8 o3 c p16 o2 l16 c d d l8 e l4 f. p16 l16 o3 d d l8 d l4 d"
1020 PLAY "l16 c c l8 c l4 c o2 l8 c c g c l4 a. p16 l 16 o3 d d l8 d l4 d l16 c c l8 c l4 c l8 c. o2 l16 b- l8 a g"
1030 PLAY "l4 f. l16 a b- o3 c c o2 l8 b- a g l4 f.":GOTO 20
1040 REM Hansie Slim  5-12-90 Johan Bijker
1050 PLAY "t100 o3 mn l8 d o2 b l4 b o3 l8 c o2 a l4 a l8 g a b o3 c d d l4 d l8 d o2 b l4 b o3 l8 c o2 a"
1060 PLAY "l4 a l8 g b o3 d d o2 l2 g l8 a a a a a b o3 l4 c l8 o2 b b b b b o3 c l4 d"
1070 PLAY "l8 d o2 b l4 b  o3 l8 c o2 a l4 a l8 g b o3 d d o2 g.":GOTO 20
1080 REM Lavender's Blue  G D Bijker
1090 PLAY "t60 mn l8 o2 g b o3 c d l16 c o2 b a g l8 a d d l4 a. l8 g b o3 c d l16 c o2 b a g l8 a d d l4 g."
1100 PLAY "l8 b g g o3 c o2 l16 b a g f+ l8 a d d l4 a. l8 g b o3 c d l16 c o2 b a g l8a d d l4 g.":GOTO 20
1110 REM Ludwig Fur Elise
1120 A$="o4ed#ed#eo3bo4dco3ao2eao3ceabo2eg#o3eg#bo4co2eao3eo4ed#ed#eo3bo4dco3ao2eao3ceabo2eg#o3do4co3ba"
1130 AC$="o2ea"
1140 B$="o3ceabo2eg#o3do4co3bao2eao3bo4cdeo2go3cgo4fedo2gbo3fo4edco2eao3go4dco3bo2eo3ep64eo4e"
1150 C$="o3eo4ep64eo5eo4d#ed#ed#ed#"
1160 D$="o3bo4cdeo2go3cgo4fedo2gbo3fo4edco2eao3eo4dco3bo2eo3eo4e"
1170 E$="l6mso4cp64cp64cmll24o3fao4cl12o2ao3co2ao4fep64edb-ap64agfedco3b-al24bagabl12o4co2ao3co2ao4dd#eo2ao3co4efo3ao4co3e"
1180 F$="mnl12o1aaaaao4c#.o1aaaaao4do1aaao4efp64fo1aaao4fo1ao4e.o1aaaaao4do1aaao4co3bao1aaao3ao1ao3ao1ao4co1ao3bo1g#o3ao1aaaaao4c#.o1aaaaao4do1aaao4efp64fo1aaao4fo1ao4f.o1b-b-b-b-b-o4e-o1b-b-b-o4dco3b-o1b-b-b-o3ao1b-o3g#o1bbbo3g#o1bmlo3a4p4b8p8"
1190 G$="l16o4cdco3bo4cdo3bo4cgo3bo4go3ao4go3bo4gcgdgego5co4bagfedgfdcgo3bo4go3cao4go3bo4gcgdgego5co4bagfedgfdeged#eo3bo4edeo3bo4edeo3bo4ed#e.l6o3bo4ed#ed#ed#el12"
1200 H$="mnl12o1aaaaao4c#.o1aaaaao4do1aaao4efp64fo1aaao4fo1ao4e.o1aaaaao4do1aaao4co3bao1aaao3ao1ao3ao1ao4co1ao3bo1g#o3ao1aaaaao4c#.o1aaaaao4do1aaao4efp64fo1aaao4fo1ao4f.o1b-b-b-b-b-o4e-o1b-b-b-o4dco3b-o1b-b-b-o3ao1b-o3g#o1bbbo3g#o1bo3a4p4b8p8"
1210 I$="o2ao3ceao4cedco3bao4ceao5cedco4bao5ceao6cedco5bb-ag#gf#fed#dc#co4bb-ag#gf#fed#eo3bo4dco3ao2eao3ceabo2eg#o3eg#bo4co2eao3e"
1220 PLAY "t110 ml l12"
1230 PLAY A$
1240 PLAY AC$
1250 PLAY "p32"
1260 PLAY A$
1270 PLAY AC$
1280 PLAY B$
1290 PLAY C$
1300 PLAY A$
1310 PLAY AC$
1320 PLAY D$
1330 PLAY C$
1340 PLAY A$
1350 PLAY AC$
1360 PLAY "p32"
1370 PLAY E$
1380 PLAY F$
1390 PLAY "p32"
1400 PLAY A$
1410 PLAY AC$
1420 PLAY D$
1430 PLAY C$
1440 PLAY A$
1450 PLAY G$
1460 PLAY H$
1470 PLAY A$
1480 PLAY AC$
1490 PLAY D$
1500 PLAY C$
1510 PLAY A$
1520 PLAY "p32"
1530 PLAY A$
1540 PLAY AC$
1550 PLAY D$
1560 PLAY C$
1570 PLAY A$
1580 PLAY G$
1590 PLAY H$
1600 PLAY A$
1610 PLAY AC$
1620 PLAY D$
1630 GOTO 20
1640 REM Skoollied  5-12-90 Johan Bijker
1650 PLAY "T60 o2 l16 mn c. l32 c. l8 f g a b- a. l16 g l8 g l16 g. l32 a l8 b-. o3 l16 d l8 ml c o2 g"
1660 PLAY "mn l4 a. l16 a. l32 a l8 g. l16 g l8 a b o3 ml c o2 g e mn a g. l16 f l8 e d"
1670 PLAY "l4 c. l 16 ml c c c mn l4 f l8 g c l4 a. l16 f f l8 b-. l16 a l8 g f l4 e. l8"
1680 PLAY "c d. l16 d  l8 e ml l16 d e mn l8 f g a a g. l16 a l8 b g a b o3 c o2 c f."
1690 PLAY "l16 f l8 g l16 f g l8 ml a b- o3 l4 c l8 d o2 b- g l16 ml a b- mn l8 ml a g mn l4 f"
1700 GOTO 20
1710 REM Die Stem van Suid-Afrika
1720 PLAY "t60 mn l16 o2 c c l8f f a a l4 o3 c l8 c o2 l16 b- a l8 g l16 b- g l8 f e l4 f."
1730 PLAY "l16 c c l8 f f a a o3 l4 c l8 c L16 c d l8 e l16 d c l8 d o2 g o3 l4 c."
1740 PLAY "o2 l16 a o3 c l8 d d o2 a a o3 l4 c l8 c l16 c o2 b- l8 a a o3 d o2 b- l4 g."
1750 PLAY "l16 o3 co2 b- l8 aab-o3 cl4 d l8 d l16e.l32 d l8 c l16 o2 b-a l8g.l16 f l4f."
1760 PLAY "l16 f g l8 f e g o3 c l4 c l8 c l16 c d l8 c o2 b- g o3 c o2 l4 a. l16 f. l32 a l8 o3 c c c f l4 f l4 d l16 c. o2 l32 b- l8 a o3 c d e l4 f."
1770 GOTO 20
1780 REM Die tweede beryming v d  STEM
1790 PLAY "t60 mn o2 l16 c c l8 c c f f l4 e l8 f l16 f f l8 f l16 f d l8 f f ml f o1 b- a"
1800 PLAY "mn l16 b- b- o2 l8 c c f f ml e l16 d e mn l8 f l16 ff l8 g e f f l4 e."
1810 PLAY "l16 f a l8 a a f f ml f d mn e l16 c c l8 c f f f l4 ml f l8 e mn l16 g.l32 g l8 g f f f l4 f l8 f l16 f. l32 f l8 f f f e l16 f l4 c."
1820 PLAY "l16 c c l8 f f f f ml f e mn f l16 f f l8 d gg f g l4 g. l16 f. f l8 e b- a a ml b- a mn l4 g l16 f+. l32 g l8 f f f b- l4 a."
1830 GOTO 20
1840 REM verjaarsdag liedjie deur een van die Bijker's
1850 PLAY "t100 mn l8 o2 b o3 c l4 d o2 b b b a l8 a b l4 o3 c o2 a o3 e d p8 l8 o2 b o3 c l4 d o2 b b b a l8 a b o3 c c l4 o2 g- a g":GOTO 20
1860 CLS:KEY OFF:PRINT TAB(25);"The William Tell Overture"
1870 PLAY"MBL16T155"
1880 FOR I=1 TO 100
1890  READ M$
1900  PLAY M$
1910 NEXT I
1920 CLEAR:P=15:GOTO 20
1930 FOR I=1 TO 5000:NEXT I:GOTO 120
1940 DATA "o2mnb4p8msbbmnb4p8msbbb8g#8
1950 DATA "e8g#8b8g#8b8o3e8o2b8g#8e8g#8
1960 DATA "b8g#8b8o3e8o2mnb4p8msbbmnb4
1970 DATA "p8msbbmnb4p8msbbmnb4p8msbb
1980 DATA "b8bbb8b8b8bbb8b8b8bb
1990 DATA "b8b8b8bbb8b8mlb2
2000 DATA "b2b8p8p4p4
2010 DATA "p8mso1bbb8bbb8bbo2e8f#8g#8o1bb
2020 DATA "b8bbo2e8g#g#f#8d#8o1b8bbb8bb
2030 DATA "b8bbo2e8f#8g#8eg#mlb4bmsag#f#
2040 DATA "e8g#8e8o3bbb8bbb8bbo4e8f#8
2050 DATA "g#8o3bbb8bbo4e8g#g#f#8d#8o3b8bb
2060 DATA "b8bbb8bbo4e8f#8g#8mleg#b4
2070 DATA "bag#f#mse8g#8e8o3g#g#g#8g#g#g#8g#g#
2080 DATA "g#8o4c#8o3g#8o4c#8o3g#8o4c#8o3g#8f#8e8d#8
2090 DATA "c#8g#g#g#8g#g#g#8g#g#g#8o4c#8o3g#8o4c#8
2100 DATA "o3g#8o4c#8o3b8a#8b8a#8b8g#g#g#8g#g#
2110 DATA "g#8g#g#g#8o4c#8o3g#8o4c#8o3g#8o4c#8o3g#8f#8
2120 DATA "e8d#8c#8g#g#g#8g#g#g#8g#g#g#8o4c#8
2130 DATA "o3g#8o4c#8o3g#8o4c#8o3b8a#8b8o2bbb8f#f#
2140 DATA "f#8f#f#f#8g#8a8f#4mna8msg#8mne4
2150 DATA "msg#8f#8f#8f#8o3f#f#f#8f#f#f#8g#8
2160 DATA "a8mnf#4msa8g#8mne4msg#8f#8o2bb
2170 DATA "b8o1bbb8bbb8bbo2mne8f#8g#8o1bb
2180 DATA "b8bbo2e8g#g#f#8d#8o1b8bbb8bb
2190 DATA "b8bbo2e8f#8g#8eg#mlb4mnbag#f#
2200 DATA "e8g#8e8o3bbb8bbb8bbo4e8f#8
2210 DATA "g#8o3bbb8bbo4e8g#g#f#8d#8o3b8bb
2220 DATA "b8bbb8bbo4e8f#8g#8mleg#mlb4
2230 DATA "mnbag#f#mne8g#8e8o3mle56f56g56a56b56o4c56d56mne8eee8e8g#4.
2240 DATA "f#8e8d#8e8c#8mso3bo4c#o3bo4c#o3b
2250 DATA "o4c#d#eo3abababo4c#d#o3g#ag#ag#abo4c#o3f#
2260 DATA "g#f#g#f#g#f#g#f#g#f#d#o2bo3mlbo4c#d#e8d#8e8
2270 DATA "c#8o3msbo4c#o3bo4c#o3bo4c#d#eo3abababo4c#d#o3g#
2280 DATA "ag#ag#abo4c#o3f#g#f#g#f#af#emne8p8mlc#4
2290 DATA "mnc#o2cmso3c#o2co3d#c#o2baag#ec#c#c#c#c#e
2300 DATA "d#o1cg#g#g#g#g#g#o2c#eg#o3c#c#c#c#c#o2co3c#o2co3d#
2310 DATA "c#o2baag#ec#c#c#c#c#ed#o1cg#g#g#g#g#mng#
2320 DATA "o2c#eg#o3msc#ed#c#d#o2cg#g#g#o3g#ec#d#o2cg#g#g#
2330 DATA "o3g#ec#d#o2bg#g#a#gd#d#g#gg#gg#ag#f#e
2340 DATA "o1ba#bo2eo1bo2f#o1bo2g#ed#eg#eaf#bo3g#f#ed#
2350 DATA "f#ec#o2bo3c#o2bo3c#d#ef#g#o2ababo3c#d#ef#o2g#
2360 DATA "ag#aco3c#d#eo2f#g#f#g#f#g#f#g#f#g#f#d#o1b
2370 DATA "co2c#d#eo1ba#bo2eo1bo2f#o1bo2g#ed#eg#eaf#b
2380 DATA "o3g#f#ed#f#ec#o2bo3c#o2bo3c#d#ef#g#o2ababo3c#
2390 DATA "d#ef#o2g#ag#abo3c#d#eo2f#o3c#o2co3c#d#c#o2af#mne
2400 DATA "o3mlef#g#abo4c#d#mne8mseee8e8g#4.
2410 DATA "msf8mse8d#8e8c#8o3bo4c#o3bo4c#o3bo4c#d#eo3a
2420 DATA "bababo4c#d#o3g#ag#ag#abo4c#o3f#g#f#g#f#
2430 DATA "g#f#g#f#g#f#d#o2bo3mlbo4c#d#mne8eee8e8g#4.
2440 DATA "msf#8e8d#8e8c#8o3bo4c#o3bo4c#o3b
2450 DATA "o4c#d#eo3abababo4c#d#o3g#ag#ag#abo4c#o3f#
2460 DATA "g#f#g#f#ag#f#e8o2b8o3e8g#g#g#8mng#g#g#8
2470 DATA "g#g#g#8o4c#8o3g#8o4c#8o3g#8o4c#8o3g#8f#8e8
2480 DATA "d#8c#8g#g#g#8g#g#g#8g#g#g#8o4c#8o3g#8
2490 DATA "o4c#8o3g#8o4c#8o3b8a#8b8a#8b8g#g#g#8
2500 DATA "g#g#g#8g#g#g#8o4c#8o3g#8o4c#8o3g#8o4c#8o3g#8
2510 DATA "f#8e8d#8c#8g#g#g#8g#g#g#8g#g#g#8
2520 DATA "o4c#8o3g#8o4c#8o3g#8o4c#8o3b8a#8b8a#8b8
2530 DATA "o2f#f#f#8f#f#f#8g#8a8f#4a8g#8
2540 DATA "e4g#8f#8o0b8o1b8o2f#f#f#8f#f#f#8
2550 DATA "g#8a8f#4a8g#8e4g#8f#8
2560 DATA "bbb8o1bbb8bbb8bbo2e8f#8g#8
2570 DATA "o1bbb8bbo2e8g#g#f#8d#8o1b8bbb8
2580 DATA "bbb8bbo2e8f#8g#8eg#mlb4mnb
2590 DATA "ag#f#e8o1b8o2e8o3bbb8bbb8bbo4e8
2600 DATA "f#8g#8o3bbb8bbo4e8g#g#f#8d#8o3b8
2610 DATA "bbb8bbb8bbo4e8f#8g#8o3eg#mlb4
2620 DATA "mnbag#f#mlef#g#mnamlg#abo4mnc#mlo3bo4c#d#mnemld#
2630 DATA "ef#mng#ao3bo4ao3bo4ao3bo4ao3bo4ao3bo4ao3bo4ao3bo4ao3bmle
2640 DATA "f#g#mnamlg#abmno4c#mlo3bo4c#d#mnemld#ef#mng#ao3bo4ao3bo4a
2650 DATA "o3bo4ao3bo4ao3bo4ao3bo4ao3bo4ao3bp16mlg#o4g#o3mng#p16mld#o4d#o3mnd#p16
2660 DATA "mleo4eo3mnep16mlao4ao3mnap16mlg#o4g#o3mng#p16mld#o4d#o3mnd#p16mleo4eo3mnep16
2670 DATA "mlao4ao3mnao4go3go4go3go4go3go4go3go4msg8e8c8e8o4mng#
2680 DATA "o3g#o4g#o3g#o4g#o3g#o4g#o3g#o4msg#8e8o3b8o4e8mng#o3g#o4g#o3g#o4g#
2690 DATA "o3g#o4g#o3g#o4msg#8f8c#8f8mna#o3a#o4a#o3a#o4a#o3a#o4a#o3a#o4msa#8
2700 DATA "g8e8g8b8p16mna#p16ap16g#p16f#p16ep16
2710 DATA "d#p16c#p16o3bp16a#p16ap16g#p16f#p16ep16d#p16f#mle
2720 DATA "f#g#mnamlg#abmno4c#o3mlbo4c#d#mnemld#ef#mng#ao3bo4ao3bo4a
2730 DATA "o3bo4ao3bo4ao3bo4ao3bo4ao3bo4ao3bmlef#g#mnamlg#abmno4c#o3mlb
2740 DATA "o4c#d#mnemld#ef#mng#ao3bo4ao3bo4ao3bo4ao3bo4ao3bo4ao3bo4a
2750 DATA "o3bo4ao3bp16mlg#o4g#o3mng#p16mld#o4d#o3mnd#p16mleo4eo3mnep16mlao4ao3mnap16
2760 DATA "mlg#o4g#o3mng#p16mld#o4d#o3mnd#p16mleo4eo3mnep16mlao4ao3mnao4go3go4go3go4g
2770 DATA "o3go4go3go4g8e8c8e8g#o3g#o4g#o3g#o4g#o3g#o4g#o3g#o4g#8
2780 DATA "e8o3b8o4e8g#o3g#o4g#o3g#o4g#o3g#o4g#o3g#o4msg#8mnf8c#8
2790 DATA "f8a#o3a#o4a#o3a#o4a#o3a#o4a#o3a#o4a#8g8e8g8b8
2800 DATA "p16a#p16ap16g#p16f#p16ep16d#p16c#p16o3bp16a#p16
2810 DATA "ap16g#p16f#p16ep16d#p16fmled#ed#mne8bbb8
2820 DATA "bbb8bbo4e8f#8g#8o3bbb8bbb8
2830 DATA "bbo4g#8a8b8p8e8f#8g#8p8o3g#8
2840 DATA "a8b8p8p2o2bco3c#dd#
2850 DATA "eff#gg#aa#bco4c#d#ed#f#d#ed#f#d#e
2860 DATA "d#f#d#ed#f#d#ed#f#d#ed#f#d#ed#f#d#e
2870 DATA "d#f#d#e8eo3eo4eo3eo4eo3eo4e8o3bo2bo3bo2bo3bo2bo3b8
2880 DATA "g#o2g#o3g#o2g#o3g#o2g#o3g8eo2eo3eo2eo3eo2eo3e8eee8
2890 DATA "e8e8o2bbb8b8b8g#g#g#8g#8g#8
2900 DATA "eee8e8e8o1b8o2e8o1b8o2g#8e8b8
2910 DATA "g#8o3e8o2b8o3e8o2b8o3g#8e8b8g#8o4e4
2920 DATA "p8eee8e8e8e8e4p8.
2930 DATA "ee4p8.o2ee2
