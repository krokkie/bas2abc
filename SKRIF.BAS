10 REM SKRIFBERYMING MELODIEE DEUR GERHARD EN JOHAN
13 DEF SEG=&H40:POKE &H17,(PEEK(&H17) OR 96)
14 DEF SEG=&H40:POKE &H17,(PEEK(&H17) AND 40)
15 CLS:COLOR 15,0:PRINT"É"STRING$(74,"Í")"»":FOR U=1 TO 21:PRINT "º";SPC(74);"º":NEXT U:PRINT"È"STRING$(74,"Í")"¼"
20 COLOR 7:LOCATE 3,6:PRINT"Skrifberyming melodi‰ deur Gerhard- en Johan Bijker":COLOR 7
30 LOCATE 6,6:PRINT "Kies 'n skrifberyming (1-50)                    ";:COLOR 15,0:LOCATE 6,35:INPUT A:IF A<1 OR A>50 THEN 32 ELSE GOTO 40
32 COLOR 7:LOCATE 6,6:PRINT "Kies 'n skrifberyming (";:COLOR 15,0:PRINT "1-50";:COLOR 7:PRINT ")                        ";:COLOR 15,0:LOCATE 6,35:INPUT A:COLOR 7:IF A<1 OR A>50 THEN 30 ELSE GOTO 40
40 COLOR 7:IF A=1 THEN LOCATE 15,6:PRINT "Ek wil die hoogste Heer ... (6)":GOTO 2000
50 IF A=2 THEN LOCATE 15,6:PRINT "Loof nou die God van Israel ... (5)":GOTO 2020
60 IF A=3 THEN LOCATE 15,6:PRINT "Neem nou maar, Heer, u kneg ... (2)":GOTO 2040
70 IF A=4 THEN LOCATE 15,6:PRINT "En daar was by Betlehem ... (8)":GOTO 2060
80 IF A=5 THEN LOCATE 15,6:PRINT "Die woord het vlees geword ... (2)":GOTO 2080
90 IF A=6 THEN LOCATE 15,6:PRINT "Oor die kruis neersinkend, bid hy: ... (7)":GOTO 2100
100 IF A=7 THEN LOCATE 15,6:PRINT "Wie het gelowig hart en oor ... (10)":GOTO 2120
110 IF A=8 THEN LOCATE 15,6:PRINT "Aan U, o Lam van God, 'n nuwe lofgesang! ... (5)":GOTO 2140
120 IF A=9 THEN LOCATE 15,6:PRINT "Toe staar ek deur die wˆreldnagte ... (6)":GOTO 2160
130 IF A=10 THEN LOCATE 15,6:PRINT "Wie kan ons voor die regter daag? ... (6)":GOTO 2180
140 IF A=11 THEN LOCATE 15,6:PRINT "Die laagte deur waar die olywe staan, ... (6)":GOTO 2200
150 IF A=12 THEN LOCATE 15,6:PRINT "'n Eng'lestem het ek hoor juig, ... (4)":GOTO 2220
160 IF A=13 THEN LOCATE 15,6:PRINT "Toe kom daar haastig 'n geluid ... (9)":GOTO 2240
170 IF A=14 THEN LOCATE 15,6:PRINT CHR$(34)+"Water sal Ek op hul sprinkel, ... (2)":GOTO 2260
180 IF A=15 THEN LOCATE 15,6:PRINT "Laat jul kinders tot My nader ... (7)":GOTO 2280
190 IF A=16 THEN LOCATE 15,6:PRINT "Salig die wat arm van gees is ... (10)":GOTO 2300
200 IF A=17 THEN LOCATE 15,6:PRINT "Wees nie besorgd nie oor jul lewe, ... (6)":GOTO 2320
210 IF A=18 THEN LOCATE 15,6:PRINT "Ek loof U, Vader, groot van mag, ... (8)":GOTO 2340
220 IF A=19 THEN LOCATE 15,6:PRINT "Nou sal ek opstaan, na die woning weer, ... (7)":GOTO 2360
230 IF A=20 THEN LOCATE 15,6:PRINT "Bo 'n strandelose en donker see ... (8)":GOTO 2380
240 IF A=21 THEN LOCATE 15,6:PRINT "Al sou 'k die skoonste taal kon spreek, ... (6)":GOTO 2400
250 IF A=22 THEN LOCATE 15,6:PRINT "Troos, troos my volk, sˆ God die Here; ... (11)":GOTO 2420
260 IF A=23 THEN LOCATE 15,6:PRINT "Toe sien ek: alles was verby - ... (4)":GOTO 2440
270 IF A=24 THEN LOCATE 15,6:PRINT "Gese‰nd sy bo alle dinge ... (7)":GOTO 2460
280 IF A=25 THEN LOCATE 15,6:PRINT "Genade en vrede sy oor ons ... (4)":GOTO 2480
290 IF A=26 THEN LOCATE 15,6:PRINT "My siel herdink met sidderbewe ... (11)":GOTO 2500
300 IF A=27 THEN LOCATE 15,6:PRINT "Ek geloof in God die Vader ... (5)":GOTO 2520
310 IF A=28 THEN LOCATE 15,6:PRINT "O Vader, in die hemel hoog, ... (4)":GOTO 2540
320 IF A=29 THEN LOCATE 15,6:PRINT "Bo lug en wolke 't ek gehoor ... (9)":GOTO 2560
330 IF A=30 THEN LOCATE 15,6:PRINT "Hy't my die lewenstroom getoon ... (6)":GOTO 2580
340 IF A=31 THEN LOCATE 15,6:PRINT "Toe sien ek teen die bloue vert' ... (3)":GOTO 2600
350 IF A=32 THEN LOCATE 15,6:PRINT "Toe het die ho‰ lug gebeef ... (4)":GOTO 2620
360 IF A=33 THEN LOCATE 15,6:PRINT "Wat 'n liefde en volheidslewe, ... (3)":GOTO 2640
370 IF A=34 THEN LOCATE 15,6:PRINT "Die Heer is God, 'n eenge Heer; ... (3)":GOTO 2660
380 IF A=35 THEN LOCATE 15,6:PRINT "O dieptes vol geheimenis ... (3)":GOTO 2680
390 IF A=36 THEN LOCATE 15,6:PRINT "Toe hoor ek uit die hemel uit ... (5)":GOTO 2700
400 IF A=37 THEN LOCATE 15,6:PRINT "Toe sien ek soos 'n see van glas, ... (3)":GOTO 2720
410 IF A=38 THEN LOCATE 15,6:PRINT "Ek wat die sewe sterre dra ... (3)":GOTO 2740
420 IF A=39 THEN LOCATE 15,6:PRINT "Dit sˆ die Eerste en Laaste, Hy ... (5)":GOTO 2760
430 IF A=40 THEN LOCATE 15,6:PRINT "Ek wat die swaard dra, weerskant skerp, ... (7)":GOTO 2780
440 IF A=41 THEN LOCATE 15,6:PRINT "Die Seun van God, die hoogste Heer, ... (7)":GOTO 2800
450 IF A=42 THEN LOCATE 15,6:PRINT "Ek wat die sewe Geeste stuur ... (5)":GOTO 2820
460 IF A=43 THEN LOCATE 15,6:PRINT "Hy wat heilig en waaragtig ... (6)":GOTO 2840
470 IF A=44 THEN LOCATE 15,6:PRINT "Dis die woord van Hom, die Amen, ... (5)":GOTO 2860
480 IF A=45 THEN LOCATE 15,6:PRINT "Hy het my die stad getoon, ... (12)":GOTO 2880
490 IF A=46 THEN LOCATE 15,6:PRINT "Bo die kimme van verdwyning ... (5)":GOTO 2900
500 IF A=47 THEN LOCATE 15,6:PRINT "Die Here sal ons se‰n, ... (1)":GOTO 2920
510 IF A=48 THEN LOCATE 15,6:PRINT "Die genade van die Here Jesus Christus ... (1)":GOTO 2940
520 IF A=49 THEN LOCATE 15,6:PRINT "Barmhartige en getroue God, ... (7)":GOTO 2960
530 IF A=50 THEN LOCATE 15,6:PRINT "O grote Christus, eewge Lig, ... (7)":GOTO 2980
540 GOTO 15
2000 PLAY "o3 l4 t100 mn c l8 d c o2 l4 b- a g b- l8 a f g g l4 f p8 f o3 l8 c c d c o2 l4 b- a"
2005 PLAY "o3 c o2 l8 a b- a g l4 f p8 f l8 d e f d l4 c p8 f l8 g a b- a l4 g f.":GOTO 5000
2020 PLAY "o3 l4 t100 mn c l8 d c l4 o2 a o3 c l8 o2 b- b- l4 a p4 l4 g l8 a b- o3 c o2 b- l4 a g f p4"
2025 PLAY "o3 c l8 d c l4 o2 a o3 c l8 o2 b- b- l4 a p4 g l8 a b- o3 c o2 b- l4 a g f p4 f l8 g a l4 b- g l8 a g l4 f p4"
2027 PLAY "f l8 a b- l4 o3 c d l8 o2 a b l4 o3 c p4 o2 a l8 o3 d d c o2 a l4 b- a p4 o3 d l8 d d l4 o2 g o3 c l8 o2 b- g a a l4 g p4"
2030 PLAY "b- l8 a g f g l4 e d c p4 c l8 f g a b- o3 c o2 a l4 b- g f.":GOTO 5000
2040 GOTO 2000
2060 PLAY "o2 l8 t75 mn a a o3 c o2 g l16 f g l8 a l4 g l8 g a o3 c d c o2 b- l4 a l8 a a o3 c o2 g l16 f g l8 a l4 g"
2065 PLAY "l8 g a o3 c d c o2 b- l4 a l8 g g l16 a b l8 o3 c c o2 b l4 o3 c l8 c c d o2 b- a g l4 f":GOTO 5000
2080 PLAY "t100 mn l4 o2 e- l8 g a- b- a- l4 g p8 a- l8 g e- f f l4 e- p8 b- o3 e- d l8 c o2 b- b- a l4 b- p8"
2090 PLAY "l8 b- a- g f f l4 e-":GOTO 5000
2100 PLAY "t80 mn l8 o2 d e f+ e f+ g+ a a o3 d d l16 c+o2 b l8 o3 c+ o2 b b l4 a l8 o3 d o2 a b b a g f+ d"
2110 PLAY "f+ l16 f+ g l8 a a g f+ l4 e l8 a a g f+ g a l4 f+ l8 e f+ g a f+ e l4 d":GOTO 5000
2120 PLAY "t100 mn l4 o2 a l8 f g f d f g l4 a p4 a l8 o3 c o2 a f g b- b- l4 a p4 a l8 f g f d f g l4 a p4"
2125 PLAY "a l8 o3 c o2 a f g b- b- l4 a p4 o3 c l8 d c o2 a f a g l4 f p4 a l8 a a g f e e l4 d p4 f l8 e d l4 c f l8 f g l4 a p4"
2130 PLAY "b- l8 a g f d l4 f e d.":GOTO 5000
2140 PLAY "T100 l4 o2 mn g l8 e d g a l4 b b l8 o3 d c l4 o2 b a g p4 g l8 e d g a l4 b b l8 o3 d c l4 o2 b a g p4"
2145 PLAY "b l8 b a g o3 c l4 o2 b g l8 a f+ g f+ l4 e d p4 d l8 e g g f+ l4 g d l8 g a b g o3 l4 c o2 b p4"
2150 PLAY "o3 d l8 c o2 b a a l4 g o3 c o2 l8 b a g f+ l4 e d p4 a l8 b a g f+ l4 g a l8 b o3 d c o2 b l4 a g.":GOTO 5000
2160 PLAY "T100 mn o2 l4 g l8 e d g g a o3 c o2 l4 b a p4 o3 d o2 l8 b b g o3 c o2 l4 b a g p4 g l8 e d g g a o3 c o2 l4 b a p4"
2165 PLAY "o3 d l8 c o2 b a g g f+ l4 g p4 o3 d l8 d c o2 l4 b a l8 g f+ l4 e d p4 d l8 g g f+ e l4 g a b p4"
2170 PLAY "g l8 g a b g o3 c c o2 l4 b a p4 o3 d l8 c o2 b a g g f+ l4 g.":GOTO 5000
2175 REM SB 10
2180 PLAY "t100 l4 mn o2 b- f d b- b- o3 c c d o2 b- o3 e- d c o2 b- a g f f g f b- f g o3 c o2 a o3 e- d o2 b- g o3 c o2 b- a b-":GOTO 5000
2200 PLAY "t100 mn l4 o2 a l8 a f+ d f+ a a b b l4 a p4 a l8 a g a d g f+ e e l4 d p4 a l8 a a o3 d c+ d o2 a b a l4 f+ p4"
2210 PLAY "f+ l8 g a f+ g f+ d e e l4 d.":GOTO 5000
2220 REM  12 12 12 12 12 12 12 12 12 12 12 12 12 12
2221 PLAY "t100 l8 o2 mn e l4 g a l8 g o3 c c o2 b o3 l4 c p8 l8 c o2 b a l4 o3 d c l8 o2 b b l4 a p8 l8 e l4 g a l8 g o3 c c o2 b o3 l4 c p8"
2222 PLAY "l8 c o2 b a l4 o3 d c l8 o2 b b l4 a p8 l8 a a a l4 b l8 a l4 g l8 f+ l4 g p8 l8 g a b l8 o3 c o2 b a a l4 b p8 l8 e l4 g a l8 g o3 c c o2 b o3 l4 c p8"
2223 PLAY "l8 c o2 b a l4 o3 d c l8 o2 b b l4 a.":GOTO 5000
2240 PLAY "t100 l4 o2 mn e  l8 g g f+ e g a l4 b p4 b l8 g b a g f+ f+ l4 e p4 b l8 o3 d c+ o2 b a l4 b a g p4 g l8 a a b e l4 g f+ e.":GOTO 5000
2260 PLAY "T100 l8 mn o2 d e f+ d g f+ e d e e f+ a a g+ l4 a l8 d e f+ d g f+ e d e e f+ a a g+ l4 a l8 a f+ b a g f+ e e a a b a b o3"
2267 PLAY "C+ l4 d l8 o2 a f+ b a g f+ e e f+ a g f+ e e l4 d.":GOTO 5000
2280 PLAY "t80 l8 mn o2 f g a g a b- l4 o3 c o2 a o3 l8 d c o2 b- a g g l4 a o3 l8 c c c o2 l16 g a l8 b- o3 c l4 o2 a g l8 f g a b- a g l4 f":GOTO 5000
2300 PLAY "t70 mn l8 o2 g g f b- a g l4 a g p4 l8 g b- o3 c o2 b- o3 c d o2 b- p8 l8 g b- o3 c o2 b- o3 c o2 b- l4 a g p4 l8 g g f b- l16 a g l8 a l4 g":GOTO 5000
2320 PLAY "t100 o2 l8 mn d g a l4 b- l8 a l4 g l8 a f+ d p8 f f e- l4 d l8 g l4 g l8 f+ l4 g. l8 d g a l4 b- l8 a l4 g l8 a f+ d p8 f f e- l4 d l8 g l4 g l8 f+ l4 g."
2323 PLAY "l8 a b- o3 c l4 d l8 d l4 c o2 l8 b- o3 l4 c o2 l8 b- b- a g l4 f+ l8 g l4 g l8 f+ l4 g.":GOTO 5000
2340 PLAY "t80 mn l8 o2 d d a a b b l4 a p8 l8 a a o3 d c+ o2 a b b l4 a p8 l8 a o3 d c+ o2 b a b a l4 f+ p4"
2350 PLAY "f+ l8 b b a g l4 f+ e d.":GOTO 5000
2360 PLAY "t100 mn l6 o3 c l4 c o2 b o3 c o2 g l8 e f d d l4 c p4 c l8 e f l4 g a l8 b o3 c o2 a a l4 g p8"
2370 PLAY "l8 g l4 o3 c c o2 b a l8 g a f f l4 e p4 c l8 g a l4 g d l8 f e d d l4 c.":GOTO 5000
2380 REM 20 20 20 20 20 20 20 20 20 20 20 20 20 20
2382 PLAY "t100 l4 o2 mn f f l8 g l4 a l8 o3 c o2 b- b- l4 a p8 l8 a l4 o3 c l8 c o2 l4 g l8 a f f l4 e p8"
2386 PLAY "l8 f g a b- a l4 g p8 l8 a l4 f l8 g l4 a o3 l8 c o2 b- b- l4 a p8 l8 a l4 o3 c l8 c l4 o2 g l8  a f f l4 e"
2390 PLAY "c l8 f g l4 a g f.":GOTO 5000
2400 PLAY "t80 l8 o3 mn c o2 a o3 c d c o2 b- a l4 g p8 l8 a g f g a b- a l4 g p8 l8 o3 c o2 a o3 c d c o2 b- a l4 g p8"
2404 PLAY "l8 a g f g a b- a l4 g p8 l8 g g g a o3 c c o2 b o3 l4 c p8 l8 c o2 a f o3 d o2 b- a g l4 f.":GOTO 5000
2420 PLAY "t100 l4 o2 mn d l8 f+ e l4 d a l8 b a l4 g f+ a l8 b o3 c+ d o2 b l4 a g+ a f+ l8 b a l4 o3 d c+ o2 l8 b a l4 g f+"
2430 PLAY "a l8 d e f+ g l4 f+ e d":GOTO 5000
2440 PLAY "t100 l4 o2 mn d f+ l8 f+ l4 a l8 a g f+ l4 e p8 l8 a b o3 c+ d c+ o2 b b l4 a d f+ l8 f+ l4 a l8 a g f+ l4 e p8"
2445 PLAY "l8 a b o3 c+ d c+ o2 b b l4 a a o3 d l8 c+ o2 l4 b l8 a g f+ l4 e a g l8 f+ l4 e l8 g l4 f+ e d.":GOTO 5000
2460 PLAY "t100 o2 l4 mn c l8 f g a o3 c o2 b- a l4 g f g l8 g a g e l4 f d c p4 c l8 f g a o3 c o2 b- a l4 g f g l8 g a g e l4 f d c p4"
2470 PLAY "c l8 f g a a o3 d c o2 l4 b o3 c c o2 f g l8 a o3 c o2 b- a l4 g p4 g g a l8 g e f e l4 d c c l8 f g a b- l4 g g f.":GOTO 5000
2480 PLAY "t100 mn l4 o2 E l8 g e d e g f+ l4 e p4 g l8 a a b g a a l4 b p4 b l8 o3 c o2 b o3 d o2 b a a l4 g p4"
2490 PLAY "b l8 g a g f+ e d+ l4 e.":GOTO 5000
2500 PLAY "t100 mn o2 l4 g l8 g e- f g e- d l4 c p4 g l8 g f b- g e- f l4 g p4 g l8 b- o3 c e- d c o2 b l4 o3 c p4"
2504 PLAY "c l8 d c o2 b- a g f+ l4 g p4 o3 c o2 l8 b- a b- g g f l4 e- p4 g l8  a- g e- f e- d l2 c":GOTO 5000
2520 PLAY "T100 l8 mn o2 d e f+ d g f+ e d e e f+ a a g+ l4 a l8 d e f+ d g f+ e d e e f+ a a g+ l4 a l8 a f+ b a g f+ e e"
2523 PLAY "a a b a b o3 c+ l4 d l8 o2 a f+ b a g f+ e e f+ a g f+ e e l4 d.":GOTO 5000
2540 PLAY "T100 mn l4 o2 g l8 g e- f g e- d l4 c p4 g l8 g f b- g e- f l4 g p4 g l8 b- o3 c e- d c o2 b o3 l4 c p4"
2550 PLAY "c l8 d c o2 b- a g f+ l4 g p4 o3 c l8 o2 b- a b- g g f l4 e- p4 g l8 a- g e- f e- d l2 c":GOTO 5000
2560 PLAY "t100 mn o2 l8 d f+ g a f+ b b l4 a p16 l8 a o3 d c+ o2 b a b b l4 a p16 l8 a a f+ g f+ l4 e d p16 l8 d f+ g a a b o3 c+ o3 l4 d p16"
2572 PLAY "o2 l8 a b a f+ a g g l4 f+ p16 o3 l8 d o2 b a g f+ l4 e l2 d":GOTO 5000
2579 REM SB 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30 30
2580 PLAY "o3 l8 t90 mn d c+ o2 b o3 c+ o2 a b o3 c+ d d d o2 a a f+ a g f+ f+ b b a o3 c+ d o2 b a o3 d c+ o2 b a g f+ e d":GOTO 5000
2600 PLAY "t100 l4 o2 mn d l8 d e f+ d f+ g+ l4 a p4 a l8 g f+ e d f+ g+ l4 a p4 b l8 a g f+ d l4 e d p4 d l8 d e f+ d f+ g+ l4 a p4"
2605 PLAY "a l8 g f+ e d f+ g+ l4 a p4 b l8 a g f+ d l4 e d p4 o3 d l8 d d c+ o2 a b b l4 a p4 o3 d l8 d d c+ o2 a b b l4 a p4"
2608 PLAY "f+ l8 g b a f+ l4 g f+ p4 e l8 f+ g a g f+ f+ l4 e p4 a l8 b o3 c+ d c+ o2 b b l4 a p4 d l8 e f+ g f+ l4 e d.":GOTO 5000
2620 PLAY "t100 mn o2 l8 g g a l4 b l8 a l4 g l8 f+ l4 g l8 g l4 g l8 g ml a g mn f+ l4 e l8 d+ l4 e p16 l8 g g a l4 b l8 a l4 g l8 f+ l4 g"
2624 PLAY "l8 g l4 g l8 g ml a g mn f+ l4 e l8 d+ l4 e l8 f+ l4 g l8 a l4 b o3 l8 d ml c o2 b mn a l4 b l8 b l4 b l8 a l4 b l8 a ml b a mn g l4 f+"
2627 PLAY "l8 f+ l4 f+ l8 g ml a g mn f+ l4 e l8 d+ l4 e l8 e l4 f+ l8 g l4 a l8 b l4 b l8 a l4 g.":GOTO 5000
2640 PLAY "o2 l4 mn t100 f l8 g l4 a l8 g f e l4 d c p4 f l8 g l4 a l8 b- l4 a g f p4 f l8 g l4 a l8 g f e l4 d c p4"
2642 PLAY "f l8 g l4 a l8 b- l4 a g f p4 a l8 a l4 o3 c l8 o2 b- a g l4 a p4 o3 c l8 c l4 d l8 c o2 b- a l4 g p4 a l8 o3 c l4 o2 b- l8 a f g l4 a f p4"
2650 PLAY "a l8 a l4 b- l8 a g f l4 e f.":GOTO 5000
2660 PLAY "t100 l4 o2 mn d l8 d e f+ d f+ g+ l4 a p4 a l8 g f+ e d f+ g+ l4 a p4 b l8 a g f+ d l4 e d p4 d l8 d e f+ d f+ g+ l4 a p4"
2662 PLAY "a l8 g f+ e d f+ g+ l4 a p4 b l8 a g f+ d l4 e d p4 o3 d l8 d d c+ o2 a b b l4 a p4 o3 d l8 d d c+ o2 a b b l4 a p4"
2665 PLAY "f+ l8 g b a f+ l4 g f+ p4 e l8 f+ g a g f+ f+ l4 e p4 a l8 b o3 c+ d c+ o2 b b l4 a p4 d l8 e f+ g f+ l4 e d.":GOTO 5000
2679 REM 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35
2680 PLAY "t100 mn l4 o2 d f g l8 a g b- a l4 g p4 a o3 l8 c o2 b- a g l4 a b- a p4 o3 c l8 o2 b- a l4 g f l8 f e l4 f p4"
2687 PLAY "f l8 g f e d d c+ l4 d p4 a l8 a b- o3 l4 c d l8 c o2 b- l4 a p4 f l8 g a l4 b- a l8 g g l4 f p4 a g e l8 f g a g l4 f e p4"
2689 PLAY "f l8 g f e d f f l4 e d.":GOTO 5000
2700 PLAY "T100 l4 o2 mn a l8 o3 d d c+ o2 a b o3 c+ l4 d p4 d l8 c+ o2 b a g l4 f+ e d p4 d l8 e f+ g a a g+ l4 a p4"
2703 PLAY "a l8 b b o3 d d l4 c+ o2 b a p4 a l8 g f+ b a l4 g f+ e p4 e l8 f+ g a b l4 a g+ a.":GOTO 5000
2720 PLAY "t100 l8 o2 mn e l4 g a l8 g o3 c c o2 b o3 l4 c p8 l8 c o2 b a l4 o3 d c l8 o2 b b l4 a p8 l8 e l4 g a l8 g o3 c c o2 b o3 l4 c p8"
2723 PLAY "l8 c o2 b a l4 o3 d c l8 o2 b b l4 a p8 l8 a a a l4 b l8 a l4 g l8 f+ l4 g p8 l8 g a b l8 o3 c o2 b a a l4 b p8 l8 e l4 g a l8 g o3 c c o2 b o3 l4 c p8"
2724 PLAY "l8 c o2 b a l4 o3 d c l8 o2 b b l4 a.":GOTO 5000
2740 PLAY "mn t100 l4 o2 g l8 g f+ e d g a l4 b p16 l8 a o3 d c o2 b a o3 c o2 a l4 g p4 g l8 g f+ e d g a l4 b p16 l8 a o3 d c o2 b a o3 c o2 a l4 g p16"
2742 PLAY "l8 g a b g a o3 c o2 b l4 a p16 l8 e a g f+ g e e l4 d p16 l8 d g f+ e d g a l4 b p16 l8 a o3 d c o2 b a o3 c o2 a l4 g":GOTO 5000
2760 PLAY "T100 mn l8 o2 d a a e g f e l4 d p16 l8 d a b o3 c o2 b a g+ l4 a p16 l8 a o3 d o2 b o3 c o2 a g f l4 e l8 p16 e d a e g f e l4 d.":GOTO 5000
2779 REM 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40 40
2780 PLAY "t100 mn o2 l4 e l8 g g f+ e g a l4 b p4 b l8 g b a g f+ f+ l4 e p4 b l8 o3 d c+ o2 b a l4 b a g p4 g l8 a a b e l4 g f+ e.":GOTO 5000
2800 PLAY "t100 mn l8 o2 a a a l4 f+ l8 a l4 b l8 g+ l4 f+ p16 l8 f+ g+ a l4 b l8 e l4 g+ l8 f+ l4 e p16 l8 e a b o3 l4 c+ o2 l8 a o3 l4 d l8 c+ o2 l4 b p16"
2810 PLAY "l8 o3 c+ c+ c+ l4 o2 b l8 a l4 a l8 g+ l4 a.":GOTO 5000
2820 PLAY "t100 mn l4 o2 g l8 g f+ g a b- a l4 g p16 l8 g a a o3 d o2 a b- b- l4 a p8 o3 l4 d l8 c o2 b- a o3 c o2 b- a l4 g p8"
2830 PLAY "o3 d l8 c o2 b- a o3 c o2 b- a l4 g p8 b- l8 a g f d f g l4 a p8 g l8 a a o3 d c l4 o2 b- a g.":GOTO 5000
2840 GOTO 2260
2860 PLAY "t100 mn l4 o2 g f l8 e- f g b- l4 a- g b- g l8 a- g l4 e- f e- p8 g f l8 e- f g b- l4 a- g b- g l8 a- g l4 e- f e- p8"
2870 PLAY "b- o3 l8 c l4 e- l8 e- d o2 b- o3 l4 c o2 b- b- o3 l8 c l4 e- l8 e- l4 d c o2 b- f g l8 a- a- g f ml f e mn l4 f g b- l8 a- g f f l4 e-.":GOTO 5000
2879 REM 45 45 45 45 45 45 45 45 45 45 45 45 45 45
2880 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2887
2885 IF G=2 THEN 2890 ELSE 2280
2887 PLAY "t100 mn l4 o2 g e l8 a b o3 c c l4 o2 b a o3 c l8 o2 a g f e l4 d c g e l8 a b o3 c c l4 o2 b a o3 c l8 o2 a g f e l4 d c"
2889 PLAY "e f+ l8 g+ a a g+ l4 a l8 b o3 c d e d d l4 c.":GOTO 5000
2890 PLAY "t100 mn o2 l4 g l8 g ml g f+ mn g ml a g mn a l4 b p16 b l8 b ml b o3 c mn d ml d c mn o2 b b a p16 l4 g l8 g ml g f+ mn g ml a g mn a l4 b p16"
2895 PLAY "b l8 b ml b o3 c mn d ml d c mn o2 b b a p16 l4 a l8 b o3 ml c o2 b mn a l4 b o3 l8 c l4 d p16 e l8 e ml d c mn o2 b o3 ml c o2 b mn a l4 g. p16":GOTO 5000
2900 PLAY "t100 mn l4 o2 d a l8 b a g o3 c o2 l4 b a p4 a g l8 b a f+ g l4 e d p4 a b l8 a g e f+ l4 g p4"
2910 PLAY "g g l8 a b o3 cc o2 l4 b a p4 o3 d c l8 o2 b a g g l4 f+ g p4 a a l8 e f+ l4 g e d.":GOTO 5000
2920 PLAY "t80 l4 o3 mn c o2 l8 a b- o3 c d l4 c c o2 l8 a b- g g l4 f p4 f o3 l8 c o2 a b- a l4 g a o3 c l8 c d c o2 b- l4 a"
2930 PLAY "b- l8 o3 c o2 f b- a g f l4 g f p4 d l8 f e l4 f g a a l8 f g l4 a o3 c o2 l8 a f b- a l4 g f p4 l8 d f ml g mn f l4 f.":GOTO 5000
2940 PLAY "t100 l4 o2 mn e e l8 d e g a l4 e e l8 g g l4 a b p4 l8 b b l4 a l8 b g l4 a p4 b l8 b a g a g f+ e d+ l4 e p4"
2950 PLAY "a l8 b a e g l4 f+ e.":GOTO 5000
2960 PLAY "T100 l4 o2 mn g a l8 f b- a l4 g l8 f+ l4 g p4 d l8 g f l4 g b- l8 b- a l4 b- p4 b- o3 c d l8 o2 g a l4 b- g f p4"
2970 PLAY "f l8 g b- l4 f g l8 f e- l4 d.":GOTO 5000
2980 GOTO 2480
5000 CLS:COLOR 15,0:PRINT"É"STRING$(74,"Í")"»":FOR U=1 TO 21:PRINT "º";SPC(74);"º":NEXT U:PRINT"È"STRING$(74,"Í")"¼":COLOR 15,0
5010 COLOR 7:LOCATE 12,25:PRINT "Nog 'n Skrifberyming (Ja/Nee)":LOCATE 12,47:COLOR 15,0:PRINT "Ja":COLOR 7:P=1
5020 U$=INKEY$
5030 IF U$="6" OR U$=CHR$(75) THEN P=2:IF P=2 THEN LOCATE 12,47:COLOR 7:PRINT "Ja/";:COLOR 15,0:PRINT "Nee":COLOR 13:COLOR 7
5040 IF U$="4" OR U$=CHR$(74) THEN P=1:IF P=1 THEN LOCATE 12,47:COLOR 15,0:PRINT "Ja";:COLOR 7:PRINT "/Nee"
5050 IF U$=CHR$(13) AND P=1 THEN GOTO 10
5060 IF U$=CHR$(13) AND P=2 THEN CLS:DEF SEG=&H40:POKE &H17,(PEEK(&H17) AND 20):SYSTEM
5070 GOTO 5020
