10 REM PSALMS MELODIEE DEUR GERHARD EN JOHAN
13 DEF SEG=&H40:POKE &H17,(PEEK(&H17) OR 96)
14 DEF SEG=&H40:POKE &H17,(PEEK(&H17) AND 40)
15 CLS:COLOR 15,0:PRINT"É"STRING$(74,"Í")"»":FOR U=1 TO 21:PRINT "º";SPC(74);"º":NEXT U:PRINT"È"STRING$(74,"Í")"¼"
20 COLOR 7:LOCATE 3,6:PRINT"Psalm melodi‰ deur Gerhard- en Johan Bijker":COLOR 7
30 LOCATE 6,6:PRINT "Kies 'n psalm (1-150)                        ";:COLOR 15,0:LOCATE 6,28:INPUT A:IF A<1 OR A>150 THEN 32 ELSE GOTO 40
32 COLOR 7:LOCATE 6,6:PRINT "Kies 'n psalm (";:COLOR 15,0:PRINT "1-150";:COLOR 7:PRINT ")                           ";:COLOR 15,0:LOCATE 6,28:INPUT A:COLOR 7:IF A<1 OR A>150 THEN 30 ELSE GOTO 40
40 COLOR 7:IF A=1 THEN LOCATE 15,6:PRINT "Welsalig hy wat op die spoor nie gaan ...(4)":GOTO 2000
50 IF A=2 THEN LOCATE 15,6:PRINT "Wat dring en dryf die wilde heidendom ...(6)":GOTO 2020
60 IF A=3 THEN LOCATE 15,6:PRINT "Hoe groei my teenparty! ...(2)":GOTO 2040
70 IF A=4 THEN LOCATE 15,6:PRINT "Verhoor my, Heer, in donker dae, ...(4)":GOTO 2060
80 IF A=5 THEN LOCATE 15,6:PRINT "O Heer, neem my gebed ter ore! ...(11)":GOTO 2080
90 IF A=6 THEN LOCATE 15,6:PRINT "Wil my, o Heer, nie straf nie, ...(8)":GOTO 2100
100 IF A=7 THEN LOCATE 15,6:PRINT "Heer, my God, by wie ek skuiling, ...(8)":GOTO 2120
110 IF A=8 THEN LOCATE 15,6:PRINT "Daar is geen land so ver of woes gele‰, ...(7)":GOTO 2140
120 IF A=9 THEN LOCATE 15,6:PRINT "Ek sal van ganser harte, Heer, ...(15)":GOTO 2160
130 IF A=10 THEN LOCATE 15,6:PRINT "Waarom, o Heer bly U so ver weg staan? ...(10)":GOTO 2180
140 IF A=11 THEN LOCATE 15,6:PRINT "Ek skuil by God in al my onweersdae. ...(5)":GOTO 2200
150 IF A=12 THEN LOCATE 15,6:PRINT "Red, red, en hoor, o Heer, ...(7)":GOTO 2220
160 IF A=13 THEN LOCATE 15,6:PRINT "Hoe lank is ek van hulp ontbloot, ...(5)":GOTO 2240
170 IF A=14 THEN LOCATE 15,6:PRINT "Vermeetle dwaas wat hoogwys spot, ...(7)":GOTO 2260
180 IF A=15 THEN LOCATE 15,6:PRINT "Wie styg so hoog in heil en eer, ...(5)":GOTO 2280
190 IF A=16 THEN LOCATE 15,6:PRINT "Bewaar my tog, o God, op wie 'k vertrou! ...(5)":GOTO 2300
200 IF A=17 THEN LOCATE 15,6:PRINT "O Heer, verleen my tog verhoor; ...(7)":GOTO 2320
210 IF A=18 THEN LOCATE 15,6:PRINT "Ek het U hartlik lief, o Heer! ...(20)":GOTO 2340
220 IF A=19 THEN LOCATE 15,6:PRINT "Die ho‰ hemelrond ...(7)":GOTO 2360
230 IF A=20 THEN LOCATE 15,6:PRINT "O, mag die Here u verhoor ...(9)":GOTO 2380
240 IF A=21 THEN LOCATE 15,6:PRINT "U maak, o Heer, die koning bly, ...(9)":GOTO 2400
250 IF A=22 THEN LOCATE 15,6:PRINT "My God, my God, waarom verlaat U my, ...(12)":GOTO 2420
260 IF A=23 THEN LOCATE 15,6:PRINT "a. Net soos 'n herder uittrek met sy skape ...(3)":LOCATE 16,6:PRINT "b. Die Here is my herder, Hy behoed my; ...(3)":GOTO 2440
270 IF A=24 THEN LOCATE 15,6:PRINT "Die ganse aarde, land en meer ...(5)":GOTO 2460
280 IF A=25 THEN LOCATE 15,6:PRINT "Tot U hef ek hart en hande; ...(10)":GOTO 2480
290 IF A=26 THEN LOCATE 15,6:PRINT "O Heer, doen aan my reg! ...(7)":GOTO 2500
300 IF A=27 THEN LOCATE 15,6:PRINT "God is my lig, my heil: ek sal nie vrees nie; ...(6)":GOTO 2520
310 IF A=28 THEN LOCATE 15,6:PRINT "Ek roep U aan, o Here hoor! ...(8)":GOTO 2540
320 IF A=29 THEN LOCATE 15,6:PRINT "Eng'lemagte, loof die Heer, ...(5)":GOTO 2560
330 IF A=30 THEN LOCATE 15,6:PRINT "Ek sal met hart en mond, o Heer, ...(8)":GOTO 2580
340 IF A=31 THEN LOCATE 15,6:PRINT "Net soos 'n rots, gesterk, gestewig ...(19)":GOTO 2600
350 IF A=32 THEN LOCATE 15,6:PRINT "Welsalig wie van skuld en straf onthewe, ...(6)":GOTO 2620
360 IF A=33 THEN LOCATE 15,6:PRINT "Sing vrolik, hef die stem na bowe; ...(11)":GOTO 2640
370 IF A=34 THEN LOCATE 15,6:PRINT "In weelde en ongeluk, ...(9)":GOTO 2660
380 IF A=35 THEN LOCATE 15,6:PRINT "Verdedig to my regsaak, Heer, ...(12)":GOTO 2680
390 IF A=36 THEN LOCATE 15,6:PRINT "Die boosheid van die bose man - ...(3)":GOTO 2700
400 IF A=37 THEN LOCATE 15,6:PRINT "Vertoorn jou oor die bose nooit: ...(25)":GOTO 2720
410 IF A=38 THEN LOCATE 15,6:PRINT "Straf tog nie in ongenade ...(17)":GOTO 2740
420 IF A=39 THEN LOCATE 15,6:PRINT "Ek het gesˆ: Ek wil my weg bewaar, ...(7)":GOTO 2760
430 IF A=40 THEN LOCATE 15,6:PRINT "Ek het die Heer lank in my druk verwag - ...(8)":GOTO 2780
440 IF A=41 THEN LOCATE 15,6:PRINT "Welsalig wie die arme in verdriet ...(6)":GOTO 2800
450 IF A=42 THEN LOCATE 15,6:PRINT "Soos 'n hert in dorre streke ...(7)":GOTO 2820
460 IF A=43 THEN LOCATE 15,6:PRINT "Heer, doen my reg, hoor my gebede, ...(5)":GOTO 2840
470 IF A=44 THEN LOCATE 15,6:PRINT "O God, het ons nie met ons oor nie ...(9)":GOTO 2860
480 IF A=45 THEN LOCATE 15,6:PRINT "My hart, ontroer deur mymering, ...(16)":GOTO 2880
490 IF A=46 THEN LOCATE 15,6:PRINT "God is 'n toevlug in die nood, ...(6)":GOTO 2900
500 IF A=47 THEN LOCATE 15,6:PRINT "Juig, o volke, juig! Handklap en betuig ...(4)":GOTO 2920
510 IF A=48 THEN LOCATE 15,6:PRINT "Die Heer is groot waar Sions top ...(5)":GOTO 2940
520 IF A=49 THEN LOCATE 15,6:PRINT "Kom luister, volke wat die aarde oordek, ...(7)":GOTO 2960
530 IF A=50 THEN LOCATE 15,6:PRINT "Die grote God, die Here spreek! ...(11)":GOTO 2980
540 IF A=51 THEN LOCATE 15,6:PRINT "Ontferm, o God, ontferm U tog oor my! ...(8)":GOTO 3000
550 IF A=52 THEN LOCATE 15,6:PRINT "Wat roem jy, wreedaard, so hooghartig? ...(4)":GOTO 3020
560 IF A=53 THEN LOCATE 15,6:PRINT "Vermeetle dwaas wat hoogwys spot, ...(7)":GOTO 3040
570 IF A=54 THEN LOCATE 15,6:PRINT "O God, verlos my deur u Naam, ...(3)":GOTO 3060
580 IF A=55 THEN LOCATE 15,6:PRINT "Verhoor, o God, verhoor my klagte, ...(10)":GOTO 3080
590 IF A=56 THEN LOCATE 15,6:PRINT "Genade, o God! Ek hoop op u alleen ...(4)":GOTO 3100
600 IF A=57 THEN LOCATE 15,6:PRINT "Wees my genadig, o my God! Ek soek ...(5)":GOTO 3120
610 IF A=58 THEN LOCATE 15,6:PRINT "Spreek jul wel waarlik reg, o regters? ...(4)":GOTO 3140
620 IF A=59 THEN LOCATE 15,6:PRINT "Red my o God, wil my bewaar! ...(10)":GOTO 3160
630 IF A=60 THEN LOCATE 15,6:PRINT "O God, hoe het U ons verstoot! ...(4)":GOTO 3180
640 IF A=61 THEN LOCATE 15,6:PRINT "Neig, o God, tot my die ore! ...(6)":GOTO 3200
650 IF A=62 THEN LOCATE 15,6:PRINT "Gewis my siel is stil tot God! ...(8)":GOTO 3220
660 IF A=63 THEN LOCATE 15,6:PRINT "My God, wat tog my lewe is, ...(6)":GOTO 3240
670 IF A=64 THEN LOCATE 15,6:PRINT "O God, verhoor nou my gebed ...(7)":GOTO 3260
680 IF A=65 THEN LOCATE 15,6:PRINT "Na bange tyd van somerdroogte ...(9)":GOTO 3280
690 IF A=66 THEN LOCATE 15,6:PRINT "Juig, aarde, juig met blye galme, ...(9)":GOTO 3300
700 IF A=67 THEN LOCATE 15,6:PRINT "Wees ons, o Heer, wees ons genadig ...(3)":GOTO 3320
710 IF A=68 THEN LOCATE 15,6:PRINT "Die Heer sal opstaan tot die stryd; ...(16)":GOTO 3340
720 IF A=69 THEN LOCATE 15,6:PRINT "Help my, o God, want ek vergaan! ...(14)":GOTO 3360
730 IF A=70 THEN LOCATE 15,6:PRINT "O God, my Hulp in nood, help nou! ...(2)":GOTO 3380
740 IF A=71 THEN LOCATE 15,6:PRINT "Op U alleen, my Rotssteen, bou ek ...(15)":GOTO 3400
750 IF A=72 THEN LOCATE 15,6:PRINT "Gee aan die koning, Heer, u regte ...(11)":GOTO 3420
760 IF A=73 THEN LOCATE 15,6:PRINT "Voorwaar, God is vir Isr'el goed, ...(12)":GOTO 3440
770 IF A=74 THEN LOCATE 15,6:PRINT "Waarom, O God, vind ons geen medely? ...(17)":GOTO 3460
780 IF A=75 THEN LOCATE 15,6:PRINT "U alleen, U loof ons bly - ...(6)":GOTO 3480
790 IF A=76 THEN LOCATE 15,6:PRINT "Die Heer wat woon in donker tent, ...(6)":GOTO 3500
800 IF A=77 THEN LOCATE 15,6:PRINT "Opwaarts klim my roep en rede, ...(10)":GOTO 3520
810 IF A=78 THEN LOCATE 15,6:PRINT "Merk op, my volk wat nou my roepstem hoor, ...(25)":GOTO 3540
820 IF A=79 THEN LOCATE 15,6:PRINT "Die heid'ne het gekom, 'n woeste hoop, ...(9)":GOTO 3560
830 IF A=80 THEN LOCATE 15,6:PRINT "O Herder Isr'els, neem ter ore, ...(10)":GOTO 3580
840 IF A=81 THEN LOCATE 15,6:PRINT "Laat ons nou die Heer ...(18)":GOTO 3600
850 IF A=82 THEN LOCATE 15,6:PRINT "God daag in regsvergadering ...(2)":GOTO 3620
860 IF A=83 THEN LOCATE 15,6:PRINT "O God, wees nie altoos ...(4)":GOTO 3640
870 IF A=84 THEN LOCATE 15,6:PRINT "Hoe lieflik is u wonings, Heer ...(6)":GOTO 3660
880 IF A=85 THEN LOCATE 15,6:PRINT "U het, o Heer, u land weer guns bewys ...(4)":GOTO 3680
890 IF A=86 THEN LOCATE 15,6:PRINT "Heer, my God, ontferm U oor my ...(9)":GOTO 3700
900 IF A=87 THEN LOCATE 15,6:PRINT "Hoe grondvas staan Jerusalem se walle, ...(5)":GOTO 3720
910 IF A=88 THEN LOCATE 15,6:PRINT "Ek roep to U, o God, ...(19)":GOTO 3740
920 IF A=89 THEN LOCATE 15,6:PRINT "Van goedertierenheid sal ek vir ewig sing, ...(18)":GOTO 3760
930 IF A=90 THEN LOCATE 15,6:PRINT "U was, o Heer, vanaf die vroegste jare ...(9)":GOTO 3780
940 IF A=91 THEN LOCATE 15,6:PRINT "Wie toevlug neem in veil'ge hut ...(8)":GOTO 3800
950 IF A=92 THEN LOCATE 15,6:PRINT "Wie sou die Heer nie prys nie, ...(7)":GOTO 3820
960 IF A=93 THEN LOCATE 15,6:PRINT "Die Heer regeer. Die kleed van heerlikheid ...(4)":GOTO 3840
970 IF A=94 THEN LOCATE 15,6:PRINT "Verskyn weer aan die onweerstranse, ...(12)":GOTO 3860
980 IF A=95 THEN LOCATE 15,6:PRINT "Kom, laat ons jubel voor die Heer, ...(6)":GOTO 3880
990 IF A=96 THEN LOCATE 15,6:PRINT "Sing vir die Heer 'n nuwe lied! ...(8)":GOTO 3900
1000 IF A=97 THEN LOCATE 15,6:PRINT "God heers as hoogste Heer: ...(7)":GOTO 3920
1010 IF A=98 THEN LOCATE 15,6:PRINT "Sing nuwe lof aan God die Here, ...(4)":GOTO 3940
1020 IF A=99 THEN LOCATE 15,6:PRINT "God die Heer regeer. Beef, o volke, eer,  ...(5)":GOTO 3960
1030 IF A=100 THEN LOCATE 15,6:PRINT "Juig, al wat leef, juig voor die Heer! ...(4)":GOTO 3980
1040 IF A=101 THEN LOCATE 15,6:PRINT "'k Wil in my lied van reg en goedheid sing ...(6)":GOTO 4000
1050 IF A=102 THEN LOCATE 15,6:PRINT "Heer, u goedheid is oneindig ...(12)":GOTO 4020
1060 IF A=103 THEN LOCATE 15,6:PRINT "Loof, o my siel, en alles wat in my is, ...(11)":GOTO 4040
1070 IF A=104 THEN LOCATE 15,6:PRINT "Waak op, my siel, en loof die Heer! ...(21)":GOTO 4060
1080 IF A=105 THEN LOCATE 15,6:PRINT "Loof, loof die Heer van alle here! ...(24)":GOTO 4080
1090 IF A=106 THEN LOCATE 15,6:PRINT "Geloofd sy God, die eewge Heer! ...(20)":GOTO 4100
1100 IF A=107 THEN LOCATE 15,6:PRINT "Loof God, omdat hy goed is, ...(21)":GOTO 4120
1110 IF A=108 THEN LOCATE 15,6:PRINT "Hoe rus, o God, my hart in my ...(4)":GOTO 4140
1120 IF A=109 THEN LOCATE 15,6:PRINT "God van my lof, merk hoe ek ly, ...(10)":GOTO 4160
1130 IF A=110 THEN LOCATE 15,6:PRINT "Die Heer is dit wat tot my Heer gespreek het: ...(8)":GOTO 4180
1140 IF A=111 THEN LOCATE 15,6:PRINT "'k Sal met my ganse hart die Heer ...(5)":GOTO 4200
1150 IF A=112 THEN LOCATE 15,6:PRINT "Welsalig is die man ...(10)":GOTO 4220
1160 IF A=113 THEN LOCATE 15,6:PRINT "Hef aan, Gods knegte, loof verblyd, ...(3)":GOTO 4240
1170 IF A=114 THEN LOCATE 15,6:PRINT "Toe Isra'el uit Egipteland, ...(4)":GOTO 4260
1180 IF A=115 THEN LOCATE 15,6:PRINT "Nie aan ons, o troue Heer, ...(9)":GOTO 4280
1190 IF A=116 THEN LOCATE 15,6:PRINT "God het ek lief, want die getroue Heer ...(10)":GOTO 4300
1200 IF A=117 THEN LOCATE 15,6:PRINT "Loof, loof die Heer, o heidendom ...(1)":GOTO 4320
1210 IF A=118 THEN LOCATE 15,6:PRINT "Loof, loof die Heer met lofgesange! ...(14)":GOTO 4340
1220 IF A=119 THEN LOCATE 15,6:PRINT "Salig wie in hul opregtheid ...(63)":GOTO 4360
1230 IF A=120 THEN LOCATE 15,6:PRINT "As die ho‰ nood by tye ...(4)":GOTO 4380
1240 IF A=121 THEN LOCATE 15,6:PRINT "Ek hef my o‰ bergwaarts heen ...(4)":GOTO 4400
1250 IF A=122 THEN LOCATE 15,6:PRINT "Ek was verheug toe die geluid ...(2)":GOTO 4420
1260 IF A=123 THEN LOCATE 15,6:PRINT "Ek hef tot U wat in die hemel sit, ...(2)":GOTO 4440
1270 IF A=124 THEN LOCATE 15,6:PRINT "Laat Israel nou uitroep bly van gees: ...(4)":GOTO 4460
1280 IF A=125 THEN LOCATE 15,6:PRINT "Die wat op God vertrou, ...(7)":GOTO 4480
1290 IF A=126 THEN LOCATE 15,6:PRINT "Toe God uit smaad en slawerny ...(3)":GOTO 4500
1300 IF A=127 THEN LOCATE 15,6:PRINT "Laat bouers bou al wat hul kan - ...(4)":GOTO 4520
1310 IF A=128 THEN LOCATE 15,6:PRINT "Wie deur Gods vrees bewo‰, ...(4)":GOTO 4540
1320 IF A=129 THEN LOCATE 15,6:PRINT "Al van my jeug af was ek martelaar! ...(6)":GOTO 4560
1330 IF A=130 THEN LOCATE 15,6:PRINT "a. Uit dieptes, gans verlore, ...(4)":LOCATE 16,6:PRINT "b. Uit dieptes van my sonde, ...(4)":GOTO 4580
1340 IF A=131 THEN LOCATE 15,6:PRINT "My hart is nie hoogmoedig, Heer! ...(3)":GOTO 4600
1350 IF A=132 THEN LOCATE 15,6:PRINT "Gedenk aan Dawid; toe hy, Heer, ...(11)":GOTO 4620
1360 IF A=133 THEN LOCATE 15,6:PRINT "Hoe goed is dit - 'n huis vol lieflikhede! - ...(2)":GOTO 4640
1370 IF A=134 THEN LOCATE 15,6:PRINT "O priesters, wat voor God bly staan, ...(4)":GOTO 4660
1380 IF A=135 THEN LOCATE 15,6:PRINT "Prys die Naam van onse Heer! ...(11)":GOTO 4680
1390 IF A=136 THEN LOCATE 15,6:PRINT "Loof die Heer met blye klank, ...(20)":GOTO 4700
1400 IF A=137 THEN LOCATE 15,6:PRINT "Aan die riviere 't ons gesit ...(9)":GOTO 4720
1410 IF A=138 THEN LOCATE 15,6:PRINT "'k Sal met my ganse hart, o Heer, ...(4)":GOTO 4740
1420 IF A=139 THEN LOCATE 15,6:PRINT "Daar's niks voor U bedek, o Heer! ...(12)":GOTO 4760
1430 IF A=140 THEN LOCATE 15,6:PRINT "O Heer, verlos my van die man ...(11)":GOTO 4780
1440 IF A=141 THEN LOCATE 15,6:PRINT "As daar vir my geen hulp of raad is, ...(10)":GOTO 4800
1450 IF A=142 THEN LOCATE 15,6:PRINT "Ek roep tot God met groot geluid; ...(6)":GOTO 4820
1460 IF A=143 THEN LOCATE 15,6:PRINT "O Heer, ek roep, hoor my gebede ...(10)":GOTO 4840
1470 IF A=144 THEN LOCATE 15,6:PRINT "Geloofd sy God wat my in wank'le tyd ...(6)":GOTO 4860
1480 IF A=145 THEN LOCATE 15,6:PRINT "U Naam, o Heer, so hoog in heerlikheid, ...(12)":GOTO 4880
1490 IF A=146 THEN LOCATE 15,6:PRINT "Prys die Heer met blye galme, ...(8)":GOTO 4900
1500 IF A=147 THEN LOCATE 15,6:PRINT "Laat voor die Heer 'n lofsang rys! ...(7)":GOTO 4920
1510 IF A=148 THEN LOCATE 15,6:PRINT "Loof uit die heemle, loof die Heer! ...(5)":GOTO 4940
1520 IF A=149 THEN LOCATE 15,6:PRINT "Sing tot Gods eer met lof en dank ...(4)":GOTO 4960
1530 IF A=150 THEN LOCATE 15,6:PRINT "Loof God, loof sy Naam alom! ...(3)":GOTO 4980
2000 PLAY "o2 l4 t100 mn a b a f# a l8 g f# l4 g e d p4 l8 d l4 f# g a a l8 b a f# g# l4 a p4 a l8 a g l4 f# f# l8 f# e f# a l4 g f# p4 f# l8 a g l4 f# b l8 b b a g l4 f# e p4 l8 f# l4 a a b o3 d l8 c# o2 b l4 a b a p4
2010 PLAY "a b a f# a l8 g f# l4 g e d":GOTO 5000
2020 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2025
2022 IF G=2 THEN 2030 ELSE 2020
2025 PLAY "t100 mn o2 l4 d l8 f e d c f a g f l4 e p4 f l8 e d l4 a g l8 f e l4 d e d p4 d l8 f e d c f a g f l4 e p4 d l8 f g l4 a a l8 g f l4 d e d p4 o3 d l8 d d l4 c o2 a l8 b- a l4 g a d p4"
2027 PLAY "d l8 f g l4 a b o3 l8 c d c o2 b l4 a p4 o3 d l8 d d l4 c o2 a l8 b- a l4 g a d p4 d l8 f g l4 a g l8 g f l4 g e d.":GOTO 5000
2030 PLAY "t100 mn l8 o2 e- l4 b- a- o3 c. l8 d- e- d- c o2 b- a-. p8 l8 o3 c o2 l4 b- b- b-. l8 a- g e- g f l4 f l8 e- p8 e- o3 c c c. l8 c d- c o2 a f b-. p8 o3 d- l4 c o2 a- f. a- a- a- a- g b- a- p8"
2035 PLAY "e- l4 b- a- o3 c. l8 d- e- d- c o2 b- a-. p8 l8 o3 c o2 l4 b- b- b-. l8 a- g e- g f l4 f l8 e- p8 e- o3 c c c. l8 c d- c o2 a f b-. p8 o3 d- l4 c o2 a- f. a- a- a- a- g b- a-":GOTO 5000
2040 PLAY "t100 l4 o2 mn d a a l8 b o3 c+ l4 d p4 d l8 c+ o2 b a g+ l4 a p4 f+ l8 f+ d g l8 f+ l4 e d p4 a l8 a b a g l4 f+ p4 f+ l8 a a g f+ l4 e p4 f+ l8 g f+ d f+ l4 e d p4 d l8 d e f+ g+ l4 a p4"
2050 PLAY "a l8 b o3 c+ d o2 b l4 a p4 a l8 a a b a l4 g f+ p4 d l8 f+ a a g+ l4 a p4 a l8 b o3 d d c+ l4 d p4 d o2 l8 b a g f+ l4 e d.":GOTO 5000
2060 PLAY "t100 l4 o2 mn g l8 g g f+ d e f+ l4 g f+ p4 g a e l8 g f+ l4 g f+ e p4 b l8 b b l4 e b l8 a f+ l4 g f+ p4 d l8 e f+ g b a g l4 a g p4 g a g l8 e f+ l4 g f+ e p4 b l8 b b l4 e b l8 a f+ l4 g f+ p4"
2070 PLAY "g l8 b b a f+ l4 a g f+ p4 b l8 b b a d e f+ l4 g f+ p4 g l8 g g e e f+ g l4 a g p4 b l8 a g f+ e l4 f+ f+ e.":GOTO 5000
2080 PLAY "t100 l4 mn o2 d l8 a b l4 o3 c c l8 d c o2 l4 b a p4 o3 d l8 d d c o2 b l4 a f e p4 a l8 a a g+ a o3 l4 c o2 b a p4 f l8 f g l4 a a l8 b- a l4 g f p4 a l8 g f l4 e d.":GOTO 5000
2100 PLAY "o2 mn t90 l4 e l8 e e l4 d d l8 e l4 g l8 f+ l4 g p4 l4 b l8 b o3 d c o2 b l4 a g p4 d l8 e g f+ e l4 b p4 o3 d l8 d c o2 b a l4 g e p4 a l8 a g f+ e l4 g d p4 l4 e l8 b b l4 a f+ e.":GOTO 5000
2120 PLAY "t100 mn l4 o2 f+ f+ l8 e c+ f+ g+ l4 a g+ p4 a a l8 b a g+ f+ l4 g+ p4 f+ f+ l8 e c+ f+ g+ l4 a g+ p4 a f+ l8 g+ a b a l4 g+ p4 a b o3 l8 c+ o2 a a b o3 l4 c+ o2 a p4 o3 d c+ o2 l8 b a g+ f+ l4 g+ p4"
2130 PLAY "f+ f+ l8 e c+ f+ g+ l4 a g+ p4 a a l8 b a g+ g+ f+.":GOTO 5000
2140 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2145
2142 IF G=2 THEN 2150 ELSE 2140
2145 PLAY "t100 mn l4 o2 d f g a o3 d l8 c o2 b a o3 c o2 l4 b a p4 o3 d l8 d d l4 c e l8 d o2 a b- a l4 g f p4 f l8 e e l4 da o3 l8 c c o2 l4 g b- a p8 l8 a o3 l4 c d o2 a o3 c o2 l8 a g l4 f e d.":GOTO 5000
2150 PLAY "t100 mn l4 o2 g l8 a a l4 g. l8 g a b o3 c d l4 c o2 b p8 o3 c l8 o2 b a l4 g g l8 a g f e l4 d c p8 g l8 a b l4 o3 c c l8 d c o2 b a l4 g. p8 o3 c o2 l8 g g a b o3 c d l4 c o2 b o3 c. ":GOTO 5000
2160 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2165
2162 IF G=2 THEN 2170 ELSE 2160
2165 PLAY "l4 mn t100 o2 d l8 d d l4 a o3 c o2 l8 b b l4 a p4 o3 c o2 b a l8 g e l4 f g a p4 a o3 l8 c c l4 d o2 a o3 l8 c c o2 l4 b a p4 l8 e l4 f a l8 g e f g l4 e d.":GOTO 5000
2170 PLAY "t100 o2 l4 d g b o3 e d c o2 b a o3 d o2 g b a g f+ e d d g g f+ g o3 c o2 b l2 b l4 a a b o3 c d o2 g o3 c o2 b l2 a l4 g":GOTO 5000
2180 PLAY "t100 mn l4 o2 a b- g f f l8 e d l4 f g a p4 a l8 a b o3 c o2 b a g l4 f e d p4 o3 d l8 c o2 b a g+ a b o3 l4 c o2 b a p4 a l8 o3 c d c o2 b a g l4 f e d.":GOTO 5000
2200 PLAY "t100 mn l4 o2 a l8 g f l4 e e l8 f g a b- l4 a g p4 a l8 d e l4 f g l8 a g f g l4 e p4 f l8 e e l4 a f l8 g a b-a l4 g a p4 o3 d l8 c o2 b- a f g a l4 f e d.":GOTO 5000
2220 PLAY "t100 mn l4 o2 e l8 b a l4 g f+ e p4 g l8g f+ l4 g a b p4 b l8 b o3 d l4 c o2 b l8 a a l4 g p4 a l8 b e l4 g f+ e.":GOTO 5000
2240 PLAY "t100 mn l4 o2 d g a l8 b b o3 c o2 b l4 a p4 o3 d l8 c o2 b a o3 c o2 l4 b a g p4 a l8 g f+ e f+ g f+ l4 e d p4 g l8 g a l4 b b o3 l8 d c o2 l4 b a p4 o3 c o2 l8 b a l4 g a l8 g f+ g.":GOTO 5000
2260 PLAY "t100 l4 o2 mn a l8 a a d e l4 f g e a l8 g g a b o3 l4 c o2 b a p4 a l8 a a l4 d a l8 g f l4 e e l8 f g a d l4 f e d.":GOTO 5000
2280 PLAY "t100 l4 mn o2 f l8 f f l4 g b- l8 a a l4 g p4 g l8 a b- o3 l4 c o2 g l8 b- b- l4 a g p4 g l8 g g l4 f f l8 g a l4 b- p4 o3 d l8 c o2 b- l4 a g l8 b- b- l4 a p4 o3 c o2 l8 b- g l4 a g l8 a b- l4 g f.":GOTO 5000
2300 PLAY "t100 l4 mn o3 d c+ o2 b l8 a f+ a a g g l4 f+ p4 f+ l8 e d l4 a a l8 b b o3 c+ o2 a o3 l4 d c+ p4 d l8 d d o2 l4 a b l8 a g l4 f+ e f+ p4 d e g l8 f+ b b a b o3 d l4 c+ o2 b p4 f+ l8 g e l4 d a l8 g f+ g a l4 b a p4"
2310 PLAY "f+ a b l8 f+ a b o3 d c+ o2 b l4 a+ b.":GOTO 5000
2320 PLAY "t100 l4 o2 mn g f+ a l8 b a g e l4 f+ p4 d l8 a a l4 b b o3 l8 d d l4 c+ o2 b p4 f+ l8 b b l4 a o3 d l8 d c+ o2 l4 b a p4 f+ l8 e d l4 e f+ l8 g g l4 f+ p4 a b o3 d o2 l8 b g a a l4 g f+ p4 f+ l8 e d g g l4 f+ e d p4"
2330 PLAY "d l8 e g f+ d l4 e e f+ p4 a b a l8 g e f+ a l4 g f+.":GOTO 5000
2340 PLAY "t100 l4 o2 mn f e d l8 a g l4 f e d p4 a l8 o3 c c o2 b a o3 l4 c o2 ba p4 f e d l8 a g l4 f e d p4 a l8 o3 c c o2 b a o3 l4 c o2 ba p4 a l8 b o3 c d o2 g a a l4 g p4 e l8 f g a e f f l4 e p4"
2350 PLAY "a l8 b o3 c o2 a f l4 a g f p4 a l8 a a g f l4 g e d.":GOTO 5000
2360 PLAY "t100 mn l4 o2 d l8 a a l4 b a g p4 g l8 a b o3 l4 c o2 b a p4 e l8 g g l4 f+ e d p4 d l8 a a l4 b a g p4 g l8 a b o3 l4 c o2 b a p4 e l8 g g l4 f+ e d p4 o3 d l8 d d l4 c o2 b a p4 g l8 a b o3 l4 c o2 b a p4"
2370 PLAY "b l8 a g f+ g l4 e d p4 o3 d o2 l8 g a l4 o3 c o2 b a p4 g l8 f+ e l4 f+ g+ a p4 a l8 b a g f+ l4 e d.":GOTO 5000
2380 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2385
2382 IF G=2 THEN 2390 ELSE 2380
2385 PLAY "T100 mn o2 l4 b l8 b a l4 b o3 d l8 c+ c+ o2 l4 b g l8 b a l4 g f+ e p4 e l8 b b l4 a b o3 l8 d c+ o2 l4 b g l8 a b l4 g f+ e.":GOTO 5000
2390 PLAY "T100 mn o2 l4 e- l8 b- a- g f l4 e- d e- g l8 f b- o3 l4 c o2 a b- b- o3 l8 e- c o2 b- o3 c o2 l4 b- a- g b- l8 b- g l4 a- b- e-":GOTO 5000
2400 PLAY "t100 mn l4 o2 a l8 a g l4 f+ b l8 a g l4 f+ p4 d l8 f+ g a a l4 b a p4 a l8 f+ f+ d f+ l4 e d p4 a l8 b b o3 l4 c+ o2 a l8 b o3 c+ l4 d p4 o2 a l8 b b l4 a g f+ d l8 e g l4 f+ e d.":GOTO 5000
2420 PLAY "t100 l4 mn o2 g e a l8 g g o3 c c o2 l4 b a g p4 d l8 g a l4 b b l8 a g l4 e e f+ p4 g l8 a b e b o3 c o2 b l4 a a g p4 b l8 a g l4 f+ e p4 b l8 b b l4 a d l8 g a b o3 c o2 l4 b a p4 b l8 b b l4 e f+ l8 g a b a l4 g f+ p4"
2430 PLAY "d l8 e f+ g g a b o3 c o2 b l4 a g p4 b a f+ e.":GOTO 5000
2440 LOCATE 8,8:PRINT "Altwee berymings se wysies is dieselfde."
2445 PLAY "t100 mn l8 o2 g l4 e a g o3 c o2 l8 b a g a l4 e l8 d g l4 o3 c c o2 b g l8 o3 c d o2 b a l4 b a g. l8 g l4 e a g o3 c o2 l8 b a g a l4 e l8 d g l4 o3 c c o2 b g l8 o3 c d o2 b a l4 b a g. 
2450 PLAY "l8 g o3 l4 e o2 b o3 c o2 a l8 f g e c l4 e l8 d g o3 l4 e o2 b o3 c o2 a l8 f g e o3 c l4 ml c mn o2 b o3 c.":GOTO 5000
2460 PLAY "t100 mn l4 o2 e b g l8 a a l4 g f+ e p4 e l8 g a l4 b o3 d l8 c+ c+ o2 l4 b p4 b o3 l8 c o2 b a g f+ e l4 f+ e p4 g l8 g g l4 a g l8 f+ e l4 d p4 e l8 b b g b l4 a g f+ p4 b l8 a g f+ e g a l4 f+ e.":GOTO 5000
2480 PLAY "t100 o2 mn l4 g l8 f+ e d l4 g l8 a l4 b g p4 b b o3 l8 c o2 b l4 g a g p4 g l8 f+ e d l4 g l8 a l4 b g p4 b b o3 l8 c o2 b l4 g a g p4 g a l8 b g o3 c c l4 o2 b p4 g l8 e l4 f+ l8 d e g l4 f+ g p4"
2490 PLAY "b b l8 g o3 c o2 b a l4 g p4 b o3 d l8 c o2 b a g l4 a g p4":GOTO 5000
2500 PLAY "t100 l4 mn o2 e a g l8 f f l4 e p4 a l8 g e f g l4 a p4 o3 c l8 o2 b a l4 g e l8 f d l4 e p4 g l8 a b o3 c o2 b l4 a g p4 o3 c l8 o2b o3 d o2 a o3 c o2 l4 b a p4 a o3 c o2 b l8 a g f f e.":GOTO 5000
2520 PLAY "t100 mn l4 o2 a f+ g l8 a a o3 d d c+ d o2 l4 b a p4 e f+ a l8 g e a g l4 f+ e d p4 a f+ g l8 a a o3 d d c+ d o2 l4 b a p4 e f+ a l8 g e a g l4 f+ e d p4 o3 d l8 c o2 b a a o3 c c o2 l4 b o3 c+ d p4"
2530 PLAY "o2 g l8 A b o3 c c o2 b A A g+ l4 A p4 A l8 o3 c c o2 b g A e l4 g f+ e p4 o3 c o2 b A l8 b b A g l4 f+ e d.":GOTO 5000
2540 PLAY "t100 l4 o2 mn e l8 g e d e g f+ l4 e p4 g l8 A A b g A A l4 b p4 b l8 o3 c o2 b o3 d o2 b A A l4 g p4 b l8 g A g f+ e d+ l4 e.":GOTO 5000
2560 PLAY "t100 mn l4 o2 A b l8 A b o3 l4 c+ d o2 A p4 o3 d c+ o2 l8 b o3 d c+ o2 b l4 A p4 A l8 f +l4 b l8 A g f+ l4 e p4 f+ l8 g l4 A l8 g f+ e l4 d p4 d A l8 A f+ g A l4 b A p4 d A l8 A f+ g A l4 b A p4"
2570 PLAY "A b l8 o3 d c+ o2 b A l4 g f+ p4 d l8 f+ l4 A l8 e g f+ l4 e d.":GOTO 5000
2580 PLAY "t100 l4 mn o2 g f e l8 d d e f+ l4 g p8 o3 c l8 c o2 A l4 o3 c o2 b l8 A A l4 g p4 g e f l8 g g l4 A o3 c o2 b p4 g o3 l8 c o2 b o3 c d l4 c o2 b o3 c p4 c o2 b A l8 g A g f l4 e d p4"
2590 PLAY "g A b o3 l8 c o2 b A g l4 f+ g.":GOTO 5000
2600 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2605
2602 IF G=2 THEN 2610 ELSE 2600
2605 PLAY "t100 l4 o2 mn d f+ a l8 a f+ g a l4 b a p4 f+ l8 a g l4 f+e f+ p4 a l8 g f+ e e l4 d p4 a l8 b o3 c+ d o2 a o3 d c+ o2 l4 b a p4 a l8 b o3 c+ o3 d o2 a l4 b a p4 f+ l8 a e f+ g l4 e d.":GOTO 5000
2610 PLAY "t100 mn o2 l4 c f e f c a g l2 g l4 f a b- a f g a a g f f e f a o3 c o2 a g f a g l2 f l4 e o3 c e o2 g o3 c o2 b- l2 a l4 g a b- a a g l2 g l4 f ":GOTO 5000
2620 PLAY "t100 l4 o2 mn d f+ g a a l8 a f+ g a l4 b a p4 g l8 f+ f+ l4 e a l8 b a g f+ l4 e d p4 d l8 f+ g l4 a a l8 b a g g l4 f+ p4 a l8 g f+ l4 e f+ l8 g f+ l4 d e d p4 o3 d l8 d d l4 c+ o2 a l8 b o3 c+ d c+ l4 o2 b a p4"
2625 PLAY "a l8 a a l4 b a l8 g b a g l4 f+ e p4 a l8 a a l4 b o3 d l8 c+ o2 a b b l4 a p4 a l8 b b l4 a g l8 f+ e l4 d e d.":GOTO 5000
2640 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2645
2642 IF G=2 THEN 2650 ELSE 2640
2645 PLAY "t100 mn l4 o2 d l8 a a o3 c o2 b a g l4 f e p4 e l8 f g a o3 d l4 c o2 b a p4 d l8 a a o3 c o2 b a g l4 f e p4 e l8 f g a o3 d l4 c o2 b a p4 l8 e e g g l4 f e p4 l8 e e g g l4 f e p4 o3 c l8 o2 b l4 a l8 g+ l4 a p4"
2647 PLAY "o3 l8 d d e d l4 c o2 a p4 e g l8 a b l4 o3 c o2 a p4 l8 a g l4 f e d.":GOTO 5000
2650 PLAY "t100 mn o2 l8 d l4 b a l8 g g a b l4 o3 c o2 l8 a a o3 d o2 f+ g a l4 b a g. l8 g l4 a b o3 l8 c o2 a o3 d c o2 l4 b a o3 d o2 l8 g o3 c o2 b a g f+. l4 g. o3 l8 d o2 b o3 e d l4 c o2 b o3 l8 d o2 b a g l4 g f+"
2655 PLAY "l8 a o3 d o2 f+ e l4 d. l8 g g a b o3 l4 c o2 a o3 l8 d o2 b o3 c d l4 e d l8 d o2 g b a l4 g.":GOTO 5000
2660 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2665
2662 IF G=2 THEN 2670 ELSE 2660
2665 PLAY "t100 mn l4 o2 e l8 g f+ l4 g a b p4 b l8 b b o3 d c+ o2 l4 b a g p4 b l8 a g f+ a l4 g f+ g p4 b l8 g e l4 a f+ e p4 e l8 g f+ e e l4 d p4 g l8 f+ g l4 a b l8 b a+ l4 b p4 b o3 l8 d d l4 c+ o2 b l8 a g l4 f+ p4"
2667 PLAY "a l8 g f+ l4 e d+ e.":GOTO 5000
2670 PLAY "t60 mn l8 o2 a o3 o2 b a g+ l4 a. p16 l8 a ml l16 f+ g+ mn l8 a b o3 c+ d c+ o2 b p16 b o3 c+. o2 l16 g+ l8 a f+ b. l16 f+ g+ p16 e o3 e l16 ml o2 a g+ mn l8 o3 c+ o2 b l4 a. p16 o3 l8 c+ o2 b e a a ml g+ a b mn p16"
2675 PLAY "e a g+ f+ f+ b a g+ p16 g+ o3 c+ o2 b a a o3 d c+ o2 b p16 e o3 e o2 l16 ml a b mn l8 o3 c+ o2 b l4 a.":GOTO 5000
2680 PLAY "t100 mn l4 o2 f l8 f g a g l4 f e f p4 a l8 a g l4 a o3 c o2 l8 b- a l4 g p4 a l8 o3 c c d c o2 b- a l4 g f p4 c l8 d e f a g f l4 e f p4 f g a l8 f g a b o3 l4 c p4 o2 f g a l8 f e d d l4 c p4"
2690 PLAY "o3 c l8 c o2 a l4 f b- l8 a g l4 a p4 a l8 g f l4 e g l8 f e f.":GOTO 5000
2700 PLAY "t100 l4 o2 mn d l8 d e f+ d f+ g+ l4 a p4 a l8 g f+ e d f+ g+ l4 a p4 b l8 a g f+ d l4 e d p4 d l8 d e f+ d f+ g+ l4 a p4 a l8 g f+ e d f+ g+ l4 a p4 b l8 a g f+ d l4 e d p4 o3 d l8 d d c+ o2 a b b l4 a p4"
2710 PLAY "o3 d l8 d d c+ o2 a b b l4 a p4 f+ l8 g b a f+ l4 g f+ p4 e l8 f+ g a g f+ f+ l4 e p4 a l8 b o3 c+ d c+ o2 b b l4 a p4 d l8 e f+ g f+ l4 e d.":GOTO 5000
2720 PLAY "t100 mn l4 o2 a l8 a a d e l4 f g e a l8 g g a b o3 l4 c o2 b a p4 a l8 a a l4 d a l8 g f l4 e e l8 f g a d l4 f e d.":GOTO 5000
2740 PLAY "t100 mn o2 l4 b- a l8 g a b- o3 c l4 d c p8 d l8 c o2 l4 b- a p4 o3 l8 c c l4 d o2 b- l8 o3 c o2 a l4 g p4 o3 d d l8 c o2 b- g a l4b- a p8 b- l8 a l4 g f p4 b- o3 d l8 c o2 b- l4 g a g.":GOTO 5000
2760 PLAY "t100 l4 mn o2 e l8 e f l4 g a l8 g o3 c c o2 b l4 o3 c p4 c l8 c c o2 b a l4 g f+ e p4 e l8 b b l4b g l8 o3 c o2 b a a l4 g p4 e l8 g a o3 c o2 b l4 a g+ a p4 e l8 f f l4 e d l8 g g l4 a b o3 c p4"
2770 PLAY "c l8 o2 b a o3 l4 d c o2 l8 b b a.":GOTO 5000
2780 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2785
2782 IF G=2 THEN 2790 ELSE 2780
2785 PLAY "t100 o2 mn l4 d l8 d e l4 f+ a l8 g f+ e e l4 f+ p4 d l8 f+ g l4 a a l8 b b l4 a p4 d l8 f+ g l4 a a l8 b b l4 a p4 f+ l8 f+ f+ l4 g f+ l8 d f+ e e l4 d p4 o3 d l8 c+ o2 b a b l4 g f+ p4 o3 d l8 c+ o2 b a b l4 g f+ p4"
2787 PLAY "b l8 a g f+ f+ l4 e p4 a l8 b o3 c+ l4 d o2 b a p4 a l8 b o3 c+ l4 d o2 b a p4 d l8 e g l4 f+ e d.":GOTO 5000
2790 PLAY "t100 mn o2 l4 g l8 e f g o3 c e d l4 c o2 b o3 c p8 e l8 d o2 b o3 c o2 a l4 g f+ g p8 g l8 e f g o3 c l4 e d c p8 o2 a l8 b o3 c d o2 g o3 c o2 b l4 a a g p8 g l8 d e f f l4 f e p8 a l8 e f g g l4 g f p8"
2795 PLAY "a l8 o3 d c l4 c o2 b o3 c p8 c l8 o2 g a b- b- l4 a p8 o3 d l8 o2 a b o3 c c l4 o2 b p8 o3 c l8 a o3 d l4 c o2 b o3 c":GOTO 5000
2800 PLAY "t100 mn l4 o2 f l8 g a l4 f a l8 o3 c c o2 b- a l4 g p4 a l8 o3 c o2 b- l4 a g a p4 f l8 g a l4 f a l8 o3 c c o2 b- a l4 g p4 a l8 f b- l4 a g f p4 o3 c l8 o2 b- a l4 g a l8 g f f e l4 f p4 a l8 g f l4 e d c p4"
2810 PLAY "f l8 e f l4 g a l8 b- a g f l4 e p4 f l8 g b- l4 a g f.":GOTO 5000
2820 PLAY "o2 l4 mn t100 f l8 g l4 a l8 g f e l4 d c p4 f l8 g l4 a l8 b- l4 a g f p4 f l8 g l4 a l8 g f e l4 d c p4 f l8 g l4 a l8 b- l4 a g f p4 a l8 a l4 o3 c l8 o2 b- a g l4 a p4 o3 c l8 c l4 d l8 c o2 b- a l4 g p4"
2830 PLAY "a l8 o3 c l4 o2 b- l8 a f g l4 a f p4 a l8 a l4 b- l8 a g f l4 e f.":GOTO 5000
2840 PLAY "t100 o2 l4 mn g a a l8 g g o3 c c o2 l4 b a p8 l8 g l4 a o3 c o2 l8 b a l4 g f+ g p4 b l8 b b a a b o3 d l4 c+ d p4 o2 a l8 a a l4 b a l8 g f+ l4 e d p4 b o3 l8 d c o2 b a l4 b o3 c o2 l8 b"
2850 PLAY "b a g g f+ g.":GOTO 5000 
2860 PLAY "t100 mn l4 o2 d l8 e f+ g g a g l4 f+ e p4 d l8 g f+ g a b a l4 g f+ p4 b l8 b b a g l4 f+ g a p4 b l8 o3 d c o2 b a l4 g f+ g p4 b l8 o3 d c o2 b a l4 g f+ g p4 a b o3 c o2 l8 b a g f+ l4 e d p4"
2870 PLAY "d l8 e f+ g a l4 b o3 c o2 b p4 o3 d l8 o2 g a b b o3 c o2 b l4 a g.":GOTO 5000
2880 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 2885
2882 IF G=2 THEN 2890 ELSE 2880
2885 PLAY "t100 mn l4 o2 f a l8 o3 c o2 l4 a l8 g a b o3 l4 c p8 d c o2 l8 a l4 g l8 f f e l4 f p8 g a l8 a l4 g o3 l8 c c o2 b o3 l4 c p8 o2 a b- o2 l8 a l4 g l8 f f e l4 f p8":GOTO 5000
2890 PLAY "T100 mn o2 l4 g l8 e a g g a b o3 l4 c c l8 d c o2 b a l4 g f+ g e l8 a g a b o3 l4 c c o2 b b o3 l8 c o2 a g o3 c l4 e d c":GOTO 5000
2900 PLAY "t100 mn l8 o3 d d d o2a b o3 d o2 b l4 a p8 l8 o3 d c+ o2 b a b g e l4 d p8 l8 o3 d d d o2a b o3 d o2 b l4 a p8 l8 o3 d c+ o2 b a b g e l4 d p8 l8 d a b a g+ l4 a p4 l8 d a a b o3 c+ l4 d p4 l8 c+ d c+ o2 b b l4 a p4"
2910 PLAY "l8 b b a b g l4 f+ p4 l8 o3 d c+ o2 b a b g e l4 d p8":GOTO 5000
2920 PLAY "t100 l4 o2 mn a f+ l8 d e f+ l4 g l8 f+ e e l4 d p4 a a l8 b o3 c+ d o2 l4 a l8 b g a l4 f+ p4 d e l8 f+ g f+ l4 e l8 d d c+ l4 d p4 a b l8 a g f+ l4 f+ l8 e f+ g+ l4 a p4 o3 d c+ l8 o2 b b a l4 b l8 a g f+ l4 e p4"
2930 PLAY "d f+ l8 a g f+ l4 e l8 d d c+ d.":GOTO 5000
2940 PLAY "t120 mn l4 o2 e- b- b- g e- f f e- p16 e- o3 c c o2 b- g e- g f p16 f b- a- g e- f f l2 e- l4 e- p16 e- o3 c c o2 b- a- g f l2 e- l4 e- p16 o1 b- o2 c c d d ml e- mn f g p16 f g g a a l2 b- l4 b- p16"
2950 PLAY "b- o3 c c o2 f f b- b- e- p16 b- o3 c c c e- d c o2 b- p16 b- g f e- b- g f e- p16 e f f b- a- g f e-":GOTO 5000
2960 PLAY "t100 mn l4 o2 g g a l8 b b o3 c o2 b l4 a a g p4 o3 d l8 d c l4 o2 b o3 d l8 c o2 b b a l4 b p4 b l8 b b l4 a b l8 o3 c o2 b a g l4 f+ p4 g l8 e a l4 g b l8 a g g f+ l4 g p4 d l8 e f+ l4 g b l8 o3 d d c c l4 o2 b p4"
2970 PLAY "o3 d l8 o2 g a l4 b o3 d l8 c o2 b b a l4 b p4 g l8 g a l4 b o3 c l8 o2 b a g f+ l4 e d p4 o3 d l8 c o2 b l4 a o3 d l8 c o2 b a g l4 a g.":GOTO 5000
2980 PLAY "t100 mn l4 o2 a o3 l8 d d c+ o2 a b o3 c+ l4 d p4 d l8 c+ o2 b a g l4 f+ e d p4 d l8 e f+ g a a g+ l4 a p4 a l8 b b o3 d d l4 c+ o2 b a p4 a l8 g f+ b a l4 g f+ e p4 e l8 f+ g a b l4 a g+ a.":GOTO 5000 
3000 PLAY "t100 o2 mn l4 e l8 g g l4 a e l8 g g l4 d f e p4 e l8 e d l4 c e l8 e f g a l4 g+ a p4 a l8 f f l4 e c l8 d e f e l4 d c p4 e l8 g a l4 b g l8 a o3 c o2 b b l4 a p4 a l8 a a l4 g e l8 a a l4 g f e p4"
3010 PLAY "a l8 a b l4 o3 c o2 a l8 g e f g l4 a e p4 e l8 f d l4 e c l8 d e l4 f d e p4 a l8 g g l4 a o3 c l8 c o2 b a g l4 f e.":GOTO 5000
3020 PLAY "t100 mn l4 o2 a l8 a b o3 c+ d d c+ o2 l4 b a f+ l8 b b a g+ l4 a p4 f+ g a l8 f+ d g f+ l4 e d a l8 g f+ d e l4 f+ p4 f+ l8 d e f+ a l4 g f+ l8 e a g f+ l4 d e d.":GOTO 5000
3040 PLAY "t100 l4 o2 mn a l8 a a d e l4 f g e a l8 g g a b o3 l4 c o2 b a p4 a l8 a a l4 d a l8 g f l4 e e l8 f g a d l4 f e d.":GOTO 5000
3060 PLAY "t100 mn l4 o2 b l8 g g l4 a b o3 l8 c c l4 o2 b p4 b l8 b a b o3 d c o2 b l4 a b p4 b l8 b a l4 g e l8 f+ g l4 e d p4 d e f+ l8 d g g f+ l4 g p4 o3 d o2 l8 g a b g o3 c c o2 l4 b p4 o3 d l8 c o2 b a o3 d c o2 b l4 a b p4"
3070 PLAY "b l8 o3 c o2 b g b a g l4 f+ g p4 o3 d l8 c o2 b a g l4 b a g.":GOTO 5000
3080 PLAY "t100 mn l4 o2 f+ l8 a a l4 b a o3 l8 d c+ o2 l4 b a p4 a l8 b b l4 a f+ l8 g f+ l4 e d p4 a l8 b b l4 a o3 d l8 c+ d o2 l4 b a p4 b l8 a g l4 f+ b l8 a g l4 f+ p4 a l8 b o3 c+ l4 d o2 a l8 b o3 c+ l4 d p4"
3090 PLAY "d l8 c+ o2 b l4 o3 c+ o2 a l8 b o3 d l4 c+ o2 b.":GOTO 5000
3100 PLAY "t100 mn o2 l4 f l8 d c l4 f l8 f l4 g l8 a l4 b- a g p4 f l8 g g l4 a o3 c l8 c o2 b- l4 g b- a p4 g l8 a g l4 e e l8 f e d d l4 c o3 c l8 o2 b- a g f l4 g f p4 f l8 b- b- l4 a g l8 a b- o3 c c l4 d c p4"
3110 PLAY "c l8 o2 f g l4 a f l8 g a b- a l4 g a p4 a l8 a a l4 g a l8 b- a g f l4 e c o3 c l8 o2 b- a g g f.":GOTO 5000
3120 PLAY "t100 mn l4 o2 a o3 c o2 b a o3 d l8 c o2 a b b l4 a p4 a l8 b o3 c l4 d d l8 c o2 a b b l4 a p4 a o3 l8 c c o2 l4 b a l8 g b a g l4 f+ e p4 g l8 a b l4 o3 c c l8 o2 b a a g+ l4 a p4 a o3 l8 c o2 b l4 a e l8 f+ a g f+ l4 e"
3130 PLAY "d.":GOTO 5000
3140 PLAY "t100 mn l4 o2 a l8 b b l4 a o3 d l8 c+ d o2 l4 b a p4 a l8 f+ f+ d g l4 f+ e d p4 d l8 f+ g l4 a a l8 b o3 c+ l4 d p4 d l8 c+ o2 b a b a g l4 f+ e p4 e l8 f+ g l4 a f+ l8 b b l4 a p4 "
3150 PLAY "o3 d l8 c+ o2 b a o3 d l4 c+ o2 b a.":GOTO 5000
3160 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 3165
3162 IF G=2 THEN 3170 ELSE 3160
3165 PLAY "t100 mn l4 o2 g l8 e d l4 g g l8 a a l4 b p4 b l8 b b a o3 c o2 l4 b a g p4 b l8 o3 d c+ l4 o2 b g l8 a a l4 b p4 o3 d l8 d c o2 b a l4 b o3 c+ d p4 b l8 b b o3 c o2 b l4 a g f+ p4"
3167 PLAY "o3 d l8 c o2 b a o3 c o2 l4 b a g":GOTO 5000
3170 PLAY "T100 mn l4 o2 a g+ f+ l2 e l4 a l2 a l4 b o3 l2 c+ o2 l4 a l2 o3 d l4 c+ l2 o2 f+ l4 a ml a o3 c+ mn o2 b l2 a. l4 b b o3 c+ l2 d o2 l4 b o3 ml c+ o2 a mn o3 c+ o2 l2 a g+ l2 g+ l4 g+ ml g+ f+ mn e l2 e l4 d+ l2 o2 e"
3175 PLAY "l4 e l2 a l4 f+ l2 e l4 a l2 a l4 b l2 a l4 a o3 l2 d l4 c+ o2 l2 f+ l4 a ml a o3 c+ mn o2 b l2 a.":GOTO 5000
3180 PLAY "T100 mn l4 o2 g l8 o3 c c o2 b a l4 g f+ g p4 g l8 g o3 c o2 b a l4 b o3 c+ d p4 o2 b o3 l8 o3 d d c o2 b l4 a g f+ p4 d l8 e f+ g a l4 g f+ g p4 b l8 b a l4 g o3 c o2 l8 b a l4 b p4 b l8 b a l4 g o3 c o2 l8 b a l4 b p4"
3190 PLAY "b o3 l8 d c l4 o2 b a l8 g e l4 d p4 g l8 a b l4 g o3 c o2 l8 b a l4 g.":GOTO 5000
3200 PLAY "t100 l4 mn o2 e l8 a a g l4 o3 c o2 l8 b l4 a l8 g o3 l4 c l8 o2 b l4 a g p4 e g l8 e a a g+ l4 a p4 a e l8 a a g g l4 f+ l8 e l4 a l8 b l4 o3 c o2 b p4 b o3 d l8 c o2 b l4 a g+ a.":GOTO 5000
3220 PLAY "t100 mn l4 o2 e b g l8 a a l4 g f+ e p4 e l8 g a l4 b o3 d l8 c+ c+ o2 l4 b p4 b o3 l8 c o2 b a g f+ e l4 f+ e p4 g l8 g g l4 a g l8 f+ e l4 d p4 e l8 b b g b l4 a g f+ p4 b l8 a g f+ e g a l4 f+ e.":GOTO 5000
3240 PLAY "t100 mn l4 o2 d f+ g a f+ l8 e e l4 d p4 o3 d l8 c+ o2 b a f+ l4 b b a p4 o3 d l8 c+ o2 b a f+ l4 g g f+ p4 a l8 b a l4 g f+ l8 e e l4 d p4 a b o3 d l8 d c+ l4 o2 b b a p4 a b o3 d l8 d c+ l4 o2 b b a p4"
3250 PLAY "f+ l8 g b a f+ g g l4 f+ p4 a l8 b a l4 g f+ l8 e e l4 d.":GOTO 5000
3260 PLAY "t100 mn l4 o2 e l8 g e d e g f+ l4 e p4 g l8 a a b g a a l4 b p4 b o3 l8 c o2 b o3 d o2 b a a l4 g p4 b l8 g a g f+ e d+ l4 e.":GOTO 5000
3280 PLAY "t100 mn l4 o2 b l8 b b l4 e b l8 o3 c o2 b l4 a g p4 a l8 b a l4 g f+ e p4 b l8 b b l4 e b l8 o3 c o2 b l4 a g p4 a l8 b a l4 g f+ e p4 e l8 g g f+ f+ g a l4 b a p4 b o3 l8 c o2 b a a l4 g p4"
3290 PLAY "b l8 a g f+ d e f+ l4 g f+ p4 g l8 a g f+ f+ l4 e.":GOTO 5000
3300 PLAY "t100 mn o2 l4 g l8 e d g g a o3 c o2 l4 b a p4 o3 d l8 o2 b b g o3 c o2 l4 b a g p4 g l8 e d g g a o3 c o2 l4 b a p4 o3 d l8 c o2 b a gg f+ l4 g p4 o3 d l8 d c o2 l4 b a l8 g f+ l4 e d p4 d l8 g g f+ e l4 g a b p4"
3310 PLAY "g l8 g a b g o3 c c l4 o2 b a p4 o3 d l8 c o2 b a g g f+ l4 g.":GOTO 5000
3320 PLAY "t100 mn l4 o2 d l8 a a o3 c o2 b a g l4 f e p4 e l8 f g a o3 d l4 c o2 b a p4 d l8 a a o3 c o2 b a g l4 f e p4 e l8 f g a o3 d l4 c o2 b a p4 l8 e e g g l4 f e p4 l8 e e g g l4 f e p4 o3 c o2 l8 b l4 a l8 g+ l4 a p4"
3330 PLAY "o3l8 d d e d l4 c o2 a p4 e g l8 a b l4 o3 c o2 a p4 l8 a g l4 f e l4 d.":GOTO 5000
3340 PLAY "t100 l4 o2 mn d l8 d e f+ d f+ g+ l4 a p4 a l8 g f+ e d f+ g+ l4 a p4 b l8 a g f+ d l4 e d p4 d l8 d e f+ d f+ g+ l4 a p4 a l8 g f+ e d f+ g+ l4 a p4 b l8 a g f+ d l4 e d p4 o3 d l8 d d c+ o2 a b b l4 a p4"
3350 PLAY "o3 d l8 d d c+ o2 a b b l4 a p4 f+ l8 g b a f+ l4 g f+ p4 e l8 f+ g a g f+ f+ l4 e p4 a l8 b o3 c+ d c+ o2 b b l4 a p4 d l8 e f+ g f+ l4 e d.":GOTO 5000
3360 PLAY "t100 mn l4 o2 g l8 g e- f g e- d l4 c p4 g l8 g f b- g e- f l4 g p4 g l8 b- o3 c e- d c o2 b o3 l4 c p4 c l8 d c o2 b- a g f+ l4 g p4 o3 c o2 l8 b- a b- g g f l4 e- p4 g l8 a- g e- f e- d l4 c.":GOTO 5000
3380 PLAY "t100 mn l4 o2 d f+ g a f+ l8 e e l4 d p4 o3 d l8 c+ o2 b a f+ l4 b b a p4 o3 d l8 c+ o2 b a f+ l4 g g f+ p4 a l8 b a l4 g f+ l8 e e l4 d p4 a b o3 d l8 d c+l4 o2 b b a p4 a b o3 d l8 d c+l4 o2 b b a p4"
3390 PLAY "f+ l8 g b a f+ g g l4 f+ p4 a l8 b a l4 g f+ l8 e e l4 d.":GOTO 5000
3400 PLAY "t100 mn l4 o2 d f+ a l8 a f+ g a l4 b a p4 f+ l8 a g l4 f+ e f+ p4 a l8 g f+ e e l4 d p4 a l8 b o3 c+ d o2 a o3 d c+ o2 l4 b a p4 a l8 b o3 c+ d o2 a l4 b a p4 f+ l8 a e f+ g l4 e d.":GOTO 5000
3420 PLAY "t100 mn l4 o2 b l8 b b l4 e b l8 o3 c o2 b l4 a g p4 a l8 b a l4 g f+ e p4 b l8 b b l4 e b l8 o3 c o2 b l4 a g p4 a l8 b a l4 g f+ e p4 e l8 g g f+ f+ g a l4 b a p4 b o3 l8 c o2 b a a l4 g p4"
3430 PLAY "b l8 a g f+ d e f+ l4 g f+ p4 g l8 a g f+ f+ l4 e.":GOTO 5000
3440 PLAY "t100 mn l4 o2 a l8 b o3 d c+ o2 b l4 a g+ a p4 o3 d l8 c+ o2 b o3 l4 d o2 a l8 b o3 c+ l4 d p4 o2 a l8 b b l4 a f+ l8 g a l4 b a p4 f+ l8 a a l4 f+ d l8 g f+ l4 e d p4 a l8 b o3 c+ d o2 b l4 a g f+ p4"
3450 PLAY "f+ l8 g a l4 b a l8 g f+ l4 e p4 a l8 b o3 c+ o2 a o3 d l4 c+ o2 b a p4 f+ l8 g a e g l4 f+ e d.":GOTO 5000
3460 PLAY "t100 o3 l4 mn c o2 l8 a b- o3 l4 c c l8 o2 a b- g g l4 f p4 f l8 a b- o3 l4 c o2 b- l8 a g a o3 c o2 l4 b o3 c p4 c l8 o2 b- a l4 g o3 c o2 l8 b- a b- o3 c l4 d c p4 o2 g l8 a b- l4 o3 c o2 g l8 a b- g g l4 f.":GOTO 5000
3480 PLAY "t100 mn l4 o3 c o2 b- l8 a o3 d l4 c o2 b- a p4 o3 l8 d c l4 o2 b- a l8 g g l4 f p4 f a l8 g o3 c c o2 b o2 l4 o3 c p4 l8 d d c o2 a b- b- l4 a p4 o3 c o2 b- l8 a o3 c o2 b- a l4 g p4 a o3 c o2 l8 f b- l4 a g f.":GOTO 5000
3500 PLAY "t100 mn l4 o2 a o3 d d l8 c+ c+ l4 d o2 b a p4 b l8 a g f+ a l4 g e d p4 d a a l8 b a b o3 c+ l4 d p4 d l8 c+o2 b a f+ l4 g g f+ p4 a b o3 d l8 c+ o2 b o3 c+ d o2 l4 b a p4 b a g l8 f+ a g f+ l4 e d.":GOTO 5000
3520 PLAY "t100 l4 o2 mn g g l8 f+ g a b- l4 a g p4 g g l8 f+ g a b- l4 a g p4 b- a l8 g b- a g l4 f p4 f e l8 d g g f+ l4 g p4 b- o3 c l8 d d c o2 b- o3 l4 c o2 b- p4 b- o3 c l8 d d c o2 b- o3 l4 c o2 b- p4 b- a l8 g b- a g l4 f p4"
3530 PLAY "f e l8 d g g f+ l4 g.":GOTO 5000
3540 PLAY "t100 mn l4 o2 g l8 g a l4 b o3 d l8 c o2 b g a l4 b p4 b l8 b b l4 a b o3 l8 c o2 a l4 g f+ g p4 d l8 g a l4 b o3 c o2 l8 b o3 d d c+ l4d p4 d l8 c o2 b l4 a g l8 a o3 c o2 l4 b a b p4 o3 c o2 l8 b a l4 g gl8 f+ g e e l4 d"
3550 PLAY "p4 d e f+ l8 g b a g l4 g f+ g.":GOTO 5000
3560 PLAY "t100 l4 mn o2 d g a b b l8 b a o3 c o2 b l4 a p4 d l8 g a l4 b o3 c o2 l8 b a b o3 c+ l4 d p4 o2 b l8 a a l4 g g l8 g f+ g e l4 d p4 g l8 g f+ l4 g d l8 e g l4 a a g.":GOTO 5000
3580 PLAY "t100 mn l4 o2 a o3 d d l8 c o2 a o3 c c o2 l4 b a p4 a l8 f f b- a g f l4 e d p4 d l8 f g l4 a a l8 g g l4 a p4 d l8 f g l4 a a l8 g g l4 a p4 a l8 o3 c c d o2 a l4 o3 c o2 b a p4 a l8 b- a g f l4 g e d.":GOTO 5000
3600 PLAY "t100 mn l4 o2 a a l8 b o3 c+ l4 d d c+ l8 o2 b a l4 b a p4 a a l8 b o3 c+ l4 d o2 l8 a b l4 a g f+ p4 l8 a b l4 a g f+ l8 b a g f+ l4 e d.":GOTO 5000
3620 PLAY "t100 mn l8 o3 d d d o2 a b o3 d o2 b l4 a p8 l8 o3 d c+ o2 b a b g e l4 d p8 l8 o3 d d d o2 a b o3 d o2 b l4 a p4 l8 o3 d c+ o2 b a b g e l4 d p8 l8 d a b a g+ l4 a p8 l8 d a a b o3 c+ l4 d p8 l8 c+ d c+ o2 b b l4 a p8"
3630 PLAY "l8 b b a b g l4 f+ p8 l8 o3 d c+ o2 b a b g e l4 d p8":GOTO 5000
3640 PLAY "t100 l4 o2 mn d a a l8 b o3 c+ l4 d p4 d l8 c+ o2 b a g+ l4 a p4 f+ l8 f+ d g f+ l4 e d p4 a l8 a b a g l4 f+ p4 f+ l8 a a g f+ l4 e p4 f+ l8 g f+ d f+ l4 e d p4 d l8 d e f+ g+ l4 a p4 a l8 b o3 c+ d o2 b l4 a p4"
3650 PLAY "a l8 a a b a l4 g f+ p4 d l8 f+ a a g+ l4 a p4 a l8 b o3 d d c+ l4 d p4 d o2 l8 b a g f+ l4 e d.":GOTO 5000
3660 PLAY "t100 mn l4 o2 d l8 a g f+ e l4 d e f+ p4 f+ l8 b b a f+ l4 a g f+ p4 a l8 g f+ e a g f+ l4 e d p4 d l8 f+ g l4 a b l8 a g+ l4 a p4 d l8 f+ g l4 a b l8 a g+ l4 a p4 o3 d l8 c+ o2 b l4 a f+ l8 g a l4 b a p4"
3670 PLAY "a l8 a g l4 f+ b l8 a g l4 f+ p4 d l8 f+ a e g l4 f+ e d.":GOTO 5000
3680 PLAY "t100 mn l4 o2 d l8 d d l4 e g l8 f+ g l4 b a g p4 g l8 g f+ g a b a l4 g f+ e p4 g l8 e f+ l4 d e l8 f+ a l4 g f+ g p4 d l8 g a l4 b a l8 g f+ e e l4 d p4 d l8 e f+ l4 g d l8 g f+ g a l4 b p4 "
3690 PLAY "b o3 l8 d c o2 l4 b a l8 g o3 c o2 l4 b a g p4 g a b l8 g g o3 c o2 b l4 g a b p4 b l8 a a l4 g e l8 f+ g e e l4 d.":GOTO 5000
3700 PLAY "t100 mn l4 o2 g g l8 f+ g a b- l4 a g p4 g g l8 f+ g a b- l4 a g p4 b- a l8 g b- a g l4 f p4 f e l8 d g g f+ l4 g p4 b- o3 c l8 d d c o2 b- l4 o3 c o2 b- p4 b- o3 c l8 d d c o2 b- l4 o3 c o2 b- p4 b- a l8 g b- a g l4 f p4"
3710 PLAY "f e l8 d g g f+ l4 g.":GOTO 5000
3720 PLAY "t100 mn l4 o2 f d g l8 f f g a b- a l4 g f p4 b- o3 l8 c c l4 d c l8 o2 a b- g g l4 f p4 f g a l8 f f g b- b- a l4 b- p4 b- l8 g f l4 b- a l8 f g a b- l4 g f.":GOTO 5000
3740 PLAY "t100 mn l4 o2 e l8 b a l4 g f+ e p4 g l8 g f+ l4 g a b p4 b l8 b o3 d l4 c o2 b l8 a a l4 g p4 a l8 b e l4 g f+ e.":GOTO 5000
3760 PLAY "T100 l4 o2 mn g l8 e d g a l4 b b l8 o3 d c l4 o2 b a g p4 g l8 e d g a l4 b b l8 o3 d c l4 o2 b a g p4 b l8 b a g o3 c l4 o2 b g l8 a f+ g f+ l4 e d p4 d l8 e g g f+ l4 g d l8 g a b g o3 l4 c o2 b p4"
3770 PLAY "o3 d l8 c o2 b a a l4 g o3 c o2 l8 b a g f+ l4 e d p4 a l8 b a g f+ l4 g a l8 b o3 d c o2 b l4 a g.":GOTO 5000
3780 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 3785
3782 IF G=2 THEN 3790 ELSE 3780
3785 PLAY "t100 mn l4 o2 d g a b b l8 b b a o3 c o2 l4 b a p4 d l8 g a l4 b o3 c o2 l8 b a b o3 c l4 d o2 b p4 b l8 a a l4 g g l8 g f+ g f+ l4 e d p4 g l8 g f+ l4 g d l8 e g a b l4 a g p4 d g f+ g a l8 b a o3 c o2 b l4 a p4"
3788 PLAY "o3 d l8 c o2 b l4 a o3 c o2 l8 b g a a l4 g.":GOTO 5000
3790 PLAY "t100 mn l4 o3 d l8 o2 a b l4 g a l8 f+ d b a l4 g f+ a l8 d e l4 f+ a l8 b o3 c+ d e l4 e c+ o2 b l8 a g l4 f+ d l8 e f+ b a l4 g f+ a l8 a g l4 f+ a l8 b o3 c+ e c+ l4 o2 b a o3 e l8 c+ o2 b l4 a a l8 b o3 e d e l4 c+"
3795 PLAY "o2 b l8 a g f+ f+ f+g l4 f+ e d":GOTO 5000
3800 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 3805
3802 IF G=2 THEN 3810 ELSE 3800
3805 PLAY "t100 mn l4 o2 a l8 g f+ b a l4 g e d p4 o3 d l8 o2 b o3 d c+ o2 a l4 b a p4 a l8 g f+ b a l4 g e d p4 o3 d l8 o2 b o3 d c+ o2 a l4 b a p4 b l8 a g f+ e l4 f+ g a p4 a l8 b o3 d c+ o2 a l4 b a p4"
3807 PLAY "a l8 f+ d e f+ l4 g g f+ p4 a l8 a g f+ g l4 e d.":GOTO 5000
3810 PLAY "t100 mn l4 o2 g l8 b- d e- f l4 g f e- g l8 a b- o3 e- d l4 c o2 b- g l8 b- d e- f l4 g f e- g l8 a b- o3 e- d l4 c o2 b- b- o3 l8 e- d- l4 c d l8 e- d- l4 c c l8 o2 b- o3 c d- o2 b- l4 a- g"
3815 PLAY "o3 c o2 l8 b- o3 e- o2 a- o3 c o2 b-. l16 a- l4 g o3 c l8 o2 b- a- g f l4 ml g f mn e-":GOTO 5000
3820 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 3825
3822 IF G=2 THEN 3830 ELSE 3820
3825 PLAY "T100 mn o3 l4 d l8 d d c+ d o2 l4 b a p4 a l8 b a g g l4 f+ p4 a l8 b a g g l4 f+ p4 d l8 g f+ e d l4 e d p4 a l8 b o3 c+ d o2 a l4 b a p4 f+ l8 b b l4 a g f+ p4 f+ l8 b b l4 a g f+ p4 o3 d o2 l8 b a g f+ l4 e d."
3828 GOTO 5000
3830 PLAY "T100 mn o2 l4 a o3 c+ o2 a b a l2 f+ l4 d f+ e a b g+ a f+ b a g e f+ a o3 d o2 a b a f+. d f+ l2 g l4 e f+ d e f+ a b o3 c+ d d c+ o2 a o3 d o2 b a g+ a f+ b a g f+ l2 e l4 d":GOTO 5000
3840 PLAY "t100 mn l4 o2 a f+ g l8 a a b o3 d d c+ l4 d p4 d o2 b o3 d l8 c+ o2 a b o3 c+ l4 d o2 b a p4 f+ l8 g a l4 b a l8 g e f+ f+ l4 e p4 a l8 a a l4 b o3 d l8 d c+ l4 d o2 b a.":GOTO 5000
3860 PLAY "t100 mn l4 o2 d l8 f+ g l4 a o3 d l8 c+ d l4 o2 b a p4 a l8 o3 d c+ l4 o2 b a l8 f+ d l4 e d p4 g l8 f+ g a b l4 a g+ a p4 a l8 f+ g a b l4 a g+ a p4 a l8 o3 d c+ o2 b a l4 g f+ e p4 a l8 f+ b a g l4 f+ e d.":GOTO 5000
3880 PLAY "t100 mn l4 o2 e b g l8 a a l4 g f+ e p4 e l8 g a l4 b o3 d l8 c+ c+ o2 l4 b p4 b o3 l8 c o2 b a g f+ e l4 f+ e p4 g l8 g g l4 a g l8 f+ e l4 d p4 e l8 b b g b l4 a g f+ p4 b l8 a g f+ e g a l4 f+ e.":GOTO 5000
3900 PLAY "t100 mn l4 o2 a l8 a g l4 f+ b l8 a g l4 f+ p4 d l8 f+ g a a l4 b a p4 a l8 f+ f+ d f+ l4 e d p4 a l8 b b o3 l4 c+ o2 a l8 b o3 c+ l4 d p4 o2 a l8 b b l4 a g f+ d l8 e g l4 f+ e d.":GOTO 5000
3920 PLAY "t100 o2 l4 mn a l8 b o3 d d c+ l4 d p4 d l8 o2 b o3 c+ d o2 b l4 a a l8 o3 d o2 b o3 c+ d o2 l4 ba p4 d l8 f+ a g f+ l4 e d p4 f+ l8 e a a g+ l4 a p4 f+ l8 e a a g+ l4 a p4 a l8 b b l4 a g l8 f+ a b b l4 a g f+ p4"
3930 PLAY "a l8 d g l4 f+ e d.":GOTO 5000
3940 PLAY "t100 o2 l4 mn g l8 e d g g a o3 c o2 l4 b a p4 o3 d o2 l8 b b g o3 c o2 l4 b a g p4 g l8 e d g g a o3 c o2 l4 b a p4 o3 d l8 c o2 b a g g f+ l4 g p4 o3 d l8 d c l4 o2 b a l8 g f+ l4 e d p4 d l8 g g f+ e l4 g a b p4"
3950 PLAY "g l8 g a b g o3 c c o2 l4 b a p4 o3 d l8 c o2 b a g g f+ g.":GOTO 5000
3960 PLAY "t100 mn l4 o2 g d l8 e f+ g l4 b l8 b o3 c o2 a l4 g p4 o3 d d l8 o2 b o3 c+ d o2 l4 b o3 l8 d c c l4 o2 b p4 o3 d c l8 o2 b a g l4 e l8 f+ g e l4 d p4 d g l8 a b o3 l4 c l8 o2 b l4 g l8 a b o3 c l4 o2 a g.":GOTO 5000
3980 PLAY "t100 l4 o2 mn b- f d b- b- o3 c c d o2 b- o3 e- d c o2 b- a g f f g f b- f g o3 c o2 a o3 e- d o2 b- g o3 c o2 b- a b-":GOTO 5000
4000 PLAY "t100 mn l4 o2 d g b l8 a b o3 d c l4 o2 b a g p4 g l8 f+ e d g g a l4 b o3 c o2 b p4 b l8 o3 d c l4 o2 b a l8 g f+ g a l4 b p4 o3 c o2 b a g.":GOTO 5000
4020 PLAY "t100 mn l4 o2 g g l8 f+ g a b- l4 a g p4 g g l8 f+ g a b- l4 a g p4 b- a l8 g b- a g l4 f p4 f e l8 d g g f+ l4 g p4 b- o3 c l8 d d c o2 b- l4 o3 c o2 b- p4 b- o3 c l8 d d c o2 b- l4 o3 c o2 b- p4"
4030 PLAY "b- a l8 g b- a g l4 f p4 f e l8 d g g f+ l4 g.":GOTO 5000
4040 PLAY "t100 mn l4 o2 a l8 b o3 c+ l4 d d l8 c+ o2 b o3 c+ d l4 o2 b a p4 a l8 a g l4 f+ f+ l8 f+ e f+ g l4 e d p8 l8 a l4 b o3 d c+ o2 b l8 o3 c+ d o2 b b l4 a p4 o3 d l8 d c+ l4 o2 b a l8 a b a g l4 f+ e p4"
4050 PLAY "e l8 a a l4 b a l8 a a b o3 c+ l4 dc+ p8 o2 l8 a l4 b o3 d c+ o2 b l8 o3 c+ d o2 b b l4 a.":GOTO 5000
4060 PLAY "t80 mn o3 l8 d c+ o2 b o3 c+ o2 a b o3 c+ d d d o2 a a f+ a g f+ f+ b b a o3 c+ d o2 b a o3 d c+ o2 b a g f+ e d":GOTO 5000
4080 PLAY "t100 mn l4 o2 d l8 f+ g l4 a o3 d l8 c+ d l4 o2 b a p4 a l8 o3 d c+ l4 o2 b a l8 f+ d l4 e d p4 g l8 f+ g a b l4 a g+ a p4 a l8 f+ g a b l4 a g+ a p4 a l8 o3 d c+ o2 b a l4 g f+ e p4"
4090 PLAY "a l8 f+ b a g l4 f+ e d.":GOTO 5000
4100 PLAY "t100 o3 mn l4 d c+ o2 l8 b o3 l4 d l8 c+ o2 b b l4 a p4 a l8 b o3 d c+ o2 a o3 l4 o3 d c+ o2 b p4 b o3 l8 d c+ l4 o2 b a l8 g f+ l4 e d p4 d l8 f+ g l4 a f+ l8 g g l4 f+ p4 f+ l8 a b o3 l4 c+ o2 a o3 l8 d c+ o2 l4 b a p4"
4110 PLAY "a o3 l8 d d o2 b a o3 l4 d c+ o2 b.":GOTO 5000
4120 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 4125
4122 IF G=2 THEN 4130 ELSE 4120
4125 PLAY "t100 mn l4 o2 c c g l8 o3 c c l4 o2 b- g p4 g l8 a- g l4 e- f e- p4 c c g l8 o3 c c l4 o2 b- g p4 e- l8 f g l4 e- d c p4 o3 c l8 o2 g a l4 b- a g p4 o3c l8 d c o2 g b- l4 a g p4 g l8 b- a l4 g f e- p4 e- l8 f g f e- l4 d c."
4128 GOTO 5000
4130 PLAY "t100 mn l4 o2 d f+ a o3 d c+ o2 l2 b l4 a f+ d d e e l2 f+. l4 d f+ a o3 d c+ o2 l2 b l4 a f+ e a b o3 c+ o2 l2 a. l4 a b a g f+ l2 g. l4 e a g f+ d l2 f+ l4 e d f+ a o3 d c+ o2 l2 b. l4 a d e f+ g l2 e l4 d":GOTO 5000
4140 PLAY "t100 mn l4 o2 g l8 o3 c c o2 b a l4 g f+ g p4 g l8 g o3 c o2 b a l4 b o3 c d p4 o2 b o3 l8 o3 d d c o2 b l4 a g f+ p4 d l8 e f+ g a l4 g f+ g p4 b l8 b a l4 g o3 c o2 l8 b a l4 b p4 b l8 b a l4 g o3 c o2 l8 b a l4 b p4"
4150 PLAY "b o3 l8 d c l4 o2 b a l8 g e l4 d p4 g l8 a b l4 g o3 c o2 l8b a l4 g.":GOTO 5000
4160 PLAY "t100 mn l4 o2 g l8 b- b- g o3 c l4 o2 b- a g p4 e- l8 f g l4 f d c p4 g l8 b- b- g o3 c l4 o2 b- a g p4 e- l8 f g l4 f d c p4 o3 c l8 c c d c l4 o2 b- a g p4 o3 c l8 d c l4 o2 b- a g p4 o3 c o2 l8 b- a g e- l4 g f e- p4"
4170 PLAY "e- l8 f g l4 fd c.":GOTO 5000
4180 PLAY "t100 mn l4 o2 a l8 f+ g l4 a a l8 b o3 d c+ o2 a l4 b a p4 b l8 a g l4 f+ a l8 g f+ e e l4 d p4 o3 d l8 d d l4 c+ o2 a l8 b o3 c+ d c+ o2 l4 b a p4 a l8 b a l4 f+ a l8 g f+ e e l4 d.":GOTO 5000
4200 PLAY "t100 mn l4 o2 e b g l8 a a l4 g f+ e p4 e l8 g a l4 b o3 d l8 c+ c+ l4 o2 b p4 b o3 l8 c o2 b a g f+ e l4 f+ e p4 g l8 g g l4 a g l8 f+ e l4 d p4 e l8 b b g b l4 a g f+ p4 b l8 a g f+ e g a l4 f+ e.":GOTO 5000
4220 PLAY "t100 mn l4 o2 d l8 a a l4 g f e p4 g l8 a d l4 f e d p4 f l8 g a b- a l4 f g a p4 a l8 b- g l4 f e d.":GOTO 5000
4240 PLAY "t100 mn l4 o2 a b o3 c+ l8 o2 a o3 d d c+ l4 d p4 d o2 b o3 c+ o2 l8 a o3 d d c+ l4 d p4 o2 a l8 b g l4 a f+ l8 g f+ l4 e d p4 d l8 f+ g a f+ l4 b b a p4 a l8 a g f+ f+ l4 b b a p4"
4250 PLAY "o3 d l8 c+ o2 b l4 a o3 d l8 c+ d l4 o2 b a.":GOTO 5000
4260 PLAY "t100 o2 mn l4 a l8 a a f+ a l4 g f+ e p4 a l8 b o3 d l4 c+ o2 b a p4 a l8 a a f+ a l4 g f+ e p4 a l8 b o3 d l4 c+ o2 b a p4 a l8 o3 d c+ o2b a l4 g f+ e p4 f+ l8 g f+ l4 e e d.":GOTO 5000
4280 PLAY "t100 mn o3 l4 d o2 b l8 a a b o3 c+ l4 d p4 o2 a b l8 a g l4 f+ e d p4 d f+ l8 e d f+ g+ l4 a p4 f+ a l8 b o3 c+ d o2 b l4 a p4 a g l8 f+ a g f+ l4 e p4 e f+ l8 a g l4 f+ e d.":GOTO 5000
4300 PLAY "t100 mn l4 o3 c o2 l8 a b- l4 o3 c c o2 l8 a b- g g l4 f p4 f l8 a b- l4 o3 c o2 b- l8 a g a o3 c o2 l4 b o3 c p4 c o2 l8 b- a l4 g o3 c o2 l8 b- a b- o3 c l4 d c p4 o2 g l8 a b- l4 o3 c o2 g l8 a b- g g f.":GOTO 5000
4320 PLAY "T100 l4 o2 mn a l8 o3 d d c+ o2 a b o3 c+ l4 d p4 d l8 c+ o2 b a g l4 f+ e d p4 d l8 e f+ g a a g+ l4 a p4 a l8 b b o3 d d l4 c+ o2 b a p4 a l8 g f+ b a l4 g f+ e p4 e l8 f+ g a b l4 a g+ a.":GOTO 5000
4340 PLAY "T100 mn o2 l4 g l8 e d g g a o3 c o2 l4 b a p4 o3 d o2 l8 b b g o3 c o2 l4 b a g p4 g l8 e d g g a o3 c o2 l4 b a p4 o3 d l8 c o2 b a g g f+ l4 g p4 o3 d l8 d c o2 l4 b a l8 g f+ l4 e d p4 d l8 g g f+ e l4 g a b p4"
4350 PLAY "g l8 g a b g o3 c c o2 l4 b a p4 o3 d l8 c o2 b a g g f+ l4 g.":GOTO 5000
4360 PLAY "t100 mn o2 l4 b o3 c l8 o2 b g a g l4 f+ e p4 e g l8 f+ e e d+ l4 e p4 b o3 c l8 o2 b g a g l4 f+ e p4 e g l8 f+ e e d+ l4 e p4 g e l8 g a b g o3 l4 c o2 b p4 b o3 c o2 l8 b a a g+ l4 a p4"
4370 PLAY "a b l8 g e g g l4 f+ e p4 e g l8 e a a g+ l4 a.":GOTO 5000
4380 PLAY "t100 mn l4 o2 d l8 d l4 a l8 g b- a l4 g f p4 a l8 a l4 o3 c o2 l8 a o3 d c o2 l4 b a p4 o3 d c o2 l8 b a a g+ l4 a p4 f l8 a l4 b- l8 a g a l4 g f p4 a l8 a l4 o3 c l8 o2 a o3 d c o2 l4b a p4"
4390 PLAY "f a l8 g f l4 g e d.":GOTO 5000
4400 PLAY "t100 mn o2 l4 f g l8 f l4 b- l8 a g g l4 f p4 b- l8 b- a l4 b- o3 c l8 d d c o2 b- b- a l4 b- p4 f l8 g a b- a l4 g f+ g p4 b- l8 a g f e- l4 d l8 c f f g l4 a b- g f.":GOTO 5000
4420 PLAY "o2 t100 mn l4 a l8 a b a o3 d l4 c+ o2 b a p4 a l8 f+ g l4 a b l8 a g l4 f+ p4 a l8 f+ g l4 a b l8 a g l4 f+ p4 o3 d l8 c+ o2 b a f+ l4 g b a p4 f+ l8 a a e f+ g f+ l4 d p4 f+ l8 a a e f+ g f+ l4 d p4"
4430 PLAY "o3 d l8 c+ d o2 a a b o3 d l4 c+ d p4 o2 a o3 l8 d c+ o2 b b l4 a g f+ p4 a o3 l8 d c+ o2 b b l4 a g f+ p4 d l8 d e l4 f+ a l8 g f+ l4 e d.":GOTO 5000
4440 PLAY "t100 mn o2 l4 a f g l8 a a g f l4 a b o3 l8 c o2 a a g l4 f e f p4 f l8 f g l4 a a o3 l8 c o2 b- a g l4 f l8 e f g a l4 b- a g f p4 a l8 a b- o3 l4 c o2 b- l8 a g f g l4 a l8 g b- a g f e l4 d c p4"
4450 PLAY "f l8 a b- o3 l4 c c o2 l8 a a l4 f g l8 a a g b- l4 a g f.":GOTO 5000
4460 PLAY "t100 mn l4 o2 f l8 g a l4 b- a l8 g f f e l4 f p4 a l8 b- o3 c l4 d c o2 l8 b- a l4 g f e p4 c l8 f f l4 e f l8 g b- a g l4 a p4 o3 c l8 c o2 b- l4 a g l8 a o3 c c o2 b l4 o3 c p4 o2 a l8 g f l4 e f l8 g b- l4 a g f."
4470 GOTO 5000
4480 PLAY "t100 mn l4 o2 d l8 a a l4 b a f+ f+ l8 e f+ l4 g f+ e p4 a l8 f+ b a g l4 f+ d l8 e g l4 f+ e d.":GOTO 5000
4500 PLAY "t100 mn o2 l4 d l8 a a b g o3 l4 c o2 b a p4 a l8 a b g f+ l4 g e d p4 d l8 a a b g o3 l4 c o2 b a p4 a l8 a b g f+ l4 g e d p4 o3 d o2 l8 g a o3 c o2 b a g l4 b a p4 o3 d o2 l8 g a o3 c o2 b a g l4 b a p4"
4510 PLAY "g l8 f+ g e d l4 g a b p4 b o3 d c l8 o2 b o3 c o2 a a l4 g.":GOTO 5000
4520 PLAY "t100 mn l4 o2 a o3 l8 d d c+ o2 a b o3 c+ l4 d p4 d l8 c+ o2 b a g l4 f+ e d p4 d l8 e f+ g a a g+ l4 a p4 a l8 b b o3 d d l4 c+ o2 b a p4 a l8 g f+ b a l4 g f+ e p4 e l8 f+ g a b l4 a g+ a.":GOTO 5000 
4540 PLAY "t100 mn o2 l8 c g g a b o3 l4 c l8 c e d c c o2 b o3 l4 c p16 l8 o2 c g g a b o3 l4 c l8 c e d c c o2 b o3 l4 c p16 l8 c e e d c l4 d o2 l8 b b o3 c o2 b a a l4 g p16 l8 g e g a g l4 g l8 e g a o3 c c o2 b o3 l4 c."
4550 GOTO 5000
4560 PLAY "o2 t100 l4 mn d l8 d e l4 f a l8 g f e e l4 d p4 a l8 f g l4 a o3 d l8 d c+ o2 b a l4 b a p4 g l8 g a l4 f d l8 g f e e l4 f p4 a l8b o3 d l4 c+ o2 b l8 a f g f l4 e d.":GOTO 5000
4580 LOCATE 8,8:PRINT "Altwee berymings se wysies is dieselfde."
4590 PLAY "t100 mn o2 l4 a l8 d e f e l4 d c p4 f l8 d e l4 f g a p4 a l8 g+ a b o3 c o2 l4 b a p4 o3 d l8 c o2 a o3 l4 c o2 b a p4 a o3 c o2 a l8 g f l4 e d p4 g l8 f e d d l4 c p4 f l8 f g a b- l4 g f p4 a l8 g f l4 g e d.":GOTO 5000
4600 PLAY "T100 l4 o2 mn g a l8 f b- a l4 g l8 f+ l4 g p4 d l8 g f l4 g b- l8 b- a l4 b- p4 b- o3 c d l8 o2 g a l4 b- g f p4 f l8 g b- l4 f g l8 f e- l4 d.":GOTO 5000
4620 PLAY "t100 mn o2 l4 f+ l8 e g l4 f+ b l8 a g l4 f+ p4 f+ l8 g f+ e d l4 g f+ e p4 b o3 l8 c+ d o2 b o3 d l4 c+ o2 b a p4 f+ l8 a a l4 b o3 c+ l8 d c+ o2 l4 b p4 f+ a e l8 g f+ l4 a g f+.":GOTO 5000
4640 PLAY "t100 mn o2 l4 d f+ e l8 d a b o3 d c+ o2 a l4 b a p4 o3 d l8 d c+ l4 d o2 a l8 b b a f+ l4 g f+ p4 a f+ b l8 a g l4 f+ e d p4 d l8 f+ g l4 a b o3 l8 c+ o2 a b o3 c+ l4 d p4 d l8 c+ o2 b l4 a f+ l8 b b a g+ l4 a p4
4650 PLAY "d a b l8 a g l4 f+ e d.":GOTO 5000
4660 PLAY "t100 mn o2 l4 g l8 g f+ e d l4 g a b p4 b l8 b b a g o3 l4 c o2 b a p4 g l8 a b a g l4 e f+ g p4 o3 d o2 b g l8 a o3 c o2 l4 b a g.":GOTO 5000
4680 PLAY "t100 mn o3 l4 d o2 b l8 a a b o3 c+ l4 d p4 o2 a b l8 a g l4 f+ e d p4 d f+ l8 e d f+ g+ l4 a p4 f+ a l8 b o3 c+ d o2 b l4 a p4 a g l8 f+ a g f+ l4 e p4 e f+ l8 a g l4 f+ e d.":GOTO 5000
4700 PLAY "t100 mn o2 l4 d d l8 g a b o3 c+ l4 d p4 o2 b o3 d l8 c o2 b l4 g a g p4 b a l8 g g o3 c c o2 l4 b p4 g a l8 f+ g e e l4 d.":GOTO 5000
4720 PLAY "T100 mn o2 l4 b l8 b a l4 b o3 d l8 c+ c+ o2 l4 bg l8 b a l4 g f+ e p4 e l8 b b l4 a b o3 l8 d c+ o2 l4 bg l8 a b l4 g f+ e.":GOTO 5000
4740 PLAY "t100 mn o2 l4 e- l8 g a- l4 b- o3 e- d c o2 b- p4 b- o3 l8 c o2 b- l4 g g l8 a- g l4 f e- p4 e- l8 g a- l4 b- o3 e- d c o2 b- p4 b- o3 l8 c o2 b- l4 g g l8 a- g l4 f e- p4 o3 e- l8 d c o2 b- o3 e- l4 d c o2 b- p4"
4750 PLAY "b- f a- l8 g f g a- ml l4 b- mn o3 c o2 b- p4 e- l8 e- f g e- l4 g a b- p4 b- f a- l8 g b- a- g l4 f e-.":GOTO 5000
4760 PLAY "t100 mn o2 l4 g f e l8 d d e f+ l4 g p4 o3 c l8 c o2 a l4 o3 c o2 b l8 a a l4 g p4 g e f l8 g g l4 a o3 c o2 b p4 g o3 l8 c o2 b o3 c d l4 c o2 b o3 c p4 c o2 b a l8 g a g f l4 e d p4 g a b o3 l8 c o2 b a g l4 f+ g.":GOTO 5000
4780 PLAY "t100 mn l4 o2 g l8 g a l4 b l8 b o3 l4 c o2 l8 b l4 a p4 b o3 l8 c o2 b a g l4 f+ g a p4 o3 d l8 c o2 b l4 a f+ l8 g e l4 d p4 b l8 o3 c o2 b a g l4 b a g.":GOTO 5000
4800 PLAY "t100 mn l4 o2 f+ l8 e d l4 f+ a l8 g f+ l4 b a p4 a l8 b b l4 a l8 f+ l4 g l8 e l4 f+ p4 a l8 g e l4 f+ l8 g l4 f+ l8 e l4 d p4 d l8 f+ g l4 a a l8 f+ a l4 g f+.":GOTO 5000
4820 PLAY "t100 l4 o2 mn g a l8 f b- a l4 g l8 f+ l4 g p4 d l8 g f l4 g b- l8 b- a l4 b- p4 b- o3 c d o2 l8 g a l4 b- g f p4 l4 f l8 g b- l4 f g l8 f e- l2 d":GOTO 5000
4840 PLAY "t100 o2 mn l4 d a a l8 g a b o3 c o2 l4 b a p4 o3 d l8 d d c o2 b a o3 c o2 l4 b a p4 d l8 a a g a o3 l4 c o2 b a p4 o3 d l8 d d c o2 b a o3 c o2 l4 b a p4 o3 c l8 o2 b a g f l4 g e d.":GOTO 5000
4860 PLAY "t100 o2 mn l4 d l8 a a l4 b o3 d l8 c+ o2 b l4 a b a p4 a l8 b a l4 f+ a l8 g f+ l4 g e d p4 d l8 a a l4 b o3 d l8 c+ o2 b l4 a b a p4 a l8 b a l4 f+ a l8 g f+ l4 g e d p4 o3 d l8 c+ o2 b a b a f+ l4 a g f+ p4"
4870 PLAY "b l8 a g f+ g a b l4 a f+ e p4 d l8 d d l4 a f+ l8 g f+ g a l4 b p4 b l8 b o3 c l4 d o2 a l8 g f+ l4 e e d.":GOTO 5000
4880 PLAY "t100 o2 l4 mn f l8 f g l4 a o3 c o2 l8 b- a f g l4 a p4 a l8 g a l4 b- a l8 g f f e l4 f p4 d l8 f g l4 a b- l8 a g a b- o3 l4 c p4 d l8 c o2 b- a f b- a l4 g g f.":GOTO 5000
4900 PLAY "t100 o2 mn l4 d g a b o3 e d d o2 b a b o3 c d o2 b a l2 g l4 d g a b o3 e d d o2 b a b o3 c d o2 b a l2 g l4 b a g f+ g e l2 d l4 o3 d e d o2 b o3 c o2 a g.":GOTO 5000
4920 PLAY "t100 mn l4 o2 d l8 d e f+ a b b l4 a p4 o3 d l8 c+ o2 b l4 a f+ l8 b b l4 a p4 f+ l8 b a g f+ e e l4 f+ p4 a l8 b o3 c+ d c+ l4 d o2 b a p4 d l8 d e f+ a b b l4 a p4 o3 d l8 c+ o2 b l4 a f+ l8 b b l4 a p4"
4930 PLAY "f+ l8 g a b a l4 g f+ e p4 a l8 b a g f+ l4 g e d.":GOTO 5000
4940 PLAY "t100 mn l4 o2 d l8 f+ g l4 a a o3 l8 d o2 b l4 a p4 a l8 g f+ b a l4 g e d p4 d l8 f+ g l4 a a o3 l8 d o2 b l4 a p4 a l8 g f+ b a l4 g e d p4 a b o3 d l8 d c+ o2 b a l4 b a p4 a b o3 d l8 d c+ o2 b a l4 b a p4"
4950 PLAY"a l8 b a l8 f+ a l8 g f+ l4 e p4 d l8 a b a f+ l4 g e d.":GOTO 5000
4960 LOCATE 8,8:PRINT "1. Eerste wysie":LOCATE 9,8:PRINT "2. Tweede wysie":LOCATE 11,10:INPUT "Keuse (1/2)";G:IF G=1 THEN 4965
4962 IF G=2 THEN 4970 ELSE 4960
4965 PLAY "t100 o2 mn l4 d l8 d e l4 f+ a l8 g f+ l4 e p4 a l8 b a g f+ l4 g e d p4 a l8 b b o3 d c+ o2 l4 b a p4 d l8 a a b a l4 b o3 c+ d p4 d l8 c+ o2 b a b l4 a g f+ p4 d l8 f+ a g f+ l4 e d.":GOTO 5000
4970 PLAY "t100 mn o3 l4 c o2 g e c g a b o3 c o2 g o3 c o2 b a g f e d g a b o3 c o2 b l2 a l4 g c e f g g f e d g a b o3 c d o2 b a g g o3 c d f e l2 d l4 c":GOTO 5000
4980 PLAY "t100 o2 l4 mn d f+ l8 e d f+ g+ l4 a d f+ l8 e d f+ g+ l4 a p4 f+ b l8 a o3 d d c+ l4 d o2 a o3 d o2 l8 b a a g+ l4 a p4 f+ a l8 b a g f+ l4 e d p4 d g l8 f+ e f+ g+ l4 a p4 d g l8 f+ e f+ g+ l4 a p4"
4981 PLAY "o3 d o2 b l8 a g f+ d l4 e d.":GOTO 5000
5000 CLS:COLOR 15,0:PRINT"É"STRING$(74,"Í")"»":FOR U=1 TO 21:PRINT "º";SPC(74);"º":NEXT U:PRINT"È"STRING$(74,"Í")"¼":COLOR 15,0
5010 COLOR 7:LOCATE 12,30:PRINT "Nog 'n Psalm (Ja/Nee)":LOCATE 12,44:COLOR 15,0:PRINT "Ja":COLOR 7:P=1
5020 U$=INKEY$
5030 IF U$="6" OR U$=CHR$(75) THEN P=2:IF P=2 THEN LOCATE 12,44:COLOR 7:PRINT "Ja/";:COLOR 15,0:PRINT "Nee":COLOR 13:COLOR 7
5040 IF U$="4" OR U$=CHR$(74) THEN P=1:IF P=1 THEN LOCATE 12,44:COLOR 15,0:PRINT "Ja";:COLOR 7:PRINT "/Nee"
5050 IF U$=CHR$(13) AND P=1 THEN GOTO 10
5060 IF U$=CHR$(13) AND P=2 THEN CLS:DEF SEG=&H40:POKE &H17,(PEEK(&H17) AND 20):SYSTEM
5070 GOTO 5020
