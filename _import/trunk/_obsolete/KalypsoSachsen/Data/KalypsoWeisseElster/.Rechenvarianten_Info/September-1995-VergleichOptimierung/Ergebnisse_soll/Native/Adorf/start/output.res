





          *****************************************************
          *****************************************************
          ****                                             ****
          ****      TU HAMBURG-HARBURG, AB WASSERBAU       ****
          ****                                             ****
          ****                                             ****
          ****            K A L Y P S O - N A              ****
          ****                                             ****
          ****                                             ****
          ****      NIEDERSCHLAG-ABFLUSS-MODELLIERUNG      ****
          ****                                             ****
          ****                                             ****
          ****                                             ****
          ****                                             ****
          ****     VERS.  2.0.1       Stand: 03.08.2004    ****
          ****                                             ****
          ****                                             ****
          *****************************************************
          *****************************************************




 bearbeitung fall: 2 .. we nat  1995 08 30 08 1995 09 08 14 start\we_nat_start.txt                                                         


 anzahl der straenge in der netzdatei:      7
 anzahl der teilgebiete in der netzdatei:   2


 fuer knoten  1001 wurde
 version zufluss einer vorgegebenen
 funktion gewaehlt.
 knotennummer und eingabefile:
 5    1234..\zufluss\Z_1001.zufluss                                             



xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

   neuer Jahreszyklus 

         Jahr:      1995
         Zyklus:       1




 differenz zwischen idata und idate :      222
 jahreszyklus :                          1

auswertung des stranges     1000


auswertung des stranges  1000

  Anfangsstrang!
  Anfangsknoten > 9000 
  Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung.


auswertung des stranges     1001


auswertung des stranges  1001
  Zuflussganglinie wird eingelesen aus File ..\zufluss\Z_1001.zufluss                                                                                               
  Eingelesener Knoten:  1234
  Fall:                    5
  (1+2 = Zufluss, 3+4 = Entnahme, 2+4 = Werte in 1000m^3/Tag)


  Abflussfuelle im Gerinne vor Zugabe/Entnahme:       0.000
  Abfluessfuelle Zugabe/Entnahme:                    47.308
  Abflussfuelle im Gerinne nach Zugabe/Entnahme:     47.308



 fiktiver strang! Es erfolgt keine abflussverzoegerung!
auswertung des stranges     1700


auswertung des stranges  1700

abflussverzoegerung mit kalinin-miljukov parametern
 anzahl der zeitschritte in gerinne durch nachlauf zu hoch geworden.
 sie wird auf 2880 begrenzt ! 

 dt [h]:   1.00      tmax [d]:     2.42

 kennwerte zuflusswelle
 ----------------------

 scheitel-zufluss               iqzmax =     58   qzmax =       0.971 [m**3/s]
 scheitel-zufluss im gerinne    iqzfmx =     58  qzfmax =       0.971 [m**3/s]
 scheitel-zufluss im vorland    iqzvmx =      0  qzvmax =       0.000 [m**3/s]

 fuelle-zuflusz                  fuqz =   0.1703 [hm**3]
 fuelle-zuflusz im gerinne      fuqzf =   0.1703 [hm**3]
 fuelle-zuflusz im vorland      fuqzv =   0.0000 [hm**3]

 kennwerte der systemfunktionen
 -------------------------------

 anzahl der zeitschritte  ituh =      3
 maximum u                imax =      1     umax = 0.991664
 summe u                   suu = 0.994809   suuh = 1.000000

 tmax [d]:     2.46

 kennwerte ausflusswelle
 -----------------------

 endordinate ausflusz           qaend =   0.003
 scheitel-ausflusz              iqmax =     59    qmax =       0.971 [m**3/s]

 fuelle-ausflusz                   fuq =   0.1703 [hm**3]
 fuelle-ausflusz im gerinne       fuqa =   0.1703 [hm**3]

 scheitel-ausflusz im gerinne   iqamax =     59   qamax =       0.971 [m**3/s]

 gesamtzufluszwelle (ohne basisabflusz)  sqz =       47.308
 gesamtabfluszwelle    "       "         sq  =       47.308


auswertung des stranges     1002


auswertung des stranges  1002

  Anfangsstrang!
  Anfangsknoten > 9000 
  Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung.




 auswertung von gebiet  1800


warnung!
file ..\lzsim\we1800.lzs                                                             
mit anfangsbedingungen fuer die kurzzeitsimulation existiert nicht!
(bodenfeuchte, grundwasserstand, interzeptionsfeuchte)

vor der kurzzeitsimulation sollte langzeitsimulation durchgefuehrt werden.


 filename der niederschlageswerte: ..\klima.dat\C_1000.N                                       

 korrekturfaktor fuer niederschl.:    1.00

 gesamtmenge an niederschlag (mm):       68.0

Subroutine INP_HYDRO: Anzahl Hydrotope im TG:      9

anfangswerte   h01 =  0.000
               w01 =  0.000
               bo1 =  0.500
            bianf1 =  1.000
             aint1 =  0.000
            aigw1 =  0.500


vers. flaeche  =      0.00
natur. flaeche =     47.67
gesamtflaeche  =     47.67




-----------------------------------------

 z u s a m m e n f a s s u n g  
 der bilanz fuer teilgebiet:    1800
 anzahl hydrotope:       9


 speicherbilanz interzeption: 

 niederschlag:(nach schneesp.)      68.0 mm
 interz.verd.:                      14.1 mm
 bestandsniederschlag:              54.0 mm
 differenz interz.speicher:          0.1 mm
 fehler:                             0.0 mm

 pot. verdunst. (grasref.):         32.9 mm
 pot. verdunst.(akt. vegetat.):      26.7 mm
 aktuelle verdunstung:              20.7 mm


 speicherbilanz bodenspeicher:   

 bestandsniederschlag:              54.0 mm
 overlandflow:                       0.0 mm
 [infiltration:                     54.0 mm]
 kapilarer aufstieg:                 0.0 mm
 verdunstung (bodensp.):             6.7 mm
 lateral. abfluss (interflow):       0.0 mm
 perkolation:                        0.0 mm
 differenz bodenspeicher:          -47.3 mm
 fehler:                             0.0 mm



 aufteilung perkolation: 

 perkolation bodenspeicher:            0.0 mm

 abgabe grundw.speicher:               0.0 mm
        tiefengrundw.leiter:           0.0 mm

        verlust tiefengw.leiter:       0.0 mm

 fehler:                               0.0 mm


 bilanz grundwasserspeicher: 

 zufluss perkolation:                  0.0 mm
        grundw. oberh.l.TG:            0.0 mm
 abgabe basisabfluss:                  2.3 mm
        grundw. unterh.l.TG:           4.2 mm
        kapilarer aufstieg:            0.0 mm
        entnahme GWS:                 0.0 mm
     pot.entnahme GWS:                 0.0 mm
 speicherinhaltsaenderung:            -7.0 mm
 fehler:                               0.5 mm


 flaechenanteile: 

   grundwasserleiter:               47.666 qkm
   tiefengrundwasserleiter:          0.000 qkm
   gesamtflaeche:                   47.666 qkm


fuellenberechnung in iso
abflusz =             0.000
niederschlag =        0.000
flaeche zft  =        1.000
flaeche eing.=       47.666

GEBIET: Abflusswelle Interflow:
ISOV:  fuellenberechnung
ISOV: Abflusz =             0.000
ISOV: Niederschlag =        0.000

GEBIET: Abfluszwelle Kluft-Grundwasserleiter
 anzahl der zeitschritte in isov durch nachlauf zu hoch geworden.
 sie wird auf IDIM begrenzt !

ISOV:  fuellenberechnung
ISOV: Abflusz =             0.000
ISOV: Niederschlag =        0.000
auswertung des stranges     1003


auswertung des stranges  1003

  Anfangsstrang!
  Anfangsknoten > 9000 
  Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung.




 auswertung von gebiet  1700


warnung!
file ..\lzsim\we1700.lzs                                                             
mit anfangsbedingungen fuer die kurzzeitsimulation existiert nicht!
(bodenfeuchte, grundwasserstand, interzeptionsfeuchte)

vor der kurzzeitsimulation sollte langzeitsimulation durchgefuehrt werden.


 filename der niederschlageswerte: ..\klima.dat\C_1001.N                                       

 korrekturfaktor fuer niederschl.:    1.28

 gesamtmenge an niederschlag (mm):       99.8

Subroutine INP_HYDRO: Anzahl Hydrotope im TG:     17

anfangswerte   h01 =  0.000
               w01 =  0.000
               bo1 =  1.219
            bianf1 =  1.000
             aint1 =  0.000
            aigw1 =  0.740


vers. flaeche  =      2.11
natur. flaeche =    120.61
gesamtflaeche  =    122.71




-----------------------------------------

 z u s a m m e n f a s s u n g  
 der bilanz fuer teilgebiet:    1700
 anzahl hydrotope:      17


 speicherbilanz interzeption: 

 niederschlag:(nach schneesp.)      99.8 mm
 interz.verd.:                      22.0 mm
 bestandsniederschlag:              76.3 mm
 differenz interz.speicher:         -1.6 mm
 fehler:                             0.0 mm

 pot. verdunst. (grasref.):         32.9 mm
 pot. verdunst.(akt. vegetat.):      38.7 mm
 aktuelle verdunstung:              37.1 mm


 speicherbilanz bodenspeicher:   

 bestandsniederschlag:              76.3 mm
 overlandflow:                       2.1 mm
 [infiltration:                     74.2 mm]
 kapilarer aufstieg:                 0.0 mm
 verdunstung (bodensp.):            15.1 mm
 lateral. abfluss (interflow):       6.8 mm
 perkolation:                       35.5 mm
 differenz bodenspeicher:          -16.8 mm
 fehler:                             0.0 mm



 aufteilung perkolation: 

 perkolation bodenspeicher:           35.5 mm

 abgabe grundw.speicher:              35.5 mm
        tiefengrundw.leiter:           0.0 mm

        verlust tiefengw.leiter:       0.0 mm

 fehler:                               0.0 mm


 bilanz grundwasserspeicher: 

 zufluss perkolation:                 35.5 mm
        grundw. oberh.l.TG:            1.7 mm
 abgabe basisabfluss:                 12.3 mm
        grundw. unterh.l.TG:           7.0 mm
        kapilarer aufstieg:            0.0 mm
        entnahme GWS:                 0.0 mm
     pot.entnahme GWS:                 0.0 mm
 speicherinhaltsaenderung:            21.2 mm
 fehler:                              -3.3 mm


 flaechenanteile: 

   grundwasserleiter:              120.606 qkm
   tiefengrundwasserleiter:          0.000 qkm
   gesamtflaeche:                  120.606 qkm


fuellenberechnung in iso
abflusz =        249808.953
niederschlag =   249808.969
flaeche zft  =        1.000
flaeche eing.=      120.606

GEBIET: Abflusswelle Interflow:
ISOV:  fuellenberechnung
ISOV: Abflusz =        819368.563
ISOV: Niederschlag =   819368.750

GEBIET: Abfluszwelle versiegelte Flaechen
ISOV:  fuellenberechnung
ISOV: Abflusz =        151362.344
ISOV: Niederschlag =   151362.406

GEBIET: Abfluszwelle Kluft-Grundwasserleiter
 anzahl der zeitschritte in isov durch nachlauf zu hoch geworden.
 sie wird auf IDIM begrenzt !

ISOV:  fuellenberechnung
ISOV: Abflusz =             0.000
ISOV: Niederschlag =        0.000
auswertung des stranges    10002


auswertung des stranges 10002

 fiktiver strang! Es erfolgt keine abflussverzoegerung!
auswertung des stranges    10003


auswertung des stranges 10003

 fiktiver strang! Es erfolgt keine abflussverzoegerung!


xxx berechnung wurde ohne fehler beendet!
