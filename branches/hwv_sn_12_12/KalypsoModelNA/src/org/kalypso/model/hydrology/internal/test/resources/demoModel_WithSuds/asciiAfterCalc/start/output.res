
          *****************************************************
          *****************************************************
          ****                                             ****
          ****      HAMBURG UNIVERSITY OF TECHNOLOGY       ****
          ****                                             ****
          **** Department of River and Coastal Engineering ****
          ****                                             ****
          ****                                             ****
          ****            K A L Y P S O - N A              ****
          ****                                             ****
          ****                                             ****
          ****           Rainfall-Runoff-Model             ****
          ****                                             ****
          ****                                             ****
          ****                                             ****
          ****     VERS.  2.1.5    STATUS: 15.12.2009      ****
          ****                                             ****
          ****                                             ****
          ****     Contact:      Dipl.-Geoˆk. C. Br¸ning   ****
          ****                   c.bruening@tuhh.de        ****
          *****************************************************
          *****************************************************

 Bearbeitung Fall / Berechnungseinstellungen aus Steuerungsdatei:
 ----------------------------------------------------------------
 2 .. we nat  1996 08 24 00 1996 08 31 00 start\we_nat_start.txt                                                         

 Anzahl der Straenge in der Netzdatei:     15
 Anzahl der Teilgebiete in der Netzdatei:   6

 K U R Z Z E I T S I M U L A T I O N
 Zyklus (iz):                         1
 Anfangsdatum Zyklus (idatsa):   960824
 Enddatum Zyklus (idatse):       960831
 Zeitschritte (idif):              2016

 ************************************************************************************
 *  Neuer Jahreszyklus                                                              *
 *      Jahr:    1996                                                              *
 *      Zyklus:     1                                                              *
 ************************************************************************************
 Auswertung des Stranges:         1000

 Anfangsstrang! 
 Fuer den Anfangsknoten des Stranges gilt 9000 < Knotennummer > 10000
 Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung im Gerinne.

 Auswertung von Gebiet:                   4503

 filename der niederschlageswerte: ..\klima.dat\C_4503.N                                       
 korrekturfaktor fuer niederschl.:    2.65
 Anzahl eingelesener Zeitreihenwerte: 2018

 Eingelesener Niederschlag:     35.7 mm
 Datenfile:  ..\klima.dat\C_4503.N                                                                                                   

 gesamtmenge an niederschlag (mm):       94.6
 control of the imported we_hyd data:
 greenroof area (sum10):  0.0000000E+00 further nat. SUDS areas:  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00 swale area with filter drain(sum30n):
  0.0000000E+00 area drained by swale (sum30v):  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00
 suds_list:           0           0           0           0           0
           0           0           0           0           0
 count_suds :           0
 suds_list:
 sudsfvsum =  0.0000000E+00
 sudsfnsum =  0.0000000E+00
 fl_suds_v(3) =  0.0000000E+00
 fl_suds_n(3) =  0.0000000E+00
 fl_suds_t(3) =  0.0000000E+00
 fl_hyd_t(3) =  0.0000000E+00

Subroutine INP_HYDRO:      Anzahl Hydrotope im TG:     96


 Anfangswerte: aus LZSIM-Datei (*.lzs) oder Endwert letzter Zyklus
Anfangswerte   h01 =  0.000
               w01 =  0.000
               bo1 =  1.000
            bianf1 =  0.000
             aint1 =  0.000
             aigw1 =  1.000

-----------------------------------------
 z u s a m m e n f a s s u n g  
 der bilanz fuer teilgebiet:    4503
 anzahl hydrotope:      96

 speicherbilanz interzeption: 
 niederschlag:(nach schneesp.)      94.6 mm
 interz.verd.:                       9.2 mm
 bestandsniederschlag:              83.6 mm
 differenz interz.speicher:         -1.9 mm
 fehler:                             0.0 mm
 pot. verdunst. (grasref.):         18.1 mm
 pot. verdunst.(akt. vegetat.):      18.8 mm
 aktuelle verdunstung:              12.5 mm

 speicherbilanz bodenspeicher:   
 bestandsniederschlag:              83.6 mm
 overlandflow:                       3.1 mm
 [infiltration:                     80.4 mm]
 verdunstung (bodensp.):             3.3 mm
 lateral. abfluss (interflow):       0.0 mm
 perkolation:                        0.0 mm
 differenz bodenspeicher:          -77.1 mm
 fehler:                             0.0 mm
  0.0000000E+00  -> zufluss grundwasserleiter in         4503
TG:    4503 -> TG:    4500
Prozent:0.20
TG:    4503 -> TG:    4501
Prozent:0.10
TG:    4503 -> TG:    4506
Prozent:0.20
TG:    4503 -> TG:    4504
Prozent:0.50

 aufteilung perkolation: 
 perkolation bodenspeicher:            0.0 mm
 abgabe grundw.speicher:               0.0 mm
        tiefengrundw.leiter:           0.0 mm
        verlust tiefengw.leiter:       0.0 mm
 fehler:                               0.0 mm

 bilanz grundwasserspeicher: 
 zufluss perkolation:                  0.0 mm
        grundw. oberh.l.TG:            0.0 mm
 abgabe basisabfluss:                  0.0 mm
        grundw. unterh.l.TG:           2.1 mm
        kapilarer aufstieg:            0.0 mm
        entnahme GWS:                -0.9 mm
     pot.entnahme GWS:                -0.9 mm
 speicherinhaltsaenderung:            -1.2 mm
 fehler:                               0.0 mm

 flaechenanteile: 
   grundwasserleiter:                0.86904 qkm
   tiefengrundwasserleiter:          0.00000 qkm
   gesamtflaeche:                    0.86904 qkm

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000
fuellenberechnung in iso
abflusz =          2710.827
niederschlag =     2710.824
flaeche zft  =        1.000
flaeche eing.=        0.869

GEBIET: Abflusswelle Interflow:

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000

 Anzahl der Zeitschritte in iso durch Nachlauf zu hoch geworden.
 Sie wird auf 460 begrenzt !  gebiet :  4503
fuellenberechnung in iso
abflusz =             0.000
niederschlag =        0.000
flaeche zft  =        1.000
flaeche eing.=        0.869

GEBIET: Abfluﬂwelle versiegelte Flaechen
ISOV: Retentionskonstanten:   2.2       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =       1.00
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH =  400
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =          5939.354
ISOV: Niederschlag =     5939.355

GEBIET: Abflusswelle Kluft-Grundwasserleiter
ISOV: Retentionskonstanten:6000.0       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =   96602.20
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH = 2880
 anzahl der zeitschritte in isov durch nachlauf zu hoch geworden.
 sie wird auf IDIM begrenzt !
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =             0.000
ISOV: Niederschlag =        0.000
 Auswertung des Stranges:         1001

 Anfangsstrang! 
 Fuer den Anfangsknoten des Stranges gilt 9000 < Knotennummer > 10000
 Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung im Gerinne.

 Auswertung von Gebiet:                   4500

 filename der niederschlageswerte: ..\klima.dat\C_4503.N                                       
 korrekturfaktor fuer niederschl.:    2.77
 Anzahl eingelesener Zeitreihenwerte: 2018

 Eingelesener Niederschlag:     35.7 mm
 Datenfile:  ..\klima.dat\C_4503.N                                                                                                   

 gesamtmenge an niederschlag (mm):       98.9
 control of the imported we_hyd data:
 greenroof area (sum10):  0.0000000E+00 further nat. SUDS areas:  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00 swale area with filter drain(sum30n):
  0.0000000E+00 area drained by swale (sum30v):  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00
 suds_list:           0           0           0           0           0
           0           0           0           0           0
 count_suds :           0
 suds_list:
 sudsfvsum =  0.0000000E+00
 sudsfnsum =  0.0000000E+00
 fl_suds_v(3) =  0.0000000E+00
 fl_suds_n(3) =  0.0000000E+00
 fl_suds_t(3) =  0.0000000E+00
 fl_hyd_t(3) =  0.0000000E+00

Subroutine INP_HYDRO:      Anzahl Hydrotope im TG:    428


 Anfangswerte: aus LZSIM-Datei (*.lzs) oder Endwert letzter Zyklus
Anfangswerte   h01 =  0.000
               w01 =  0.000
               bo1 =  1.000
            bianf1 =  0.000
             aint1 =  0.000
             aigw1 =  1.000

-----------------------------------------
 z u s a m m e n f a s s u n g  
 der bilanz fuer teilgebiet:    4500
 anzahl hydrotope:     428

 speicherbilanz interzeption: 
 niederschlag:(nach schneesp.)      98.9 mm
 interz.verd.:                       9.2 mm
 bestandsniederschlag:              87.9 mm
 differenz interz.speicher:         -1.8 mm
 fehler:                             0.0 mm
 pot. verdunst. (grasref.):         18.1 mm
 pot. verdunst.(akt. vegetat.):      19.2 mm
 aktuelle verdunstung:              12.9 mm

 speicherbilanz bodenspeicher:   
 bestandsniederschlag:              87.9 mm
 overlandflow:                       9.4 mm
 [infiltration:                     78.5 mm]
 verdunstung (bodensp.):             3.7 mm
 lateral. abfluss (interflow):       0.0 mm
 perkolation:                        0.0 mm
 differenz bodenspeicher:          -74.8 mm
 fehler:                             0.0 mm
   1.235995      -> zufluss grundwasserleiter in         4500

 aufteilung perkolation: 
 perkolation bodenspeicher:            0.0 mm
 abgabe grundw.speicher:               0.0 mm
        tiefengrundw.leiter:           0.0 mm
        verlust tiefengw.leiter:       0.0 mm
 fehler:                               0.0 mm

 bilanz grundwasserspeicher: 
 zufluss perkolation:                  0.0 mm
        grundw. oberh.l.TG:            0.4 mm
 abgabe basisabfluss:                  0.0 mm
        grundw. unterh.l.TG:           2.3 mm
        kapilarer aufstieg:            0.0 mm
        entnahme GWS:                -0.9 mm
     pot.entnahme GWS:                -0.9 mm
 speicherinhaltsaenderung:            -1.1 mm
 fehler:                               0.0 mm

 flaechenanteile: 
   grundwasserleiter:                0.93193 qkm
   tiefengrundwasserleiter:          0.00000 qkm
   gesamtflaeche:                    0.93193 qkm

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000
fuellenberechnung in iso
abflusz =          8798.073
niederschlag =     8798.089
flaeche zft  =        1.000
flaeche eing.=        0.932

GEBIET: Abflusswelle Interflow:

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000

 Anzahl der Zeitschritte in iso durch Nachlauf zu hoch geworden.
 Sie wird auf 460 begrenzt !  gebiet :  4500
fuellenberechnung in iso
abflusz =             0.000
niederschlag =        0.000
flaeche zft  =        1.000
flaeche eing.=        0.932

GEBIET: Abfluﬂwelle versiegelte Flaechen
ISOV: Retentionskonstanten:   2.0       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =       1.00
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH =  380
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =         16813.410
ISOV: Niederschlag =    16813.424

GEBIET: Abflusswelle Kluft-Grundwasserleiter
ISOV: Retentionskonstanten:6000.0       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =   96602.20
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH = 2880
 anzahl der zeitschritte in isov durch nachlauf zu hoch geworden.
 sie wird auf IDIM begrenzt !
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =             0.000
ISOV: Niederschlag =        0.000
 Auswertung des Stranges:         1002

 Anfangsstrang! 
 Fuer den Anfangsknoten des Stranges gilt 9000 < Knotennummer > 10000
 Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung im Gerinne.

 Auswertung von Gebiet:                   4501

 filename der niederschlageswerte: ..\klima.dat\C_4503.N                                       
 korrekturfaktor fuer niederschl.:    2.48
 Anzahl eingelesener Zeitreihenwerte: 2018

 Eingelesener Niederschlag:     35.7 mm
 Datenfile:  ..\klima.dat\C_4503.N                                                                                                   

 gesamtmenge an niederschlag (mm):       88.5
 control of the imported we_hyd data:
 greenroof area (sum10):  0.0000000E+00 further nat. SUDS areas:  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00 swale area with filter drain(sum30n):
  0.0000000E+00 area drained by swale (sum30v):  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00
 suds_list:           0           0           0           0           0
           0           0           0           0           0
 count_suds :           0
 suds_list:
 sudsfvsum =  0.0000000E+00
 sudsfnsum =  0.0000000E+00
 fl_suds_v(3) =  0.0000000E+00
 fl_suds_n(3) =  0.0000000E+00
 fl_suds_t(3) =  0.0000000E+00
 fl_hyd_t(3) =  0.0000000E+00

Subroutine INP_HYDRO:      Anzahl Hydrotope im TG:     22


 Anfangswerte: aus LZSIM-Datei (*.lzs) oder Endwert letzter Zyklus
Anfangswerte   h01 =  0.000
               w01 =  0.000
               bo1 =  1.000
            bianf1 =  0.000
             aint1 =  0.000
             aigw1 =  5.000

-----------------------------------------
 z u s a m m e n f a s s u n g  
 der bilanz fuer teilgebiet:    4501
 anzahl hydrotope:      22

 speicherbilanz interzeption: 
 niederschlag:(nach schneesp.)      88.5 mm
 interz.verd.:                       8.9 mm
 bestandsniederschlag:              77.8 mm
 differenz interz.speicher:         -1.8 mm
 fehler:                             0.0 mm
 pot. verdunst. (grasref.):         18.1 mm
 pot. verdunst.(akt. vegetat.):      19.0 mm
 aktuelle verdunstung:              14.4 mm

 speicherbilanz bodenspeicher:   
 bestandsniederschlag:              77.8 mm
 overlandflow:                      42.4 mm
 [infiltration:                     35.4 mm]
 verdunstung (bodensp.):             5.5 mm
 lateral. abfluss (interflow):       0.0 mm
 perkolation:                        0.0 mm
 differenz bodenspeicher:          -30.0 mm
 fehler:                             0.0 mm
  0.6179976      -> zufluss grundwasserleiter in         4501
TG:    4501 -> TG:    4502
Prozent:0.80
TG:    4501 -> TG:    4506
Prozent:0.20

 aufteilung perkolation: 
 perkolation bodenspeicher:            0.0 mm
 abgabe grundw.speicher:               0.0 mm
        tiefengrundw.leiter:           0.0 mm
        verlust tiefengw.leiter:       0.0 mm
 fehler:                               0.0 mm

 bilanz grundwasserspeicher: 
 zufluss perkolation:                  0.0 mm
        grundw. oberh.l.TG:            1.6 mm
 abgabe basisabfluss:                  0.0 mm
        grundw. unterh.l.TG:           2.5 mm
        kapilarer aufstieg:            0.0 mm
        entnahme GWS:                 0.0 mm
     pot.entnahme GWS:                 0.0 mm
 speicherinhaltsaenderung:            -1.0 mm
 fehler:                               0.0 mm

 flaechenanteile: 
   grundwasserleiter:                0.11396 qkm
   tiefengrundwasserleiter:          0.00000 qkm
   gesamtflaeche:                    0.11397 qkm

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000
fuellenberechnung in iso
abflusz =          4830.995
niederschlag =     4830.991
flaeche zft  =        1.000
flaeche eing.=        0.114

GEBIET: Abflusswelle Interflow:

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000

 Anzahl der Zeitschritte in iso durch Nachlauf zu hoch geworden.
 Sie wird auf 460 begrenzt !  gebiet :  4501
fuellenberechnung in iso
abflusz =             0.000
niederschlag =        0.000
flaeche zft  =        1.000
flaeche eing.=        0.114

GEBIET: Abfluﬂwelle versiegelte Flaechen
ISOV: Retentionskonstanten:   2.0       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =       1.00
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH =  374
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =           254.831
ISOV: Niederschlag =      254.831

GEBIET: Abflusswelle Kluft-Grundwasserleiter
ISOV: Retentionskonstanten:6000.0       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =   96602.20
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH = 2880
 anzahl der zeitschritte in isov durch nachlauf zu hoch geworden.
 sie wird auf IDIM begrenzt !
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =             0.000
ISOV: Niederschlag =        0.000
 Auswertung des Stranges:         1003

 Anfangsstrang! 
 Fuer den Anfangsknoten des Stranges gilt 9000 < Knotennummer > 10000
 Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung im Gerinne.

 Auswertung von Gebiet:                   4502

 filename der niederschlageswerte: ..\klima.dat\C_4503.N                                       
 korrekturfaktor fuer niederschl.:    2.48
 Anzahl eingelesener Zeitreihenwerte: 2018

 Eingelesener Niederschlag:     35.7 mm
 Datenfile:  ..\klima.dat\C_4503.N                                                                                                   

 gesamtmenge an niederschlag (mm):       88.5
 control of the imported we_hyd data:
 greenroof area (sum10):  0.0000000E+00 further nat. SUDS areas:  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00 swale area with filter drain(sum30n):
  0.0000000E+00 area drained by swale (sum30v):  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00
 suds_list:           0           0           0           0           0
           0           0           0           0           0
 count_suds :           0
 suds_list:
 sudsfvsum =  0.0000000E+00
 sudsfnsum =  0.0000000E+00
 fl_suds_v(3) =  0.0000000E+00
 fl_suds_n(3) =  0.0000000E+00
 fl_suds_t(3) =  0.0000000E+00
 fl_hyd_t(3) =  0.0000000E+00

Subroutine INP_HYDRO:      Anzahl Hydrotope im TG:    210


 Anfangswerte: aus LZSIM-Datei (*.lzs) oder Endwert letzter Zyklus
Anfangswerte   h01 =  0.000
               w01 =  0.000
               bo1 =  1.000
            bianf1 =  0.000
             aint1 =  0.000
             aigw1 =  1.000

-----------------------------------------
 z u s a m m e n f a s s u n g  
 der bilanz fuer teilgebiet:    4502
 anzahl hydrotope:     210

 speicherbilanz interzeption: 
 niederschlag:(nach schneesp.)      88.5 mm
 interz.verd.:                       8.9 mm
 bestandsniederschlag:              77.9 mm
 differenz interz.speicher:         -1.7 mm
 fehler:                             0.0 mm
 pot. verdunst. (grasref.):         18.1 mm
 pot. verdunst.(akt. vegetat.):      18.8 mm
 aktuelle verdunstung:              12.5 mm

 speicherbilanz bodenspeicher:   
 bestandsniederschlag:              77.9 mm
 overlandflow:                       7.3 mm
 [infiltration:                     70.7 mm]
 verdunstung (bodensp.):             3.6 mm
 lateral. abfluss (interflow):       0.0 mm
 perkolation:                        0.0 mm
 differenz bodenspeicher:          -67.1 mm
 fehler:                             0.0 mm
  0.7698497      -> zufluss grundwasserleiter in         4502

 aufteilung perkolation: 
 perkolation bodenspeicher:            0.0 mm
 abgabe grundw.speicher:               0.0 mm
        tiefengrundw.leiter:           0.0 mm
        verlust tiefengw.leiter:       0.0 mm
 fehler:                               0.0 mm

 bilanz grundwasserspeicher: 
 zufluss perkolation:                  0.0 mm
        grundw. oberh.l.TG:            0.6 mm
 abgabe basisabfluss:                  0.0 mm
        grundw. unterh.l.TG:           1.8 mm
        kapilarer aufstieg:            0.0 mm
        entnahme GWS:                 0.0 mm
     pot.entnahme GWS:                 0.0 mm
 speicherinhaltsaenderung:            -1.3 mm
 fehler:                               0.0 mm

 flaechenanteile: 
   grundwasserleiter:                0.41181 qkm
   tiefengrundwasserleiter:          0.00000 qkm
   gesamtflaeche:                    0.41181 qkm

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000
fuellenberechnung in iso
abflusz =          2986.227
niederschlag =     2986.224
flaeche zft  =        1.000
flaeche eing.=        0.412

GEBIET: Abflusswelle Interflow:

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000

 Anzahl der Zeitschritte in iso durch Nachlauf zu hoch geworden.
 Sie wird auf 460 begrenzt !  gebiet :  4502
fuellenberechnung in iso
abflusz =             0.000
niederschlag =        0.000
flaeche zft  =        1.000
flaeche eing.=        0.412

GEBIET: Abfluﬂwelle versiegelte Flaechen
ISOV: Retentionskonstanten:   1.2       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =       1.00
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH =  239
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =          8175.339
ISOV: Niederschlag =     8175.344

GEBIET: Abflusswelle Kluft-Grundwasserleiter
ISOV: Retentionskonstanten:6000.0       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =   96602.20
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH = 2880
 anzahl der zeitschritte in isov durch nachlauf zu hoch geworden.
 sie wird auf IDIM begrenzt !
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =             0.000
ISOV: Niederschlag =        0.000
 Auswertung des Stranges:         1004

 Anfangsstrang! 
 Fuer den Anfangsknoten des Stranges gilt 9000 < Knotennummer > 10000
 Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung im Gerinne.

 Auswertung von Gebiet:                   4506

 filename der niederschlageswerte: ..\klima.dat\C_4503.N                                       
 korrekturfaktor fuer niederschl.:    2.65
 Anzahl eingelesener Zeitreihenwerte: 2018

 Eingelesener Niederschlag:     35.7 mm
 Datenfile:  ..\klima.dat\C_4503.N                                                                                                   

 gesamtmenge an niederschlag (mm):       94.6
 control of the imported we_hyd data:
 greenroof area (sum10):  0.0000000E+00 further nat. SUDS areas:  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00 swale area with filter drain(sum30n):
  0.0000000E+00 area drained by swale (sum30v):  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00
 suds_list:           0           0           0           0           0
           0           0           0           0           0
 count_suds :           0
 suds_list:
 sudsfvsum =  0.0000000E+00
 sudsfnsum =  0.0000000E+00
 fl_suds_v(3) =  0.0000000E+00
 fl_suds_n(3) =  0.0000000E+00
 fl_suds_t(3) =  0.0000000E+00
 fl_hyd_t(3) =  0.0000000E+00

Subroutine INP_HYDRO:      Anzahl Hydrotope im TG:     30


 Anfangswerte: aus LZSIM-Datei (*.lzs) oder Endwert letzter Zyklus
Anfangswerte   h01 =  0.000
               w01 =  0.000
               bo1 =  1.000
            bianf1 =  0.000
             aint1 =  0.000
             aigw1 =  1.000

-----------------------------------------
 z u s a m m e n f a s s u n g  
 der bilanz fuer teilgebiet:    4506
 anzahl hydrotope:      30

 speicherbilanz interzeption: 
 niederschlag:(nach schneesp.)      94.6 mm
 interz.verd.:                       9.0 mm
 bestandsniederschlag:              83.8 mm
 differenz interz.speicher:         -1.8 mm
 fehler:                             0.0 mm
 pot. verdunst. (grasref.):         18.1 mm
 pot. verdunst.(akt. vegetat.):      18.7 mm
 aktuelle verdunstung:              14.3 mm

 speicherbilanz bodenspeicher:   
 bestandsniederschlag:              83.8 mm
 overlandflow:                      46.4 mm
 [infiltration:                     37.4 mm]
 verdunstung (bodensp.):             5.3 mm
 lateral. abfluss (interflow):       0.0 mm
 perkolation:                        0.0 mm
 differenz bodenspeicher:          -32.1 mm
 fehler:                             0.0 mm
   1.428457      -> zufluss grundwasserleiter in         4506

 aufteilung perkolation: 
 perkolation bodenspeicher:            0.0 mm
 abgabe grundw.speicher:               0.0 mm
        tiefengrundw.leiter:           0.0 mm
        verlust tiefengw.leiter:       0.0 mm
 fehler:                               0.0 mm

 bilanz grundwasserspeicher: 
 zufluss perkolation:                  0.0 mm
        grundw. oberh.l.TG:            2.2 mm
 abgabe basisabfluss:                  0.2 mm
        grundw. unterh.l.TG:           2.9 mm
        kapilarer aufstieg:            0.0 mm
        entnahme GWS:                 0.0 mm
     pot.entnahme GWS:                 0.0 mm
 speicherinhaltsaenderung:            -0.9 mm
 fehler:                               0.0 mm

 flaechenanteile: 
   grundwasserleiter:                0.19318 qkm
   tiefengrundwasserleiter:          0.00000 qkm
   gesamtflaeche:                    0.19318 qkm

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000

 Anzahl der Zeitschritte in iso durch Nachlauf zu hoch geworden.
 Sie wird auf 460 begrenzt !  gebiet :  4506
fuellenberechnung in iso
abflusz =          8967.207
niederschlag =     8967.679
flaeche zft  =        1.000
flaeche eing.=        0.193

GEBIET: Abflusswelle Interflow:

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000

 Anzahl der Zeitschritte in iso durch Nachlauf zu hoch geworden.
 Sie wird auf 460 begrenzt !  gebiet :  4506
fuellenberechnung in iso
abflusz =             0.000
niederschlag =        0.000
flaeche zft  =        1.000
flaeche eing.=        0.193

GEBIET: Abfluﬂwelle versiegelte Flaechen
ISOV: Retentionskonstanten:   3.0       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =       1.00
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH =  544
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =           295.787
ISOV: Niederschlag =      295.787

GEBIET: Abflusswelle Kluft-Grundwasserleiter
ISOV: Retentionskonstanten:6000.0       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =   96602.20
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH = 2880
 anzahl der zeitschritte in isov durch nachlauf zu hoch geworden.
 sie wird auf IDIM begrenzt !
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =             0.000
ISOV: Niederschlag =        0.000
 Auswertung des Stranges:         1005

 Anfangsstrang! 
 Fuer den Anfangsknoten des Stranges gilt 9000 < Knotennummer > 10000
 Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung im Gerinne.

 Auswertung von Gebiet:                   4504

 filename der niederschlageswerte: ..\klima.dat\C_4503.N                                       
 korrekturfaktor fuer niederschl.:    2.65
 Anzahl eingelesener Zeitreihenwerte: 2018

 Eingelesener Niederschlag:     35.7 mm
 Datenfile:  ..\klima.dat\C_4503.N                                                                                                   

 gesamtmenge an niederschlag (mm):       94.6
 control of the imported we_hyd data:
 greenroof area (sum10):  0.0000000E+00 further nat. SUDS areas:  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00 swale area with filter drain(sum30n):
  0.0000000E+00 area drained by swale (sum30v):  0.0000000E+00  0.0000000E+00
  0.0000000E+00  0.0000000E+00  0.0000000E+00
 suds_list:           0           0           0           0           0
           0           0           0           0           0
 count_suds :           0
 suds_list:
 sudsfvsum =  0.0000000E+00
 sudsfnsum =  0.0000000E+00
 fl_suds_v(3) =  0.0000000E+00
 fl_suds_n(3) =  0.0000000E+00
 fl_suds_t(3) =  0.0000000E+00
 fl_hyd_t(3) =  0.0000000E+00

Subroutine INP_HYDRO:      Anzahl Hydrotope im TG:    256


 Anfangswerte: aus LZSIM-Datei (*.lzs) oder Endwert letzter Zyklus
Anfangswerte   h01 =  0.000
               w01 =  0.000
               bo1 =  1.000
            bianf1 =  0.000
             aint1 =  0.000
             aigw1 =  1.250

-----------------------------------------
 z u s a m m e n f a s s u n g  
 der bilanz fuer teilgebiet:    4504
 anzahl hydrotope:     256

 speicherbilanz interzeption: 
 niederschlag:(nach schneesp.)      94.6 mm
 interz.verd.:                       9.1 mm
 bestandsniederschlag:              83.8 mm
 differenz interz.speicher:         -1.7 mm
 fehler:                             0.0 mm
 pot. verdunst. (grasref.):         18.1 mm
 pot. verdunst.(akt. vegetat.):      19.3 mm
 aktuelle verdunstung:              13.2 mm

 speicherbilanz bodenspeicher:   
 bestandsniederschlag:              83.8 mm
 overlandflow:                      22.5 mm
 [infiltration:                     61.3 mm]
 verdunstung (bodensp.):             4.1 mm
 lateral. abfluss (interflow):       0.0 mm
 perkolation:                        0.0 mm
 differenz bodenspeicher:          -57.1 mm
 fehler:                             0.0 mm
   3.089987      -> zufluss grundwasserleiter in         4504

 aufteilung perkolation: 
 perkolation bodenspeicher:            0.0 mm
 abgabe grundw.speicher:               0.0 mm
        tiefengrundw.leiter:           0.0 mm
        verlust tiefengw.leiter:       0.0 mm
 fehler:                               0.0 mm

 bilanz grundwasserspeicher: 
 zufluss perkolation:                  0.0 mm
        grundw. oberh.l.TG:            1.5 mm
 abgabe basisabfluss:                  0.0 mm
        grundw. unterh.l.TG:           2.6 mm
        kapilarer aufstieg:            0.0 mm
        entnahme GWS:                 0.0 mm
     pot.entnahme GWS:                 0.0 mm
 speicherinhaltsaenderung:            -1.2 mm
 fehler:                               0.0 mm

 flaechenanteile: 
   grundwasserleiter:                0.62059 qkm
   tiefengrundwasserleiter:          0.00000 qkm
   gesamtflaeche:                    0.62060 qkm

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000

 Anzahl der Zeitschritte in iso durch Nachlauf zu hoch geworden.
 Sie wird auf 460 begrenzt !  gebiet :  4504
fuellenberechnung in iso
abflusz =         13967.062
niederschlag =    13967.482
flaeche zft  =        1.000
flaeche eing.=        0.621

GEBIET: Abflusswelle Interflow:

 ISO: Normierungsfaktor   = 1.0001
 ISO: Summe normierter uh =   1.0000

 Anzahl der Zeitschritte in iso durch Nachlauf zu hoch geworden.
 Sie wird auf 460 begrenzt !  gebiet :  4504
fuellenberechnung in iso
abflusz =             0.000
niederschlag =        0.000
flaeche zft  =        1.000
flaeche eing.=        0.621

GEBIET: Abfluﬂwelle versiegelte Flaechen
ISOV: Retentionskonstanten:   1.8       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =       1.00
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH =  334
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =          8700.098
ISOV: Niederschlag =     8700.102

GEBIET: Abflusswelle Kluft-Grundwasserleiter
ISOV: Retentionskonstanten:6000.0       0.0       0.0
ISOV: Speicheranzahl:         3.0       0.0       3.0
ISOV: Aufteilngsfaktor:      1.00      0.00
 ISOV: Normierungsfaktor =   96602.20
 ISOV: Summe normierter UH-Ordinaten =     1.0000
 ISOV: Anz. Zeitschritte UH = 2880
 anzahl der zeitschritte in isov durch nachlauf zu hoch geworden.
 sie wird auf IDIM begrenzt !
ISOV:  fuellenberechnung
ISOV: Abfluﬂ =             0.000
ISOV: Niederschlag =        0.000
 Auswertung des Stranges:         4500

 Anfangsstrang! 
 Fuer den Anfangsknoten des Stranges gilt 9000 < Knotennummer > 10000
 Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung im Gerinne.
 Keine angeschlossenen Teilgebiete an Strang      4500
 Auswertung des Stranges:         4503

 Anfangsstrang! 
 Fuer den Anfangsknoten des Stranges gilt 9000 < Knotennummer > 10000
 Es erfolgt kein Zufluss und daher auch keine Abflussverzoegerung im Gerinne.
 Keine angeschlossenen Teilgebiete an Strang      4503
 Auswertung des Stranges:         4504

 Anfangswert aus LZSIM-Datei (*.lzg)
 Uebernommener Schwellwert des Abflusses:     0.000

 Abflussverzoegerung mit Kalinin-Miljukov Parametern

 gerinne: Anzahl der Zeitschritte in Gerinne durch Nachlauf zu hoch geworden.
 Sie wird auf  2880 begrenzt!

 dt [h]:   0.08      tmax [d]:     4.97

 Kennwerte Zuflusswelle
 ----------------------
 Scheitel-Zufluss               iqzmax =   1432   qzmax =       0.209 [m**3/s]
 Scheitel-Zufluss im Gerinne    iqzfmx =   1432  qzfmax =       0.209 [m**3/s]
 Scheitel-Zufluss im Vorland    iqzvmx =      0  qzvmax =       0.000 [m**3/s]
 Fuelle-Zufluss                   fuqz =   0.0087 [hm**3]
 Fuelle-zufluss im Gerinne       fuqzf =   0.0087 [hm**3]
 Fuelle-zufluss im Vorland       fuqzv =   0.0000 [hm**3]

 Kennwerte der Systemfunktionen
 -------------------------------
 Anzahl der Zeitschritte  ituh =     17
 Maximum u                imax =      3     umax = 0.269426
 Summe u                   suu = 0.999999   suuh = 1.000000

 tmax [d]:     4.99

 Kennwerte Ausflusswelle
 -----------------------
 Endordinate Ausfluss           qaend =   0.000
 Scheitel-Ausfluss              iqmax =   1436    qmax =       0.208 [m**3/s]
 Fuelle-Ausfluss                  fuq =   0.0087 [hm**3]
 Fuelle-Ausfluss im Gerinne      fuqa =   0.0087 [hm**3]
 Scheitel-Ausfluss im Gerinne  iqamax =   1436   qamax =       0.208 [m**3/s]

 Gesamtzuflusswelle (ohne Basisabfluss)  sqz =       28.867
 Gesamtabflusswelle    "       "         sq  =       28.867
 Keine angeschlossenen Teilgebiete an Strang      4504
 Auswertung des Stranges:         4505

 Anfangswert aus LZSIM-Datei (*.lzg)
 Uebernommener Schwellwert des Abflusses:     0.000

 Aufruf der Speicherroutine 


 zufluss-fuelle [hm]        0.030293 speicher 4505
 zusaetzl.zufl im nachl.ast [hm]  0.001071

 speicher   0.031364 abfluss-fuelle [hm]      4505

 speicherinhaltsaenderung [hm]  0.000000
 Keine angeschlossenen Teilgebiete an Strang      4505
 Auswertung des Stranges:         4603

 Anfangswert aus LZSIM-Datei (*.lzg)
 Uebernommener Schwellwert des Abflusses:     0.000

 Fiktiver Strang! Es erfolgt keine Abflussverzoegerung!
 Keine angeschlossenen Teilgebiete an Strang      4603
 Auswertung des Stranges:         4501

 Anfangswert aus LZSIM-Datei (*.lzg)
 Uebernommener Schwellwert des Abflusses:     0.000

 Abflussverzoegerung mit Kalinin-Miljukov Parametern

 gerinne: Anzahl der Zeitschritte in Gerinne durch Nachlauf zu hoch geworden.
 Sie wird auf  2880 begrenzt!

 gerinne: Anzahl der Zeitschritte in Gerinne durch Nachlauf zu hoch geworden.
 Sie wird auf  2880 begrenzt!

 dt [h]:   0.08      tmax [d]:     4.97

 Kennwerte Zuflusswelle
 ----------------------
 Scheitel-Zufluss               iqzmax =   1430   qzmax =       0.645 [m**3/s]
 Scheitel-Zufluss im Gerinne    iqzfmx =   1496  qzfmax =       0.242 [m**3/s]
 Scheitel-Zufluss im Vorland    iqzvmx =   1430  qzvmax =       0.534 [m**3/s]
 Fuelle-Zufluss                   fuqz =   0.0256 [hm**3]
 Fuelle-zufluss im Gerinne       fuqzf =   0.0144 [hm**3]
 Fuelle-zufluss im Vorland       fuqzv =   0.0112 [hm**3]

 Kennwerte der Systemfunktionen
 -------------------------------
 Anzahl der Zeitschritte  ituh =     33
 Maximum u                imax =      3     umax = 0.173632
 Summe u                   suu = 1.000082   suuh = 1.000000

 tmax [d]:     4.98

 Kennwerte Ausflusswelle
 -----------------------

 Endordinate Ausfluss           qaend =   0.000

 Scheitel-Ausfluss              iqmax =   1433    qmax =       0.643 [m**3/s]

 Fuelle-Ausfluss                  fuq =   0.0256 [hm**3]

 Fuelle-Ausfluss im Gerinne      fuqa =   0.0144 [hm**3]

 Fuelle-Ausfluss im vorland      fuqa =   0.0112 [hm**3]

 Scheitel-Ausfluss im Gerinne  iqamax =   1497   qamax =       0.241 [m**3/s]

 Scheitel-Ausfluss im Vorland  iqamax =   1433   qamax =       0.532 [m**3/s]

 Gesamtzuflusswelle (ohne Basisabfluss)  sqz =       85.432
 Gesamtabflusswelle    "       "         sq  =       85.255
 Keine angeschlossenen Teilgebiete an Strang      4501
 Auswertung des Stranges:         4502

 Anfangswert aus LZSIM-Datei (*.lzg)
 Uebernommener Schwellwert des Abflusses:     0.000

 Aufruf der Speicherroutine 


 zufluss-fuelle [hm]        0.040638 speicher 4502
 zusaetzl.zufl im nachl.ast [hm]  0.001196

 speicher   0.041834 abfluss-fuelle [hm]      4502

 speicherinhaltsaenderung [hm]  0.000000
 Keine angeschlossenen Teilgebiete an Strang      4502
 Auswertung des Stranges:         4506

 Anfangswert aus LZSIM-Datei (*.lzg)
 Uebernommener Schwellwert des Abflusses:     0.000

 Abflussverzoegerung mit Kalinin-Miljukov Parametern

 gerinne: Anzahl der Zeitschritte in Gerinne durch Nachlauf zu hoch geworden.
 Sie wird auf  2880 begrenzt!

 gerinne: Anzahl der Zeitschritte in Gerinne durch Nachlauf zu hoch geworden.
 Sie wird auf  2880 begrenzt!

 dt [h]:   0.08      tmax [d]:     4.98

 Kennwerte Zuflusswelle
 ----------------------
 Scheitel-Zufluss               iqzmax =   1433   qzmax =       1.117 [m**3/s]
 Scheitel-Zufluss im Gerinne    iqzfmx =   1523  qzfmax =       0.242 [m**3/s]
 Scheitel-Zufluss im Vorland    iqzvmx =   1433  qzvmax =       0.924 [m**3/s]
 Fuelle-Zufluss                   fuqz =   0.0419 [hm**3]
 Fuelle-zufluss im Gerinne       fuqzf =   0.0203 [hm**3]
 Fuelle-zufluss im Vorland       fuqzv =   0.0216 [hm**3]

 Kennwerte der Systemfunktionen
 -------------------------------
 Anzahl der Zeitschritte  ituh =     33
 Maximum u                imax =      3     umax = 0.173632
 Summe u                   suu = 1.000082   suuh = 1.000000

 tmax [d]:     4.99

 Kennwerte Ausflusswelle
 -----------------------

 Endordinate Ausfluss           qaend =   0.000

 Scheitel-Ausfluss              iqmax =   1436    qmax =       1.113 [m**3/s]

 Fuelle-Ausfluss                  fuq =   0.0416 [hm**3]

 Fuelle-Ausfluss im Gerinne      fuqa =   0.0202 [hm**3]

 Fuelle-Ausfluss im vorland      fuqa =   0.0213 [hm**3]

 Scheitel-Ausfluss im Gerinne  iqamax =   1524   qamax =       0.241 [m**3/s]

 Scheitel-Ausfluss im Vorland  iqamax =   1436   qamax =       0.921 [m**3/s]

 Gesamtzuflusswelle (ohne Basisabfluss)  sqz =      139.516
 Gesamtabflusswelle    "       "         sq  =      138.532
 Keine angeschlossenen Teilgebiete an Strang      4506
 Auswertung des Stranges:        10002

 Fiktiver Strang! Es erfolgt keine Abflussverzoegerung!
 Keine angeschlossenen Teilgebiete an Strang     10002

 ------------------------------------------------------------------
           Berechnung wurde ohne Fehler beendet!                   
 ------------------------------------------------------------------
