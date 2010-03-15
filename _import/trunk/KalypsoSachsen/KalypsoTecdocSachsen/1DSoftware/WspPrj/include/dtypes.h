// DTypes.h

/********************************/
/*  Querprofil Datensaetzetypen */
/********************************/
#define DST_UNKNOWN				-4	// unbekannter/anderer Datensatz
#define DST_COMMENT				-3	// Kommentar in Profildatei
#define DST_GAUSSRUECK		-2	// Gauss-Profil mit Rueckspruengen
#define DST_FUELLHOEHE		-1	// Fuellhoehen-Profil
#define DST_NAU_MED				 0	// Naudascher-Medlarz

#define DST_GELAENDEHOEHE		1	// Gelaendehoehe NN+m
#define DST_TRENNFLAECHEN		2	// Trennflaechen
#define DST_RAUHIGKEIT_KST		3	// Rauhigkeit KST
#define DST_RAUHIGKEIT			4	// Rauhigkeit k-s [m]
#define DST_DURCHST_BEREICH		5	// Durchstroemte Bereiche
#define DST_UK_BRUECKE			6	// UK-Bruecke
#define DST_OK_BRUECKE			7	// OK-Bruecke
#define DST_AXM					8	// AX m
#define DST_AYM					9	// AY m
#define DST_DPM					10	// DP m
#define DST_OK_WEHRS			11	// OK-Wehr
#define DST_GELAENDE2			12	// 2. Gelaendehoehe
#define DST_FLAECHE				13	// Flaeche
#define DST_KREISSEGM			14	// Kreissegment
#define DST_SVA_WERT			15
#define DST_OK_GELAENDE			16	// Gelaende ueber Sonderprofil
#define DST_KASTEN				17	// Kastenprofil
#define DST_LWA_FELDER			18	// U-Werte bei Sonderprofil Wehr
#define DST_GAUSS				19	// Gauss-Krueger-Koordinaten
#define DST_RECHTSWERT			20	// Rechtswertkoordinate
#define DST_HOCHWERT			21	// Hochwertkoordinate
#define DST_PUNKT_NR			22
#define DST_WSP_HOEHE			23	// Wasserspiegelhoehe
#define DST_WASSERSP1			24	// Wasserspiegel NN+m HQ1	-Best
#define DST_WASSERSP100			25	// Wasserspiegel NN+m HQ100	-Best	
#define DST_WASSERSP5			26	// Wasserspiegel NN+m HQ5	-Best
#define DST_BORDVOLL			27	// Bordvollpunkte
#define DST_TRENN_WEHR			28	// Trennlinie Wehr
// Sonderprofile
#define DST_MAUL				29	// Maulprofil
#define DST_EIPROFIL			30	// Eiprofil
#define DST_KREIS				31	// Kreisprofil
#define DST_ARMCO84				32	// Sonderprofil LWA
#define DST_ARMCO71				33	// Sonderprofil LWA
#define DST_NWRINNE				34	// Niedrigwasserrinne
#define DST_TRAPEZ				35	// Trapezprofil
/********************************/
/*Laengsschnitt Datensaetzetypen*/
/********************************/
#define DST_SOHLHOEHE			36
#define DST_LAENGE				37
#define DST_ABFLUSS				38
#define DST_WASSERSPIEGEL		39
#define DST_WSP_BREITE			40
#define DST_BOESCHUNG_LINKS		41
#define DST_BOESCHUNG_RECHTS	42
#define DST_PROFILART			43
#define DST_VZKENNG				44
#define DST_PROFILKENNG			45
#define DST_DKUK				46
#define DST_DKOK				47
#define DST_BAUWERK				48
#define DST_GEFAELLE			49
#define DST_VMITTEL				    50
#define DST_BVHOEHE				    51
#define DST_SOHLHOEHE_2			  52
#define DST_WASSERSPIEGEL_2		53
#define DST_BOESCHUNG_LINKS_2	54
#define DST_BOESCHUNG_RECHTS_2	55
#define DST_STATION				    56	// Station

// Neue Datensaetze
#define DST_WSP_FIXIERUNG		  57	// Laengsschnitt
#define DST_WSP_MESSUNG			  58	// Profil
#define DST_RECHTSWERT_2		  59	// 2.Rechtswert
#define DST_HOCHWERT_2			  60	// 2.Hochwert
#define DST_RE_BOESCHUNG_RE		61	// Rechtswert für Boeschung-Rechts
#define DST_HO_BOESCHUNG_RE		62	// Hochwert für Boeschung-Rechts
#define DST_RE_BOESCHUNG_LI		63	// Rechtswert für Boeschung-Links
#define DST_HO_BOESCHUNG_LI		64	// Hochwert für Boeschung-Links
#define DST_RE_BOESCHUNG_RE_2	65	// Rechtswert für 2.Boeschung-Rechts
#define DST_HO_BOESCHUNG_RE_2	66	// Hochwert für 2.Boeschung-Rechts
#define DST_RE_BOESCHUNG_LI_2	67	// Rechtswert für 2.Boeschung-Links
#define DST_HO_BOESCHUNG_LI_2	68	// Hochwert für 2.Boeschung-Links
#define DST_DEICH_RECHTS		  69	// Deich-Rechts
#define DST_DEICH_LINKS			  70	// Deich-Links
#define DST_DEICH_RECHTS_2		71	// 2.Deich-Rechts
#define DST_DEICH_LINKS_2		  72	// 2.Deich-Links
#define DST_RE_DEICH_RE			  73	// Rechtswert für Deich-Rechts
#define DST_HO_DEICH_RE			  74	// Hochwert für Deich-Rechts
#define DST_RE_DEICH_LI			  75	// Rechtswert für Deich-Links
#define DST_HO_DEICH_LI			  76	// Hochwert für Deich-Links
#define DST_RE_DEICH_RE_2		  77	// Rechtswert für 2.Deich-Rechts
#define DST_HO_DEICH_RE_2		  78	// Hochwert für 2.Deich-Rechts
#define DST_RE_DEICH_LI_2		  79	// Rechtswert für 2.Deich-Links
#define DST_HO_DEICH_LI_2		  80	// Hochwert für 2.Deich-Links
#define DST_SCHLEPPSPANN		  81
#define DST_AUSUFERUNG_LI		  82
#define DST_AUSUFERUNG_RE		  83
#define DST_ENERGIEHOEHE		  84
#define DST_BUHNEN            85  // Buhnen je drei Koordinaten, 2.Namenszeile beinhaltet Information über rechte/linke Buhne
#define DST_BOESCHUNGSKANTEN  86  
#define DST_WSP_DIFFERENZ     87  // Längsschnitt Differenz zwischen gemessenen und berechneten Wasserspiegel
#define DST_MODELLGRENZEN     88  // Aussengrenzen des Retentionsgebietes ( vor allem für GIDHAM Hyk - Import/Export
#define DST_LP_TEXT				    89
#define DST_POL_GRENZEN		    90

#define N_DSTYPES_MIN		-3
#define N_DSTYPES				91
