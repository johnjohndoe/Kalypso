//////////////////////////////////////
// Globale Variablen für WspWin.exe //
//////////////////////////////////////

#ifndef _GLOBAL_VARS_H_
#define _GLOBAL_VARS_H_

#include "global_types.h"

class List;
class Paint;

// _GLOBAL_VARS_CPP_ :
// nur in der global_vars.cpp liegt jeweil die echte (einzige) Kopie der globalen Variable
// alle anderen .cpp's ( d.h. .obj's ) greifen GlobalVar auf dieselbe Variable zu
// Vorsicht: die Variablen dürfen nur in der global_vars.cpp initialisiert werden -> sonst Linker - Fehler

#ifdef _GLOBAL_VARS_CPP_
  #define GlobalVar
  #define InitGlobal( a )  = a
#else
  #define GlobalVar extern
  #define InitGlobal( a )
#endif //_GLOBAL_VARS_CPP_

// Kommentare: Auftreten der Globalen Variablen. *: hier wurde die Variable ursprünglich definiert

GlobalVar BOOLEAN war_schon;  // in Aufnehm.cpp* und Verzweig.cpp
GlobalVar BOOLEAN interp_prog InitGlobal( FALSE );  // in Aufnehm.cpp und wspdde.cpp*
GlobalVar char* lpszData;  // in aufnehm.cpp und wspdde.cpp*
GlobalVar BOOLEAN zustand_kopieren;  // in Profpro2.cpp, Util2.cpp, Verzweig.cpp, wspm001.cpp*, Aufnehm.cpp
GlobalVar SLIST profil_list; // in Lese_str.cpp, Util2.cpp, Aufnehm.cpp, Read.cpp*
GlobalVar BOOLEAN profil_aufnehmen InitGlobal( FALSE ); // in Profproj.cpp*, Aufnehm.cpp, wspd136.cpp
GlobalVar BOOLEAN kopiert; // in Profproj.cpp*, Aufnehm.cpp, wspd136.cpp
GlobalVar char uebergabe_name[15]; // in Aufnehm.cpp, Strang.cpp, Strang2.cpp, wspd136.cpp, wspw120.cpp*, waspila.cpp
GlobalVar BOOLEAN abbruch208; // in Aufnehm.cpp, wspd136, wsp208.cpp*, wspd211.cpp, 
GlobalVar BOOLEAN neukopieren; // in Profpro2.cpp, Aufnehm.cpp, wsp136.cpp*, wspd208.cpp
GlobalVar char zustand[20]; // profpro2.cpp, profpro.cpp, aufnehm.cpp, util.cpp, wspd136.cpp, wspd166.cpp, 
                            // wspd208.cpp*, wspw116.cpp, wspw120.cpp, waspila.cpp
GlobalVar BOOLEAN vergleich; // profproj.cpp*, wspm001.cpp, aufnehm.cpp, wspd136.cpp, wspd208.cpp, wspw120.cpp,
                              // waspila.cpp
GlobalVar char dateiname[15]; // in Profproj2.cpp, profproj.cpp, aufnehm.cpp, readprof.cpp, util.cpp*, wspd138.cpp,
                              // wspd136.cpp, wspw120.cpp, waspila.cpp
GlobalVar BOOLEAN istverzweigt; // in Verzwig.cpp*, aufnehm.cpp, wspd136.cpp, wspd208, wspw120.cpp
GlobalVar char vzk[20]; // in Profpro2.cpp, profproj.cpp, Aufnehm.cpp, strang.cpp, strang2.cpp, util.cpp, 
                        // wspd136, wspd166.cpp, wspd208.cpp*, wspw116.cpp, wspw120.cpp, waspila.cpp
GlobalVar char pk[20]; // Profpro2.cpp, propfproj.cpp, Aufnehm.cpp, strang.cpp, strang2.cpp, util.cpp, 
                        // wspd136.cpp, wspd166.cpp, wspd208.cpp*, wspw116.cpp, wspw120.cpp, waspila.cpp
GlobalVar char station208[20]; // in Propfpro2.cpp, propfproj.cpp, Aufnehm.cpp, strang.cpp, strang2.cpp, 
                                // util.cpp, wspd136.cpp, wspd208.cpp*, wspd211.cpp, wspw120.cpp, waspila.cpp
GlobalVar BOOLEAN nicht_posten InitGlobal( FALSE ); // Aufnehm.cpp*, profproj.cpp
GlobalVar SLIST verzweigt_new; // verzweig.cpp*, Aufnehm.cpp
GlobalVar WINDOW dlg_136 InitGlobal( NULL_WIN ); // in wspd136.cpp*, global.h, wspm001.cpp, aufnehm.cpp, flaeche.cpp, 
                                      // read.cpp, util.cpp, wspd162.cpp, wspd166.cpp, wspw116.cpp, wspw120.cpp, 
                                      // wspw135.cpp, wspw136.cpp, wspdde.cpp
GlobalVar WINDOW lwin; // in aufnehm.cpp, readprof.cpp, wspd136.cpp*, wspd166.cpp, wspw120.cpp, 
GlobalVar int scroll_position; // in aufnehm.cpp, util.cpp, wspd136.cpp*, wspw120.cpp
GlobalVar char profil_nr_string[8]; // in Profpro2.cpp*, Aufnehm.cpp, wspd136.cpp, wspd208.cpp, wspw120.cpp,
                                    // waspila.cpp
GlobalVar int dlg136_select;      // Aufnehm.cpp*, wspd166.cpp
GlobalVar STRANG* strang_ptr InitGlobal( NULL ); // in Init.h*
GlobalVar STRANG* strang_anfang InitGlobal( NULL ); // in Init.h*
GlobalVar STRANG* strang_ende InitGlobal( NULL ); // in Init.h*

// alles aus Init.h

GlobalVar int dialog_ende InitGlobal( -1 );    // ID für beendeten Dialog Button-Nr

GlobalVar double screen_scale;     //Skalierungsfaktor für Fenstergrösse

GlobalVar WINDOW Menu_Win InitGlobal( NULL_WIN );    // ID für Fenster  :Menü
GlobalVar WINDOW  WIN_116 InitGlobal( NULL_WIN );
GlobalVar WINDOW  WIN_117 InitGlobal( NULL_WIN );
GlobalVar WINDOW WIN120 InitGlobal(NULL_WIN );

GlobalVar int anzahl_profil_dat_entries InitGlobal( 0 ); // Anzahl der *.prf -Dateien in WSP.CFG
GlobalVar int anzahl_str_dat_entries InitGlobal( 0 );    // tatsächliche Anzahl der *.str -Dateien in WSP.CFG
GlobalVar int anzahl_strang_entries InitGlobal( 0 );     // Anzahl der Einträge in der Strangtabelle
GlobalVar int anzahl_profile_abs InitGlobal( 0 );         //Anzahl/Zähler aller Profildateien
GlobalVar int anzahl_str_dat_entries_abs InitGlobal( 0 ); // absolute Anzahl der *.str-Dateien in der WSP.CFG

GlobalVar BOOLEAN gen_new_SLIST,
		  is_Entry_in_SLIST ,
		  new_profil,
		  choice_neu,
		  new_strang;

GlobalVar BOOLEAN SaveStrangFile InitGlobal( FALSE );
GlobalVar BOOLEAN SaveProfilFile InitGlobal( FALSE );
GlobalVar BOOLEAN SaveNetzFile InitGlobal( FALSE );

GlobalVar BOOLEAN is_profil_open InitGlobal( FALSE );
GlobalVar BOOLEAN is_win_121_open InitGlobal( FALSE );

GlobalVar char    *profildatei;        // Name der ausgewählten Profildatei
GlobalVar char    str_netz[100];           // Name der ausgewählten/neuen .str-Datei
GlobalVar char    netz_dat[3][16];        //Gewässername,Datum,Zustand aus dlg128

GlobalVar double  cfg_anfangsprofil InitGlobal( 0.0 );  //Anfangs-Endprofil in der wsp.cfg-Datei
GlobalVar double  cfg_endprofil InitGlobal( 0.0 );    // aus: Profiltabelle


GlobalVar OPENFILENAME ofn;
GlobalVar FILE_SPEC    file_spec,
				 STR_SPEC,
				 PROJ_SPEC;

GlobalVar SLIST     header,
			 proj_list;          // Liste der Projektverzeichnisse
GlobalVar SLIST cfg_list InitGlobal( NULL );  //enthält *.str-Dateien /Anzeige in dlg135
GlobalVar SLIST prof_datei InitGlobal( NULL );
GlobalVar SLIST strangtabelle, archiv_list;//Dick 17.12.98 Liste der Archiv-Projektverzeichnisse
GlobalVar BOOLEAN IS_COMMENT ;    //ist ein Kommentar als Datenblocktyp vorhanden
// Projekt
GlobalVar char* WSP_START_VERZEICHNIS;
GlobalVar BOOLEAN  is_projekt_neu InitGlobal( FALSE );

GlobalVar int cfg_list_index InitGlobal( -1 );  //Position der ausgewählten *.str-Datei aus der SLIST:cfg_list
GlobalVar int ds[50];
GlobalVar int ds_info[TYPE_SIZE], anzahl_ds;
GlobalVar int     typ[TYPE_SIZE];

GlobalVar List* list;   // ->class: list.h
GlobalVar Paint* paint; // ->class: paint.h

GlobalVar char* start_dir;
GlobalVar char* Projektname_aktuell,*Gewaessername_aktuell,*Zustand_aktuell;

GlobalVar char Plot_Ini_Datei[151];


// andere

GlobalVar XVT_HELP_INFO hi InitGlobal( NULL_HELP_INFO ); // in wspwin.cpp*
GlobalVar QWertDatei* ptr_qwert; // in Qwert.cpp*
GlobalVar QWertDatei* ptr_qwert_anfang; // in Qwert.cpp*
GlobalVar QWertDatei* ptr_qwert_ende; // in Qwert.cpp*
GlobalVar SLIST abflussereignisse InitGlobal( NULL ); // in Qwert.cpp*
GlobalVar WINDOW win122; // in Wspw122.cpp*
GlobalVar BOOLEAN teilgebiete InitGlobal( FALSE ); //Dick 2.06.99 erst mal nicht als extern (für Kalinin-Milukow) // in Wspd201.cpp*
GlobalVar BOOLEAN change_strang InitGlobal( FALSE ); // in QWert.cpp*
GlobalVar FILE_SPEC qwert_spec; // in QWert.cpp*
GlobalVar FILE_SPEC wsfix_spec; // in QWert.cpp*
GlobalVar FILE* qwert_datei; // in QWert.cpp*
GlobalVar FILE* wsfix_datei; // in QWert.cpp*
GlobalVar BOOLEAN exist_wsf_wert; // in Wspw122.cpp*, Qwert.cpp
GlobalVar BOOLEAN exist_qwert; // Wspw122.cpp*
GlobalVar Scroller scr; // in Wspd137.cpp*
GlobalVar int anzahl_elem; // in Wspw122.cpp*
GlobalVar SLIST list_ende; // Read.cpp*
GlobalVar SLIST list_anfang; // Read.cpp*
GlobalVar COLOR WspwinMainBackgroundColor; //globale Hintergrunndfarbe für alle Fenster // wspwin.cpp
GlobalVar int fehler InitGlobal( 0 ); // in Wspwin.cpp*
GlobalVar BOOLEAN qwert_fehlt;  // in QWert.cpp*

GlobalVar WINDOW dlg136_edit[30]; // in wpsdlg136.cpp*, strang.cpp
GlobalVar char name_anfang[5][15]; // in wspd136.cpp*, Wspm001.cpp, strang.cpp
GlobalVar char name_ende[5][15];   // in strang.cpp, wspd136.cpp*, wspm001.cpputil.cpp, 
GlobalVar SLIST header_profil;    // in plot.cpp, plot101, readprof.cpp*, wspd128.cpp, wsplist.cpp, wspw116, 
                                  // wspw120.cpp, wspw121.cpp, waspila.cpp, 
GlobalVar SLIST slist_comment InitGlobal( NULL );  // in flaeche.cpp, list.cpp, readprof.cpp, util.cpp*, 
                                                   // wspd166.cpp, wsplist.cpp, wspw116.cpp, wspw120.cpp
GlobalVar int exist_plot InitGlobal( FALSE ); // enthält Profildatei Plotterdatensatz am Ende
                                              // in readprof.cpp*, util.cpp?, wsplist.cpp, wspw116.cpp

GlobalVar bool LWA_PROJEKT InitGlobal( true ); // Flag, ob das aktuelle Projekt ein LWA oder BCE projekt ist

#endif // _GLOBAL_VARS_H_
