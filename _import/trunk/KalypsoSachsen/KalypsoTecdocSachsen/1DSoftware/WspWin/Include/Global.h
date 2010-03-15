/****************************************************************************
*             GLOBAL.H                                                      *
*             06.01.1995                                                    *
****************************************************************************/
#ifndef  GLOBAL_H
#define  GLOBAL_H

#ifndef NO_STD_HELP_MENU
#define NO_STD_HELP_MENU
#endif
#ifndef NO_HELP_RESOURCES
#define NO_HELP_RESOURCES
#endif

class Paint;
class List;

extern int dialog_ende;    // ID f�r beendeten Dialog Button-Nr

extern double screen_scale;     //Skalierungsfaktor f�r Fenstergr�sse

extern WINDOW Menu_Win,    // ID f�r Fenster  :Men�
				  WIN_116,
				  WIN_117,
				  WIN120;
//				  dlg_136;   // ID f�r Dialog 136
extern BOOLEAN gen_new_SLIST;
extern BOOLEAN is_Entry_in_SLIST ;
extern BOOLEAN new_profil,
					choice_neu,
					new_strang;

extern BOOLEAN SaveStrangFile,
					SaveProfilFile,
					SaveNetzFile;
extern BOOLEAN is_profil_open;

extern int anzahl_profil_dat_entries,  // Anzahl der Profildateien in einer str-Datei
			  anzahl_strang_entries,      // Anzahl  Eintr�ge in der Strangtabelle
			  anzahl_str_dat_entries,    // Anzahl der *.str-Dateien
			  anzahl_profile_abs,        //Anzahl/Z�hler aller Profildateien
			  anzahl_str_dat_entries_abs; // absolute Anzahl der *.str-Dateien in der WSP.CFG

extern int     ds_info[TYPE_SIZE],//Dick 22.06.99
					anzahl_ds;
extern int     typ[TYPE_SIZE];//Dick 8.12.98
extern char    *profildatei;        // Name der ausgew�hlten Profildatei
extern char    str_netz[100];       // Name der ausgew�hlten/neuen .str-Datei
extern char    netz_dat[3][16];     //Gew�ssername,Datum,Zustand aus dlg128
extern double cfg_anfangsprofil,    //Anfangs-Endprofil in der wsp.cfg-Datei
				  cfg_endprofil    ;    // aus: Profiltabelle

extern int cfg_list_index;  //Position der ausgew�hlten *.str-Datei
									 //aus der SLIST:cfg_list
extern OPENFILENAME ofn;
extern FILE_SPEC file_spec,
					  STR_SPEC,
					  PROJ_SPEC;

extern SLIST header,
				 proj_list,
				 cfg_list,
				 prof_datei,
				 strangtabelle,
                 archiv_list;//Dick 17.12.98 Liste der Archiv-Projektverzeichnisse
extern List *list;    // ->list.h
extern Paint *paint;
extern Scroller scr; //->list.h
extern STRANG *strang_ptr, *strang_anfang, *strang_ende;
extern BOOLEAN IS_COMMENT ;    //ist ein Kommentar als Datenblocktyp vorhanden

// Projekt
extern char *WSP_START_VERZEICHNIS;
extern BOOLEAN  is_projekt_neu;
extern char *start_dir;

extern char Plot_Ini_Datei[151];

#endif
