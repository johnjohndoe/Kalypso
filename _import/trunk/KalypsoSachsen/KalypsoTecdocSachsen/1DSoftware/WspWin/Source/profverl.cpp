/**************************************************************
profverl.cpp		14.05.97

  Profilverlängerung durch Auswahl einer neuen Profildatei
  und Angabe der Einfügeposition
  
**************************************************************/

#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global_vars.h"

#include "strang.h"

#include "typen.h"

#include "readprof.h"
#include "dis_prof.h"

#include "list.h"
#include "profverl.h"
#include "aufnehm.h"
#include <commdlg.h>
#include "wsplist.h"

#define  LINKS_VERL  1
#define  RECHTS_VERL 2

extern FILE_SPEC file_spec;
extern int       ds_info[TYPE_SIZE];// Dick 22.06.99
extern Scroller  scr;
extern MinMax    pmm;
extern BOOLEAN   SaveProfilFile;
extern WINDOW WIN_117;

extern WINDOW WIN120,  win_120[100];
extern WINDOW WIN_116, Edit_Win116[15];


int ExecProfilverlaengerung(WSP_PROFIL_LISTE *pWPL)
{
  FILE_SPEC fs_in;
  WSP_PROFIL_LISTE *tmpWPL;
  int verl =0;
  int anzahl;
  char* files = new char[AUFNEHM_BUFFER_SIZE];
  
  xvt_fsys_get_default_dir(&fs_in.dir);
  
  //Neu 12.05.98
  char buf[200],buf2[200],buf3[200],buf4[200];
  
  
  
  int back=auswahl2(files, 0, 40001L); //in aufnehm.cpp
  
  switch(back)
  {
  case IDOK:break;
  case IDCANCEL:return 0;
  default:
    xvt_res_get_str(STR_FASCHE_PROFAUSWAHL,buf,sizeof(buf));
    xvt_dm_post_error("%s",buf);
    //xvt_dm_post_error("Falsche Profilauswahl!");
    return 0;
  }
  // Ende Neu
  xvt_res_get_str(STR_ABBRECHEN,buf,sizeof(buf));
  xvt_res_get_str(STR_LINKS,buf2,sizeof(buf2));
  xvt_res_get_str(STR_RECHTS,buf3,sizeof(buf3));
  xvt_res_get_str(STR_VERL_ASK,buf4,sizeof(buf4));
  switch (xvt_dm_post_ask(buf,buf2,buf3,"%s",buf4))
    //switch (xvt_dm_post_ask("Abbrechen", "links","rechts",
    //      "Auf welcher Seite soll das Profil verlängert werden?")) 
  {
  case RESP_DEFAULT:
    return 0;
  case RESP_2:  // links
    verl = LINKS_VERL;
    break;
  case RESP_3:  // rechts
    verl = RECHTS_VERL;
    break;
  }
  
  
  pWPL=Init_Profil_Liste(pWPL);//neues WPL-Element anhängen
  if (pWPL->PListNext !=NULL)
    tmpWPL = pWPL->PListNext;
  
  SaveGlobalData(pWPL->data);
  
  xvt_fsys_set_dir(&fs_in.dir);
  strcpy(file_spec.name,files); // obsolet?
  if( !read_profildatei( tmpWPL, &STR_SPEC.dir, files ) )  // Verlängerungsprofil einlesen
  {
    SaveGlobalData(tmpWPL->data);
    GetGlobalData(pWPL->data);
    if (anzahl = pWPL->PList->ProfilVerl(tmpWPL,verl))
    {
      ds_info[1]= ds_info[1]+anzahl;
      SaveProfilFile = TRUE;
    }
  }
  
  Delete_Profil_Liste(pWPL->PListNext);
  pWPL->PListNext =NULL;
  
  scr.scrollpos =1;
  scr.datensatz =1;
  pWPL->PList->GetMinMax(&pmm,scr.datensatz);
  pWPL->PList->GetScrollDaten(&scr);
  if (WIN_116 != NULL_WIN)
  {
    display_prof116(&Edit_Win116[0]);
    xvt_dwin_invalidate_rect(WIN_117,0);
  }
  else if (WIN120 != NULL_WIN)
    display_prof120(&win_120[0]);
  
  return 1;
}



