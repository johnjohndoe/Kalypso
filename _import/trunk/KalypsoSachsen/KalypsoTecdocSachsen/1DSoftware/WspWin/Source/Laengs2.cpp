#include <windows.h>

#include <math.h>
#include "xvt.h"
#include "resource.h"

#include "global_types.h"

#include "list.h"
#include "readprof.h"
#include "l_typen.h"
#include "readprof.h"
#include "wsplist.h"

extern FILE_SPEC STR_SPEC;
extern BOOLEAN SaveProfilFile;


int MakeLaengsprofilVerknuepfung(WSP_PROFIL_LISTE *pWPL,const char *lpFile)
/*
		2 Längsprofile miteinander verknüpfen(ineinander kopieren):

		- WSP_PROFIL_LISTE *pWPL : Zeiger aus Hauptprogramm(1.Längsschnitt)
		- lpFile : Pfad/Dateiname 2.Längsschnittdatei
*/
{
 char *lpStr,*tmp;
 WSP_PROFIL_LISTE *tmpWPL;

 if ((pWPL==NULL)||(lpFile==NULL ))  return FALSE;

 xvt_scr_set_busy_cursor();

 /*aktuelle Daten sichern*/
 SaveGlobalData(pWPL->data);

 pWPL=Init_Profil_Liste(pWPL);//neues WPL-Element anhängen
 tmpWPL = pWPL->PListNext;
 
 if (tmpWPL->PList!=NULL)
	 tmpWPL->PList->DeleteKoord(0);

 if (tmpWPL->data == NULL)
  {
	 //xvt_dm_post_note("Fehler: zu wenig Speicher in laengs2.cpp");
   char buf[200];
   xvt_res_get_str(STR_MEM_OUT_2,buf,sizeof(buf));
   xvt_dm_post_note("%s",buf);
	 Delete_Profil_Liste(tmpWPL);
    pWPL->PListNext = NULL;
	 return FALSE;
  }
 tmp= new char[strlen(lpFile)+1];
 strcpy(tmp,lpFile);
 lpStr = strrchr(tmp,'\\');
 if (lpStr!=NULL)
 {
  lpStr[0]='\0'; //jetzt zeigt lpStr auf den Dateinamen
  lpStr++;
  strcpy(tmpWPL->data->file.name,lpStr);
  xvt_fsys_convert_str_to_dir(tmp,&tmpWPL->data->file.dir);
 }
 else   //Dick 6.08.98 sonst Absturz
 {
   delete tmp;
   return 0;
 } 
 delete tmp;
 if (!read_profildatei( tmpWPL, &tmpWPL->data->file.dir, tmpWPL->data->file.name, FALSE ))//Dick 
 {   //kein Fehler
   pWPL->PList->ConcatDatensatz(pWPL);
   // jetzt noch ds_info + typ-Liste aktualisieren;

   ds_save_anzahl( pWPL->data );
   
   SaveProfilFile = TRUE;//Dick 26.10.98 anstatt Lsave_profildatei(pWPL);
   Delete_Profil_Liste(tmpWPL);
   pWPL->PListNext = NULL;
   
   GetGlobalData(pWPL->data);
 }
 else return 0; // Lesefehler in Lread_profildatei
 return 1;
}


