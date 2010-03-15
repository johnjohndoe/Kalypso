#include <windows.h>
#include "xvt.h"

#include "resource.h"


#include "global_types.h"

#include "error1.h"

/*Funktionsprototyp*/

#define ID_PROF1	 100   // Edit--Felder
#define ID_PROF2	 200
#define ID_STATION 300
#define ID_MSG     400


typedef struct _TEXT
  {
	  char *name1,
			 *name2,
			 *station;
     short zustand;
  }TEXT;


TEXT *error_msg;


/* DlgError1 : Funktion zur Bearbeitung des Infodialoges */
BOOL DlgError1(HWND hDlg, UINT msg,WPARAM wParam, LPARAM lParam)
{
  switch(msg)
  {
	case WM_INITDIALOG :
	  {
       char buf[200];//Dick 11.01.00
		if (error_msg)
		  {
			switch (error_msg->zustand)
			 {
			  case 1:
                  xvt_res_get_str(STR_ERRORBOX_1,buf,sizeof(buf));
				  SetDlgItemText(hDlg, ID_MSG,buf);
                  //SetDlgItemText(hDlg, ID_MSG,"In folgenden Profilen stimmen die ersten Profilpunkte nicht überein:");
				  break;
			  case 2:
                  xvt_res_get_str(STR_ERRORBOX_2,buf,sizeof(buf));
				  SetDlgItemText(hDlg, ID_MSG,buf);
                  //SetDlgItemText(hDlg, ID_MSG,"In folgenden Profilen stimmen die letzten Profilpunkte nicht überein:");
				  break;
			  case 3:
                  xvt_res_get_str(STR_ERRORBOX_3,buf,sizeof(buf));
				  SetDlgItemText(hDlg, ID_MSG,buf);
                  //SetDlgItemText(hDlg, ID_MSG,"In folgenden Profilen existiert keine Geländehöhe:");
				  break;
			  default:break;
			 };
			SetDlgItemText(hDlg, ID_PROF1, error_msg->name1);
			SetDlgItemText(hDlg, ID_PROF2, error_msg->name2);
			SetDlgItemText(hDlg, ID_STATION, error_msg->station);
		  }
		return TRUE;
	  }
	case WM_COMMAND :
		switch(wParam)
		{
			case IDOK:
				EndDialog(hDlg, TRUE);
				return TRUE;
		}
  };
  return FALSE;
}

/*******************************************************************/
/*******************************************************************/
void Display_Errorbox1(char *name1,char *name2,double station,int zustand)
{
  DLGPROC lpDlgProc;

  error_msg = new TEXT;
  error_msg->name1  =new char[15];
  error_msg->name2  =new char[15];
  error_msg->station=new char[15];

  strcpy(error_msg->name1,name1);
  strcpy(error_msg->name2,name2);
  gcvt(station,6,error_msg->station);
  error_msg->zustand = zustand;

  HINSTANCE hInstance = GetModuleHandle( NULL );

  DialogBox( hInstance, "ERRORBOX1", NULL, (DLGPROC)DlgError1 );
  FreeProcInstance(lpDlgProc);
  delete  error_msg->name1;
  delete  error_msg->name2;
  delete  error_msg->station;
  delete  error_msg;

}
/*******************************************************************/
