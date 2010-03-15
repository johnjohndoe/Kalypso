/*****               file : dxf_edit.cpp  10.06.97           ************/
#include <windows.h>

#include "xvt.h"

#include "resource.h"

#include "dxf_edit.h"


typedef struct _ant2data
{
  char name[40],ort[40],res[40],jahr[40];
} ant2data; // nuer hier

static UINT dxf_name,dxf_ort,dxf_jahr,dxf_ortjahr;

// Vorwärtsdeklarationen

BOOL FAR PASCAL DlgAntragsteller(HWND hDlg,WORD msg,WPARAM wParam,LPARAM lParam);
int read_ant2_blk(ant2data *data);
int write_ant1_blk(ant2data *data);
int write_ant2_blk(ant2data *data);
int GetDxfLineDef(UINT *name,UINT *ort,UINT *jahr,UINT *ortjahr);


BOOL FAR PASCAL DlgAntragsteller(HWND hDlg,WORD msg,WPARAM wParam,LPARAM lParam)
{
 lParam = lParam;
 ant2data data;

 switch (msg)
  {
	case WM_INITDIALOG:
	  if (read_ant2_blk(&data))
	  {
		SetDlgItemText(hDlg,IDC_ANTRAGST,data.name);
		SetDlgItemText(hDlg,IDC_JAHR,data.jahr);
		SetDlgItemText(hDlg,IDC_ORT,data.ort);
	  }
	break;
	case WM_COMMAND:
	  switch(wParam)
		 {
		  case IDC_OK:
			 GetDlgItemText(hDlg,IDC_ANTRAGST,data.name,40);
			 GetDlgItemText(hDlg,IDC_JAHR,data.jahr,5);
			 GetDlgItemText(hDlg,IDC_ORT,data.ort,25);

       if (strlen(data.ort)>20)
       {
         char buf[200];
         xvt_res_get_str(STR_DXF_NOTE,buf,sizeof(buf));
         xvt_dm_post_note("%s",buf); // ("Eintrag 'Ort' darf nicht länger als 20 Zeichen sein!");
				 break;
				}
       write_ant1_blk(&data);
       write_ant2_blk(&data);
       EndDialog(hDlg,TRUE);
       return TRUE;
		  
      case IDC_CANCEL:
        EndDialog(hDlg,wParam);
        return TRUE;
		 }

	break;
	default: break;
  }
 return FALSE;
}
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

int EditAntragsteller(WINDOW parentWin)
{
  HWND hwndParent;

  if (!GetDxfLineDef(&dxf_name,&dxf_ort,&dxf_jahr,&dxf_ortjahr))return FALSE;

  if(hwndParent = (HWND)xvt_vobj_get_attr(parentWin,ATTR_NATIVE_WINDOW))
	 {
    HINSTANCE hInstance = GetModuleHandle( NULL );
	  DialogBox( hInstance, "DLG_ANTRAGSTELLER", hwndParent, (DLGPROC)DlgAntragsteller );
	  return TRUE;
	 }
	else return FALSE;
}
//////////////////////////////////////////////////////////////////////
int read_ant2_blk(ant2data *data)
{
 FILE *blk;
 char *ptr,*temp;
 char *ModPathName;
 char *lpBlk;
 int ret;
 unsigned long line =1;

 ModPathName = new char[256];
 lpBlk = new char[256];
 temp = new char[101];

 if ((GetModuleFileName( NULL,ModPathName,255))>0)
	{
	 ptr = strrchr(ModPathName,'\\');
	 if (ptr) ptr[1]='\0';
	}
 else {delete[] ModPathName;
		 delete[] temp;
		 delete[] lpBlk;
		 return FALSE;}

 strcpy(lpBlk,ModPathName);
 strcat(lpBlk,"prodxf\\ant1.blk");

 blk = fopen(lpBlk,"r");
 if (blk)
 {
  while (!feof(blk))
  {
	fgets(temp,100,blk);
	 if(line == dxf_name)
     {
	  if (ptr = strrchr(temp,'\n')) ptr[0]='\0';
	  strcpy(data->name,temp);
	 }
	 if(line == dxf_ort)
	 {
	 if (ptr =strrchr(temp,'\n')) ptr[0]='\0';
	 strcpy(data->ort,temp);
	 }
	 if(line == dxf_jahr)
	 {
	 if (ptr =strrchr(temp,'\n')) ptr[0]='\0';
	 if (ptr =strrchr(temp,'.')) ptr++;
		strcpy(data->jahr,ptr);
	 }
	 if(line == dxf_ortjahr)
	 {
	 if (ptr =strrchr(temp,'\n')) ptr[0]='\0';
	 strcpy(data->res,temp);
	 }
	line++;
  }
  ret = TRUE;
  fclose(blk);
 }
 else ret=FALSE;

 delete[] temp;
 return ret;
}

//////////////////////////////////////////////////////////////////////
int write_ant1_blk(ant2data *data)
{
 OFSTRUCT ofStr;
// HFILE hfFile;
 FILE *blk,*out;
 char *ptr,*temp;
 char *ModPathName;
 char *lpBlk,*lpBak;
 int ret;
 unsigned long line =1;

 ModPathName = new char[256];
 temp = new char[256];

 lpBlk = new char[256];
 lpBak = new char[256];


 if ((GetModuleFileName( NULL,ModPathName,255))>0)
	{
	 ptr = strrchr(ModPathName,'\\');
	 if (ptr) ptr[1]='\0';
	}
 else {delete[] ModPathName;
		 delete[] temp;
		 delete[] lpBlk;
		 delete[] lpBak;
		 return FALSE;}

 strcpy(lpBlk,ModPathName);
 strcpy(lpBak,ModPathName);
 strcat(lpBak,"prodxf\\ant1.bl~");
 strcat(lpBlk,"prodxf\\ant1.blk");


// if((hfFile = LZOpenFile(lpBak, &ofStr, OF_EXIST))==-1)  // test ob File "ant2.bl~" existiert
  {
	LZOpenFile(lpBak, &ofStr, OF_DELETE);  //löschen
  }
 if (rename(lpBlk,lpBak)==-1) return FALSE;


 blk = fopen(lpBak,"r");
 out = fopen(lpBlk,"w");
 if ((blk)&&(out))
 {
  while (!feof(blk))
  {
	fgets(temp,100,blk);
	if(line == dxf_name)
	 {
	 strcpy(temp,data->name);
	 strcat(temp,"\n");
	 }
	if(line == dxf_ort)
	 {
	 strcpy(temp,data->ort);
	 strcat(temp,"\n");
	 }
	if(line == dxf_jahr)
	 {
	 strcpy(temp,".................");
	 strcat(temp,data->jahr);
	 strcat(temp,"\n");
	 }
	if(line == dxf_ortjahr)
	 {
	 strcpy(temp,data->ort);
	 if (strlen(data->ort)>=10)
		 strcat(temp," den ......");
	 else
		 strcat(temp," den  ............   ");
	 strcat(temp,data->jahr);
	 strcat(temp,"\n");
	 }
	if (!feof(blk)) 	fputs(temp,out);
	line++;
  }
  ret = TRUE;
  fclose(blk);
  fclose(out);
  LZOpenFile(lpBak, &ofStr, OF_DELETE);  //löschen

 }
 else ret=FALSE;

 delete[] temp;
 delete[] ModPathName;
 return ret;
}

//////////////////////////////////////////////////////////////////////
int write_ant2_blk(ant2data *data)
{
 OFSTRUCT ofStr;
// HFILE hfFile;
 FILE *blk,*out;
 char *ptr,*temp;
 char *ModPathName;
 char *lpBlk,*lpBak;
 int ret;
 unsigned long line =1;

 ModPathName = new char[256];
 temp = new char[256];

 lpBlk = new char[256];
 lpBak = new char[256];


 if ((GetModuleFileName( NULL,ModPathName,255))>0)
	{
	 ptr = strrchr(ModPathName,'\\');
	 if (ptr) ptr[1]='\0';
	}
 else {delete[] ModPathName;
		 delete[] temp;
		 delete[] lpBlk;
		 delete[] lpBak;
		 return FALSE;}

 strcpy(lpBlk,ModPathName);
 strcpy(lpBak,ModPathName);
 strcat(lpBak,"prodxf\\ant2.bl~");
 strcat(lpBlk,"prodxf\\ant2.blk");


// if((hfFile = LZOpenFile(lpBak, &ofStr, OF_EXIST))==-1)  // test ob File "ant2.bl~" existiert
  {
	LZOpenFile(lpBak, &ofStr, OF_DELETE);  //löschen
  }
 if (rename(lpBlk,lpBak)==-1) return FALSE;


 blk = fopen(lpBak,"r");
 out = fopen(lpBlk,"w");
 if ((blk)&&(out))
 {
  while (!feof(blk))
  {
	fgets(temp,100,blk);
    if(line == dxf_name)
	{
	 strcpy(temp,data->name);
	 strcat(temp,"\n");
	}
	if(line == dxf_ort)
	{
	 strcpy(temp,data->ort);
	 strcat(temp,"\n");
	}
	if(line == dxf_jahr)
	{
	 strcpy(temp,".................");
	 strcat(temp,data->jahr);
	 strcat(temp,"\n");
	}
	if(line == dxf_ortjahr)
	{
	 strcpy(temp,data->ort);
	 if (strlen(data->ort)>=10)
		 strcat(temp," den ......");
	 else
		 strcat(temp," den  ............   ");
	 strcat(temp,data->jahr);
	 strcat(temp,"\n");
	}
	if (!feof(blk)) 	fputs(temp,out);
	line++;
  }
  ret = TRUE;
  fclose(blk);
  fclose(out);
  LZOpenFile(lpBak, &ofStr, OF_DELETE);  //löschen
 }
 else ret=FALSE;

 delete[] temp;
 delete[] ModPathName;
 return ret;
}
//////////////////////////////////////////////////////////////////////
int GetDxfLineDef(UINT *name,UINT *ort,UINT *jahr,UINT *ortjahr)
{
 FILE *in;
 char *ptr,*temp;
 char *ModPathName;

 ModPathName = new char[256];
 temp = new char[256];

 if ((GetModuleFileName( NULL,ModPathName,255))>0)
 {
	 ptr = strrchr(ModPathName,'\\');
	 if (ptr) ptr[1]='\0';
	 strcat(ModPathName,"ant.ini");
	 if ((in = fopen(ModPathName,"r"))!=NULL)
	    {
		 fscanf(in,"%d",name);
		 fscanf(in,"%d",ort);
		 fscanf(in,"%d",jahr);
		 fscanf(in,"%d",ortjahr);
		 fclose(in);
     delete[] ModPathName;
     delete[] temp;
     return TRUE;
   }
   else 
   {
     delete[] ModPathName;
	   delete[] temp;
     return FALSE;
   }
 }
 else 
 {
   delete[] ModPathName;
   delete[] temp;
   return FALSE;
 }
}

//////////////////////////////////////////////////////////////////////
