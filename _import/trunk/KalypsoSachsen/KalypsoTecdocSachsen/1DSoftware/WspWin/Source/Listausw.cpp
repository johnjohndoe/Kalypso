#include <windows.h>
#include <process.h>
#include <errno.h>
#include "xvt.h"
#include "wspwin.h"
#include "listausw.h"
#include "resource.h"

#include "..\..\wspdlg\Include\export.h"


HANDLE hProc_Auswert = NULL;
extern BOOL bBreakBerechnung;
extern WINDOW main_win;      
extern char *start_dir;
extern FILE_SPEC STR_SPEC, ber_spec;

// vorwärtsdeklarationen
int WriteWspListHeader(FILE *f, char *BerVarainte); // hier

int WriteWsplistenDatei(char *BerVariante)
{
  char *tmp;
  char * path, *name;
  
  tmp=new char[255];
  path= new char [255];
  name=new char [15];
  char tmp2[MAX_PATH];
  FILE *WspList,*WspList_neu;
  
  if (strlen(BerVariante) >60)
  {
    //xvt_dm_post_error("Längs der Berechnungsvarainte (incl.Pfad) zu lang!");
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_LISTAUSW_NOTE,buf,sizeof(buf));
    xvt_dm_post_note("%s",buf);
    return FALSE;
  }
  strcpy(tmp,start_dir);
  strcat(tmp,"\\wsp.lis");
  if((WspList=fopen(tmp,"w+"))==NULL)
  {
    delete[] tmp;
    delete[] path;
    delete[] name;
    
    return FALSE;
  }
  strcpy(tmp2,start_dir);
  strcat(tmp2,"\\wsplist.ctr");
  if((WspList_neu=fopen(tmp2,"w+"))==NULL)
  {
    delete[] tmp;
    delete[] path;
    delete[] name;
    
    return FALSE;
  }
	 if(!WriteWspListHeader(WspList, BerVariante))
	  {
     char buf[200];//Dick 26.11.99
     xvt_res_get_str(STR_LISTAUSW_NOTE2,buf,sizeof(buf));
     xvt_dm_post_note("%s",buf);
     //xvt_dm_post_note("Fehler: Kann WspListenkopf nicht schrieben");
     fclose(WspList);
     fclose(WspList_neu);
     delete[] tmp;
     delete[] path;
     delete[] name;
     return FALSE;
	  }
   if(!WriteWspListHeader(WspList_neu, BerVariante))
	  {
     char buf[200];//Dick 26.11.99
     xvt_res_get_str(STR_LISTAUSW_NOTE2,buf,sizeof(buf));
     xvt_dm_post_note("%s",buf);
     //xvt_dm_post_note("Fehler: Kann WspListenkopf nicht schrieben");
     fclose(WspList);
     fclose(WspList_neu);
     delete[] tmp;
     delete[] path;
     delete[] name;
     return FALSE;
	  }
   fclose(WspList);
   fclose(WspList_neu);
   
   delete[] tmp;
   delete[] path;
   delete[] name;
   return TRUE;
   
}

/*************************************************/
int WriteWspListHeader (FILE *f, char *BerVariante)
{
  char *line,*tmp,*path,*name,*p;
  
  if (f==NULL) return FALSE;
  
  line = new char[255];
  tmp = new char[255];
  path = new char[255];
  name = new char[15];
  
  strcpy(tmp,BerVariante); p = strrchr(tmp,'\\');p++; strcpy(name,p);
  
  strcpy(path,BerVariante);
  p = strrchr(path,'\\');
  if (p) p[0]='\0';
  
  memset(line,SPACE,255);
  strcpy(tmp,BerVariante); p = strrchr(tmp,'\\');p++; strcpy(name,p);
  name[2]='w';
  name[3]='s';
  strcpy(line,"Datei1  :");
  strcat(line,path);
  strcat(line,"\\");
  strcat(line,name);
  fprintf( f ,line);
  fprintf( f ,"\n");
  
  memset(line,SPACE,255);
  strcpy(tmp,BerVariante); p = strrchr(tmp,'\\');p++; strcpy(name,p);
  name[2]='e';
  name[3]='r';
  strcpy(line,"Datei2  :");
  strcat(line,path);
  strcat(line,"\\");
  strcat(line,name);
  fprintf( f ,line);
  fprintf( f ,"\n");
  
  memset(line,SPACE,255);
  strcpy(tmp,BerVariante); p = strrchr(tmp,'\\'); p++;strcpy(name,p);
  int len=strlen(name);
  name[len-4]='\0';
  
  strcpy(line,"Datei   :");
  
  // strcat(line,path);     //pfad in version 1 von wsplist nicht bei ausgabe
  // strcat(line,"\\");     "
  strcat(line,name);
  
  fprintf( f ,line);
  fprintf( f ,"\n");
  
  
  memset(line,SPACE,255);
  strcpy(line,"Fehler  :");
  strcat(line,"lis.log");
  fprintf( f ,line);
  fprintf( f ,"\n");
  
  delete[] line;
  delete[] tmp;
  delete[] name;
  delete[] path;
  return TRUE;
}

/************ALLGEMEINES für alle 3 Listauswertungsprogramme*******/
BOOLEAN write_bat_list(int prgtyp)
{
	 FILE *batfile;
   //FILE *batchdatei;
   char filename[200];
   char verzeichnis [150], bat_file_text[150];
   static int y=0,i=0;
   static int z=0;
   char helpstring[15];
   y=0; 
   strcpy(filename,start_dir);
   strcat(filename,"\\wsplist.bat");
   
   batfile=fopen(filename,"w+");
   
   
   strcpy(verzeichnis, start_dir);
   verzeichnis[i+2]='\0';						//SICHERHEITSHALBER C:	 
   fprintf(batfile,"%s\n",verzeichnis);
   verzeichnis[2]='\\';
   verzeichnis[(strlen(verzeichnis)-1)]='\0'; //letzter Backslash weg
   /******************************************************/
   fprintf(batfile,"cd\\\n");
   bat_file_text[0]='\0';
   for(z=2;z<(INT)strlen(verzeichnis);z++) // CD START-DIRECTORY
   {
     y=0;
     bat_file_text[0]='\0';
     
     while (verzeichnis[z]!='\\' && verzeichnis[z]!='\0')
				 {
       bat_file_text[y]=verzeichnis[z];
       z++;
       y++;
				 }
			  bat_file_text[y]='\0';
        if (bat_file_text[0]!='\0')
        {
          fprintf(batfile,"cd ");
          fprintf(batfile,"%s\n",bat_file_text);
        }
   }
   fprintf(batfile,"if exist %s%s del %s%s\n",
     start_dir,"error.log",start_dir,"error.log");
   strcpy(filename,start_dir);
   switch (prgtyp)
   {
	  case LISTE:
      strcat(filename,"wsplist.exe>error.log");//Dick 4.03.99
      break;
    case VERGLEICH:
      strcat(filename,"wspvgl.exe>error.log");//Dick 4.03.99
      break;
    case PRUEFUNG:
      strcat(filename,"wspruef.exe>error.log");//Dick 4.03.99
      break;
    default:break;
   }
   fprintf(batfile,"%s\n",filename);
   
   /***/
   
   /***/
   xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,bat_file_text,50);
   int len=strlen(bat_file_text);
   bat_file_text[len-4]='\0';
   strcat(bat_file_text,"dath\\");
   strcpy(helpstring,ber_spec.name);
   if(prgtyp==LISTE)
   {
     for (int i=0; i<=3; i++)
     {
						 helpstring[(strlen(helpstring))-4]='\0';
             if (i==0)
             {
               strcat(helpstring,".wkm");
               ber_spec.name[2]='w';
               ber_spec.name[3]='k';
             }
             if (i==1)
             {
               strcat(helpstring,".ueb");
               ber_spec.name[2]='u';
               ber_spec.name[3]='e';
             }
             if (i==2)
             {
               strcat(helpstring,".max");
               ber_spec.name[2]='m';
               ber_spec.name[3]='a';
             }
             if (i==3)
             {
               strcat(helpstring,".exc");
               ber_spec.name[2]='e';
               ber_spec.name[3]='x';
             }
             /*			 fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",
             bat_file_text,helpstring,bat_file_text,helpstring,
             bat_file_text, ber_spec.name);
             */
             //solange bei ausgabe in wsplist nur Datei ohne Pfad:
             //sonst wie oben auskommentiert
             /*	 fprintf(batfile,"if exist %s\\%s copy %s\\%s %s%s\n",
             start_dir,helpstring,start_dir,helpstring,
             bat_file_text, ber_spec.name);*/
             fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",//Dick 24.03.99
               start_dir,helpstring,start_dir,helpstring,
               bat_file_text, ber_spec.name);
             
     }   //for
   } //prgtyp==LISTE
   if(prgtyp==VERGLEICH)
   {
     helpstring[(strlen(helpstring))-4]='\0';
     strcat(helpstring,".vgl");
     ber_spec.name[2]='v';
     ber_spec.name[3]='g';
     /*	fprintf(batfile,"if exist %s\\%s copy %s\\%s %s%s\n",
     start_dir,helpstring,start_dir,helpstring,
     bat_file_text, ber_spec.name);*/
     fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",//Dick 24.03.99
							start_dir,helpstring,start_dir,helpstring,
              bat_file_text, ber_spec.name);
   }
   if(prgtyp==PRUEFUNG)
   {       char temp[150];//Dick 14.01.99
   helpstring[(strlen(helpstring))-4]='\0';
   strcpy(temp,helpstring);//Dick 14.01.99
   strcat(helpstring,".prf");
   strcat(temp,".tab");//Dick 14.01.99
   ber_spec.name[2]='p';
   ber_spec.name[3]='r';
   
   /*
   fprintf(batfile,"if exist %s\\%s copy %s\\%s %s%s\n",
   start_dir,helpstring,start_dir,helpstring,
   bat_file_text, ber_spec.name);
   
     fprintf(batfile,"if exist %s\\%s copy %s\\%s %s%s\n",//Dick 14.01.99
     start_dir,temp,start_dir,temp,
   bat_file_text, ber_spec.name);*/
   fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",
     start_dir,helpstring,start_dir,helpstring,
     bat_file_text, ber_spec.name);
   
   fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",//Dick 24.03.99
     start_dir,temp,start_dir,temp,
     bat_file_text, ber_spec.name);
   }
   /***/
   
   
   strcpy(filename,start_dir);
   strcat(filename,"\\wsplist.bat");//Dick 4.03.99
   if (prgtyp==LISTE)
   {
     //		 fprintf(batfile,"del %s\n",filename);    vorläufig nicht löschen
   }
   xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,bat_file_text,50);
   len=strlen(bat_file_text);
   bat_file_text[len-4]='\0';
   strcat(bat_file_text,"dath\\");
   fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",
     start_dir,"error.log",start_dir,"error.log",
     bat_file_text, "error.log");
   
   
   xvt_fsys_set_dir_startup();
   fclose(batfile);
   
   // WinExec(filename,SW_SHOWMAXIMIZED);
   //Dick 3.03.99
   STARTUPINFO sui_ausw;
   PROCESS_INFORMATION pi_ausw;
   HANDLE hProc = NULL;
   BOOL bSuccess = FALSE;
   //LPDWORD exit_ausw;
   DWORD exit_ausw;
   //int *termstat;
   GetStartupInfo(&sui_ausw);
   
   sui_ausw.lpReserved = NULL;
   
   sui_ausw.lpTitle = NULL;
   sui_ausw.dwFlags |= STARTF_USESHOWWINDOW;
   sui_ausw.wShowWindow = SW_SHOW;
   
   bSuccess = CreateProcess(NULL, filename, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui_ausw, &pi_ausw);
   //hProc=OpenProcess(PROCESS_QUERY_INFORMATION,FALSE,pi_ausw.dwProcessId);
   hProc = pi_ausw.hProcess;
   /*if(bSuccess)
   {
   _cwait(NULL,(int) hProc, NULL );
   
     }
   return bSuccess;*/
   //BOOL err=TRUE;
   bSuccess=GetExitCodeProcess(hProc,&exit_ausw);
   while(exit_ausw==STILL_ACTIVE && bSuccess)
     bSuccess= GetExitCodeProcess(hProc,&exit_ausw);
   return bSuccess;
   //
  }
  /************ALLGEMEINES für alle 3 Listauswertungsprogramme*******/
  BOOLEAN win_list(int prgtyp)
  {
    char filename[200];
    char *filename1,*filename2;
    filename1 = new char[MAX_PATH];
    filename2 = new char[MAX_PATH];	
    char bat_file_text[150];
    int i=0;
    char helpstring[15];
    int teste=0;
    STARTUPINFO sui_ausw;
    PROCESS_INFORMATION pi_ausw;
    BOOL bSuccess = FALSE;
    DWORD exit_ausw;
    
    
    
    strcpy(filename1,start_dir);
    strcat(filename1, "error.log");
    DeleteFile(filename1);
    //fprintf(batfile,"if exist %s%s del %s%s\n",
    //						start_dir,"error.log",start_dir,"error.log");
    strcpy(filename,start_dir);
    switch (prgtyp)
    {
    case LISTE:
      sprintf(filename1, "LISTE");
      strcat(filename,"wsplist.exe");
      break;
    case VERGLEICH:
      sprintf(filename1, "VERGLEICH");
      strcat(filename,"wspvgl.exe");
      break;
    case PRUEFUNG:
      sprintf(filename1, "PRÜFUNG");
      strcat(filename,"wspruef.exe");
      break;
    default:break;
    }
    //fprintf(batfile,"%s\n",filename);
    
    /***/
    strcpy(filename2, ">error.log");
    char start_folder[MAX_PATH];
    strcpy(start_folder,start_dir);
    start_folder[strlen(start_folder)-1]='\0';
    SetCurrentDirectory(start_folder);
    //xvt_fsys_set_dir_startup();
    
    DoProgressDlg((HWND)xvt_vobj_get_attr(main_win, ATTR_NATIVE_WINDOW));
    SetProgressTitle(filename1);
    strcpy(filename1, "Auswertungsprogramm läuft...");
    SetProgressText(filename1);
    IncProgress();
    if (bBreakBerechnung)
      return bSuccess;
    
    GetStartupInfo(&sui_ausw);
    
    sui_ausw.lpReserved = NULL;
    sui_ausw.lpTitle = NULL;
    sui_ausw.dwFlags |= STARTF_USESHOWWINDOW;
    sui_ausw.wShowWindow = SW_SHOWNORMAL;
    
    bSuccess = CreateProcess(filename, filename2, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui_ausw, &pi_ausw);
    //hProc=OpenProcess(PROCESS_QUERY_INFORMATION,FALSE,pi_ausw.dwProcessId);
    hProc_Auswert = pi_ausw.hProcess;
    
    i = 0;
    while ((bSuccess=GetExitCodeProcess(pi_ausw.hProcess,&exit_ausw)) && exit_ausw==STILL_ACTIVE)
    {
      if (!(i%1000))
        IncProgress();
      i++;
    }
    
    /***/
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,bat_file_text,50);
    int len=strlen(bat_file_text);
    bat_file_text[len-4]='\0';
    strcat(bat_file_text,"dath\\");
    strcpy(helpstring,ber_spec.name);
    int delete_num=0;
    if(prgtyp==LISTE)
    {
      for (int i=0; i<=4; i++)
						{
        IncProgress();
        helpstring[(strlen(helpstring))-4]='\0';
        if (i==0)
						  {
          strcat(helpstring,".wkm");
          ber_spec.name[2]='w';
          ber_spec.name[3]='k';
						  }
        if (i==1)
						  {
          strcat(helpstring,".ueb");
          ber_spec.name[2]='u';
          ber_spec.name[3]='e';
						  }
        if (i==2)
						  {
          strcat(helpstring,".max");
          ber_spec.name[2]='m';
          ber_spec.name[3]='a';
						  }
        if (i==3)
						  {
          strcat(helpstring,".exc");
          ber_spec.name[2]='e';
          ber_spec.name[3]='x';
						  }
        if (i==4)
						  {
          strcat(helpstring,".xlx");
          ber_spec.name[2]='e';
          ber_spec.name[3]='x';
						  }
              /*			 fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",
              bat_file_text,helpstring,bat_file_text,helpstring,
              bat_file_text, ber_spec.name);
        */
        //solange bei ausgabe in wsplist nur Datei ohne Pfad:
        //sonst wie oben auskommentiert
        /*	 fprintf(batfile,"if exist %s\\%s copy %s\\%s %s%s\n",
						  start_dir,helpstring,start_dir,helpstring,
						  bat_file_text, ber_spec.name);*/
        strcpy(filename1, start_dir);
        strcat(filename1, helpstring);
        teste=access(filename1,00);
        if(teste==0)
        {                             
          strcpy(filename2, bat_file_text);
          strcat(filename2, ber_spec.name);
          if(CopyFile(filename1, filename2, FALSE))
          {
            DeleteFile(filename1);
            delete_num=i;
          }
        }
        else if(delete_num!=3)
        {                              
          strcpy(filename2, bat_file_text);
          strcat(filename2, ber_spec.name);
          DeleteFile(filename2);
        }
        /* fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",//Dick 24.03.99
						  start_dir,helpstring,start_dir,helpstring,
						  bat_file_text, ber_spec.name);*/
        
					 }   //for
      strcpy(filename1, start_dir);
      strcat(filename1, "wsplist.ctr");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wsplist.ctr");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      strcpy(filename1, start_dir);
      strcat(filename1, "wsplist.err");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wsplist.err");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      strcpy(filename1, start_dir);
      strcat(filename1, "wsplist.log");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wsplist.log");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      strcpy(filename1, start_dir);
      strcat(filename1, "wsp.lis");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wsp.lis");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
    } //prgtyp==LISTE
    if(prgtyp==VERGLEICH)
    {
      IncProgress();
      helpstring[(strlen(helpstring))-4]='\0';
      strcat(helpstring,".vgl");
      ber_spec.name[2]='v';
      ber_spec.name[3]='g';
      /*   fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",//Dick 24.03.99
      start_dir,helpstring,start_dir,helpstring,
      bat_file_text, ber_spec.name);*/
      strcpy(filename1, start_dir);
      strcat(filename1, helpstring);
      teste=access(filename1,00);
      if(teste==0)
      {                             
        strcpy(filename2, bat_file_text);
        strcat(filename2, ber_spec.name);
        if(CopyFile(filename1, filename2, FALSE))
          DeleteFile(filename1);
      }
      else
      {
        strcpy(filename2, bat_file_text);
        strcat(filename2, ber_spec.name);
        DeleteFile(filename2);
      }
      helpstring[(strlen(helpstring))-4]='\0';
      strcat(helpstring,".log");
      strcpy(filename1, start_dir);
      strcat(filename1, helpstring);
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wspvgl.log");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      
      strcpy(filename1, start_dir);
      strcat(filename1, "vgl.ctr");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "vgl.ctr");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      strcpy(filename1, start_dir);
      strcat(filename1, "wspvgl.ctr");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wspvgl.ctr");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      strcpy(filename1, start_dir);
      strcat(filename1, "wspvgl.err");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wspvgl.err");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      strcpy(filename1, start_dir);
      strcat(filename1, "wsp.vgl");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wsp.vgl");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
    }
    if(prgtyp==PRUEFUNG)
    {                                   
      char temp[150];//Dick 14.01.99
      
      IncProgress();
      
      helpstring[(strlen(helpstring))-4]='\0';
      strcpy(temp,helpstring);//Dick 14.01.99
      strcat(helpstring,".prf");
      strcat(temp,".tab");//Dick 14.01.99
      ber_spec.name[2]='p';
      ber_spec.name[3]='r';
      
      /*fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",
      start_dir,helpstring,start_dir,helpstring,
      bat_file_text, ber_spec.name);
      
        fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",//Dick 24.03.99
        start_dir,temp,start_dir,temp,
      bat_file_text, ber_spec.name);*/
      strcpy(filename1, start_dir);
      strcat(filename1, helpstring);
      strcpy(filename2, bat_file_text);
      strcat(filename2, ber_spec.name);
      
      teste=access(filename1,00);
      if(teste==0)
      {
        if(CopyFile(filename1, filename2, FALSE))
          DeleteFile(filename1); 
      }
      else
      {
        DeleteFile(filename2);
      }
      strcpy(filename1, start_dir);
      strcat(filename1, temp);
      teste=access(filename1,00);
      if(teste==0)
      {
        if(CopyFile(filename1, filename2, FALSE))
          DeleteFile(filename1);
      }
      
      helpstring[(strlen(helpstring))-4]='\0';
      strcat(helpstring,".log");
      strcpy(filename1, start_dir);
      strcat(filename1, helpstring);
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wspruef.log");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      
      strcpy(filename1, start_dir);
      strcat(filename1, "prf.ctr");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "prf.ctr");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      strcpy(filename1, start_dir);
      strcat(filename1, "wspruef.ctr");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wspruef.ctr");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      strcpy(filename1, start_dir);
      strcat(filename1, "wspruef.err");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wspruef.err");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
      strcpy(filename1, start_dir);
      strcat(filename1, "wsp.prf");
      strcpy(filename2, bat_file_text);
      strcat(filename2, "wsp.prf");                            
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
    }
    /***/
    
    IncProgress();
    
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,bat_file_text,50);
    len=strlen(bat_file_text);
    bat_file_text[len-4]='\0';
    strcat(bat_file_text,"dath\\");
    
    /*fprintf(batfile,"if exist %s%s copy %s%s %s%s\n",
    start_dir,"error.log",start_dir,"error.log",
    bat_file_text, "error.log");*/
    
    strcpy(filename1, start_dir);
    strcat(filename1, "error.log");
    teste=access(filename1,00);
    if(teste==0)
    {                             
      strcpy(filename2, bat_file_text);
      strcat(filename2, "error.log");
      if(CopyFile(filename1, filename2, FALSE))
        DeleteFile(filename1);
    }
    hProc_Auswert=NULL;
    EndProgressDlg();
    
    //xvt_fsys_set_dir_startup();
    //fclose(batfile);
    
    delete[] filename1;
    delete[] filename2;
    return bSuccess;
    //
  }