//FUNKTIONEN ZUM BEARBEITEN VON VERZWEIGUNGEN


#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"
#include "global_vars.h"

#include "list.h"
#include "read_cfg.h"
#include "typen.h"
#include "strang.h"

#include "util2.h"
#include "verzweig.h"
#include "bce_allg.h"

#include "global.h"

extern char string209[20],
ber_name_extern[15],
zufluss1[3],
zufluss2[3],
abfluss1[3],
abfluss2[3],
qmenge[4],
qiterativ[3];
extern FILE_SPEC ber_spec, alte_zustandsdatei, neue_zustandsdatei;
extern BOOLEAN aendern, konvert, abbruch209, war_schon;

char  char_vzk[5],
help1[120],
profilvzk[5],
vernetzungsdatei[100];
int intprofilvzk=0,a=0, b=0;
BOOLEAN stop=FALSE;
extern BOOLEAN zustand_kopieren;
extern WINDOW main_win;

/***********************************************************************/

void vzk_block()   //Allgemeine Funktion (Zustandsbezogen)	
{
  FILE *state_file;
  double doubleprofilstation;
  SLIST vzk_list, helpliste;
  BOOLEAN  gefunden=FALSE, found=FALSE;
  int  intvzkslist, anzahl_vzk_list;
  FILE *vzkblock, *helpfile;
  int i, c, wieviel, int_char_vzk, int_vergleich2;
  
  char profilstation[10],
    vergleich2[3],
    *ptr_vzk,
    *helpptr;
  char vzk_str_bezogen[100];
  
  
  char *vzkblockdatei,
    *help2,
    *vzk_name,
    *inhalt,
    *vergleich;
  
  char* help4         = new char[100];
  help4[0] = 0;

  vzkblockdatei = new char[100];
  help2         = new char[120];
  vzk_name      = new char[15];
  inhalt        = new char[30];
  vergleich     = new char[30];
  /**************************/
  stop=FALSE;
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, vernetzungsdatei, 50);
  strcat(vernetzungsdatei,"\\");
  strcat(vernetzungsdatei,STR_SPEC.name);
  //			strcat(vernetzungsdatei,"\0");
  if ((state_file= fopen(vernetzungsdatei,"r"))==NULL)
  {
    //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",
    //						  vernetzungsdatei);
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
    xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
    xvt_dm_post_error("%s%s%s",buf,vernetzungsdatei,buf2);
    exit(-1);
  }
  else
  {
    /****erste Zeile: 4 Angaben****/
    
    fscanf(state_file,"%d",&anzahl_profil_dat_entries);
    fscanf(state_file,"%d",&anzahl_strang_entries);
    fscanf(state_file,"%s",help1);
    fscanf(state_file,"%s",help1);
    
    fgets(help1,110,state_file);
    
    vzk_list=xvt_slist_create();
    
    for (i=1;i<=anzahl_profil_dat_entries;i++)
    {
      if(!feof(state_file))
      {
        fgets(help2,110,state_file);
        b=0;
        for(a=10; a<=17; a++)
        {
          profilstation[b]=help2[a];
          b++;
        }
        profilstation[b]='\0';
        doubleprofilstation=atof(profilstation);
        b=0;
        for(a=29; a<=31; a++)
        {
          if(help2[a]!=' ')
          {
            profilvzk[b]=help2[a];
            b++;
          }
        }
        profilvzk[b]='\0';
        intprofilvzk=atoi(profilvzk);//Dick 11.02.99 atof -> atoi
        if(intprofilvzk>0)
        {						
          anzahl_vzk_list=xvt_slist_count(vzk_list);
          if(anzahl_vzk_list>0)
          {
            gefunden=FALSE;
            for(a=0; a<anzahl_vzk_list;a++)
            {
              if(!gefunden)
              {
                ptr_vzk=xvt_slist_get_elt(vzk_list,a,0L);
                strcpy(char_vzk,ptr_vzk);
                strcat(char_vzk,"\0");
                intvzkslist=atoi(char_vzk);
                if(intvzkslist==intprofilvzk)
                  gefunden=TRUE;
              } //gefunden
            } //for anzahl Slist
            if(!gefunden)
            {
              ptr_vzk=xvt_slist_get_elt(vzk_list,0,0L);
              strcpy(char_vzk,ptr_vzk);
              strcat(char_vzk,"\0");
              intvzkslist=atoi(char_vzk);
              if(intprofilvzk<intvzkslist)
                xvt_slist_add_at_pos(vzk_list,0,profilvzk,0L);
              else
              {
                for(a=0; a<anzahl_vzk_list; a++)
                {
                  ptr_vzk=xvt_slist_get_elt(vzk_list,a,0L);
                  strcpy(char_vzk,ptr_vzk);
                  intvzkslist =atoi(char_vzk);
                  if((intprofilvzk>intvzkslist) || (a==anzahl_vzk_list-1))
                  {
                    xvt_slist_add_at_pos(vzk_list,a+1,profilvzk,0L);
                    break;
                  }
                }
              }
              /****/							
            }	
          } //if anzahl>0
          if(anzahl_vzk_list==0)
            //							  xvt_slist_add_at_elt(vzk_list,NULL,profilvzk,0L);
            xvt_slist_add_sorted(vzk_list,profilvzk,0L,TRUE,FALSE);                                                    
        } //anzahl vzk_list>0
      } /*if !=EOF*/
    }  /*for <prof-dat-entries*/
    
    anzahl_vzk_list=xvt_slist_count(vzk_list);
    
    if (anzahl_vzk_list>0)
    {
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, vzk_str_bezogen, 50);
      strcat(vzk_str_bezogen,"\\");
      strcat(vzk_str_bezogen,STR_SPEC.name);
      vzk_str_bezogen[strlen(vzk_str_bezogen)-3]='\0';
      strcat(vzk_str_bezogen,"vzk");
      
      
      int vzkallgemeinda=100;
      
      
      vzkallgemeinda=access(vzk_str_bezogen,4);
      
      helpliste=xvt_slist_create();
      
      if(!vzkallgemeinda)  //existiert
      {
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, help4, 50);
        strcat(help4,"\\help.tmp");
        
        copy_fileb(vzk_str_bezogen,help4);
        
        helpfile=fopen(help4,"r");
        inhalt[0]='\0';
        
        fgets(inhalt,30,helpfile);
        
        while(!feof(helpfile))
        {
          for(i=0;i<=(int)strlen(inhalt);i++)//Dick 11.02.99
          {
            if(inhalt[i]=='\n')
              inhalt[i]='\0';
          }
          xvt_slist_add_at_elt(helpliste,NULL,inhalt,0L);
          fgets(inhalt,30,helpfile);
        }
        fclose(helpfile);
 //       remove(help4); //Bley April 2002
        
      } 
      
      vzkblock=fopen(vzk_str_bezogen,"w");
      
      for(a=0; a<anzahl_vzk_list; a++)
      {
        if(!stop)
        {
          ptr_vzk=xvt_slist_get_elt(vzk_list,a,0L);
          strcpy(char_vzk,ptr_vzk);
          strcat(char_vzk,"\0");
          wieviel=xvt_slist_count(helpliste);
          if(wieviel!=0)
          {
            found=FALSE;
            for(i=0;i<wieviel;i++)
            {
              if(!found)
              {
                helpptr=xvt_slist_get_elt(helpliste,i,0L);
                strcpy(vergleich,helpptr);
                c=0;
                for(b=0; b<2;b++)
                {
                  vergleich2[c]=vergleich[b];
                  c++;
                }
                vergleich2[c]='\0';
                int_char_vzk=atoi(char_vzk);
                int_vergleich2=atoi(vergleich2);
                if(int_char_vzk==int_vergleich2)
                  found=TRUE;
              }
              
            } //for wieviel
          } //if wieviel
          if(found)
          {
            
            b=0;
            for(i=3;i<5;i++)
            {
              if(vergleich[i]!=' ')
              {
                zufluss1[b]=vergleich[i];
                b++;
              }
            }
            zufluss1[b]='\0';
            
            b=0;
            for(i=6;i<8;i++)
            {
              if(vergleich[i]!=' ')
              {
                zufluss2[b]=vergleich[i];
                b++;
              }
            }
            zufluss2[b]='\0';
            
            b=0;
            for(i=9;i<11;i++)
            {
              if(vergleich[i]!=' ')
              {
                abfluss1[b]=vergleich[i];
                b++;
              }
            }
            abfluss1[b]='\0';
            
            b=0;
            for(i=12;i<14;i++)
            {
              if(vergleich[i]!=' ')
              {
                abfluss2[b]=vergleich[i];
                b++;
              }
            }
            abfluss2[b]='\0';
            
            
            
            //Dick 23.07.99 Neu
            double q_menge_f=0.0;
            int qiterativ_i=0;
            char *temp_hilfe=&vergleich[15];
            sscanf(temp_hilfe,"%lf%d",&q_menge_f,&qiterativ_i);
            if(qiterativ_i!=0 && qiterativ_i!=1)
              qiterativ_i=0;
            sprintf(qmenge,"%.2f",q_menge_f);
            sprintf(qiterativ,"%d",qiterativ_i);
            //
            
            
          } //if found
          
          
          if(!xvt_dlg_create_res(WD_MODAL,DLG_209, EM_ALL, DLG_209_eh, 0L)) //("Can't open dialog 209");
            xvt_dm_post_note("Can't open dlg_209");
          if(!abbruch209)
            fprintf(vzkblock,"%s\n",string209);
          else
            stop=TRUE;
        }//if(!stop)
        } //for(anzahl_vzk_list)
        fclose(vzkblock);
   
		
        
        if(stop)
		{
			remove(vzk_str_bezogen);
            copy_fileb(help4, vzk_str_bezogen);  //Bley April 2002

		}
        
        if(helpliste!=NULL)
        {
          xvt_slist_destroy(helpliste);
          helpliste=NULL;
        }
      } //if anzahlvzklist>0
      else
      {
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_NO_VERZ_CREATE,buf,sizeof(buf));
        MessageBox((HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW),buf,"WSPWIN",MB_OK|MB_ICONINFORMATION|MB_APPLMODAL );
        //MessageBox((HWND)xvt_vobj_get_attr(main_win,ATTR_NATIVE_WINDOW),"Sie haben kein verzweigtes System erstellt!","WSPWIN",MB_OK|MB_ICONINFORMATION|MB_APPLMODAL );
      }
      
      fclose(state_file);
   } //else
	  if (vzk_list!=NULL)
    {
      xvt_slist_destroy(vzk_list);
      vzk_list=NULL;
    }
    
    delete[] help4;
    delete[] vzkblockdatei;
    delete[] help2;
    delete[] vzk_name;
    delete[] inhalt;
    delete[] vergleich;
} //FUNKTION

/*********************************************************************/
void teste_str_verzweigt(void)
{
	 int i, zaehler, zaehler3;
   FILE *file_zustand;
   
   /*********/
   
   xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, vernetzungsdatei, 50);
   strcat(vernetzungsdatei,"\\");
   if(zustand_kopieren)
     strcat(vernetzungsdatei,alte_zustandsdatei.name);
   else
     strcat(vernetzungsdatei,STR_SPEC.name);
   istverzweigt=FALSE;
 	 if ((file_zustand= fopen(vernetzungsdatei,"r"))==NULL)
		 {
     // (" Datei : %s läßt sich nicht öffnen !",
     char buf[200],buf2[200];//Dick 26.11.99
     xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
     xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
     xvt_dm_post_error("%s%s%s",buf,vernetzungsdatei,buf2);
     exit(-1);
		 }
   else
		 {
     fscanf(file_zustand,"%d",&zaehler);
     fscanf(file_zustand,"%d",&zaehler3);
     fscanf(file_zustand,"%s",help1);
     fscanf(file_zustand,"%s",help1);
     
     fgets(help1,110,file_zustand);
     if(zaehler>0)
     {
       for (i=1;i<=zaehler;i++)
       {
         if(!feof(file_zustand))
         {
           fgets(help1,110,file_zustand);
           
           //Verzweigunskennung:
           b=0;
           for(a=29; a<=31; a++)
           {
             if(help1[a]!=' ')
             {
               profilvzk[b]=help1[a];//variable profilvzk=Verzweigungskn.
               b++;
             }
           }
           profilvzk[b]='\0';
           intprofilvzk=atoi(profilvzk);//Dick 11.02.99 atof -> atoi
           
           //Profilkennung:
           b=0;
           for(a=19;a<=27;a++)
           {
             if(help1[a]!=' ')
             {
               profilvzk[b]=help1[a]; //Variable profilvzk=Profilkennung
               b++;
             }
           }
           profilvzk[b]='\0';
           if((intprofilvzk>0) || (profilvzk[0]!='0'))
             istverzweigt=TRUE;
         }
       } //FOR ANZAHL PROFILDATENTRIES
     } //if zaehler>0
     fclose(file_zustand);
		 } //ELSE STR EXISTIERT
} //FUNKTION


/*******************************************************************/
void copy_old_to_new_strverweigt(char *old_file, char *new_file)//in verwezig.cpp
{
  
  int i, zaehler, zaehler3, i2, anzahl_elements, zaehler_slist;
	 char help2[16], help3[16], *hilfs_ptr;
   //char zaehlerstring[20], zaehler3string[20];
   
   char string1 [20],
     string2 [20],
     string3 [20],
     string4 [20],
     string5 [20],
     string6 [20],
     string7 [20];
		 
   
   anzahl_elements=xvt_slist_count(verzweigt_new);
   zaehler_slist=0;	
   for(zaehler_slist=0; zaehler_slist<anzahl_elements; zaehler_slist++)
   {
     hilfs_ptr=xvt_slist_get_elt(verzweigt_new,zaehler_slist,0L);
     help1[0]='\0';
     strcpy(help1,hilfs_ptr);
     
     if(zaehler_slist==0)
     {
       sscanf(help1,"%d%d%s%s",&zaehler, &zaehler3, help3, help2);
       
       i=sprintf(help1+0,"%5d ",zaehler);
       i+=sprintf(help1+i,"%5d ",zaehler3);
       
       i+=sprintf(help1+i,"%10s ",netz_dat[0]);       //Gewässername
       i+=sprintf(help1+i,"%10s\n",netz_dat[2]);     // Zustand
       xvt_slist_change_str(verzweigt_new,help1, zaehler_slist);
       //
     }
     else
     {
       if (zaehler_slist<=zaehler)
       {
         //Profiletabelle			 
         sscanf(help1, "%s %s %s %s %s %s", 
         string1, string2, string3, string4, string5, string6);
         if ( xvt_str_compare_ignoring_case(string6, old_file)==0  )
         {
           int len=strlen(help1);
           //Neu Dick 17.03.99
           int len_netz_dat=strlen(netz_dat[0]); //Gewässername
           for(i=0;i<9;i++)//Dick 10.09.99 11->9 sonst Probleme beim Einlesen und Station begint ab 10  
           {
             if(i<len_netz_dat) 
               help1[i]=netz_dat[0][i];
             else
               help1[i]=' ';
           }
           len_netz_dat=strlen(netz_dat[2]);  //Zustand
           for(i=0;i<10;i++)
           {
             if(i<len_netz_dat) 
               help1[33+i]=netz_dat[2][i];
             else
               help1[33+i]=' ';
           }
           //Ende neu
           for( i2=0; i2<=len; i2++)
           {
             if((help1[i2]==old_file[0]) &&
               (help1[i2+1]==old_file[1]) &&
               (help1[i2+2]==old_file[2]) &&
               (help1[i2+3]==old_file[3]) &&
               (help1[i2+4]==old_file[4]) &&
               (help1[i2+5]==old_file[5]) &&
               (help1[i2+6]==old_file[6]) &&
               (help1[i2+7]==old_file[7]) )
               
             {
               help1[i2]=new_file[0]; 
               help1[i2+1]=new_file[1];
               help1[i2+2]=new_file[2];
               help1[i2+3]=new_file[3];
               help1[i2+4]=new_file[4];
               help1[i2+5]=new_file[5];
               help1[i2+6]=new_file[6];
               help1[i2+7]=new_file[7];
               
             }
             
										 }  //for i2 bis len
           xvt_slist_change_str(verzweigt_new,help1, zaehler_slist);
								 }   //if strcompare gleich
       } //if zaehler_lsit<zaehler =Profiltabelle
       if (zaehler_slist>zaehler)  //Strangtabelle
       {
         sscanf(help1, "%s%s%s%s%s%s%s", 
           string1, string2, string3, string4, string5, string6, string7);
         if ( (xvt_str_compare_ignoring_case(string7, old_file)==0 ) ||
           (xvt_str_compare_ignoring_case(string6, old_file)==0 ))
         {
           int len=strlen(help1);
           
           for( i2=0; i2<=len; i2++)
           {
             if((help1[i2]==old_file[0]) &&
               (help1[i2+1]==old_file[1]) &&
               (help1[i2+2]==old_file[2]) &&
               (help1[i2+3]==old_file[3]) &&
               (help1[i2+4]==old_file[4]) &&
               (help1[i2+5]==old_file[5]) &&
               (help1[i2+6]==old_file[6]) &&
               (help1[i2+7]==old_file[7]) )
               
             {
               help1[i2]=new_file[0]; 
               help1[i2+1]=new_file[1];
               help1[i2+2]=new_file[2];
               help1[i2+3]=new_file[3];
               help1[i2+4]=new_file[4];
               help1[i2+5]=new_file[5];
               help1[i2+6]=new_file[6];
               help1[i2+7]=new_file[7];
             }
           }  //for i2 bis len
           
           xvt_slist_change_str(verzweigt_new,help1, zaehler_slist);
         }   //if strcompare gleich
       }  //if zaehler_slist >zaehler
       
     }  //else
    } //for
    
   }  //funktion
   /*****************************************************************/
   void copy_vzk_default_datei(char *old_zustandsdatei, char *new_zustandsdatei)
     
   {
     char hilfs_name_old[150];
     char hilfs_name_new[150];
     
     int counter=0;//Dick 24.11.99 sonst wenn Datei nicht existiert -> Absturz
     FILE *in;
     
     
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, vernetzungsdatei, 50);
     strcat(vernetzungsdatei,"\\");
     strcat(vernetzungsdatei,alte_zustandsdatei.name);
     
     int len=strlen(vernetzungsdatei);
     
     vernetzungsdatei[len-3]='b';
     vernetzungsdatei[len-2]='e';
     vernetzungsdatei[len-1]='r';
     
     if ((in= fopen(vernetzungsdatei,"r+"))!=NULL)
     {
       fscanf(in,"%d",&counter);
       fclose(in);
     }
     
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, vernetzungsdatei, 50);
     strcat(vernetzungsdatei,"\\");
     strcpy(hilfs_name_old, alte_zustandsdatei.name);
     hilfs_name_old[2]='v';
     hilfs_name_old[3]='z';
     strcat(vernetzungsdatei,hilfs_name_old);
     
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, hilfs_name_new, 50);
     strcat(hilfs_name_new,"\\");
     strcpy(hilfs_name_old, neue_zustandsdatei.name); //!Vorsicht old=New
     hilfs_name_old[2]='v';
     hilfs_name_old[3]='z';
     strcat(hilfs_name_new,hilfs_name_old);
     
     char counter_char[10];
     
     for(int i=1; i<=counter; i++)
     {
       hilfs_name_new[strlen(hilfs_name_new)-3]='0';
       hilfs_name_new[strlen(hilfs_name_new)-2]='0';
       hilfs_name_new[strlen(hilfs_name_new)-1]='0';
       
       vernetzungsdatei[strlen(hilfs_name_new)-3]='0';
       vernetzungsdatei[strlen(hilfs_name_new)-2]='0';
       vernetzungsdatei[strlen(hilfs_name_new)-1]='0';
       
       if(i<=9)
       {
         itoa(counter,counter_char,10);
         hilfs_name_new[(strlen(hilfs_name_new))-1]='\0';
         vernetzungsdatei[(strlen(vernetzungsdatei))-1]='\0';
         strcat(hilfs_name_new,counter_char);
         strcat(vernetzungsdatei,counter_char);
       }
       if((i>9) && (i<=99))
       {
         itoa(counter,counter_char,10);
         hilfs_name_new[(strlen(hilfs_name_new))-2]='\0';
         vernetzungsdatei[(strlen(vernetzungsdatei))-2]='\0';
         strcat(hilfs_name_new,counter_char);
         strcat(vernetzungsdatei,counter_char);
       }
       
       if(i>99)
       {
         itoa(counter,counter_char,10);
         hilfs_name_new[(strlen(hilfs_name_new))-3]='\0';
         strcat(hilfs_name_new,counter_char);
         vernetzungsdatei[(strlen(vernetzungsdatei))-3]='\0';
         strcat(vernetzungsdatei,counter_char);
       }
       
       if((access(vernetzungsdatei,00))==0)
         copy_fileb(vernetzungsdatei, hilfs_name_new);
     }
     
   }
   
   /******************************************************************/
   void alte_str_in_slist()
   {
     FILE *file_zustand;
     
     verzweigt_new=xvt_slist_create();
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, vernetzungsdatei, 50);
     strcat(vernetzungsdatei,"\\");
     strcat(vernetzungsdatei,alte_zustandsdatei.name);
     
     if ((file_zustand= fopen(vernetzungsdatei,"r"))==NULL)
     {
       //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",
       //						  vernetzungsdatei);
       char buf[200],buf2[200];//Dick 26.11.99
       xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
       xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
       xvt_dm_post_error("%s%s%s",buf,vernetzungsdatei,buf2);
       exit(-1);
     }
     else
     {
       
       fgets(help1,110,file_zustand);
       
       
       while(!feof(file_zustand))
       {
         xvt_slist_add_at_elt(verzweigt_new,NULL,help1,0L);
         fgets(help1,110,file_zustand);
       }
       fclose(file_zustand);
       
     }
     war_schon=TRUE;
     
   }
   
   /******************/
   void schreibe_slist_verzweigt()
   {
     
     FILE *file_out;
     SLIST_ELT ee;
     char *hilfs_ptr;
     
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, vernetzungsdatei, 50);
     strcat(vernetzungsdatei,"\\");
     strcat(vernetzungsdatei,neue_zustandsdatei.name);
     
     if ((file_out= fopen(vernetzungsdatei,"w+"))==NULL)
     {
       //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",
       //						  vernetzungsdatei);
       char buf[200],buf2[200];//Dick 26.11.99
       xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
       xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
       xvt_dm_post_error("%s%s%s",buf,vernetzungsdatei,buf2);
       exit(-1);
     }
     else
     {
       
       for(ee=xvt_slist_get_first(verzweigt_new);
       ee!=NULL;ee=xvt_slist_get_next(verzweigt_new,ee))
       {
         hilfs_ptr=xvt_slist_get(verzweigt_new,ee,0L);
         help1[0]='\0';
         strcpy(help1, hilfs_ptr);
         
         fprintf(file_out,"%s",help1);            
       }
       
       fclose(file_out);
     } //else
   }
   
   /*********************************************************************/
void teste_str_verzweigt_intern(void)
{
  
  if (strang_anfang)
    strang_ptr = strang_anfang;
  else  strang_ptr = NULL;
  
  while (strang_ptr != NULL)
  {
    if ((strang_ptr->strang_vzk >0)||(strang_ptr->strang_pk)>0)
      istverzweigt=TRUE;
    strang_ptr = strang_ptr->next;
  }
}