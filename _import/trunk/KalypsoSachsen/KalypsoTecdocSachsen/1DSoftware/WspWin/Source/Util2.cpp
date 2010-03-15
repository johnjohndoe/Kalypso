#include "resource.h"

#include <windows.h>
#include <math.h>

#include "xvt.h"

#include "global_types.h"
#include "global_vars.h"

#include "..\..\wspdlg\Include\export.h"

#include "list.h"
#include "readprof.h"

#include "read_cfg.h"

#include "typen.h"
#include "strang.h"

#include "global.h"

#include "util.h"
#include "util2.h"
#include "qlist.h"

#include <fcntl.h>
#include <process.h>	// GHJ

#include "verluste.h"

#include "lzexpand.h"
#include "profpro2.h"



#ifndef tabellenlaenge
#define tabellenlaenge 100
#endif

extern char *start;
extern char *start_dir;

extern SLIST ber_list;
//extern FILE *in;
extern WINDOW main_win;
extern BOOLEAN aendern,
konvert,
stop;
extern FILE_SPEC ber_spec;

extern TABELLE *nasim_anfang, *nasim_ptr, *nasim_ende,
*ber_anfang, *ber_ptr, *ber_ende,
*kal_anfang, *kal_ptr, *kal_ende;
extern WSP_PROFIL_LISTE *pWPL;

char ber_name_extern[MAX_PATH];  //externe Variable
//verwendet in verzweig.cpp: Fkt. vzk_block
//hier verwendet in Fkt.:savedat
WINDOW DLG203;             //externe Variable
TABELLE *ptr_test;
long timer_main2;
BOOLEAN timermain2;
int timer_zaehler2;
extern BOOLEAN zustand_kopieren; //8.2.99 bley
extern SLIST profil_list; //8.2.99 bley

HANDLE hProc_dag_bat = NULL;

char bat_start[100];//Dick 2.06.99 für BCE-Version
extern QWertDatei *ptr_qwert_anfang;//Dick 2.06.99 für BCE-Version
extern SLIST abflussereignisse;//Dick 2.06.99 für BCE-Version


HANDLE hProc_Wspr = NULL;		// GHJ
extern BOOL bBreakBerechnung;	// GHJ


extern DWORD exit_ausw;				// GHJ

/**********************SAVEDAT*************************************/

void savedat(st_daten *daten)
{
  if( LWA_PROJEKT )
  {
  SLIST merke_list=NULL;
  FILE *out, *ber_file, *merke_datei;
  int len, m=0, position=0;
  unsigned int i, zaehler;
  char *help2, *temp, * merke;
  SLIST_ELT e;
  char  anfang[15],
    ende[15],
    str[100],
    str2[15],
    help[101];

  char		*name,
		*zaehler2,
      *zaehler_help,
      *aendern_name,
      *merke_name;
    
    
      /*  str          =new char[100];
      str2			=new char[15];
      help         =new char[101];
    */  name			=new char[16];
    zaehler2		=new char[4];
    zaehler_help	=new char[4];
    aendern_name =new char[100];
    merke_name   =new char[100];
    /****************************/
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
    strcat(str,"\\");
    
    strcpy(str2,STR_SPEC.name);
    str2[9]='\0';
    strcat(str2,"BER");
    strcat(str,str2);
    
    ///////////////////////////Wenn noch kein ber.file existiert:
    
    if ((ber_file = fopen(str,"r+"))==NULL)
    {
      for (i=0;i<=100;i++)  help[i]=' ';
      help[100]='\0';
      
      unsigned int len=strlen(daten->info);
      if (len>0)
        for (i=0;i<len;i++)
          help[i]=daten->info[i];
        
        //gcvt(daten->anfang,8,anfang);
        sprintf(anfang,"%.4lf",daten->anfang);
        for (i=61;i<61+strlen(anfang);i++)
          help[i]=anfang[i-61];
        
        //gcvt(daten->ende,8,ende);
        sprintf(ende,"%.4lf",daten->ende);
        for (i=70;i<70+strlen(ende);i++)
          help[i]=ende[i-70];
        
        /***/
        
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,merke_name,50);
        strcat(merke_name,"\\");
        
        strcat(merke_name,STR_SPEC.name);
        merke_name[strlen(merke_name)-3]='\0';
        strcat(merke_name,"mrk");
        
        if ((merke_datei=fopen(merke_name,"r"))!=NULL)
        {
          if (merke_list!=NULL)
          {
            xvt_slist_destroy(merke_list);
            merke_list=NULL;
          }
          merke_list=xvt_slist_create();
          name[0]='\0';
          while (!feof(merke_datei))
          {
            fgets (name,15,merke_datei);
            if (name[0]!='\n')
              xvt_slist_add_at_elt(merke_list,NULL,name,0);
            name[0]='\n';
          }
          
          e=xvt_slist_get_first(merke_list);
          merke=xvt_slist_get(merke_list,e,0L);
          for(i=79;i<=90;i++)
          {
            help[i]=merke[i-79];
            name[i-79]=merke[i-79];
          }
          help[91]='\0';
          name[12]='\0';
          xvt_slist_rem(merke_list,e);
          fclose(merke_datei);
          
          
          int	x=xvt_slist_count(merke_list);
          if(x>0)
          {
            merke_datei=fopen(merke_name,"w");
            for(e=xvt_slist_get_first(merke_list);e!=NULL;
            e=xvt_slist_get_next(merke_list,e))
            {
              merke=xvt_slist_get(merke_list,e,0L);
              fprintf(merke_datei,"%s\n",merke);
            }
            fclose(merke_datei);
            if (merke_list!=NULL)
            {
              xvt_slist_destroy(merke_list);
              merke_list=NULL;
            }
          }
          if (x==0)
          {
            if (merke_list!=NULL)
            {
              xvt_slist_destroy(merke_list);
              merke_list=NULL;
            }
            remove(merke_name);
          }
          
          
        } //if
        
        else
        {
          for(i=79;i<=87;i++)
          {
            help[i]=STR_SPEC.name[i-79];
            name[i-79]=STR_SPEC.name[i-79];
          }
          help[88]='0';
          name[9]='0';
          help[89]='0';
          name[10]='0';
          help[90]='1';
          name[11]='1';
          help[91]='\0';
          name[12]='\0';
        }
        /***/
        
        ber_file=fopen(str,"w");
        zaehler=1;
        
        fprintf(ber_file,"%d\n",zaehler);
        
        fprintf(ber_file,"%s",help);
        fclose(ber_file);
        
        
        if (ber_list !=NULL)
        {
          xvt_slist_destroy(ber_list);
          ber_list=NULL;
        }
        if  ((ber_list = xvt_slist_create())==NULL)
        {
          xvt_dm_post_error(" Can't create _SLIST:ber_list");
        }
        else
          
          xvt_slist_add_at_elt(ber_list,NULL,help,0);
        
        
  }
  
  else  /////////////////Ber.file existiert schon  -Datei einlesen
  {
    
    fgets(help,10,ber_file);
    
    zaehler = atoi(help);
    
    if (ber_list !=NULL)
    {
      xvt_slist_destroy(ber_list);
      ber_list=NULL;
    }
    if  ((ber_list = xvt_slist_create())==NULL)
			 {
      xvt_dm_post_error(" Can't create _SLIST:ber_list");
			 }
    else
    {
      for (i=1;i<=zaehler;i++)
      {
        if (!feof(ber_file))
        {
          fgets(help,100,ber_file);
          if (help[0]!='\n')
          {
            
            for (unsigned int k=0;k<=strlen(help);k++)
            {
              if(help[k]=='\n')
                help[k] ='\0';
            }
            xvt_slist_add_at_elt(ber_list,NULL,help,i-1);
            
          }
          help[0]='\n';
        }
      }
		  }
    int x=xvt_slist_count(ber_list);
    fclose(ber_file);
    
    
    
    //Datei .ber neu schreiben, um ein Element anzufügen
    
    if (aendern==FALSE)
    {
      ber_file = fopen(str,"w+");
      
      zaehler=zaehler+1;
      
      for (i=0;i<=100;i++)  help[i]=' ';
      
      help[100]='\0';
      
      if (strlen(daten->info)>0)
        for (i=0;i<strlen(daten->info);i++)
          help[i]=daten->info[i];
        
        //	gcvt(daten->anfang,8,anfang);
        sprintf(anfang,"%.4lf",daten->anfang);
        for (i=61;i<61+strlen(anfang);i++)
          help[i]=anfang[i-61];
        
        //	gcvt(daten->ende,8,ende);
        sprintf(ende,"%.4lf",daten->ende);
        for (i=70;i<70+strlen(ende);i++)
          help[i]=ende[i-70];
        
        /****/
        
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,merke_name,50);
        strcat(merke_name,"\\");
        
        strcat(merke_name,STR_SPEC.name);
        merke_name[strlen(merke_name)-3]='\0';
        strcat(merke_name,"mrk");
        
        if ((merke_datei=fopen(merke_name,"r"))!=NULL)
        {
          if (merke_list!=NULL)
          {
            xvt_slist_destroy(merke_list);
            merke_list=NULL;
          }
          merke_list=xvt_slist_create();
          name[0]='\0';
          while (!feof(merke_datei))
          {
            fgets (name,15,merke_datei);
            if (name[0]!='\n')
              xvt_slist_add_at_elt(merke_list,NULL,name,0);
            name[0]='\n';
          }
          
          e=xvt_slist_get_first(merke_list);
          merke=xvt_slist_get(merke_list,e,0L);
          for(i=79;i<=90;i++)
          {
            help[i]=merke[i-79];
            name[i-79]=merke[i-79];
          }
          help[91]='\0';
          name[12]='\0';
          xvt_slist_rem(merke_list,e);
          fclose(merke_datei);
          
          
          int	x=xvt_slist_count(merke_list);
          if(x>0)
          {
            merke_datei=fopen(merke_name,"w");
            for(e=xvt_slist_get_first(merke_list);e!=NULL;
            e=xvt_slist_get_next(merke_list,e))
            {
              merke=xvt_slist_get(merke_list,e,0L);
              fprintf(merke_datei,"%s\n",merke);
            }
            fclose(merke_datei);
            if (merke_list!=NULL)
            {
              xvt_slist_destroy(merke_list);
              merke_list=NULL;
            }
          }
          if (x==0)
          {
            if (merke_list!=NULL)
            {
              xvt_slist_destroy(merke_list);
              merke_list=NULL;
            }
            remove(merke_name);
          }
          
          
        } //if
        
        
        
        
        
        /****/
        else
        {
          for(i=79;i<=87;i++)
          {
            help[i]=STR_SPEC.name[i-79];
            name[i-79]=STR_SPEC.name[i-79];
          }
          
          itoa(zaehler, zaehler_help,10);
          
          len=strlen(zaehler_help);
          
          if (len==1)
          {
            zaehler2[0]='0';
            zaehler2[1]='0';
            zaehler2[2]=zaehler_help[0];
          }
          
          if (len==2)
          {
            zaehler2[0]='0';
            zaehler2[1]=zaehler_help[0];
            zaehler2[2]=zaehler_help[1];
          }
          if (len==3)
          {
            zaehler2[0]=zaehler_help[0];
            zaehler2[1]=zaehler_help[1];
            zaehler2[2]=zaehler_help[2];
          }
          
          zaehler2[3]='\0';
          name[9]='\0';
          strcat(name,zaehler2);
          
          
          for(i=79;i<91;i++)
            help[i]=name[i-79];
          
          help[91]='\0';
        } //else
        /***/
        
        fprintf(ber_file,"%d\n",zaehler);
        
        
        xvt_slist_add_at_elt(ber_list,NULL,help,0);
        int y=xvt_slist_count(ber_list);
        for(e = xvt_slist_get_first(ber_list);e!=NULL;
        e = xvt_slist_get_next(ber_list,e))
        {
          help2=xvt_slist_get(ber_list,e,0L);
          fprintf(ber_file,"%s\n",help2);
        }
        
        
        fclose(ber_file);
 } //if  aendern=false
 
 /************************************/
	if (aendern)
  {
    ber_file = fopen(str,"w+");
    
    zaehler=zaehler;
    
    for (i=0;i<=100;i++)  help[i]=' ';
    
    help[100]='\0';
    
    if (strlen(daten->info)>0)
    {
      for (i=0;i<strlen(daten->info);i++)
        help[i]=daten->info[i];
    }
    sprintf(anfang,"%.4lf",daten->anfang);
    for (i=61;i<61+strlen(anfang);i++)
      help[i]=anfang[i-61];
    
    sprintf(ende,"%.4lf",daten->ende);
    for (i=70;i<70+strlen(ende);i++)
      help[i]=ende[i-70];
    
    for(i=79;i<=90;i++)
    {
      help[i]=ber_spec.name[i-79];
      name[i-79]=ber_spec.name[i-79];
    }
    name[12]='\0';
    
    help[91]='\0';
    
    
    
    
    
    /*Element suchen (nach name-position) und austauschen: */
    
    m=0;
    for(e=xvt_slist_get_first(ber_list);e!=NULL;
    e=xvt_slist_get_next(ber_list,e))
      
    {
      temp=xvt_slist_get(ber_list,e,0L);
      
      for (i=79;i<=91;i++)
      {
        aendern_name[i-79]=temp[i];
      }
      aendern_name[12]='\0';
      if(xvt_str_match(aendern_name,name,1)==TRUE)
        position=m;
      
      m++;
    }
    
    e=xvt_slist_get_first(ber_list);
    for(int h=1;h<=position;h++)
      e=xvt_slist_get_next(ber_list,e);
    xvt_slist_rem(ber_list,e);
    xvt_slist_add_at_pos(ber_list,position,help,0L);
    
    fprintf(ber_file,"%d\n",zaehler);
    
    for(e = xvt_slist_get_first(ber_list);e!=NULL;
    e = xvt_slist_get_next(ber_list,e))
    {
      help2=xvt_slist_get(ber_list,e,0L);
      fprintf(ber_file,"%s\n",help2);
    }
    
    fclose(ber_file);
    
    
    
    
    
  } //aendern=TRUE
  
  /******************************************/
  
  } //else ber-file existiert



  /*Eingabedaten speichern:*/
  
  if (aendern==FALSE)
  {
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
    strcat(str,"\\");
    strcat(str,name);
    strcpy(ber_name_extern,name);
  }
  
  else
  {
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
    strcat(str,"\\");
    strcat(str,ber_spec.name);
    strcpy(ber_name_extern,ber_spec.name);
  }
  
  if ((out= fopen(str,"w+"))==NULL)
  {
    //xvt_dm_post_error("Fehler beim öffnen von %s",str);
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_OPEN_ERR,buf,sizeof(buf));
    xvt_dm_post_error("%s %s",buf,str);
  }
  
  else
  {
    fprintf (out,"%s\n", daten->info);
    fprintf (out,"%f\n", daten->anfang);
    fprintf (out,"%f\n", daten->ende);
    fprintf (out,"%s\n", daten->eich);
    fprintf (out,"%s\n", daten->he);
    fprintf (out,"%s\n", daten->hgralle);
    fprintf (out,"%s\n", daten->normalle);
    fprintf (out,"%s\n", daten->wtau);
    fprintf (out,"%s\n", daten->wasser);
    fprintf (out,"%s\n", daten->hoehe);
    fprintf (out,"%s\n", daten->gefaelle);
    fprintf (out,"%d\n", daten->sel_index);
    fprintf (out,"%s\n", daten->q);
    fprintf (out,"%s\n", daten->qplot);
    fprintf (out,"%s\n", daten->lplot);
    fprintf (out,"%s\n", daten->nasall);
    fprintf (out,"%s\n", daten->nasabs);
    fprintf (out,"%s\n", daten->wqbez);
    fprintf (out,"%s\n", daten->qwv);
    fprintf (out,"%s\n", daten->kalmin);
    fprintf (out,"%s\n", daten->wehran);
    fprintf (out,"%s\n", daten->qmin);
    fprintf (out,"%s\n", daten->qstep);
    fprintf (out,"%s\n", daten->qmax);
    
    fprintf (out,"%d\n", daten->ia);
    fprintf (out,"%d\n", daten->ncar);
    fprintf (out,"%d\n", daten->nhyd);
    fprintf (out,"%d\n", daten->idat);
    fprintf (out,"%d\n", daten->nfrou);
    fprintf (out,"%d\n", daten->iauto);
    
    fprintf (out,"%s\n", daten->epsh);
    fprintf (out,"%s\n", daten->epsv);
    fprintf (out,"%s\n", daten->rny);
    fprintf (out,"%s\n", daten->cwr);
    fprintf (out,"%d\n", daten->schiess);
    
    write_tabelle(out,nasim_anfang,"NASIM");
    
    fprintf (out,"%d\n", daten->ifp);
    fprintf (out,"%d\n", daten->idr);
    
    fprintf (out,"%d\n", daten->ipunkt);
    fprintf (out,"%s\n", daten->izmax);
    fprintf (out,"%d\n", daten->nposey);
    fprintf (out,"%d\n", daten->nbeta);
    fprintf (out,"%d\n", daten->iform);
    fprintf (out,"%d\n", daten->inn);
    fprintf (out,"%s\n", daten->sm);
    fprintf (out,"%d\n", daten->hmo);//Dick 22.09.98
    fprintf (out,"%d\n", daten->wsf_q);//Dick 4.2.99
    fprintf (out,"%d\n", daten->wsf_l);//Dick 4.2.99
    fprintf (out,"%d\n", daten->u_grenze);//Dick 7.9.99
    
    fprintf (out,"%8.2lf\n", daten->dhwmax);//Dick 28.09.99
    fprintf (out,"%8.2lf\n", daten->vfmax);//Dick 28.09.99
    fprintf (out,"%8.2lf\n", daten->hzvmax);//Dick 28.09.99
    fprintf (out,"%8.2lf\n", daten->faklhg);//Dick 28.09.99
    fprintf (out,"%8.2lf\n", daten->ffmax);//Dick 28.09.99
    fclose(out);
    
    //falls Abbruch in DLG_209 (aufruf aus vzk_block) stop=true
    if((stop) &&(!aendern))
    {
      remove(str);
      for(e=xvt_slist_get_first(ber_list);e!=NULL;
      e=xvt_slist_get_next(ber_list,e))
      {
        temp=xvt_slist_get(ber_list,e,0L);
        for (i=79;i<=91;i++)
          aendern_name[i-79]=temp[i];
        aendern_name[12]='\0';
        if(xvt_str_match(aendern_name,ber_name_extern,0)==TRUE)
          xvt_slist_rem(ber_list,e);
      }
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
      strcat(str,"\\");
      strcpy(str2,STR_SPEC.name);
      str2[9]='\0';
      strcat(str2,"BER");
      strcat(str,str2);
      ber_file=fopen(str,"w");
      zaehler=zaehler-1;
      fprintf(ber_file,"%d\n",zaehler);
      
      for(e=xvt_slist_get_first(ber_list);e!=NULL;
      e=xvt_slist_get_next(ber_list,e))
      {
        temp=xvt_slist_get(ber_list,e,0L);
        strcpy(str,temp);
        for(i=0;i<=strlen(str);i++)
        {
          if(str[i]=='\n')
            str[i]='\0';
        }
        fprintf(ber_file,"%s\n",temp);
      }
      fclose(ber_file);
      stop=FALSE;
    } //if stop
 }
 
 delete[] aendern_name;
 delete[] merke_name;
 delete[] name;
 delete[] zaehler2;
 delete[] zaehler_help;

 }
 else // Version == BCE
 {
   FILE *out, *ber_file, *merke_datei;
  int dag;
  int i,len;
  char str[100],
    str2[15],
    help[101],
    anfang[8],
    ende[8],
    name[14],
    zaehler2[4],
    zaehler_help[4],
    aendern_name[100],
    merke_name[100];
  name[16];
  char *help2;
  char *temp;
  char * merke;
  extern FILE_SPEC ber_spec;
  int zaehler;
  SLIST_ELT e;
  int m=0, position=0;
  SLIST merke_list=NULL;
  
  
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
  strcat(str,"\\");
  
  strcpy(str2,STR_SPEC.name);
  str2[9]='\0';
  strcat(str2,"BER");
  strcat(str,str2);
  
  ///////////////////////////Wenn noch kein ber.file existiert:
  
  if ((ber_file = fopen(str,"r+"))==NULL)
  {
    for (i=0;i<=100;i++)  help[i]=' ';
    help[100]='\0';
    
    int len=strlen(daten->info);
    if (len>0)
      for (i=0;i<len;i++)
        help[i]=daten->info[i];
      
      //gcvt(daten->anfang,8,anfang);
      sprintf(anfang,"%.4lf",daten->anfang);
      for (i=61;i<61+(INT)strlen(anfang);i++)
        help[i]=anfang[i-61];
      
      //	 gcvt(daten->ende,8,ende);
      sprintf(ende,"%.4lf",daten->ende);
      for (i=70;i<70+(INT)strlen(ende);i++)
        help[i]=ende[i-70];
      
      /***/
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,merke_name,50);
      strcat(merke_name,"\\");
      strcat(merke_name,STR_SPEC.name);
      merke_name[strlen(merke_name)-3]='\0';
      strcat(merke_name,"mrk");
      
      if ((merke_datei=fopen(merke_name,"r"))!=NULL)
      {
        if (merke_list!=NULL)
        {
          xvt_slist_destroy(merke_list);
          merke_list=NULL;
        }
        merke_list=xvt_slist_create();
        name[0]='\0';
        while (!feof(merke_datei))
        {
          fgets (name,15,merke_datei);
          if (name[0]!='\n')
            xvt_slist_add_at_elt(merke_list,NULL,name,0);
          name[0]='\n';
        }
        
        e=xvt_slist_get_first(merke_list);
        merke=xvt_slist_get(merke_list,e,0L);
        for(i=79;i<=90;i++)
        {
          help[i]=merke[i-79];
          name[i-79]=merke[i-79];
        }
        help[91]='\0';
        name[12]='\0';
        xvt_slist_rem(merke_list,e);
        fclose(merke_datei);
        
        
        int	x=xvt_slist_count(merke_list);
        if(x>0)
        {
          merke_datei=fopen(merke_name,"w");
          for(e=xvt_slist_get_first(merke_list);e!=NULL;
          e=xvt_slist_get_next(merke_list,e))
          {
            merke=xvt_slist_get(merke_list,e,0L);
            fprintf(merke_datei,"%s\n",merke);
          }
          fclose(merke_datei);
          if (merke_list!=NULL)
          {
            xvt_slist_destroy(merke_list);
            merke_list=NULL;
          }
        }
        if (x==0)
        {
          if (merke_list!=NULL)
          {
            xvt_slist_destroy(merke_list);
            merke_list=NULL;
          }
          remove(merke_name);
        }
        
        
      } //if
      
      else
      {
        for(i=79;i<=87;i++)
        {
          help[i]=STR_SPEC.name[i-79];
          name[i-79]=STR_SPEC.name[i-79];
        }
        help[88]='0';
        name[9]='0';
        help[89]='0';
        name[10]='0';
        help[90]='1';
        name[11]='1';
        help[91]='\0';
        name[12]='\0';
      }
      /***/
      
      ber_file=fopen(str,"w");
      zaehler=1;
      
      fprintf(ber_file,"%d\n",zaehler);
      
      fprintf(ber_file,"%s",help);
      fclose(ber_file);
      
      
      if (ber_list !=NULL)
      {
        xvt_slist_destroy(ber_list);
        ber_list=NULL;
      }
      if  ((ber_list = xvt_slist_create())==NULL)
      {
        xvt_dm_post_error(" Can't create _SLIST:ber_list");
      }
      else
        
        xvt_slist_add_at_elt(ber_list,NULL,help,0);
      
      
      zeige_slist(ber_list);
      
  }
  
  else  /////////////////Ber.file existiert schon  -Datei einlesen
  {
    
    fgets(help,10,ber_file);
    
    zaehler = atoi(help);
    
    if (ber_list !=NULL)
    {
      xvt_slist_destroy(ber_list);
      ber_list=NULL;
    }
    if  ((ber_list = xvt_slist_create())==NULL)
			 {
      xvt_dm_post_error(" Can't create _SLIST:ber_list");
			 }
    else
    {
      for (i=1;i<=zaehler;i++)
      {
        if (!feof(ber_file))
        {
          fgets(help,100,ber_file);
          if (help[0]!='\n')
          {
            for (int k=0;k<=(INT)strlen(help);k++)
            {
              if(help[k]=='\n')
                help[k] ='\0';
            }
            xvt_slist_add_at_elt(ber_list,NULL,help,i-1);
            
          }
          help[0]='\n';
        }
      }
		  }
    int x=xvt_slist_count(ber_list);
    fclose(ber_file);
    
    
    
    //Datei .ber neu schreiben, um ein Element anzufügen
    
    if (aendern==FALSE)
    {
      ber_file = fopen(str,"w+");
      zaehler=zaehler+1;
      for (i=0;i<=100;i++)  help[i]=' ';
      help[100]='\0';
      
      if (strlen(daten->info)>0)
        for (i=0;i<(INT)strlen(daten->info);i++)
          help[i]=daten->info[i];
        
        //gcvt(daten->anfang,8,anfang);
        sprintf(anfang,"%.4lf",daten->anfang);
        for (i=61;i<61+(INT)strlen(anfang);i++)
          help[i]=anfang[i-61];
        
        //gcvt(daten->ende,8,ende);
        sprintf(ende,"%.4lf",daten->ende);
        for (i=70;i<70+(INT)strlen(ende);i++)
          help[i]=ende[i-70];
        
        /****/
        
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,merke_name,50);
        strcat(merke_name,"\\");
        
        strcat(merke_name,STR_SPEC.name);
        merke_name[strlen(merke_name)-3]='\0';
        strcat(merke_name,"mrk");
        
        if ((merke_datei=fopen(merke_name,"r"))!=NULL)
        {
          if (merke_list!=NULL)
          {
            xvt_slist_destroy(merke_list);
            merke_list=NULL;
          }
          merke_list=xvt_slist_create();
          name[0]='\0';
          while (!feof(merke_datei))
          {
            fgets (name,15,merke_datei);
            if (name[0]!='\n')
              xvt_slist_add_at_elt(merke_list,NULL,name,0);
            name[0]='\n';
          }
          
          e=xvt_slist_get_first(merke_list);
          merke=xvt_slist_get(merke_list,e,0L);
          for(i=79;i<=90;i++)
          {
            help[i]=merke[i-79];
            name[i-79]=merke[i-79];
          }
          help[91]='\0';
          name[12]='\0';
          xvt_slist_rem(merke_list,e);
          fclose(merke_datei);
          
          
          int	x=xvt_slist_count(merke_list);
          if(x>0)
          {
            merke_datei=fopen(merke_name,"w");
            for(e=xvt_slist_get_first(merke_list);e!=NULL;
            e=xvt_slist_get_next(merke_list,e))
            {
              merke=xvt_slist_get(merke_list,e,0L);
              fprintf(merke_datei,"%s\n",merke);
            }
            fclose(merke_datei);
            if (merke_list!=NULL)
            {
              xvt_slist_destroy(merke_list);
              merke_list=NULL;
            }
          }
          if (x==0)
          {
            if (merke_list!=NULL)
            {
              xvt_slist_destroy(merke_list);
              merke_list=NULL;
            }
            remove(merke_name);
          }
          
          
        } //if
        
        
        
        
        
        /****/
        else
        {
          for(i=79;i<=87;i++)
          {
            help[i]=STR_SPEC.name[i-79];
            name[i-79]=STR_SPEC.name[i-79];
          }
          
          itoa(zaehler, zaehler_help,10);
          
          len=strlen(zaehler_help);
          
          if (len==1)
          {
            zaehler2[0]='0';
            zaehler2[1]='0';
            zaehler2[2]=zaehler_help[0];
          }
          
          if (len==2)
          {
            zaehler2[0]='0';
            zaehler2[1]=zaehler_help[0];
            zaehler2[2]=zaehler_help[1];
          }
          if (len==3)
          {
            zaehler2[0]=zaehler_help[0];
            zaehler2[1]=zaehler_help[1];
            zaehler2[2]=zaehler_help[2];
          }
          
          zaehler2[3]='\0';
          name[9]='\0';
          strcat(name,zaehler2);
          
          
          for(i=79;i<91;i++)
            help[i]=name[i-79];
          
          help[91]='\0';
        } //else
        /***/
        
        fprintf(ber_file,"%d\n",zaehler);
        xvt_slist_add_at_elt(ber_list,NULL,help,0);
        xvt_slist_count(ber_list);
        
        for(e = xvt_slist_get_first(ber_list);e!=NULL;
        e = xvt_slist_get_next(ber_list,e))
        {
          help2=xvt_slist_get(ber_list,e,0L);
          fprintf(ber_file,"%s\n",help2);
        }
        
        
        
        fclose(ber_file);
 } //if  aendern=false
 
 /************************************/
	if (aendern)
  {
    ber_file = fopen(str,"w+");
    
    zaehler=zaehler;
    
    for (i=0;i<=100;i++)  help[i]=' ';
    
    help[100]='\0';
    
    if (strlen(daten->info)>0)
    {
      for (i=0;i<(INT)strlen(daten->info);i++)
        help[i]=daten->info[i];
    }
    //gcvt(daten->anfang,8,anfang);
    sprintf(anfang,"%.4lf",daten->anfang);
    for (i=61;i<61+(INT)strlen(anfang);i++)
      help[i]=anfang[i-61];
    
    //gcvt(daten->ende,8,ende);
    sprintf(ende,"%.4lf",daten->ende);
    for (i=70;i<70+(INT)strlen(ende);i++)
      help[i]=ende[i-70];
    
    for(i=79;i<=90;i++)
    {
      help[i]=ber_spec.name[i-79];
      name[i-79]=ber_spec.name[i-79];
    }
    name[12]='\0';
    
    help[91]='\0';
    
    
    
    
    
    /*Element suchen (nach name-position) und austauschen: */
    
    m=0;
    for(e=xvt_slist_get_first(ber_list);e!=NULL;
    e=xvt_slist_get_next(ber_list,e))
      
    {
      temp=xvt_slist_get(ber_list,e,0L);
      
      for (i=79;i<=91;i++)
      {
        aendern_name[i-79]=temp[i];
      }
      aendern_name[12]='\0';
      if(xvt_str_match(aendern_name,name,1)==TRUE)
        position=m;
      
      m++;
    }
    
    e=xvt_slist_get_first(ber_list);
    for(int h=1;h<=position;h++)
      e=xvt_slist_get_next(ber_list,e);
    xvt_slist_rem(ber_list,e);
    xvt_slist_add_at_pos(ber_list,position,help,0L);
    
    fprintf(ber_file,"%d\n",zaehler);
    
    for(e = xvt_slist_get_first(ber_list);e!=NULL;
    e = xvt_slist_get_next(ber_list,e))
    {
      help2=xvt_slist_get(ber_list,e,0L);
      fprintf(ber_file,"%s\n",help2);
    }
    
    fclose(ber_file);
    
  } //aendern=TRUE
  
  /******************************************/
  
} //else ber-file existiert



/*Eingabedaten speichern:*/

if (aendern==FALSE)
{
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
  strcat(str,"\\");
  strcat(str,name);
}

else
{
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
  strcat(str,"\\");
  strcat(str,ber_spec.name);
}

if ((out= fopen(str,"w+"))==NULL)
{
	 //xvt_dm_post_error("Fehler beim öffnen");
  char buf[200];//Dick 26.11.99
  xvt_res_get_str(STR_OPEN_ERR,buf,sizeof(buf));
  xvt_dm_post_error("%s %s",buf,str);
}

else
{
  for (dag=0; dag<13; dag++)
    fprintf (out,"%d\n", daten->werte[dag]);
  
  
  for (int k=0; k<=(INT)strlen(daten->gefaelle);k++)
		{
    if (daten->gefaelle[k]=='\n')
      daten->gefaelle[k]='\0';
		}
  fprintf (out,"%s\n", daten->gefaelle);
  
  for (k=0; k<=(INT)strlen(daten->qmin);k++)
		{
    if (daten->qmin[k]=='\n')
      daten->qmin[k]='\0';
		}
  fprintf (out,"%s\n", daten->qmin);
  
  for (k=0; k<=(INT)strlen(daten->qstep);k++)
		{
    if (daten->qstep[k]=='\n')
      daten->qstep[k]='\0';
		}
  fprintf (out,"%s\n", daten->qstep);
  
  for (k=0; k<=(INT)strlen(daten->qmax);k++)
		{
    if (daten->qmax[k]=='\n')
      daten->qmax[k]='\0';
		}
  fprintf (out,"%s\n", daten->qmax);
  
  if (daten->abfluss != MAXINT)
    fprintf (out,"%d\n", daten->abfluss);
  else
    fprintf (out,"\n");
  fprintf (out,"%f\n", daten->anfang);
  fprintf (out,"%f\n", daten->ende);
  fprintf (out,"%s\n", daten->info);
  fprintf (out,"%s\n", daten->hoehe);
  fprintf (out,"%d\n", daten->hmo);//Dick 10.06.99
  fprintf (out,"%d\n", daten->wsf_q);//Dick 10.06.99
  fprintf (out,"%d\n", daten->wsf_l);//Dick 10.06.99
  fprintf (out,"%s\n", daten->q);//Dick 10.06.99
  fprintf (out,"%d\n", daten->u_grenze);//Dick 7.9.99
  fprintf (out,"%d\n", daten->kalinin);//Dick 7.9.99
  
  fclose(out);
}
}; // Version == ?
}
/*************************/

void schreibe_bat_lwa()
{
  /* WIRD VERMUTLICH NICHT MEHR BENUTZT */
  extern FILE_SPEC qwert_spec, STR_SPEC;
  extern SLIST batch_list;
  extern st_daten steuerdaten;
  extern ZEILE *zeile, *zeile_anfang;

  char *bat, *strang_ptr;
  int  len=0,  zaehler=1, q_return=1, anzahl_qsatz=0, m,
    anzahl_slist, counter=1;
  unsigned int i=0,j=0,z=0;
  FILE *in,  *out, *qwert, *bat_file;
  
  char vzk_datei[MAX_PATH];//Dick 9.09.99
  
  char *help,
    *str,
    *qwert_datei,
    *temp_zaehler,
    *bat_file_text,
    *daten_verzeichnis,
    *daten_ausgabe;
  
  
  char *batbuffer,
		*str_zustand,
      *qsaetze,
      *gewaesser_name,
      *q_compare,
      *abfluss,
      *anfang_chr,
      *leer,
      *datei_anfang,
      *datei_ende,
      *stationen_text,
      *counter_char,
      *vzkdatei;
  
  
  ABSTAENDE *pabstand=NULL;
  ABSTAENDE *abstand_anfang=NULL;
  ABSTAENDE *abstand_ende=NULL;
  
  char bufgewname[20];
  
  
  SLIST_ELT e, element;
  
  double ende_compare, anfang_compare, ap=0, ep=0;
  double station_compare=0;
  
  //unsigned fuCmdShow;
  
  BOOLEAN k=FALSE;
  BOOLEAN k2=FALSE;//Dick 26.11.98 weil k2 nicht deklariert war
  SLIST strang_list1, strang_list2, strang_list3, datei_list_a;
  SLIST datei_list_e, stationen_list;
  
  float strang1, strang2, strang3; //war vor Fehler double
  double abfluss_zahl, wsp, he, sjo;
  double ap_double, min, step, max;
  
  MinMax Maxwert;
  
  help              = new char[120];// geändert:andresen/12.09.96
  str               = new char[100];
  qwert_datei       = new char[100];
  temp_zaehler      = new char[150];
  bat_file_text     = new char[100];
  daten_verzeichnis = new char[100];
  daten_ausgabe     = new char[100];
  
  batbuffer= new char[30];
  str_zustand= new char[20];//Dick 10 ->20 weil kann mehr als 10 sein
  qsaetze= new char[21];//Dick 13.07.99 12->21
  gewaesser_name= new char[20];
  q_compare= new char[21];
  abfluss= new char[15];//Dick 16.12.98 10->15
  anfang_chr= new char[16];//Dick 12 ->16 sonst ab und zu Problemen mit delete
  leer= new char[26];
  datei_anfang= new char[13];
  datei_ende= new char[13];
  stationen_text= new char[12];//Dick 10 ->12 weil kann mehr als 10 sein
  counter_char= new char[9];
  vzkdatei= new char[15];
  
  /***************ALLGEMEINES IN BATCHDATEI (DAG.BAT) SCHREIBEN********/
  //HIlfsdatei erzeugen, damit Meldung Stapeldatei fehlt nicht mehr kommt
  start[0]='\0';
		xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, start, 50);
    strcat(start,"\\help.bat");
    bat_file = fopen(start,"w");
    fprintf(bat_file,"Helpbat ist Testdatei\n");
    fclose(bat_file);
    
    start[0]='\0';
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, start, 50);
    strcat(start,"\\dag.bat");
    
    daten_verzeichnis[0]='\0';
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, daten_verzeichnis, 50);
    strcpy(daten_ausgabe,daten_verzeichnis);
    daten_ausgabe[strlen(daten_verzeichnis)-4]='\0';
    strcat(daten_ausgabe,"dath");
    strcat(daten_ausgabe,"\\");
    strcat(daten_verzeichnis,"\\");
    //für PROF-Verz.
    
    bat_file = fopen(start,"w");
    
    fprintf(bat_file,"@echo off\n");
    fprintf(bat_file,"@echo Bitte warten...\n");
    fprintf(bat_file,"cd\\\n");
    
    /******************************************************/
    
			 daten_verzeichnis[2]='\0'; 						//SICHERHEITSHALBER C:
       strcpy(bat_file_text,daten_verzeichnis);
       fprintf(bat_file,"%s\n",bat_file_text);
       daten_verzeichnis[2]='\\';
       bat_file_text[0]='\0';
       /******************************************************/
       for(z=2;z<strlen(daten_verzeichnis);z++) // CD START-DIRECTORY
       {
         bat_file_text[0]='\0';
         int y=0;
         
         while (daten_verzeichnis[z]!='\\')
         {
           bat_file_text[y]=daten_verzeichnis[z];
           z++;
           y++;
         }
         bat_file_text[y]='\0';
         if (bat_file_text[0]!='\0')
         {
           fprintf(bat_file,"cd ");
           fprintf(bat_file,"%s\n",bat_file_text);
         }
       }
       
       fprintf(bat_file,"@echo off\n");
       
       bat_file_text[0]='\0';                   // KOPF.TXT
       strcat(bat_file_text,"copy ");
       strcat(bat_file_text,start_dir);
       strcat(bat_file_text,"kopf.txt ");
       strcat(bat_file_text,daten_verzeichnis);
       strcat(bat_file_text,"kopf.txt");
       fprintf(bat_file,"%s>NUL\n",bat_file_text);
       
       fprintf(bat_file,"if exist %sf77l3.eer copy %sf77l3.eer %sf77l3.eer\n",
         start_dir, start_dir, daten_verzeichnis);
       
       //Dick 17.12.98
       fprintf(bat_file,"if exist %slf90.eer copy %slf90.eer %slf90.eer\n",
         start_dir, start_dir, daten_verzeichnis);
       //
       
       char str_help[100];
       strcpy(str_help,daten_verzeichnis);
       strcat(str_help,"verluste.tmp");
       int test_help=access(str_help,00);
       if(test_help==0)
       {
       /*		fprintf(bat_file,"del ");                      //verluste.tmp löschen
       fprintf(bat_file,"%s",daten_verzeichnis);       //sicherheitshalber
       fprintf(bat_file,"verluste.tmp>NUL\n");
         */
         fprintf(bat_file,"if exist verluste.tmp del verluste.tmp>NUL\n");
       }
       
       
       strcpy(str_help,daten_verzeichnis);
       strcat(str_help,"profil.vzk");
       test_help=access(str_help,00);
       if(test_help==0)
       {
       /*	  fprintf(bat_file,"del ");                      //profil.vzk löschen
       fprintf(bat_file,"%s",daten_verzeichnis);       //sicherheitshalber
       fprintf(bat_file,"profil.vzk>NUL\n");
         */
         fprintf(bat_file,"if exist profil.vzk del profil.vzk>NUL\n");
       }
       
       /*************EINLESEN BERECHNUNGSVARIANTE***************************/
       counter=1;
       
       for (e=xvt_slist_get_first(batch_list);e !=NULL;e=xvt_slist_get_next(batch_list,e))
       {
         bat = xvt_slist_get(batch_list,e,0L);
         strcpy(batbuffer,bat);
         strcat(batbuffer,"\0");
         str[0]='\0';
         xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
         strcat(str,"\\");
         strcat(str,batbuffer);
         
         read_varianten(str);
         
         
         /************Einlesen aus Vernetzungs-Datei**********/
         
         len=strlen(str);
         str[len-3]='\0';
         strcat(str,"str");
         
         if ((in= fopen(str,"r+"))==NULL)
         {
           //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",str);
           char buf[200],buf2[200];//Dick 26.11.99
           xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
           xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
           xvt_dm_post_error("%s%s%s",buf,str,buf2);
           exit(-1);
         }
         else
         {
           /****erste Zeile: 4 Angaben****/
           
           fscanf(in,"%d",&anzahl_profil_dat_entries);
           fscanf(in,"%d",&anzahl_strang_entries);
           fscanf(in,"%s",gewaesser_name);
           fscanf(in,"%s",str_zustand);
           strcpy(bufgewname, gewaesser_name);
           
           bufgewname[8]='\0';
           gewaesser_name[8]='\0';
           str_zustand[strlen(str_zustand)]='\0';
           
           
           /****\n einlesen****/
           
           fgets(help,110,in);
           
           
           /****oberen Block einlesen(profil_dat_entries)****/
           for (i=1;i<=(unsigned)anzahl_profil_dat_entries;i++)
           {
             if(!feof(in))
             {
               fgets(help,110,in);
             } /*if !=EOF*/
           }  /*for <prof-dat-entries*/
           
           /****Leerzeile einlesen****/
           
           fgets(help,110,in);
           
           
           /****unteren Block einlesen (strang_etriese)****/
           
           ende_compare=steuerdaten.ende;
           anfang_compare=steuerdaten.anfang;
           
           strang_list1=xvt_slist_create();
           strang_list2=xvt_slist_create();
           strang_list3=xvt_slist_create();
           datei_list_a=xvt_slist_create();
           datei_list_e=xvt_slist_create();
           stationen_list=xvt_slist_create();
           
           for (j=1;j<=(unsigned)anzahl_strang_entries;j++)
           {
             pabstand=new ABSTAENDE;
             pabstand->pabstand1=0;
             pabstand->pabstand2=0;
             pabstand->pabstand3=0;
             pabstand->next_abstand=NULL;
             if(!abstand_anfang)
               abstand_anfang=pabstand;
             else
               abstand_ende->next_abstand=pabstand;
             abstand_ende=pabstand;
             abstand_ende->next_abstand=NULL;
           }
           pabstand=abstand_anfang;
           for (j=1;j<=(unsigned)anzahl_strang_entries;j++)
           {
             if(!feof(in))
             {
               fscanf(in,"%s",stationen_text);
               fscanf(in,"%lf",&ep);
               fscanf(in,"%f",&strang1);
               fscanf(in,"%f",&strang2);
               fscanf(in,"%f",&strang3);
               fscanf(in,"%s",datei_anfang);
               fscanf(in,"%s",datei_ende);
               
               ap=atof(stationen_text);
               ap_double=ap;
               /*1000*/		strang1=strang1*1000;
               
               /*1000*/		   strang2=strang2*1000;
               
               /*1000*/		   strang3=strang3*1000;
               
               if ((ap>=anfang_compare && ep<=ende_compare) ||
                 (ap<=anfang_compare && ep>=ende_compare))
                 
               {
                 xvt_slist_add_at_elt(datei_list_a,NULL,datei_anfang,0);
                 xvt_slist_add_at_elt(datei_list_e,NULL,datei_ende,0);
                 xvt_slist_add_at_elt(stationen_list,NULL,stationen_text,0);
                 pabstand->pabstand1=strang1;
                 pabstand->pabstand2=strang2;
                 pabstand->pabstand3=strang3;
                 pabstand=pabstand->next_abstand;
               }
             }//eof
           } // <Anzahl strang entries
         } //else
         
         fclose(in);
         
         /********************Profil.str schreiben**********************/
         
         itoa(counter,counter_char,10);
         str[0]='\0';
         strcat(str,daten_verzeichnis);
         strcat(str,counter_char);
         strcat(str,".str");
         out=fopen(str,"w");
         
         /**** Kopf schreiben****/
         
         fprintf(out,"CC Gewaesser\n");
         //	fprintf(out,"%s\n", gewaesser_name); //konnte nicht gedruckt werden
         //	fprintf(out,"%s\n",bufgewname);  17.9.96 geändert
         char bezeichnung[100];
         lese_projektbezeichnung(bezeichnung);
         fprintf(out,"%s\n",bezeichnung);
         fprintf(out,"Station 0 + ");
         
         double st_anfang;
         st_anfang=steuerdaten.anfang*1000;
         
         fprintf(out,"%.2lf ",st_anfang);
         fprintf(out,"bis 0 + ");
         
         double st_ende;
         st_ende=steuerdaten.ende*1000;
         
         fprintf(out,"%.2f ",st_ende);
         fprintf(out,"m\n");
         //fprintf(out,"%s\n", str_zustand);  17.9.96 geändert
         fprintf(out,"%s\n",steuerdaten.info);
         /*			fprintf(out,"% 4d% 4d% 4d% 4d% 4d% 4d\n",
         steuerdaten.ia,steuerdaten.nhyd,steuerdaten.ncar,
         steuerdaten.idat, steuerdaten.iauto,steuerdaten.nfrou);
         */
         /*			fprintf(out,"% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d\n",
         steuerdaten.ia,steuerdaten.nhyd,steuerdaten.ncar,
         steuerdaten.idat, steuerdaten.iauto,steuerdaten.nfrou,
         steuerdaten.ifp, steuerdaten.idr);
         */
         int intizmax=atoi(steuerdaten.izmax);
         double doublesm=atof(steuerdaten.sm);
         fprintf(out,"% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 8.3lf\n",
           steuerdaten.ia,steuerdaten.nhyd,steuerdaten.ncar,
           steuerdaten.idat, steuerdaten.iauto,steuerdaten.nfrou,
           steuerdaten.ifp, steuerdaten.idr, steuerdaten.ipunkt,
           intizmax,steuerdaten.nposey, steuerdaten.nbeta,
           steuerdaten.iform, steuerdaten.inn, doublesm);
         
         double a,b,c,d;
         a=atof(steuerdaten.epsh);
         b=atof(steuerdaten.epsv);
         c=atof(steuerdaten.rny);
         d=atof(steuerdaten.cwr);
         //			fprintf(out,"% 8.2lf% 8.2lf% 12.2lf% 10.2lf\n",
         //					  a,b,c,d);
         /**16.02.96: geändert**/
         fprintf(out,"%lf %lf %lf %lf %8.2lf %8.2lf %8.2lf %8.2lf %8.1lf\n",
           a, b, c, d,steuerdaten.dhwmax,steuerdaten.vfmax,steuerdaten.hzvmax,
           steuerdaten.faklhg,steuerdaten.ffmax);//Dick 28.09.99 //Grenzwerte
         /****oberen Block schreiben****/
         
         ende_compare=steuerdaten.ende;
         anfang_compare=steuerdaten.anfang;
         
         anzahl_slist = xvt_slist_count(datei_list_a);
         pabstand=abstand_anfang;
         for(zaehler=1;zaehler<=anzahl_slist;zaehler++)
         {
           strang_ptr=xvt_slist_get_elt(datei_list_a,zaehler-1,0L);
           strcpy(datei_anfang,strang_ptr);
           strang_ptr=xvt_slist_get_elt(datei_list_e,zaehler-1,0L);
           strcpy(datei_ende,strang_ptr);
           strang_ptr=xvt_slist_get_elt(stationen_list,zaehler-1,0L);
           strcpy(stationen_text,strang_ptr);
           
           datei_anfang[12]='\0';
           datei_ende[12]='\0';
           stationen_text[strlen(stationen_text)]='\0';
           
           /**** Ablußwert holen****/
           
           qwert_datei[0]='\0';
           xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,qwert_datei,50);
           strcat(qwert_datei,"\\");
           strcat(qwert_datei,batbuffer);
           qwert_datei[strlen(qwert_datei)-3]='\0';
           strcat(qwert_datei,"qwt");
           qwert=fopen(qwert_datei,"r");
           
           q_compare[0]='\0';
           q_return=1;
           ap_double=atof(stationen_text);
           ap=ap_double;
           
           while ((q_return!=0) && (!feof(qwert)))
           {
             fscanf(qwert,"%s",q_compare);
             q_return=strcmp(q_compare,steuerdaten.q);
             fgets(qsaetze,20,qwert);
             anzahl_qsatz=atoi(qsaetze);
           }
           if (anzahl_qsatz!=0)
           {
             fscanf(qwert,"%lf",&station_compare);
             fgets(abfluss,15,qwert);
             j=1;
             char muell[20];
             while ((station_compare!=ap) && (j<(unsigned)anzahl_qsatz))//Dick 13.07.99 j< statt j<= sonst geht es zu weit
             {
               fscanf(qwert,"%lf",&station_compare);
               if(station_compare<=ap)//Dick 13.07.99 <= damit letzter definierter genommen wird
                 fgets(abfluss,15,qwert);
               else
                 fgets(muell,10,qwert);
               j++;
             }
           }
           if ((station_compare!=ap) && (zaehler!=1))
           {
             abfluss[0]='\0';
           }
           else
           {
             for (j=0;j<=(strlen(abfluss)+1);j++)
             {
               if (abfluss[j]=='\n')
                 abfluss[j]='\0';
             }
             abfluss[strlen(abfluss)-1]='\0'; //auf 2Stelle h.Komma kuerzen
           }
           fclose(qwert);
           /****/
           k=FALSE;
           k2=FALSE;
           if ((steuerdaten.kalmin[0]=='K') || (steuerdaten.nasabs[0]=='N'))
           {
             k=teste_k(nasim_anfang, ap_double);
             if(zaehler==anzahl_slist)
               k2=teste_k(nasim_anfang,steuerdaten.ende);
             if(k2==TRUE)
               k=FALSE;
             
           }
           
           abfluss_zahl=atof(abfluss);
           
           /*		fprintf(out,"         %s  %s  % 8.2lf% 8.2lf% 8.2lf",
           datei_anfang, datei_ende, pabstand->pabstand1,
           pabstand->pabstand2,pabstand->pabstand3);
           geändert 30.9.96
           */
           fprintf(out,"         %s  %s  % 11.2lf% 11.2lf% 11.2lf",
             datei_anfang, datei_ende, pabstand->pabstand1,
             pabstand->pabstand2,pabstand->pabstand3);
           
           if (abfluss_zahl>0)
             fprintf(out,"% 8.2lf", abfluss_zahl);
           else
             fprintf(out,"        ");
           if ((k==TRUE) ||(k2==TRUE))
           {
             if(k==TRUE)
               fprintf(out,"         K\n");
             if(k2==TRUE)
               fprintf(out,"        KK\n");
           }
           else
             fprintf(out,"          \n");
           pabstand=pabstand->next_abstand;
         } //for
         
         while(abstand_anfang)
         {
           pabstand=abstand_anfang;
           abstand_anfang=pabstand->next_abstand;
           if(pabstand!=NULL)
           {
             delete pabstand;
             pabstand=NULL;
           }
         }
         
         /**************Wert für Anfangswasserspiegel schaetzen*******/
         element=xvt_slist_get_first(datei_list_a);
         strang_ptr=xvt_slist_get(datei_list_a,element,0L);
         strcpy(datei_anfang,strang_ptr);
         strcpy(file_spec.name,datei_anfang);
         daten_verzeichnis[strlen(daten_verzeichnis)-1]='\0';
         xvt_fsys_convert_str_to_dir(daten_verzeichnis,&file_spec.dir);
         xvt_fsys_set_dir(&file_spec.dir);
         read_profildatei( pWPL, &STR_SPEC.dir, datei_anfang );
         list->GetMinMax(&Maxwert,1);
         strcat(daten_verzeichnis,"\\\0");
         steuerdaten.w_anfang=(float)Maxwert.maxY+2;
         list->DeleteList();
         
         
         /***************unteren Teil schreiben***********************/
         
         fprintf(out,"RANDBEDINGUNG\n");
         fprintf(out,"CC Text\n");
         
         /****EICHVO/EICHFL****/
         
         if (steuerdaten.eich[0]=='E')
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //	for (i=0;i<strlen(gewaesser_name);i++)     //GEWAESSERNAME
           //		temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q) && (i+14)<(25-1) ;i++) // Q-EREIGNIS Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.eich);i++)   // TYP
             temp_zaehler[i+25]=steuerdaten.eich[i];
           
           he=atof(steuerdaten.he);                   // HE-ENDW.
           wsp=atof(steuerdaten.hoehe);               // WSP-ANFW.
           
           fprintf(out,"%s% 8.2lf% 8.2lf                \n",
             temp_zaehler,wsp,he);
         } //eich
         
         /*****WSP******/
         if (steuerdaten.wasser[1]=='S' && steuerdaten.wehran[0]!='W' && steuerdaten.eich[0]!='E')//Dick 12.11.99
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //				  for (i=0;i<strlen(gewaesser_name);i++)       //GEWAESSERNAME
           //						temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++)// Q-EREIGNIS Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.wasser);i++)   // TYP
             temp_zaehler[i+25]=steuerdaten.wasser[i];
           if(steuerdaten.schiess==1)
             temp_zaehler[i+25]='-';
           
           gcvt(st_anfang,10,anfang_chr);
           m=45;
           for (i=strlen(anfang_chr);i>0;i--)           // STATION
           {
             temp_zaehler[m]=anfang_chr[i-1];
             m--;
           }
           
           wsp=atof(steuerdaten.hoehe);                 // WSP
           
           for(i=0;i<=15;i++)                           // LEER
             leer[i]=' ';
           leer[16]='\0';
           
           fprintf(out,"%s% 8.2lf%s        \n",temp_zaehler,wsp,leer);
           
         } //WSP
         /*******Ende WSP*******/
         
         /****HGRENZ****/
         
         if (steuerdaten.wasser[1]=='G' && steuerdaten.wehran[0]!='W')//Dick 12.11.99
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++) // Q-EREIGNIS 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.wasser);i++)  // TYP
             temp_zaehler[i+25]=steuerdaten.wasser[i];
           if(steuerdaten.schiess==1)
             temp_zaehler[i+24]='-';
           
           gcvt(st_anfang,10,anfang_chr);
           m=45;
           for (i=strlen(anfang_chr);i>0;i--)          // STATION
           {
             temp_zaehler[m]=anfang_chr[i-1];
             m--;
           }
           
           for(i=0;i<=15;i++)                           // LEER
             leer[i]=' ';
           leer[16]='\0';
           
           fprintf(out,"%s% 8.2f%s        \n",temp_zaehler,steuerdaten.w_anfang,leer);
           
         } //HRENZ
         
         /****HNORM****/
         
         if (steuerdaten.wasser[1]=='N' && steuerdaten.wehran[0]!='W')//Dick 12.11.99
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //			for (i=0;i<strlen(gewaesser_name);i++)      // GEWAESSENAME
           //				temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++) // Q-EREIGNIS Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.wasser);i++)  // TYP
             temp_zaehler[i+25]=steuerdaten.wasser[i];
           if(steuerdaten.schiess==1)
             temp_zaehler[i+25]='-';
           
           gcvt(st_anfang,10,anfang_chr);
           m=45;
           for (i=strlen(anfang_chr);i>0;i--)          // STATION
           {
             temp_zaehler[m]=anfang_chr[i-1];
             m--;
           }
           temp_zaehler[51]='\0';
           sjo=atof(steuerdaten.gefaelle);              // GEFAELLE
           float dummy=0.0;
           fprintf(out,"%s% 8.2f% 8.2f% 8.5lf        \n",
             temp_zaehler,steuerdaten.w_anfang,dummy,sjo);
         } //HNORM
         
         
         /****LPLOT****/
         
         if ((steuerdaten.lplot[0]=='L') && (steuerdaten.qplot[0]!='Q'))
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //		for (i=0;i<strlen(gewaesser_name);i++)      //GEWAESSERNAME
           //			temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++) // Q-EREIGNIS Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.lplot);i++)   // TYP
             temp_zaehler[i+25]=steuerdaten.lplot[i];
           
           for(i=0;i<=23;i++)                          // LEER
             leer[i]=' ';
           leer[24]='\0';
           
           fprintf(out,"%s%s        \n",temp_zaehler,leer);
           
         } //LPLOT
         
         
         /****QLPLOT****/
         
         if ((steuerdaten.lplot[0]=='L') && (steuerdaten.qplot[0]=='Q'))
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //		for (i=0;i<strlen(gewaesser_name);i++)    //GEWAESSERNAME
           //			temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++)// Q-EREIGNIS 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           temp_zaehler[25]='Q';		               // TYP
           temp_zaehler[26]='L';
           temp_zaehler[27]='P';
           temp_zaehler[28]='L';
           temp_zaehler[29]='O';
           temp_zaehler[30]='T';
           
           for(i=0;i<=23;i++)                       // LEER
             leer[i]=' ';
           leer[24]='\0';
           
           fprintf(out,"%s%s        \n",temp_zaehler,leer);
         } //QLPLOT
         
         
         
         /****QPLOT****/
         
         if ((steuerdaten.qplot[0]=='Q') && (steuerdaten.lplot[0]!='L'))
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //		for (i=0;i<strlen(gewaesser_name);i++)      //GEWAESSERNAME
           //			temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++) // Q-EREIGNIS Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.qplot);i++)   // TYP
             temp_zaehler[i+25]=steuerdaten.qplot[i];
           
           for(i=0;i<=23;i++)                          // LEER
             leer[i]=' ';
           leer[24]='\0';
           
           fprintf(out,"%s%s        \n",temp_zaehler,leer);
         } //QPLOT
         
         
         
         /****HGRALL****/
         
         st_anfang=steuerdaten.anfang*1000;
         
         if (steuerdaten.hgralle[0]=='H')
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //		  for (i=0;i<strlen(gewaesser_name);i++)       //GEWAESSERNAME
           //				temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++)// Q-EREIGNIS Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.hgralle);i++)  // TYP
             temp_zaehler[i+25]=steuerdaten.hgralle[i];
           
           for(i=0;i<=15;i++)                           // LEERZEICHEN
             leer[i]=' ';
           leer[16]='\0';
           
           fprintf(out,"%s% 8.2f%s        \n",temp_zaehler,steuerdaten.w_anfang,leer);
         } //hgrall
         
         
         
         /****HNORMA****/
         
         if (steuerdaten.normalle[0]=='H')
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //	  for (i=0;i<strlen(gewaesser_name);i++)        // GEWAESSERNAME
           //			temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++) // Q-EREIGNIS  Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.normalle);i++)  // Typ
             temp_zaehler[i+25]=steuerdaten.normalle[i];
           
           for(i=0;i<=15;i++)                            // LEERZEICHEN
             leer[i]=' ';
           leer[16]='\0';
           
           fprintf(out,"%s% 8.2f%s        \n",temp_zaehler,steuerdaten.w_anfang,leer);
         } //hnorma
         
         /****WEHRAN****/
         
         if (steuerdaten.wehran[0]=='W')
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++)// Q-EREIGNIS Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.wehran);i++)  // TYP
             temp_zaehler[i+25]=steuerdaten.wehran[i];
           
           gcvt(st_anfang,10,anfang_chr);
           m=45;
           for (i=strlen(anfang_chr);i>0;i--)          // STATION
           {
             temp_zaehler[m]=anfang_chr[i-1];
             m--;
           }
           
           for(i=0;i<=15;i++)                           // LEER
             leer[i]=' ';
           leer[16]='\0';
           
           fprintf(out,"%s% 8.2f%s\n",temp_zaehler,steuerdaten.w_anfang,leer);
           
         }
         
         
         
         if (steuerdaten.kalmin[0]=='K')
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           for (i=0;i<6;i++)  // TYP
             temp_zaehler[i+25]=steuerdaten.kalmin[i];
           for(i=0;i<=23;i++)                           // LEER
             leer[i]=' ';
           leer[24]='\0';
           
           fprintf(out,"%s%s\n",temp_zaehler,leer);
         }
         
         /**alles folgende ist noch eventuell zu aendern****/
         
         if (steuerdaten.nasabs[0]=='N')
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           for (i=0;i<6;i++)  // TYP
             temp_zaehler[i+25]=steuerdaten.nasabs[i];
           for(i=0;i<=23;i++)                           // LEER
             leer[i]=' ';
           leer[24]='\0';
           
           fprintf(out,"%s%s\n",temp_zaehler,leer);
           
           
         }
         
         
         /****NASALL****/
         
         if (steuerdaten.nasall[0]=='N')
         {
           steuerdaten.nasall[0]='\0';
           strcat(steuerdaten.nasall,"NASALL");
           steuerdaten.nasall[strlen(steuerdaten.nasall)]='\0';
           
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //	  for (i=0;i<strlen(gewaesser_name);i++)          //GEWAESSERNAME
           //			temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++)// Q-EREIGNSI  Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.nasall);i++)  // TYP
             temp_zaehler[i+25]=steuerdaten.nasall[i];
           
           for(i=0;i<=23;i++)                             // LEER
             leer[i]=' ';
           leer[24]='\0';
           
           
           fprintf(out,"%s%s        \n",temp_zaehler,leer);
         } //NASALL
         
         /****WQBEZ****/
         
         if (steuerdaten.wqbez[0]=='W')
         {
           /*************/
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++)// Q-EREIGNIS  Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           steuerdaten.wqbez[0]='\0';
           strcat(steuerdaten.wqbez,"WQBEZ");
           for (i=0;i<strlen(steuerdaten.wqbez);i++)  // TYP
             temp_zaehler[i+25]=steuerdaten.wqbez[i];
           
           gcvt(st_anfang,10,anfang_chr);
           m=45;
           for (i=strlen(anfang_chr);i>0;i--)          // STATION
           {
             temp_zaehler[m]=anfang_chr[i-1];
             m--;
           }
           temp_zaehler[51]='\0';
           min=atof(steuerdaten.qmin);
           step=atof(steuerdaten.qstep);
           max=atof(steuerdaten.qmax);
           fprintf(out,"%s% 8.2lf% 8.2lf% 8.2lf\n",
             temp_zaehler,min,step,max);
           //				  if(steuerdaten.wasser[1]=='N')
           //					fprintf(out,"  HNORM\n");
           //				  if (steuerdaten.wasser[1]=='G')
           //					fprintf(out,"  HGRENZ\n");
           //				  if (steuerdaten.wasser[1]=='S')
           //					fprintf(out,"        \n");
           
           /*****WEITERE ZEILE BEI WQBEZ************/
           if (steuerdaten.wasser[1]=='S')
           {
             for (i=0; i<=50;i++)
               temp_zaehler[i]=' ';
             temp_zaehler[51]='\0';
             
             for (i=0;i<strlen(steuerdaten.wasser);i++)   // TYP
               temp_zaehler[i+25]=steuerdaten.wasser[i];
             if(steuerdaten.schiess==1)
               temp_zaehler[i+25]='-';
             
             for(i=0;i<=15;i++)
               leer[i]=' ';
             leer[16]='\0';
             
             wsp=atof(steuerdaten.hoehe);
             fprintf(out,"%s% 8.2lf%s        \n",temp_zaehler,wsp,leer);
           } //WSP
           
           /*************/
           if (steuerdaten.wasser[1]=='G')
           {
             for (i=0; i<=50;i++)
               temp_zaehler[i]=' ';
             temp_zaehler[51]='\0';
             for (i=0;i<strlen(steuerdaten.wasser);i++)  // TYP
               temp_zaehler[i+25]=steuerdaten.wasser[i];
             if(steuerdaten.schiess==1)
               temp_zaehler[i+24]='-';
             
             for(i=0;i<=15;i++)                           // LEER
               leer[i]=' ';
             leer[16]='\0';
             fprintf(out,"%s% 8.2f%s        \n",temp_zaehler,steuerdaten.w_anfang,leer);
           } //HRENZ
           
           if (steuerdaten.wasser[1]=='N')
           {
             for (i=0; i<=50;i++)
               temp_zaehler[i]=' ';
             temp_zaehler[51]='\0';
             
             for (i=0;i<strlen(steuerdaten.wasser);i++)  // TYP
               temp_zaehler[i+25]=steuerdaten.wasser[i];
             if(steuerdaten.schiess==1)
               temp_zaehler[i+25]='-';
             
             sjo=atof(steuerdaten.gefaelle);              // GEFAELLE
             float dummy=0.0;
             fprintf(out,"%s% 8.2f% 8.2f% 8.5lf        \n",
               temp_zaehler,steuerdaten.w_anfang,dummy,sjo);
           } //HNORM
           
           if (steuerdaten.wehran[0]=='W')
           {
             for (i=0; i<=50;i++)
               temp_zaehler[i]=' ';
             temp_zaehler[51]='\0';
             
             for (i=0;i<strlen(steuerdaten.wehran);i++)  // TYP
               temp_zaehler[i+25]=steuerdaten.wehran[i];
             
             for(i=0;i<=15;i++)                           // LEER
               leer[i]=' ';
             leer[16]='\0';
             
             fprintf(out,"%s% 8.2f%s\n",temp_zaehler,steuerdaten.w_anfang,leer);
             
           }
           
         } //WQBEZ
         
         
         
         /****WTAU****/
         
         if (steuerdaten.wtau[0]=='W')
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //	  for (i=0;i<strlen(gewaesser_name);i++)       //GEWAESSERNAME
           //			temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++)// Q-EREIGNIS  Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.wtau);i++)     // TYP
             temp_zaehler[i+25]=steuerdaten.wtau[i];
           
           for(i=0;i<=23;i++)                           // LEERZ.
             leer[i]=' ';
           leer[24]='\0';
           
           fprintf(out,"%s%s        \n",temp_zaehler,leer);
         } //WTAU
         
         /*****neu 20.3***/
         if (steuerdaten.qwv[0]=='Q')
         {
           for (i=0; i<=50;i++)
             temp_zaehler[i]=' ';
           temp_zaehler[51]='\0';
           
           //	  for (i=0;i<strlen(gewaesser_name);i++)       //GEWAESSERNAME
           //			temp_zaehler[i+2]=gewaesser_name[i];
           
           for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
             temp_zaehler[i+2]=bufgewname[i];
           
           for (i=0;i<strlen(steuerdaten.q)&& (i+14)<(25-1);i++)// Q-EREIGNIS Dick 8.07.99
             temp_zaehler[i+14]=steuerdaten.q[i];
           
           for (i=0;i<strlen(steuerdaten.qwv);i++)     // TYP
             temp_zaehler[i+25]=steuerdaten.qwv[i];
           
           for(i=0;i<=23;i++)                           // LEERZ.
             leer[i]=' ';
           leer[24]='\0';
           
           fprintf(out,"%s%s        \n",temp_zaehler,leer);
         } //QWV
         
         /****************/
         
         
         destroy_tabelle();
         
         
         Delete_Zeile();
         fclose(out);
         
         if (strang_list1!=NULL)
         {
           xvt_slist_destroy(strang_list1);
           strang_list1=NULL;
         }
         if (strang_list2!=NULL)
         {
           xvt_slist_destroy(strang_list2);
           strang_list2=NULL;
         }
         if (strang_list3!=NULL)
         {
           xvt_slist_destroy(strang_list3);
           strang_list3=NULL;
         }
         if (datei_list_a!=NULL)
         {
           xvt_slist_destroy(datei_list_a);
           datei_list_a=NULL;
         }
         if (datei_list_e!=NULL)
         {
           xvt_slist_destroy(datei_list_e);
           datei_list_e=NULL;
         }
         if (stationen_list!=NULL)
         {
           xvt_slist_destroy(stationen_list);
           stationen_list=NULL;
         }
         /************************************************************************/
         
         strcpy(str_help,daten_verzeichnis);
         strcat(str_help,batbuffer);
         str_help[strlen(str_help)-3]='\0';
         strcat(str_help,"psi");
         test_help=access(str_help,00);
         if(test_help==0)
           Schreibe_Verlust_Datei_Knauf(batbuffer,counter_char); //in verluste.cpp
         /***********STARTBATCH (DAG.BAT) SCHREIBEN*FF****************************/
         //NEU Dick 24.11.99
         for(i=1;i<=6;i++)
         {
           if(i==1)
           {
             batbuffer[2]='w';      //tmp.wsp ins ausgabeverzeichnis
             batbuffer[3]='s';
           }
           if(i==2)
           {
             batbuffer[2]='e'; //27.9.96     //tmp.erg
             batbuffer[3]='r';  //27.9.96
           }
           if(i==3)
           {                                //tmp.bew
             batbuffer[2]='b'; //27.9.96
             batbuffer[3]='e';  //27.9.96
           }
           if(i==4)
           {                                //tmp.qpo
             batbuffer[2]='q'; //27.9.96
             batbuffer[3]='p';  //27.9.96
           }
           if(i==5)
           {                                 //tmp.lpo
             batbuffer[2]='l'; //27.9.96
             batbuffer[3]='p';  //27.9.96
             
           }
           if(i==6)
           {                                 //tmp.lpo
             batbuffer[2]='p'; //27.9.96
             batbuffer[3]='l';  //27.9.96
             
           }
           fprintf(bat_file,"if exist %s%s ",daten_ausgabe, batbuffer);
           fprintf(bat_file,"del %s%s>NUL\n",daten_ausgabe,batbuffer);
         }
         
         batbuffer[2]='0'; //27.9.96
         batbuffer[3]='0';  //27.9.96
         //ENDE NEU
         //profil.str
         fprintf(bat_file,"copy %s%s.str %sprofil.str>NUL\n",
           daten_verzeichnis,counter_char,daten_verzeichnis);
         
         //			fprintf(bat_file,"del %s%s.str>NUL\n",
         //			 daten_verzeichnis,counter_char);
         
         
         fprintf(bat_file,"if exist %s%s.psi ",daten_verzeichnis,counter_char);
         fprintf(bat_file,"copy %s%s.psi %sverluste.tmp>NUL\n",
           daten_verzeichnis,counter_char,daten_verzeichnis);
         fprintf(bat_file,"if exist %s%s.psi ",daten_verzeichnis,counter_char);
         fprintf(bat_file,"del %s%s.psi\n",daten_verzeichnis,counter_char);
         
         //Dick 9.09.99 für neue Ordnung des verzweigtes System
         //batbuffer[2]='v';
         //batbuffer[3]='z';
         vzk_datei[0]='\0';
         xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,vzk_datei,MAX_PATH);
         strcat(vzk_datei,"\\");
         strcat(vzk_datei,batbuffer);
         vzk_datei[strlen(vzk_datei)-3]='\0';
         strcat(vzk_datei,"vzk");
         
         
         fprintf(bat_file,"if exist %s ",vzk_datei);
         fprintf(bat_file,"copy %s %sprofil.vzk>NUL\n",vzk_datei,daten_verzeichnis);
         // ende
         
         fprintf(bat_file,"del %s%s.str>NUL\n", daten_verzeichnis,counter_char);                      //XXX.STR LOESCHEN
         
         //13.3.97 rausgenommen
         //			 fprintf(bat_file,"copy profil.str %sprofil.str>NUL\n",start_dir);
         fprintf(bat_file,"copy %sprofil.str %sprofil.str>NUL\n",daten_verzeichnis,start_dir);
         
         /***********/
         /*zum Testen für Knauf, Datei erhalten: 26.9.1996*/
         //		 fprintf(bat_file,"copy profil.str %sprofil$.str>NUL\n",start_dir);
         fprintf(bat_file,"copy profil.str %sprofil$.str>NUL\n",daten_ausgabe);
         /****************/
         fprintf(bat_file,"%s",start_dir);
         fprintf(bat_file,"b2lwa ");
         //	fprintf(bat_file,daten_verzeichnis); //auskomm. wg. Knauf s.o.
         //	fprintf(bat_file,"profil.str ");
         batbuffer[2]='w';
         batbuffer[3]='s';
         fprintf(bat_file,"%s ",batbuffer);
         
         if(counter==1)
           //				fprintf(bat_file,">%serror2.log\n",daten_verzeichnis);
           fprintf(bat_file,">%serror2.log\n",daten_ausgabe);
         else
           //				fprintf(bat_file,">>%serror2.log\n",daten_verzeichnis);
           fprintf(bat_file,">>%serror2.log\n",daten_ausgabe);
         
         fprintf(bat_file,"@echo Konvertierungsprogramm laeuft...\n");
         /*****/
         fprintf(bat_file,"@echo off\n");
         //			fprintf(bat_file,"rename %stmp.wsp %s\n",daten_verzeichnis,batbuffer);
         fprintf(bat_file,"if exist %stmp.wsp copy %stmp.wsp %s\n",daten_verzeichnis,daten_verzeichnis,batbuffer);
         
         batbuffer[2]='0';
         batbuffer[3]='0';
         
         //Dick 22.02.2000
         fprintf(bat_file,"if exist %stmp.wsp ",daten_verzeichnis);
         fprintf(bat_file,"del %stmp.wsp>NUL\n",daten_verzeichnis);
         //
         fprintf(bat_file,"if exist %sverluste.tmp ",daten_verzeichnis);
         fprintf(bat_file,"del %sverluste.tmp>NUL\n",daten_verzeichnis);
         /*****/
         fprintf(bat_file,"if exist %sprofil.vzk ",daten_verzeichnis);
         fprintf(bat_file,"del %sprofil.vzk>NUL\n",daten_verzeichnis);
         
         /****/
         /* tmp.wsp nach Datei mit namen schreiben vor ber. 27.9.96 */
         batbuffer[2]='w'; //27.9.96
         batbuffer[3]='s'; //27.9.96
         
         fprintf(bat_file,"if exist %stmp.wsp ", daten_verzeichnis);
         fprintf(bat_file,"copy %stmp.wsp %s%s\n",
           daten_verzeichnis, daten_verzeichnis, batbuffer);
         /****/
         fprintf(bat_file,"@echo ");
         fprintf(bat_file,"%s ",start_dir);
         //fprintf(bat_file,"%s ",daten_verzeichnis);
         fprintf(bat_file,"> wsp.ctr\n");
         //fprintf(bat_file,"@echo tmp.wsp >> wsp.ctr\n");  27.9.96 auskommentiert
         fprintf(bat_file,"@echo %s >> wsp.ctr\n", batbuffer);  //statt darüberliegender Zeile
         batbuffer[2]='e';
         batbuffer[3]='r';
         //fprintf(bat_file,"@echo tmp.erg >> wsp.ctr\n");
         fprintf(bat_file,"@echo %s >> wsp.ctr\n",batbuffer);
         batbuffer[2]='b';
         batbuffer[3]='e';
         //fprintf(bat_file,"@echo tmp.bew >> wsp.ctr\n");
         fprintf(bat_file,"@echo %s >> wsp.ctr\n",batbuffer);
         batbuffer[2]='q';
         batbuffer[3]='p';
         fprintf(bat_file,"@echo %s >> wsp.ctr\n",batbuffer);
         //	fprintf(bat_file,"@echo tmp.qpo >> wsp.ctr\n");
         batbuffer[2]='l';
         batbuffer[3]='p';
         fprintf(bat_file,"@echo %s >> wsp.ctr\n",batbuffer);
         //	fprintf(bat_file,"@echo tmp.lpo >> wsp.ctr\n");
         
         fprintf(bat_file,"@echo 0 >> wsp.ctr\n");
         
         fprintf(bat_file,"@echo Berechnungsprogramm laeuft...\n");
         
         fprintf(bat_file,"%s",start_dir);
         fprintf(bat_file,"wspr\n");
         
         fprintf(bat_file,"@echo off\n");
         
         for(i=1;i<=5;i++)
         {
           if(i==1)
           {
             batbuffer[2]='w';      //tmp.wsp ins ausgabeverzeichnis
             batbuffer[3]='s';
           }
           if(i==2)
           {
             batbuffer[2]='e'; //27.9.96     //tmp.erg
             batbuffer[3]='r';  //27.9.96
           }
           if(i==3)
           {                                //tmp.bew
             batbuffer[2]='b'; //27.9.96
             batbuffer[3]='e';  //27.9.96
           }
           if(i==4)
           {                                //tmp.qpo
             batbuffer[2]='q'; //27.9.96
             batbuffer[3]='p';  //27.9.96
           }
           if(i==5)
           {                                 //tmp.lpo
             batbuffer[2]='l'; //27.9.96
             batbuffer[3]='p';  //27.9.96
             
           }
           fprintf(bat_file,"if exist %s%s ",daten_verzeichnis, batbuffer);
           fprintf(bat_file,"copy %s%s %s%s>NUL\n",
             daten_verzeichnis,batbuffer,daten_ausgabe,batbuffer);
           fprintf(bat_file,"if exist %s%s ",daten_verzeichnis, batbuffer);
           fprintf(bat_file,"del %s%s>NUL\n",daten_verzeichnis,batbuffer);
         }
         
         batbuffer[2]='0'; //27.9.96
         batbuffer[3]='0';  //27.9.96
         
         //TAPE18.N85 UMBENENNEN
         batbuffer[2]='N'; //27.9.96
         batbuffer[3]='5';  //27.9.96
         fprintf(bat_file,"if exist %stape18.n85 ",daten_verzeichnis);
         fprintf(bat_file,"copy %stape18.n85 %s%s>NUL\n",
           daten_verzeichnis,daten_ausgabe,batbuffer);
         fprintf(bat_file,"if exist %stape18.n85 ",daten_verzeichnis);
         fprintf(bat_file,"del %stape18.n85>NUL\n",daten_verzeichnis);
         
         //TAPE18.N86 UMBENENNEN
         batbuffer[2]='N'; //27.9.96
         batbuffer[3]='6';  //27.9.96
         fprintf(bat_file,"if exist %stape18.n86 ",daten_verzeichnis);
         fprintf(bat_file,"copy %stape18.n86 %s%s>NUL\n",
           daten_verzeichnis,daten_ausgabe,batbuffer);
         fprintf(bat_file,"if exist %stape18.n86 ",daten_verzeichnis);
         fprintf(bat_file,"del %stape18.n86>NUL\n",daten_verzeichnis);
         
         bat_file_text[0]='\0';
         
         counter++;
    } //FOR BATCHLIST
    
		  /************ALLGEMEINES LOESCHEN***************************/
      char del_datei[20];
      for(i=1; i<=8;i++)
      {
        if (i==1)
          strcpy(del_datei,"tape18.n85");
        if (i==2)
          strcpy(del_datei,"kopf.txt");
        if (i==3)
          strcpy(del_datei,"profil.str");
        if (i==4)
          strcpy(del_datei,"wsp.ctr");
        if (i==5)
          strcpy(del_datei,"qvou**.tmp");
        if (i==5)
          strcpy(del_datei,"profil.vzk");
        if (i==6)
          strcpy(del_datei,"verluste.tmp");
        if (i==7)
          strcpy(del_datei,"f77l3.eer");
        if (i==8)  //Dick 17.12.98
          strcpy(del_datei,"lf90.eer");
        fprintf(bat_file,"if exist ");
        fprintf(bat_file,"%s ",del_datei);
        fprintf(bat_file,"del ");
        fprintf(bat_file,"%s>NUL\n", del_datei);
      }
      
      //	fprintf(bat_file,"del %sprofil.str>NUL\n",start_dir);
      
      fprintf(bat_file,"if exist profil.log copy profil.log %sprofil.log\n",daten_ausgabe);
      fprintf(bat_file,"if exist %sdag.bat copy %sdag.bat %sbat$.bat\n",
        //						daten_verzeichnis,daten_verzeichnis);
        daten_verzeichnis,daten_verzeichnis,daten_ausgabe);
      fprintf(bat_file,"if exist %shelp.bat ",daten_verzeichnis);
      fprintf(bat_file,"del ");                      //DAG.BAT selbst
      fprintf(bat_file,"%s",daten_verzeichnis);
      fprintf(bat_file,"help.bat>NUL\n");
      
      fprintf(bat_file,"@echo on\n");
      fprintf(bat_file,"@echo SCHLIESSEN SIE JETZT DIE DOS-BOX!\n");
      
      /********************************START******************************/
      fclose(bat_file);
      start[0]='\0';
      strcat(start,daten_verzeichnis);
      strcat(start,"dag.bat");
      
      //WinExec(start,SW_SHOWMAXIMIZED);
      
      //Dick 21.04.99
      STARTUPINFO sui_ausw;
      PROCESS_INFORMATION pi_ausw;
      
      BOOL bSuccess = FALSE;
      
      GetStartupInfo(&sui_ausw);
      
      sui_ausw.lpReserved = NULL;
      
      sui_ausw.lpTitle = NULL;
      sui_ausw.dwFlags |= STARTF_USESHOWWINDOW;
      sui_ausw.wShowWindow = SW_SHOW;
      
      bSuccess = CreateProcess(NULL, start, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui_ausw, &pi_ausw);
      //hProc=OpenProcess(PROCESS_QUERY_INFORMATION,FALSE,pi_ausw.dwProcessId);
      hProc_dag_bat = pi_ausw.hProcess;
      
      
      start[0]='\0';
      strcat(start,daten_verzeichnis);
      strcat(start,"help.bat"); //weil nach help.bat in timer sucht
      
      timer_zaehler2=0;
      timermain2=FALSE;
      timer_main2=xvt_timer_create(main_win,1000); //in wspwin.cpp
      
      
      
      delete[] help ; // geändert:andresen/12.09.96
      delete[] str  ;
      delete[] qwert_datei;
      delete[] temp_zaehler;
      delete[] bat_file_text;
      delete[] daten_verzeichnis;
      delete[] daten_ausgabe ;
      delete[] batbuffer;
      delete[] str_zustand;
      delete[] qsaetze;
      delete[] gewaesser_name;
      delete[] q_compare;
      delete[] abfluss;
      delete[] anfang_chr;
      delete[] leer;
      delete[] datei_anfang;
      delete[] datei_ende;
      delete[] stationen_text;
      delete[] counter_char;
      delete[] vzkdatei;
};


/*************SCHREIBE_BAT - BCE****************************************/
/***********************************************************/
void schreibe_bat(void)
{
  if( LWA_PROJEKT )
  {
    // wird noch benötigt, da Listauswertung und Verlgeichsauswertung diese Funktion benutzt
    schreibe_bat_lwa();
    return;
  }

  // die wspbce.cfg auslesen
  CharBuffer cfgPath( PATH_LEN );
  strcpy( cfgPath, start_dir );
  strcat( cfgPath, "wspbce.cfg" );
  FILE* cfgFile = fopen( cfgPath, "r" );
  if( cfgFile == NULL )
  {
    char buf[200];
    xvt_res_get_str( STR_WSPBCE_CFG_NO_FOUND, buf, sizeof(buf) ); // WspBce.Cfg nicht gefunden. Berechnung wurde nicht gestartet"
    xvt_dm_post_error( "%s",buf );
    return;
  };

  CharBuffer wspFile( PATH_LEN );
  fgets( wspFile, PATH_LEN, cfgFile );
  char* slashNPtr = strchr( wspFile, '\n' );
  if( slashNPtr )
    slashNPtr[0] = '\0';

  CharBuffer pauseStr( PATH_LEN );
  pauseStr[0] = '\0';
  if( !feof( cfgFile ) )
    fgets( pauseStr, PATH_LEN, cfgFile );

  fclose( cfgFile );
  
  extern char *str_zustand;
  extern SLIST batch_list;
  extern FILE_SPEC STR_SPEC, ber_spec, qwert_spec;
  extern st_daten steuerdaten;
  extern ZEILE *zeile, *zeile_anfang;

  CharBuffer new_file_name( PATH_LEN ), 
    old_file_name( PATH_LEN ), 
    help( PATH_LEN ),
    str( PATH_LEN ),
    out_name( PATH_LEN ), 
    verzeichnis( PATH_LEN );

  char strnamehelp[15];
  char zaehler_string[5];
  char datei_geben1[13], datei_geben2[13], gewaesser_datei_drucken[13];
  char gewaesser_name[20], zaehler_neu[5];
  
  int i=0, len=0, j=0, zaehler=1, retur;
  
  FILE *in, *out, *batxxx, *start_file;
  
  double km_geben1, km_geben2, ende_compare, anfang_compare;
  
  BOOLEAN start_bat_offen=FALSE;
  
  loesche_alle_mit_log(); // in util2.cpp
  for( SLIST_ELT e = xvt_slist_get_first( batch_list ); e != NULL; e = xvt_slist_get_next( batch_list, e ) )
  {
    /*************EINLESEN BERECHNUNGSVARIANTE***********/
    char* bat = xvt_slist_get(batch_list,e,0L);
    strcat(bat,"\0");
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,str,50);
    strcat(str,"\\");
    strcat(str,bat);
    
    read_varianten(str);
    
    //STR-SPEC:NAME DEFINIEREN FUER ANZAHL ABFLUSSEREIGNISSE BESTIMMEN:
    
    strcpy(strnamehelp,bat);
    strnamehelp[strlen(strnamehelp)-3]='\0';
    strcat(strnamehelp,"str");
    strcpy(STR_SPEC.name,strnamehelp);
    
    /******PROFILDATEIEN AUS ZUSTANDSDATEI SORTIEREN******/
    
    len=strlen(str);
    str[len-3]='\0';
    strcat(str,"str");
    
    if ((in= fopen(str,"r+"))==NULL)
			 {
      //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",str);
      char buf[200],buf2[200];//Dick 26.11.99
      xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
      xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
      xvt_dm_post_error("%s%s%s",buf,str,buf2);
      exit(-1);
			 }
    else
		  {
      fscanf(in,"%d",&anzahl_profil_dat_entries);
      fscanf(in,"%d",&anzahl_strang_entries);
      fscanf(in,"%s",&gewaesser_name);
      gewaesser_name[8]='\0';
      fscanf(in,"%s",&str_zustand);
      
      fgets(help,110,in);  //\N UEBERLESEN
      
      for (i=1;i<=anzahl_profil_dat_entries;i++)    //Profiltabelle lesen
      {
        if(!feof(in))
        {
          fgets(help,110,in);
        }
      }
      fgets(help,110,in); //Leerzeile einlesen
      
      for(j=1;j<=anzahl_strang_entries;j++)
      {
        if(!feof(in))
        {
          fscanf(in,"%lf",&km_geben1);
          fscanf(in,"%lf",&km_geben2);
          fscanf(in,"%s",(char*)help);
          fscanf(in,"%s",(char*)help);
          fscanf(in,"%s",(char*)help);
          fscanf(in,"%s",&datei_geben1);
          fscanf(in,"%s",&datei_geben2);
          
          for(i=0;i<=(INT)strlen(datei_geben2);i++)
          {
            if(datei_geben2[i]=='\n')
              datei_geben2[i]='\0';
          }
          AddNewZeile(datei_geben1,km_geben1);
          
        } /*if !=EOF*/
				  }  /*for <prof-dat-entries*/
      
      AddNewZeile(datei_geben2,km_geben2);
      
      fclose(in);
      
      
      /***************SORTIERTE DATEI FLUSSNAME.00X SCHREIBEN*****************/
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str,50);
      strcat(str, "\\");
      strcat(str, gewaesser_name);
      strcat(str,".");
      
      gewaesser_datei_drucken[0]='\0';
      strcat(gewaesser_datei_drucken, gewaesser_name);
      strcat(gewaesser_datei_drucken,".");
      
      itoa(zaehler,zaehler_string,10);
      len=strlen(zaehler_string);
      zaehler_neu[0]='\0';
      
      if (len==1)
        strcat(zaehler_neu,"00");
      
      if (len==2)
        strcat(zaehler_neu,"0");
      
      strcat(zaehler_neu,zaehler_string);
      strcat(str, zaehler_neu);
      strcat(gewaesser_datei_drucken,zaehler_neu);
      gewaesser_datei_drucken[strlen(gewaesser_datei_drucken)]='\0';
      
      out=fopen(str,"w");
      
      ende_compare=steuerdaten.ende;
      anfang_compare=steuerdaten.anfang;
      
      zeile=zeile_anfang;
      
      while ((zeile!=NULL)&&(zeile->station < anfang_compare))
        zeile=zeile->next_zeile;
      
      while ((zeile!=NULL)&&(zeile->station < ende_compare))
      {
        fprintf(out,"%s %.4lf\n", zeile->line, zeile->station);//Dick 24.08.99
        zeile=zeile->next_zeile;
      }
      
      //if ((zeile->station==ende_compare))
      if (fabs(zeile->station-ende_compare) < 1.e-006)//Dick 5.06.99 weil float ist nie gleich double !!!
        fprintf(out,"%s %.4lf", zeile->line, zeile->station);//Dick 24.08.99
      
      fclose(out);
      Delete_Zeile();
      
      
      }    /*else str.Datei einlesen*/
      
      /***************QWERT NACH \\PROF SCHREIBEN*************************/
      
      str[0]='\0';
      strcpy(str,bat);
      len=strlen(str);
      str[len-3]='\0';
      len=0;
      strcat(str,"qwt");
      strcpy(qwert_spec.name, str);
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 50);
      xvt_fsys_convert_str_to_dir(str,&qwert_spec.dir);
      str[0]='\0';
      
      xvt_fsys_convert_dir_to_str(&qwert_spec.dir,old_file_name,79);
      strcat(old_file_name,"\\");
      strcat(old_file_name,qwert_spec.name);
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,new_file_name,90);
      strcat(new_file_name, "\\");
      strcat(new_file_name,"qwert.");
      strcat(new_file_name,zaehler_neu);
      
      retur=-1;
      //	retur=copy_file(&old_file_name[0],&new_file_name[0]);
      retur=access(old_file_name,00);
      if(retur==0)
        retur=copy_fileb(old_file_name,new_file_name);
      
      /****************teilgebietsdatei nach prof schreiben: Dick 28.10.99 ***********/
      str[0]='\0';
      strcpy(str,bat);
      len=strlen(str);
      str[len-3]='\0';
      len=0;
      strcat(str,"tgb");
      strcpy(qwert_spec.name, str);
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 50);
      xvt_fsys_convert_str_to_dir(str,&qwert_spec.dir);
      str[0]='\0';
      
      xvt_fsys_convert_dir_to_str(&qwert_spec.dir,old_file_name,79);
      strcat(old_file_name,"\\");
      strcat(old_file_name,qwert_spec.name);
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,new_file_name,90);
      strcat(new_file_name, "\\");
      strcat(new_file_name,"teilg.");
      strcat(new_file_name,zaehler_neu);
      
      retur = -1;
      retur = access( old_file_name, 00 );
      bool bKalininTGProblem = false; // falls true, muss wird die Teilgebietsnummer vom Rechenkern abgefragt
      if( retur == 0 )
        retur=copy_fileb(old_file_name,new_file_name);
      else if( steuerdaten.kalinin == 1 && steuerdaten.werte[0] != 1 ) // Bordvoll mit Kalinin
      {
        // falls die Teilgebietsdatei nicht existiert, aber Kalinin-gerechnet werden soll
        // gibts nen Fehler und wir brechen ab
        char buf[200];
        xvt_res_get_str( STR_KALININ_FEHLER_TEILG, buf, sizeof( buf ) );
        xvt_dm_post_error( "%s", buf );
        
        // ein 'return' wäre hier schöner, das klappt aber mit dem Rest der Steuerung nicht
        bKalininTGProblem = true;
      }
      
      /***************VERLUSTDATEI*************************************/
      
      old_file_name[0]='\0';
      new_file_name[0]='\0';  
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,old_file_name,90);
      strcat(old_file_name, "\\");
      strcat(old_file_name,bat);
      len=strlen(old_file_name);
      old_file_name[len-3]='\0';
      strcat(old_file_name,"psi");
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,new_file_name,90);
      strcat(new_file_name, "\\");
      strcat(new_file_name,"psiver.");
      strcat(new_file_name,zaehler_neu);
      
      int handle=open(old_file_name,O_RDONLY);
      if (handle>0)
      {
        long laenge=filelength(handle);
        if (laenge>0)
          retur=copy_fileb(old_file_name,new_file_name);
      }
      close(handle);
      /************** DATEI BAT.00X SCHREIBEN***************************/
      
      /***OEFFNEN****/
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, out_name,50);
      strcat(out_name,"\\bat.");
      len=strlen(zaehler_string);
      
      if (len==1)
        strcat(out_name,"00");
      if (len==2)
        strcat(out_name,"0");
      
      strcat(out_name, zaehler_string);
      batxxx=fopen(out_name,"w+");
      
      /****SCHREIBEN***/
      
      /***ALLGEMEIN SCHREIBEN***/
      
      verzeichnis[0]='\0';
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, verzeichnis, 50);
      len=strlen(verzeichnis);
      verzeichnis[len-5]='\0';  //später eventuell rausnehmen für PROF
      
      fprintf(batxxx,"j\n");
      fprintf(batxxx,"%s\n", verzeichnis);
      fprintf(batxxx,"%s\n", gewaesser_datei_drucken); //Datei Flussname.00x
      fprintf(batxxx,"%.4lf\n", steuerdaten.anfang);//Dick 24.08.99
      fprintf(batxxx,"%.4lf\n", steuerdaten.ende);//Dick 24.08.99
      
      
      /****SCHREIBEN BORDVOLL-STAT-GLEICHF****/
      
      if (steuerdaten.werte[0]==2)
      {
        if (steuerdaten.werte[1]==3)
          fprintf(batxxx,"3\n");
        if ((steuerdaten.werte[1]==1) || (steuerdaten.werte[1]==2))
          fprintf(batxxx,"4\n");
        
        fprintf(batxxx,"1\n");
          /******kalinin miljukov neu****/ //Dick 28.10.99
          if( steuerdaten.kalinin == 1 )
          {
            fprintf( batxxx, "j\n" );
            if( bKalininTGProblem )
              fprintf( batxxx, "0\n" );
          }
          else
            fprintf(batxxx,"n\n");
          /*****************************/
        
        if (steuerdaten.werte[2]==1)
          fprintf(batxxx,"1\n");
        else
          fprintf(batxxx,"2\n");
        
        if (steuerdaten.werte[3]==0)
          fprintf(batxxx,"n\n");
        else
          fprintf(batxxx,"j\n");
        if (steuerdaten.werte[1]==1)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[1]==2)
          fprintf(batxxx,"2\n");
        
        if (steuerdaten.werte[10]==0)
          fprintf(batxxx,"1\n");
        else
          fprintf(batxxx,"2\n");
        
      } //IF BORDVOLL-STAT.GLEICHF.
      
      
      /****SCHREIBEN:BORDVOLL STAT - UNGLEICHF****/
      
      if (steuerdaten.werte[0]==3)
      {
        if (steuerdaten.werte[1]==3)
          fprintf(batxxx,"3\n");
        if ((steuerdaten.werte[1]==1) || (steuerdaten.werte[1]==2))
          fprintf(batxxx,"4\n");
        
        fprintf(batxxx,"2\n");

		// Erkenntnisse zu der Einheitenproblematik bzgl.
		// Abflussbereich Bordvollberechnung
		// der Rechenkern hat offensichtlich zwischen Eingabe als Integer und
		// als Double unterschieden. Im ersteren Fall hat er die Eingaben
		// als deci-liter/sec interpretiert, im letzteren Fall als m³/s
		// Neue Versionen des Rechenkern erlauben aber nur Eingaben in m³/s
		// deshalb erfolgt jetzt hier die Ausgabe als double in m³/s
		double qmin, qmax, qstep;
		sscanf( steuerdaten.qmin, "%lf", &qmin );
		sscanf( steuerdaten.qmax, "%lf", &qmax );
		sscanf( steuerdaten.qstep, "%lf", &qstep );

		const double qfaktor = 100;

        fprintf( batxxx,"%lf\n", qmax / qfaktor );
        fprintf( batxxx,"%lf\n", qmin / qfaktor );
        fprintf( batxxx,"%lf\n", qstep / qfaktor );

		// Ende Abflussbereich Bordvoll

        if( steuerdaten.werte[11] == 0 )
          fprintf(batxxx,"n\n");
        else
          fprintf(batxxx,"j\n");
        if (steuerdaten.werte[12]==0)
          fprintf(batxxx,"n\n");
        else
          fprintf(batxxx,"j\n");
        if (steuerdaten.werte[7]==2)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[7]==3)
        {
          fprintf(batxxx,"2\n");
          fprintf(batxxx,"%s\n",steuerdaten.gefaelle);
        }
          /******kalinin miljukov neu****/
          if( steuerdaten.kalinin == 1 )   //Dick 28.10.99
          {
            fprintf( batxxx, "j\n" );
            if( bKalininTGProblem )
              fprintf( batxxx, "0\n" ); // sonst Teilgebietsnummer 0 annehmen
          }
          else
            fprintf( batxxx, "n\n" );
          /*****************************/
        
        if (steuerdaten.werte[2]==1)
          fprintf(batxxx,"1\n");
        else
          fprintf(batxxx,"2\n");
        if (steuerdaten.werte[6]==1)
          fprintf(batxxx, "1\n");
        if (steuerdaten.werte[6]==2)
          fprintf(batxxx, "2\n");
        if (steuerdaten.werte[6]==3)
          fprintf(batxxx, "3\n");
        if (steuerdaten.werte[8]==0)
          fprintf(batxxx, "n\n");
        else
          fprintf(batxxx,"j\n");
        if (steuerdaten.werte[9]==0)
          fprintf(batxxx,"n\n");
        else
          fprintf(batxxx,"j\n");
        if (steuerdaten.werte[3]==0)
          fprintf(batxxx,"n\n");
        else
          fprintf(batxxx,"j\n");
        if (steuerdaten.werte[1]==1)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[1]==2)
          fprintf(batxxx,"2\n");
        if (steuerdaten.werte[5]==1)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[5]==2)
          fprintf(batxxx,"2\n");
        if (steuerdaten.werte[4]==1)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[4]==2)
          fprintf(batxxx,"2\n");
        if (steuerdaten.werte[10]==0)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[10]==1)
          fprintf(batxxx,"2\n");
        
      } //IF BORDVOLL STATIONAER UNGLEICHFOERMIG
      
      
      /****SCHREIBE WSP-LAGENBERECHNUNG****/
      
      if (steuerdaten.werte[0]==1)
      {
        if (steuerdaten.werte[1]==3)
          fprintf(batxxx,"1\n");
        if ((steuerdaten.werte[1]==1) || (steuerdaten.werte[1]==2))
          fprintf(batxxx,"2\n");
        
        int abfluss=steuerdaten.abfluss+1;
        int anzahlhq=teste_anzahl_hq(); //in qwert.cpp
        if(anzahlhq>1)
          fprintf(batxxx,"%d\n",abfluss);
        
        if (steuerdaten.werte[7]==1)
        {
          fprintf(batxxx, "1\n");
          fprintf(batxxx, "%s\n", steuerdaten.hoehe);
        }
        if (steuerdaten.werte[7]==2)
          fprintf(batxxx, "2\n");
        if (steuerdaten.werte[7]==3)
        {
          fprintf(batxxx, "3\n");
          fprintf(batxxx, "%s\n",steuerdaten.gefaelle);
        }
        if (steuerdaten.werte[2]==1)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[2]==2)
          fprintf(batxxx,"2\n");
        
        if (steuerdaten.werte[6]==1)
          fprintf(batxxx, "1\n");
        if (steuerdaten.werte[6]==2)
          fprintf(batxxx, "2\n");
        if (steuerdaten.werte[6]==3)
          fprintf(batxxx, "3\n");
        if (steuerdaten.werte[8]==0)
          fprintf(batxxx, "n\n");
        else
          fprintf(batxxx,"j\n");
        if (steuerdaten.werte[9]==0)
          fprintf(batxxx,"n\n");
        else
          fprintf(batxxx,"j\n");
        if (steuerdaten.werte[3]==0)
          fprintf(batxxx,"n\n");
        else
          fprintf(batxxx,"j\n");
        if (steuerdaten.werte[1]==1)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[1]==2)
          fprintf(batxxx,"2\n");
        if (steuerdaten.werte[5]==1)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[5]==2)
          fprintf(batxxx,"2\n");
        if (steuerdaten.werte[4]==1)
          fprintf(batxxx,"1\n");
        if (steuerdaten.werte[4]==2)
          fprintf(batxxx,"2\n");
      } //if Wsp.-lagen-berechnung***)
      
      fclose(batxxx);
      
      
      /*****************************STARTEN/LOESCHEN***********************/
      char daten_verzeichnis[MAX_PATH];
      str[0]='\0';
      strcat(str,start_dir);
      strcat( str, wspFile );
      strcat(str," < ");
      strcat(str,out_name);
      
      bat_start[0]='\0';
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, bat_start, 50);
      
      strcpy(daten_verzeichnis,bat_start);
      
      strcat(bat_start,"\\start.bat");
      if (start_bat_offen==FALSE)
        start_file=fopen(bat_start,"w");
      
      fprintf(start_file,"@echo Bitte warten...\n");
      fprintf(start_file,"@echo Berechnung laeuft.\n");
      
      //Dick 20.07.99
      fprintf(start_file,"if exist %sf77l3.eer copy %sf77l3.eer %s\\f77l3.eer\n",
        start_dir, start_dir, daten_verzeichnis);
      fprintf(start_file,"if exist %slf90.eer copy %slf90.eer %s\\lf90.eer\n",
        start_dir, start_dir, daten_verzeichnis);
      
      len=strlen(daten_verzeichnis);
      daten_verzeichnis[len-4]='\0';
      strcat(daten_verzeichnis,"dath\\");
      
      fprintf(start_file,"if exist %sf77l3.eer copy %sf77l3.eer %sf77l3.eer\n",
        start_dir, start_dir, daten_verzeichnis);
      fprintf(start_file,"if exist %slf90.eer copy %slf90.eer %slf90.eer\n",
        start_dir, start_dir, daten_verzeichnis);
      
		fprintf( start_file, "%s\n", pauseStr );
      
      fprintf(start_file,"%s",str);
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str,50);
      /**/
      len=strlen(str);
      str[len-4]='\0';
      strcat(str,"dath\\");
      /**/
      
      if(zaehler==1)
        fprintf(start_file,">%serror2.log\n",str);
      else
        fprintf(start_file,">>%serror2.log\n",str);

		fprintf( start_file, "%s\n", pauseStr );

      /*****************DATEIEN LOESCHEN********************/
      fprintf(start_file,"if exist %s del %s>NUL\n",out_name,out_name);			    //BAT.XXX LOESCHEN

      /*****************************************************/
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 50);
      strcat(str,"\\");
      strcat(str,gewaesser_datei_drucken);
      fprintf(start_file,"if exist %s del %s>NUL\n",str,str);       //GEWAESSERNAME.00X loeschen
      
      /*******************************************************/
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,new_file_name,90);
      len=strlen(new_file_name);
      new_file_name[len-5]='\0';
      strcat(new_file_name,"\\prof");
      strcat(new_file_name, "\\");
      strcat(new_file_name,"qwert.");
      strcat(new_file_name,zaehler_neu);
      fprintf(start_file,"if exist %s del %s>NUL\n",new_file_name,new_file_name);                  //QWERT.00X LOESCHEN
      
      /*****************************************/
      new_file_name[0]='\0';
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,new_file_name,90);
      strcat(new_file_name, "\\");
      strcat(new_file_name,"psiver.");
      strcat(new_file_name,zaehler_neu);
      fprintf(start_file,"if exist %s del %s>NUL\n",new_file_name,new_file_name);                  //verlustdatei LOESCHEN
      
      /******************************************/
      int abflussdateida=0;
      if( steuerdaten.werte[0] != 2 && steuerdaten.werte[0] != 3 )  //bei bordvoll gleichf. muß kein Abfluß da sein
      {
        xvt_fsys_convert_dir_to_str(&qwert_spec.dir,str,79);
        strcat(str,"\\");
        strcat(str,qwert_spec.name);
        if((in=fopen(str,"r"))==NULL)
        {
          //xvt_dm_post_note("Abflußdatei fehlt!");
          char buf[200];//Dick 26.11.99
          xvt_res_get_str(STR_KEINE_QWERTDATEI,buf,sizeof(buf));
          xvt_dm_post_note("%s",buf);
        }
        else
        {
          abflussdateida=1;
          /****/
          if (ptr_qwert_anfang!=NULL)
            Delete_Qwert_Datei();
          lese_qwert_datei();
          TesteEndeQwert();
          lese_ereignisse();
          char *help2=xvt_slist_get_elt(abflussereignisse,steuerdaten.abfluss,0L);
          strcpy(help,help2);
          help[3]='\0'; //20.01.99 bley
          if (ptr_qwert_anfang!=NULL)
            Delete_Qwert_Datei();
          if (abflussereignisse !=NULL)
          {
            xvt_slist_destroy(abflussereignisse);
            abflussereignisse=NULL;
          }
          /****/
          
          fclose(in);
        }
      }																						//*.tab umbenennen
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, old_file_name,50);
      len=strlen(old_file_name);
      old_file_name[len-4]='\0';
      strcat(old_file_name,"\\dath\\");
      strcpy(str,gewaesser_name);
      //neu 17.12.98 bley:
      int len2=strlen(gewaesser_name);
      if(len2<4)
        str[len2]='\0';
      else   //ende neu 17.12.98 bley
        str[4]='\0';
      
      if((steuerdaten.werte[0]==1)&&(abflussdateida))
      {
        strcat(str,"_");
        strcat(str,help);
        int len=strlen(help);

        str[8]='\0';
        strcat(str,".tab");
        
      }
      if(steuerdaten.werte[0]==2)
      {
        strcat(str,".tab");
      }
      strcat(old_file_name,str);
      
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, new_file_name,50);
      len=strlen(new_file_name);
      new_file_name[len-4]='\0';
      strcat(new_file_name,"\\dath\\");
      bat[2]='t';
      bat[3]='b';
      strcat(new_file_name,bat);
      
      fprintf(start_file,"copy %s %s\n",old_file_name,new_file_name);
      
      if(steuerdaten.werte[0]==1)
      {
        len=strlen(old_file_name);
        old_file_name[len-3]='\0';
        strcat(old_file_name,"wsl");
      }
      if((steuerdaten.werte[0]==2) || (steuerdaten.werte[0]==3))
      {
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, old_file_name,50);
        len=strlen(old_file_name);
        old_file_name[len-4]='\0';
        strcat(old_file_name,"\\dath\\");
        str[0]='\0';
        for(i=0;i<=7;i++)
          str[i]=gewaesser_name[i];
        str[8]='\0';
        strcat(str,".qb1");
        strcat(old_file_name,str);
        //			  strcpy(str,gewaesser_name);
        //			  str[4]='\0';
      }
      len=strlen(new_file_name);
      new_file_name[len-10]='w';
      new_file_name[len-9]='l';
      fprintf(start_file,"copy %s %s\n",old_file_name,new_file_name);
      fprintf(start_file,"if not exist %s del %s>NUL\n",old_file_name,new_file_name);//Dick 28.06.99 damit klar ist ,ob Berechnung erfolgreich war
      bat[2]='0';
      bat[3]='0';
      
      /******************************************/
      //Dick 28.06.99 damit klar ist ,ob Berechnung erfolgreich war
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, old_file_name,50);
      len=strlen(old_file_name);
      old_file_name[len-4]='\0';
      strcat(old_file_name,"dath\\");
      fprintf(start_file,"del %s*.tab\n",old_file_name);
      fprintf(start_file,"del %s*.wsl\n",old_file_name);
      //
      start_bat_offen=TRUE;
      zaehler++;
  } // FOR ENDE BATCHLIST
  
  
  /**************************/
  
  start_bat_offen=FALSE;

  CharBuffer help_start( PATH_LEN );
	strcpy( help_start, bat_start );

  fprintf(start_file,"copy %s",bat_start);
  bat_start[0]='\0';
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, bat_start, 50);
  strcat(bat_start,"\\start$.bat");
  fprintf(start_file," %s\n",bat_start);
  
  fprintf(start_file,"if exist %s del ",bat_start);
  fprintf(start_file,"%s\n",bat_start);           //START.BAT SELBST LOESCHEN
  fprintf(start_file,"@echo on\n");
  fprintf(start_file,"@echo SCHLIESSEN SIE JETZT DIE DOS-BOX!\n");
    /********************STARTEN*****************************/
    
  fprintf( start_file, "%s\n", pauseStr );
    
  fclose(start_file);
       
  STARTUPINFO sui_ausw;
  PROCESS_INFORMATION pi_ausw;
  
  BOOL bSuccess = FALSE;
  
  GetStartupInfo( &sui_ausw );
  
  sui_ausw.lpReserved = NULL;
  
  sui_ausw.lpTitle = NULL;
  sui_ausw.dwFlags |= STARTF_USESHOWWINDOW;
  sui_ausw.wShowWindow = SW_SHOW;
  
//MessageBox( 0, "Jetzt kommt die wsp.exe", "WSPWIN", MB_OK );

  bSuccess = CreateProcess(NULL, help_start, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui_ausw, &pi_ausw);
  hProc_dag_bat = pi_ausw.hProcess;

  timer_zaehler2=0;
  timermain2=FALSE;
  timer_main2=xvt_timer_create(main_win,1000); //in wspwin.cpp
} //Funktion

/**********************************************************************/
/************************BCE-Version****************************************/
void read_varianten (char *string)
{
  if( !LWA_PROJEKT )
  {
  FILE *in;
  char temp[100];
  char read_temp[256];
  extern st_daten steuerdaten;
  
  if ((in= fopen(string,"r+"))==NULL)
  {
    //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",string);
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
    xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
    xvt_dm_post_error("%s%s%s",buf,string,buf2);
    exit(-1);
  }
		else
    {
      for (int i=0; i<=12; i++)
      {
        fgets(temp,4,in);
        steuerdaten.werte[i] =atoi(temp);
      }
      
      fgets (steuerdaten.gefaelle,10,in);
      for (int k=0; k<=(INT)strlen(steuerdaten.gefaelle); k++)
      {
        if (steuerdaten.gefaelle[k]=='\n')
          steuerdaten.gefaelle[k]='\0';
      }
      fgets (steuerdaten.qmin,11,in);
      for (k=0; k<=(INT)strlen(steuerdaten.qmin); k++)
      {
        if (steuerdaten.qmin[k]=='\n')
          steuerdaten.qmin[k]='\0';
      }
      fgets (steuerdaten.qstep,11,in);
      for (k=0; k<=(INT)strlen(steuerdaten.qstep); k++)
      {
        if (steuerdaten.qstep[k]=='\n')
          steuerdaten.qstep[k]='\0';
      }
      fgets (steuerdaten.qmax,11,in);
      for (k=0; k<=(INT)strlen(steuerdaten.qmax); k++)
      {
        if (steuerdaten.qmax[k]=='\n')
          steuerdaten.qmax[k]='\0';
      }
      fgets (temp,25,in);
      steuerdaten.abfluss=atoi(temp);
      
      fgets (temp,25,in);
      steuerdaten.anfang=atof(temp);
      
      fgets (temp,25,in);
      steuerdaten.ende=atof(temp);
      
      fgets (steuerdaten.info,60,in);
      for (k=0; k<=(INT)strlen(steuerdaten.info); k++)
      {
        if (steuerdaten.info[k]=='\n')
          steuerdaten.info[k]='\0';
      }
      fgets (steuerdaten.hoehe,60,in);
      for (k=0; k<=(INT)strlen(steuerdaten.hoehe); k++)
      {
        if (steuerdaten.hoehe[k]=='\n')
          steuerdaten.hoehe[k]='\0';
      }
      
      /***********hmo*****************///Dick 22.09.98
      temp[0]='\0';
      if(!feof(in))
      {
        //fgets(temp,5,in);
        fgets (read_temp,256,in);
        sscanf(read_temp,"%5c",temp);
        if((temp[0]!='\0')&& (temp[0]!='\n'))
          steuerdaten.hmo=atoi(temp);
        else
          steuerdaten.hmo=0;
      }
      else
        steuerdaten.hmo=0;
      /*******************************/
      /***********wsf_q*****************///Dick 4.2.99
      temp[0]='\0';
      if(!feof(in))
      {
        //fgets(temp,5,in);
        fgets (read_temp,256,in);
        sscanf(read_temp,"%5c",temp);
        if((temp[0]!='\0')&& (temp[0]!='\n'))
          steuerdaten.wsf_q=atoi(temp);
        else
          steuerdaten.wsf_q=0;
      }
      else
        steuerdaten.wsf_q=0;
      /*******************************/
      /***********wsf_l*****************///Dick 4.2.99
      temp[0]='\0';
      if(!feof(in))
      {
        //fgets(temp,5,in);
        fgets (read_temp,256,in);
        sscanf(read_temp,"%5c",temp);
        if((temp[0]!='\0')&& (temp[0]!='\n'))
          steuerdaten.wsf_l=atoi(temp);
        else
          steuerdaten.wsf_l=0;
      }
      else
        steuerdaten.wsf_l=0;
      /*******************************/
      /***********q*****************///Dick 10.6.99
      if(!feof(in))
      {
        fgets (read_temp,256,in);
        sscanf(read_temp,"%20c",steuerdaten.q);
        slashn_entfernenb(steuerdaten.q);
      }
      /***********u_grenze*****************///Dick 28.09.99
      temp[0]='\0';
      if(!feof(in))
      {
        //fgets(temp,5,in);
        fgets (read_temp,256,in);
        sscanf(read_temp,"%5c",temp);
        if((temp[0]!='\0')&& (temp[0]!='\n'))
          steuerdaten.u_grenze=atoi(temp);
        else
          steuerdaten.u_grenze=0;
      }
      else
        steuerdaten.u_grenze=0;
      /*******************************/
      /***********kalinin*****************///Dick 28.10.99
      temp[0]='\0';
      if(!feof(in))
      {
        //fgets(temp,5,in);
        fgets (read_temp,256,in);
        sscanf(read_temp,"%5c",temp);
        if((temp[0]!='\0')&& (temp[0]!='\n'))
          steuerdaten.kalinin=atoi(temp);
        else
          steuerdaten.kalinin=0;
      }
      else
        steuerdaten.kalinin=0;
      /*******************************/
      
      
     } //else
     fclose(in);
 }
 else // Version == LWA
 {
   FILE *in;
   char temp[100];
   extern st_daten steuerdaten;
   char read_temp[256];
   xvt_fsys_set_dir(&STR_SPEC.dir);//Dick 28.07.99
   if ((in= fopen(string,"r+"))==NULL)
   {
     //xvt_dm_post_error(" Datei : %s läßt sich nicht öffnen !",string);
     char buf[200],buf2[200];//Dick 26.11.99
     xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
     xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
     xvt_dm_post_error("%s%s%s",buf,string,buf2);
     exit(-1);
   }
   else
   {
     nasim_anfang = MakeNewTabelle(tabellenlaenge);
     
     //fgets (steuerdaten.info,60,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%60c",steuerdaten.info);
     slashn_entfernenb(steuerdaten.info);
     
     //fgets (temp,25,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%25c",temp);
     steuerdaten.anfang=atof(temp);
     
     //fgets (temp,25,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%25c",temp);
     steuerdaten.ende=atof(temp);
     
     //fgets (steuerdaten.eich,9,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%9c",steuerdaten.eich);
     slashn_entfernenb(steuerdaten.eich);
     
     //fgets (steuerdaten.he,10,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%10c",steuerdaten.he);
     slashn_entfernenb(steuerdaten.he);
     
     //fgets (steuerdaten.hgralle,9,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%9c",steuerdaten.hgralle);
     slashn_entfernenb(steuerdaten.hgralle);
     
			  //fgets (steuerdaten.normalle,10,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%10c",steuerdaten.normalle);
     slashn_entfernenb(steuerdaten.normalle);
     
     //fgets (steuerdaten.wtau,7,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%7c",steuerdaten.wtau);
     slashn_entfernenb(steuerdaten.wtau);
     
     //fgets (steuerdaten.wasser,9,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%9c",steuerdaten.wasser);
     slashn_entfernenb(steuerdaten.wasser);
     
     //fgets (steuerdaten.hoehe,21,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%21c",steuerdaten.hoehe);
     slashn_entfernenb(steuerdaten.hoehe);
     
     //fgets (steuerdaten.gefaelle,100,in);//Dick 28.04.99 10->100
     fgets (read_temp,256,in);
     sscanf(read_temp,"%10c",steuerdaten.gefaelle);
     slashn_entfernenb(steuerdaten.gefaelle);
     
     //fgets (temp,10,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%10c",temp);
     steuerdaten.sel_index=atoi(temp);
     
     //fgets (steuerdaten.q,20,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%20c",steuerdaten.q);
     slashn_entfernenb(steuerdaten.q);
     
     //fgets (steuerdaten.qplot,7,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%7c",steuerdaten.qplot);
     slashn_entfernenb(steuerdaten.qplot);
     
     //fgets (steuerdaten.lplot,7,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%7c",steuerdaten.lplot);
     slashn_entfernenb(steuerdaten.lplot);
     
     //fgets (steuerdaten.nasall,11,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%11c",steuerdaten.nasall);
     slashn_entfernenb(steuerdaten.nasall);
     
     //fgets (steuerdaten.nasabs,18,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%18c",steuerdaten.nasabs);
     slashn_entfernenb(steuerdaten.nasabs);
     
     //fgets (steuerdaten.wqbez,7,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%7c",steuerdaten.wqbez);
     slashn_entfernenb(steuerdaten.wqbez);
     
     //fgets (steuerdaten.qwv,5,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%5c",steuerdaten.qwv);
     slashn_entfernenb(steuerdaten.qwv);
     
     //fgets (steuerdaten.kalmin,9,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%9c",steuerdaten.kalmin);
     slashn_entfernenb(steuerdaten.kalmin);
     
     //fgets (steuerdaten.wehran,9,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%9c",steuerdaten.wehran);
     slashn_entfernenb(steuerdaten.wehran);
     
     //fgets (steuerdaten.qmin,9,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%9c",steuerdaten.qmin);
     slashn_entfernenb(steuerdaten.qmin);
     
     //fgets (steuerdaten.qstep,9,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%9c",steuerdaten.qstep);
     slashn_entfernenb(steuerdaten.qstep);
     
     //fgets (steuerdaten.qmax,9,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%9c",steuerdaten.qmax);
     slashn_entfernenb(steuerdaten.qmax);
     
     
     for(int k=1;k<=6;k++)
     {
       //fgets (temp,5,in);
       fgets (read_temp,256,in);
       sscanf(read_temp,"%5c",temp);
       if(k==1)
         steuerdaten.ia=atoi(temp);
       if(k==2)
         steuerdaten.ncar=atoi(temp);
       if(k==3)
         steuerdaten.nhyd=atoi(temp);
       if(k==4)
         steuerdaten.idat=atoi(temp);
       if(k==5)
         steuerdaten.nfrou=atoi(temp);
       if(k==6)
         steuerdaten.iauto=atoi(temp);
     }
     //fgets(steuerdaten.epsh,10,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%10c",steuerdaten.epsh);
     slashn_entfernenb(steuerdaten.epsh);
     
     //fgets(steuerdaten.epsv,10,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%10c",steuerdaten.epsv);
     slashn_entfernenb(steuerdaten.epsv);
     
     //fgets(steuerdaten.rny,14,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%14c",steuerdaten.rny);
     slashn_entfernenb(steuerdaten.rny);
     
     //fgets(steuerdaten.cwr,12,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%12c",steuerdaten.cwr);
     slashn_entfernenb(steuerdaten.cwr);
     
     //fgets(temp,3,in);
     fgets (read_temp,256,in);
     sscanf(read_temp,"%3c",temp);
     steuerdaten.schiess=atoi(temp);
     
     read_tabelle(in,nasim_anfang);
     
			  temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%5c",temp);
          if((temp[0]!='\0') && (temp[0]!='\n'))
            steuerdaten.ifp=atoi(temp);
          else
            steuerdaten.ifp=0;
        }
        else
          steuerdaten.ifp=0;
        
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%5c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.idr=atoi(temp);
          else
            steuerdaten.idr=0;
        }
        else
          steuerdaten.idr=0;
        
        /************ipunkt*************/
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,6,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%6c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.ipunkt=atoi(temp);
          else
            steuerdaten.ipunkt=0;
        }
        else
          steuerdaten.ipunkt=0;
        /*******************************/
        
        /************izmax**************/
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(steuerdaten.izmax,10,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%10c",steuerdaten.izmax);
          if((steuerdaten.izmax[0]=='\0')|| (steuerdaten.izmax[0]=='\n'))
            strcpy(steuerdaten.izmax,"67");
          else
            slashn_entfernenb(steuerdaten.izmax);
        }
        else
          strcpy(steuerdaten.izmax,"67");
        
        /*******************************/
        
        /************nposey*************/
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,6,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%6c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.nposey=atoi(temp);
          else
            steuerdaten.nposey=0;
        }
        else
          steuerdaten.nposey=0;
        
        /*******************************/
        
        /***********nbeta***************/
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,6,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%6c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.nbeta=atoi(temp);
          else
            steuerdaten.nbeta=0;
        }
        else
          steuerdaten.nbeta=0;
        
        /*******************************/
        
        /***********iform***************/
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,6,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%6c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.iform=atoi(temp);
          else
            steuerdaten.iform=0;
        }
        else
          steuerdaten.iform=0;
        /*******************************/
        
        /***********inn*****************/
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%5c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.inn=atoi(temp);
          else
            steuerdaten.inn=0;
        }
        else
          steuerdaten.inn=0;
        /*******************************/
        
        /***********sm*******************/
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(steuerdaten.sm,15,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%15c",steuerdaten.sm);
          if((steuerdaten.sm[0]=='\0')|| (steuerdaten.sm[0]=='\n'))
            steuerdaten.sm[0]='\0';
          else
            slashn_entfernenb(steuerdaten.sm);
        }
        else
          steuerdaten.sm[0]='\0';
        /********************************/
        
        /***********hmo*****************///Dick 22.09.98
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%5c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.hmo=atoi(temp);
          else
            steuerdaten.hmo=0;
        }
        else
          steuerdaten.hmo=0;
        /*******************************/
        /***********wsf_q*****************///Dick 4.2.98
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%5c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.wsf_q=atoi(temp);
          else
            steuerdaten.wsf_q=0;
        }
        else
          steuerdaten.wsf_q=0;
        /*******************************/
        /***********wsf_l*****************///Dick 4.2.98
        temp[0]='\0';
        read_temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%5c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.wsf_l=atoi(temp);
          else
            steuerdaten.wsf_l=0;
        }
        else
          steuerdaten.wsf_l=0;
        /*******************************/
        /***********u_grenze*****************///Dick 28.09.99
        temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%5c",temp);
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.u_grenze=atoi(temp);
          else
            steuerdaten.u_grenze=0;
        }
        else
          steuerdaten.u_grenze=0;
        /*******************************/
        /***********dhwmax*****************///Dick 28.09.99
        temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%25c",temp);
          
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.dhwmax=atof(temp);
          else
            steuerdaten.dhwmax=2.0;
        }
        else
          steuerdaten.dhwmax=2.0;
        /*******************************/
        /***********vfmax*****************///Dick 28.09.99
        temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%25c",temp);
          
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.vfmax=atof(temp);
          else
            steuerdaten.vfmax=8.0;
        }
        else
          steuerdaten.vfmax=8.0;
        /*******************************/
        /***********hzvmax*****************///Dick 28.09.99
        temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%25c",temp);
          
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.hzvmax=atof(temp);
          else
            steuerdaten.hzvmax=1.0;
        }
        else
          steuerdaten.hzvmax=1.0;
        /*******************************/
        /***********faklhg*****************///Dick 28.09.99
        temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%25c",temp);
          
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.faklhg=atof(temp);
          else
            steuerdaten.faklhg=5.0;
        }
        else
          steuerdaten.faklhg=5.0;
        /*******************************/
        /***********ffmax*****************///Dick 28.09.99
        temp[0]='\0';
        if(!feof(in))
        {
          //fgets(temp,5,in);
          fgets (read_temp,256,in);
          sscanf(read_temp,"%25c",temp);
          
          if((temp[0]!='\0')&& (temp[0]!='\n'))
            steuerdaten.ffmax=atof(temp);
          else
            steuerdaten.ffmax=5000.0;
        }
        else
          steuerdaten.ffmax=5000.0;
        /*******************************/
        } //else
        fclose(in);
        
 } // Version == LWA
 /********************************************************************/
 }; // 
 
 ZEILE	 *zeile=NULL,
		 *zeile_ende=NULL,
     *zeile_anfang = NULL;
 
 
 void AddNewZeile(char str[90], double km) //Dick 16.07.99
 {
   ZEILE *help;
   int gefunden=0;
   
   zeile	= new ZEILE;   //alloc memory
   zeile->station = km;
   strcpy(zeile->line,str);
   zeile->next_zeile	= NULL;
   
   if (!zeile_anfang)
     zeile_anfang = zeile;
   else
   {
     help = zeile_anfang;
     
     if (zeile_anfang->station > zeile->station)
     {                                   //am Kopf einfügen
       zeile->next_zeile = zeile_anfang;
       zeile_anfang = zeile;
     }
     else
     {
       if ( (help->station < zeile->station) && (help->next_zeile !=NULL) &&(help->next_zeile->station > zeile->station))
       {
         zeile->next_zeile = help->next_zeile;
         help->next_zeile = zeile;
       }
       else
       {
         while (!gefunden)
         {
           if (help->next_zeile != NULL)
           {
             if ((help->station < zeile->station)&&
               (help->next_zeile->station <= zeile->station ))
               help = help->next_zeile;
             else
             {
               gefunden = 1;
               zeile->next_zeile = help->next_zeile;
               help->next_zeile = zeile;
             }
           }
           else
           {
             help->next_zeile = zeile;
             gefunden =1;
           }
         } //end of while
       }
     }
   }
 }
 
 /*******Kopieren eines Files zum Merken**********************************************************
 OFSTRUCT ofstrsource,ofstrdest;
 HFILE hfilesource,hfiledest;
 
   hfilesource=LZOpenFile(alte_datei,&ofstrsource,OF_READ);
   hfiledest=LZOpenFile(neue_datei,&ofstrdest,OF_CREATE);
   
     LZCopy(hfilesource,hfiledest);
     
       LZClose(hfilesource);
       LZClose(hfiledest);
       
 ******************************************************************/
 void Delete_Zeile(void)
 {
   
   while(zeile_anfang)
   {
     zeile=zeile_anfang;
     zeile_ende=zeile_anfang;
     zeile_anfang=zeile_anfang->next_zeile;
     
     if (zeile!=NULL)
				 {
       delete zeile;
       zeile=NULL;
				 }
	  } //while
   
 }  //Funktion
 /*****************************************************************/
 void Delete_203(void)
 {
   extern SLIST ber_list, batch_list;
   extern WINDOW listbox203, DLG204;
   extern long timer_203, timer_204;
   extern FILE_SPEC STR_SPEC;
   extern BOOLEAN new_batch;
   
   int batch_remove, position=0, m=0, len=0, zaehler=0, anzahl_slist=0;
   
   SLIST_ELT ebatch, e,eglobal;
   
   SLIST helplist=NULL,helplist_global=NULL;
   
   char *temp,
     *merke_da,
     *temp2,
     *str;
   
   char *datei_name,
     *ber_datei,
     *help,
     *remember,
     *delete_vzk;
   
   FILE *ber_file, *namen_merken;
   
   
   datei_name = new char[100];
   ber_datei  = new char[100];
   help       = new char[100];
   remember   = new char[100];
   delete_vzk = new char[100];
   temp       = new char [15];
   merke_da   = new char [16];
   /*****Dateinamen listbox203*****/
   
   batch_remove=xvt_list_count_all(listbox203);
   ebatch=xvt_slist_get_first(ber_list);
   
   int h;
   SLIST listbox203_list;
   listbox203_list=xvt_list_get_all(listbox203);
   for(int index=0;index<batch_remove;index++)
   {
     if(xvt_list_is_sel(listbox203,index))
     {
       char *temp_list,*temp_ber;
       ebatch=xvt_slist_get_first(listbox203_list);
       for (h=0;h<index;h++)
         ebatch=xvt_slist_get_next(listbox203_list,ebatch);
       temp_list=xvt_slist_get(listbox203_list,ebatch,0L);
       for(ebatch=xvt_slist_get_first(ber_list);ebatch!=NULL;ebatch=xvt_slist_get_next(ber_list,ebatch))
       {
         temp_ber=xvt_slist_get(ber_list,ebatch,0L);
         if(xvt_str_match(temp_list,temp_ber,1)==TRUE)
         {
           xvt_slist_rem(ber_list,ebatch);
           break;
         }
         
       }
       
     }
   }
   xvt_slist_destroy(listbox203_list);
   timer_203=xvt_timer_create(DLG203,10);
   
   /*****Dateinamen listbox203*****/
   //Neu Dick 29.01.99
   helplist_global=xvt_slist_create();
   helplist_global=xvt_list_get_sel(listbox203);
   for(eglobal = xvt_slist_get_first(helplist_global);eglobal!=NULL;
   eglobal = xvt_slist_get_next(helplist_global,eglobal))
   {
     str=xvt_slist_get(helplist_global,eglobal,0L);
     
     for (int i=79;i<=91;i++)
       temp[i-79] = str[i];
     temp[12] = '\0';
     
     /*************Datei physisch loeschen*******************/
     
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, datei_name, 50);
     strcat(datei_name,"\\");
     strcat(datei_name,temp);
     datei_name[strlen(datei_name)]='\0';
     remove(datei_name);
     

	 
	 /**************vzk-Datei löschen falls vorhanden********/
	 /** DAS IST DOCH UNSINN? WARUM SOLLTE MAN DIE VZK LÖSCHEN, WENN MAN EINE BERECHNUNGSVARIANTE LÖSCHT? */
	 /*
     strcpy(delete_vzk,datei_name);
     delete_vzk[strlen(delete_vzk)-3]='\0';
     strcat(delete_vzk,"vzk");
     back=access(delete_vzk,00);
     if (back==0)
       remove(delete_vzk);
	 */
     
     /*********Namen merken**************************/
     
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, remember, 50);
     strcat(remember,"\\");
     strcat(remember,temp);
     len=strlen(remember);
     remember[len-3]='\0';
     strcat(remember,"mrk");
     
     if ((namen_merken=fopen(remember,"r"))!=NULL)
     {
       if (helplist!=NULL)
       {
         xvt_slist_destroy(helplist);
         helplist=NULL;
       }
       helplist=xvt_slist_create();
       merke_da[0]='\0';
       while(!feof(namen_merken))
       {
							  fgets(merke_da,13,namen_merken);
                if (merke_da[0]!='\n')
                  xvt_slist_add_at_elt(helplist,NULL,merke_da,0);
       }
       xvt_slist_add_at_elt(helplist,NULL,temp,0);
       fclose(namen_merken);
       namen_merken=fopen(remember,"w");
       for(e = xvt_slist_get_first(helplist);e!=NULL;
					  e = xvt_slist_get_next(helplist,e))
            {
              temp2=xvt_slist_get(helplist,e,0L);
              fprintf(namen_merken,"%s\n",temp2);
            }
            fclose(namen_merken);
     }
     
     else
     {
       namen_merken=fopen(remember,"w");
       fprintf(namen_merken,"%s\n",temp);
       fclose(namen_merken);
     }
   }//for eglobal
   
   /***************aus .ber-Datei loeschen*******************/
   
   xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, ber_datei, 50);
   strcat(ber_datei,"\\");
   strcat(ber_datei,temp);
   len=strlen(ber_datei);
   ber_datei[len-3]='\0';
   strcat(ber_datei,"ber");
   ber_file=fopen(ber_datei,"r");
   if(ber_file!=NULL)
   {
     fgets(help,10,ber_file);
     zaehler = atoi(help);
     fclose(ber_file);
   }
   else
     zaehler=0;
   ber_file=fopen(ber_datei,"w");
   fprintf(ber_file,"%d\n",zaehler);
   
   for(e = xvt_slist_get_first(ber_list);e!=NULL;
   e = xvt_slist_get_next(ber_list,e))
   {
     temp2=xvt_slist_get(ber_list,e,0L);
     fprintf(ber_file,"%s\n",temp2);
   }
   fclose(ber_file);
   
   
   /************.ber loeschen wenn keine Elemente mehr*******/
   
   anzahl_slist=xvt_slist_count(ber_list);
   if (anzahl_slist==0)
     remove(ber_datei);
   
   
   
   if (helplist!=NULL)
   {
     xvt_slist_destroy(helplist);
     helplist=NULL;
   }
   if (helplist_global!=NULL)
   {
     xvt_slist_destroy(helplist);
     helplist_global=NULL;
   }
   
   delete[] datei_name;
   delete[] ber_datei ;
   delete[] help ;
   delete[] remember;
   delete[] delete_vzk;
   delete[] merke_da;
   delete[] temp;
} //Funktion

/******************************************************/
void loesche_alle_mit_strname(void)
{
  char *pruefedatei; //	char pruefedatei[100];
  char *removedatei;   //	char removedatei[200];
  struct _finddata_t  data;
  
  long sHandle;
  int done;
  
  pruefedatei = new char[100];
  removedatei = new char[200];
  
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,pruefedatei,80);
  strcat(pruefedatei,"\\");
  strcat(pruefedatei,STR_SPEC.name);
  pruefedatei[strlen(pruefedatei)-3]='\0';
  strcat(pruefedatei,"*");
  sHandle = _findfirst(pruefedatei,&data);
  if (sHandle==-1) //Fehler
	   return;
  done=0;
  
  while(done==0)
	 {
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,removedatei,80);
    strcat(removedatei,"\\");
    strcat(removedatei,data.name);
    
    int len=strlen(removedatei);
    if(((removedatei[len-3]=='p') || (removedatei[len-3]=='P')) &&
      ((removedatei[len-2]=='r') || (removedatei[len-2]=='R')) &&
      ((removedatei[len-1]=='f') || (removedatei[len-1]=='F')))
    {
    }
    else
    {
      int fuernichtsgut;
      fuernichtsgut=4711;
      
      if(!zustand_kopieren)  //8.2.99 bley
      {                     //8.2.99 bley
    	   if (remove(removedatei)==-1)
         {
           //xvt_dm_post_note("Datei: %s kann nicht gelöscht werden.Schreibrechte beachten!",removedatei);
           char buf[200],buf2[200];//Dick 26.11.99
           xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
           xvt_res_get_str(STR_CANNOT_DELETE,buf2,sizeof(buf2));
           xvt_dm_post_error("%s%s%s",buf,removedatei,buf2);
         }
      }                      //8.2.99 bley
      else                    //8.2.99 bley
      {                    //8.2.99 bley
        xvt_slist_add_at_elt(profil_list,NULL,data.name,0L);
        
      }                     //8.2.99 bley
      
      fuernichtsgut=-4711;
    }
    done=_findnext(sHandle,&data);
	 }
  //Neu: 16.3.99 Bley: vz kopieren
  if(zustand_kopieren)
  {
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,pruefedatei,80);
    strcat(pruefedatei,"\\");
    strcat(pruefedatei,STR_SPEC.name);
    pruefedatei[strlen(pruefedatei)-10]='v'; //Dick 18.03.99 9->10
    pruefedatei[strlen(pruefedatei)-9]='z'; //Dick 18.03.99 8->9
    pruefedatei[strlen(pruefedatei)-3]='\0';
    strcat(pruefedatei,"*");
    
    sHandle = _findfirst( pruefedatei, &data );
    
    if (sHandle==-1) //Fehler
	     return;
    done=0;
    
    while(done==0)
    {
		    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,removedatei,80);
        strcat(removedatei,"\\");
        //		strcat(removedatei,ffblk.ff_name);
        strcat(removedatei,data.name);
        
        int len=strlen(removedatei);
        
        xvt_slist_add_at_elt(profil_list,NULL,data.name,0L);
        
        done=_findnext(sHandle,&data);
    }
    
  }
  //Ende 16.3.99 Bley
  
  delete[] pruefedatei;
  delete[] removedatei;
 }
 void loesche_alle_in_dath(void)
 {
   //Löscht alle zustandbezogene Dateien im \dath Verzeichnis
   char pruefedatei[MAX_PATH],removedatei[MAX_PATH];
   struct _finddata_t  data;
   long sHandle;
   
   
   strcpy(pruefedatei,Projektname_aktuell);
   strcat(pruefedatei,"\\dath\\");
   strcat(pruefedatei,STR_SPEC.name);
   
   pruefedatei[strlen(pruefedatei)-3]='\0';
   strcat(pruefedatei,"*");
   for(int i=0;i<10;i++)
   {
     switch(i)
     {
     case 0:
       pruefedatei[strlen(pruefedatei)-8]='l';
       pruefedatei[strlen(pruefedatei)-7]='p';
       break;
     case 1:
       pruefedatei[strlen(pruefedatei)-8]='p';
       pruefedatei[strlen(pruefedatei)-7]='l';
       break;
     case 2:
       pruefedatei[strlen(pruefedatei)-8]='b';
       pruefedatei[strlen(pruefedatei)-7]='e';
       break;
     case 3:
       pruefedatei[strlen(pruefedatei)-8]='e';
       pruefedatei[strlen(pruefedatei)-7]='r';
       break;
     case 4:
       pruefedatei[strlen(pruefedatei)-8]='w';
       pruefedatei[strlen(pruefedatei)-7]='s';
       break;
     case 5:
       pruefedatei[strlen(pruefedatei)-8]='q';
       pruefedatei[strlen(pruefedatei)-7]='p';
       break;
     case 6:
       pruefedatei[strlen(pruefedatei)-8]='u';
       pruefedatei[strlen(pruefedatei)-7]='e';
       break;
     case 7:
       pruefedatei[strlen(pruefedatei)-8]='v';
       pruefedatei[strlen(pruefedatei)-7]='g';
       break;
     case 8:
       pruefedatei[strlen(pruefedatei)-8]='w';
       pruefedatei[strlen(pruefedatei)-7]='k';
       break;
     case 9:
       pruefedatei[strlen(pruefedatei)-8]='e';
       pruefedatei[strlen(pruefedatei)-7]='x';
       break;
     }
     sHandle = _findfirst(pruefedatei,&data);
     if (sHandle==-1) //Fehler
       continue;
     strcpy(removedatei,Projektname_aktuell);
     strcat(removedatei,"\\dath\\");
     strcat(removedatei,data.name);
     remove(removedatei);
     while(_findnext(sHandle,&data)==0)
     {
       strcpy(removedatei,Projektname_aktuell);
       strcat(removedatei,"\\dath\\");
       strcat(removedatei,data.name);
       remove(removedatei);
     }
     
   }
 }
 /**********************************************************************/
 void pruefe_errorlog(char *which_file)
 {
   //DIE FUNKTION PRUEFT,OB DIE KONVERTIERUNG B2LWA BZW. LWA2B
   //ERFOLGREICH WAR
   //SIE WAR ERFOLGREICH WENN IM FILE ERROR2.LOG ODER ERROR.LOG
   //NICHTS STEHT
   //AUFRUF AUS WSPWIN.cpp BEI TIMER, der in FKT. schreibe.bat (s.o)
   //KREIERT WURDE NACH START DES FORTRAN-BERECHNUNGSPROGRAMMES
   //UND AUFRUF AUS FKT. lese_str, die in Timer kreiert wurde NACH
   //START DES FORTRAN-BERECHNUNGSPROGRAMMES
   
   char *errordatei; //errordatei[100];
   FILE *errorfile;
   int chr;
   
   errordatei = new char[100];
   
   xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, errordatei, 50);
   //
   errordatei[strlen(errordatei)-4]='\0';
   strcat(errordatei,"\\dath");
   //
   strcat(errordatei,"\\");
   strcat(errordatei,which_file);
   if((errorfile=fopen(errordatei,"r+"))!=NULL)
	  {
     chr=fgetc(errorfile);
     if(chr!=EOF)
     {
       if((xvt_str_compare_ignoring_case(which_file,"error.log"))==0)
       {
       /*xvt_dm_post_note("Konvertierung fehlerhaft ausgeführt. \n"
       " Sehen Sie die Dateien %s und Gewässername.log "
         " im Projektunterverzeichnis \\dath ein.",which_file);*/
         char buf[200],buf2[200];//Dick 26.11.99
         xvt_res_get_str(STR_KONVERT_ERROR,buf,sizeof(buf));
         xvt_res_get_str(STR_KONVERT_ERROR2,buf2,sizeof(buf2));
         xvt_dm_post_error("%s%s%s",buf,which_file,buf2);
       }
       else
       {
         if( LWA_PROJEKT )
         {
           
         /*xvt_dm_post_note("Berechnung fehlerhaft ausgeführt. \n"
         " Sehen Sie die Dateien %s und profil.log "
           " im Projektunterverzeichnis \\dath ein.",which_file);*/
           char buf[200],buf2[200];//Dick 26.11.99
           xvt_res_get_str(STR_BER_LWA_ERROR,buf,sizeof(buf));
           xvt_res_get_str(STR_BER_LWA_ERROR2,buf2,sizeof(buf2));
           xvt_dm_post_error("%s%s%s",buf,which_file,buf2);
         }
         else
         {
         /*xvt_dm_post_note("Berechnung ausgeführt. \n"
         " Sehen Sie bei Bedarf die Protokolldatei %s "
           " im Projektunterverzeichnis \\dath ein.",which_file);*/
           /*
           char buf[200],buf2[200];//Dick 26.11.99
           xvt_res_get_str(STR_BER_BCE_ERROR,buf,sizeof(buf));
           xvt_res_get_str(STR_BER_BCE_ERROR2,buf2,sizeof(buf2));
           xvt_dm_post_error("%s%s%s",buf,which_file,buf2);
           */
         }
       }
     }
     else
     {
       if((xvt_str_compare_ignoring_case(which_file,"error.log"))==0)
       {
       /*xvt_dm_post_note("Konvertierung erfolgreich ausgeführt. "
       "Protokoll der Konvertierung in Datei Gewässername.log "
									"im Projektunterverzeichnis \\dath.");*/
         char buf[200];//Dick 26.11.99
         xvt_res_get_str(STR_KONVERT_SUCCESS,buf,sizeof(buf));
         xvt_dm_post_note("%s",buf);
       }
       else
       {
				   /*xvt_dm_post_note("Berechnung erfolgreich ausgeführt. "
           "Protokoll der Berechnung in Datei profil.log "
									"im Projektunterverzeichnis \\dath.");*/
         /*
         char buf[200];//Dick 26.11.99
         xvt_res_get_str(STR_BER_SUCCESS,buf,sizeof(buf));
         xvt_dm_post_note("%s",buf);
         */
       }
       
     }
     fclose(errorfile);
	  }
   delete[] errordatei;
 }
 /*************************************************************************/
 
 void loesche_alle_mit_log(void)
 {
	/**Vor Start einer neuen Konvertierung BCE-LWA werden alle alten
   Dateien *.log (Fehler und Protokolldateien) gelöscht*****/
   /**Aufruf aus wspm0001.cpp**/
   
   char *pruefedatei; //	char pruefedatei[100];
   char *removedatei;   //	char removedatei[200];
   
   struct _finddata_t  data;
   long sHandle;
   
   int done;
   
   pruefedatei = new char[100];
   removedatei = new char[200];
   
   xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,pruefedatei,80);
   strcat(pruefedatei,"\\");
   strcat(pruefedatei,"*.log");
   sHandle = _findfirst(pruefedatei,&data);
   if (sHandle==-1) //Fehler
     return;
   done=0;
   while(!done)
   {
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,removedatei,80);
     strcat(removedatei,"\\");
     strcat(removedatei,data.name);
     
     remove(removedatei);
     done=_findnext(sHandle,&data);
   }
   
   delete[] pruefedatei;
   delete[] removedatei;
}

void DeleteFileFromDir( const char* dir, const char* fileName )
{
  CharBuffer buffer( PATH_LEN );

  strncpy( buffer, dir, PATH_LEN );

  strncat( buffer, fileName, PATH_LEN - strlen( buffer ) );

  DeleteFile( buffer );
};
 
 /*************START_WSPR****************************************/
 // GHJ
void start_wspr()
{
  extern FILE_SPEC STR_SPEC;
  extern SLIST batch_list;

  STARTUPINFO sui;
  PROCESS_INFORMATION pi;

  CharBuffer daten_verzeichnis( PATH_LEN );
  CharBuffer daten_ausgabe( PATH_LEN );
  CharBuffer filename1( PATH_LEN ), filename2( PATH_LEN );
   
   hProc_Wspr = NULL;
   exit_ausw = 1;
   bBreakBerechnung = FALSE;
   DoProgressDlg( (HWND)xvt_vobj_get_attr( main_win, ATTR_NATIVE_WINDOW ) );
   
   start[0] = '\0';
   xvt_fsys_convert_dir_to_str( &STR_SPEC.dir, start, PATH_LEN );
   strcat( start, "\\dag.bat" );

   daten_verzeichnis[0] = '\0';
   xvt_fsys_convert_dir_to_str( &STR_SPEC.dir, daten_verzeichnis, 50 );
   strcpy( daten_ausgabe, daten_verzeichnis );
   
   daten_ausgabe[strlen( daten_verzeichnis ) - 4] = '\0';
   strcat(daten_ausgabe,"dath");
   strcat(daten_ausgabe,"\\");
   strcat(daten_verzeichnis,"\\");
   
   daten_verzeichnis[2] = '\\';
   
   // Kopiere Kopt.txt nach Dath
   strcpy( filename1, start_dir );
   strcat(filename1, "kopf.txt" );
   strcpy( filename2, daten_verzeichnis );
   strcat( filename2,"kopf.txt" );
   CopyFile( filename1, filename2, FALSE );
   
   // Kopiere f7713.eer nach Dath
   strcpy( filename1, start_dir );
   strcat( filename1, "f77l3.eer" );
   strcpy( filename2, daten_verzeichnis );
   strcat( filename2, "f77l3.eer" );
   CopyFile( filename1, filename2, FALSE );
   
   // Kopiere lf90.eer nach Dath
   strcpy(filename1,start_dir);
   strcat(filename1,"lf90.eer");
   strcpy(filename2,daten_verzeichnis);
   strcat(filename2,"lf90.eer");
   CopyFile(filename1, filename2, FALSE);

   // Alte Rechenzwischenergebnisse aus \prof löschen
   // Verluste.tmp löschen aus Dath
   DeleteFileFromDir( daten_verzeichnis, "verluste.tmp" );
   DeleteFileFromDir( daten_verzeichnis, "profil.vzk" );
   DeleteFileFromDir( daten_verzeichnis, "wspr.err" );
   DeleteFileFromDir( daten_verzeichnis, "wspr.log" );
   DeleteFileFromDir( daten_verzeichnis, "b2lwa.err" );
   DeleteFileFromDir( daten_verzeichnis, "b2lwa.log" );
   DeleteFileFromDir( daten_verzeichnis, "tmp.wsp" );
   
   /*************EINLESEN BERECHNUNGSVARIANTE***************************/
   int counter = 1;
   
   for( SLIST_ELT e = xvt_slist_get_first( batch_list ); e != NULL; e = xvt_slist_get_next( batch_list, e ) )
   {
     extern FILE_SPEC qwert_spec;
     extern st_daten steuerdaten;
     extern ZEILE* zeile;
     extern ZEILE* zeile_anfang;
     
     int zaehler = 1;
     int q_return = 1;
     int anzahl_qsatz = 0;
     int m;
     int anzahl_slist;
     
     unsigned int i = 0, 
       j = 0,
       z = 0;
     
     FILE* in;
     FILE* out;
     FILE* qwert;
     FILE* ctr_file;
     
     
     ABSTAENDE* pabstand = NULL;
     ABSTAENDE* abstand_anfang = NULL;
     ABSTAENDE* abstand_ende = NULL;
     
     int test_help;
     
     SLIST_ELT element;
     
     double ende_compare, anfang_compare, ap=0, ep=0;
     double station_compare=0;
     
     BOOLEAN k=FALSE;
     BOOLEAN k2=FALSE;//Dick 26.11.98 weil k2 nicht deklariert war
     SLIST strang_list1, strang_list2, strang_list3, datei_list_a;
     SLIST datei_list_e, stationen_list;
     
     float strang1, strang2, strang3; //war vor Fehler double
     double abfluss_zahl, wsp, he, sjo;
     double ap_double, min, step, max;
     
     MinMax Maxwert;
     
     // char Buffer
     CharBuffer batbuffer( 30 ),
       gewaesser_name( 20 ),
       bufgewname( 20 ),
       str_zustand( 30 ), //Dick 10 ->20 weil kann mehr als 10 sein
       help( 120 ),
       stationen_text( 12 ), //Dick 10 ->12 weil kann mehr als 10 sein
       datei_anfang( 13 ), datei_ende( 13 ),
       counter_char( 9 ),
       q_compare( 21 ), 
       qsaetze( 20 ),
       abfluss( 15 ),
       temp_zaehler( 150 ),
       anfang_chr( 16 ),
       leer( 26 );//Dick 12 ->16 sonst ab und zu Problemen mit delete;//Dick 16.12.98 10->15;
     
     // CharBuffer für Dateien
     
     CharBuffer str( PATH_LEN ),
       qwert_datei( PATH_LEN ),
       str_help( PATH_LEN ),
       vzk_datei( PATH_LEN ),
       error_line( MAX_PATH );
     
     hProc_Wspr = NULL;
     char* bat = xvt_slist_get( batch_list, e, 0L );
     strcpy( batbuffer, bat );
     strcat( batbuffer, "\0" );
     str[0] = '\0';
     xvt_fsys_convert_dir_to_str( &STR_SPEC.dir, str, PATH_LEN );
     strcat( str, "\\" );
     strcat( str, batbuffer );
     
     read_varianten( str );
     sprintf( filename1, "Berechnungsvariante %d", counter );
     SetProgressTitle( filename1 );
     strcpy( filename1, "Daten werden vorbereitet..." );
     SetProgressText( filename1 );
     IncProgress();
     if( bBreakBerechnung )
       break;
     
     /************Einlesen aus Vernetzungs-Datei**********/
     
     int len = strlen( str );
     str[len - 3] = '\0';
     strcat( str, "str" );
     
     if( ( in = fopen( str,"r+" ) ) == NULL )
     {
       char buf[200],buf2[200];//Dick 26.11.99
       xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
       xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
       xvt_dm_post_error("%s%s%s",buf,str,buf2); //	" Datei : %s läßt sich nicht öffnen !"
       exit( -1 );
     }
     else
     {
       /****erste Zeile: 4 Angaben****/
       fscanf(in,"%d",&anzahl_profil_dat_entries);
       fscanf(in,"%d",&anzahl_strang_entries);
       fscanf(in,"%s",gewaesser_name);
       fscanf(in,"%s",str_zustand);
       strcpy(bufgewname, gewaesser_name);
       
       bufgewname[8]='\0';
       gewaesser_name[8]='\0';
       str_zustand[strlen(str_zustand)]='\0';
       
       
       /****\n einlesen****/
       
       fgets(help,110,in);
       
       
       IncProgress();
       if (bBreakBerechnung)
         break;
       /****oberen Block einlesen(profil_dat_entries)****/
       for( i = 1;i <= (unsigned)anzahl_profil_dat_entries;i++)
       {
         if(!feof(in))
         {
           fgets(help,110,in);
         } /*if !=EOF*/
       }  /*for <prof-dat-entries*/
       
       /****Leerzeile einlesen****/
       
       fgets(help,110,in);
       
       
       /****unteren Block einlesen (strang_etriese)****/
       
       ende_compare=steuerdaten.ende;
       anfang_compare=steuerdaten.anfang;
       
       strang_list1=xvt_slist_create();
       strang_list2=xvt_slist_create();
       strang_list3=xvt_slist_create();
       datei_list_a=xvt_slist_create();
       datei_list_e=xvt_slist_create();
       stationen_list=xvt_slist_create();
       
       IncProgress();
       if (bBreakBerechnung)
         break;
       for (j=1;j<=(unsigned)anzahl_strang_entries;j++)
       {
         pabstand=new ABSTAENDE;
         pabstand->pabstand1=0;
         pabstand->pabstand2=0;
         pabstand->pabstand3=0;
         pabstand->next_abstand=NULL;
         if(!abstand_anfang)
           abstand_anfang=pabstand;
         else
           abstand_ende->next_abstand=pabstand;
         abstand_ende=pabstand;
         abstand_ende->next_abstand=NULL;
       }
       IncProgress();
       if (bBreakBerechnung)
         break;
       pabstand=abstand_anfang;
       for (j=1;j<=(unsigned)anzahl_strang_entries;j++)
       {
         if(!feof(in))
         {
           fscanf(in,"%s",stationen_text);
           fscanf(in,"%lf",&ep);
           fscanf(in,"%f",&strang1);
           fscanf(in,"%f",&strang2);
           fscanf(in,"%f",&strang3);
           fscanf(in,"%s",datei_anfang);
           fscanf(in,"%s",datei_ende);
           
           ap=atof(stationen_text);
           ap_double=ap;
           /*1000*/		strang1=strang1*1000;
           
           /*1000*/		   strang2=strang2*1000;
           
           /*1000*/		   strang3=strang3*1000;
           
           if ((ap>=anfang_compare && ep<=ende_compare) ||
             (ap<=anfang_compare && ep>=ende_compare))
             
           {
             xvt_slist_add_at_elt(datei_list_a,NULL,datei_anfang,0);
             xvt_slist_add_at_elt(datei_list_e,NULL,datei_ende,0);
             xvt_slist_add_at_elt(stationen_list,NULL,stationen_text,0);
             pabstand->pabstand1=strang1;
             pabstand->pabstand2=strang2;
             pabstand->pabstand3=strang3;
             pabstand=pabstand->next_abstand;
           }
         }//eof
       } // <Anzahl strang entries
    } //else
    
    fclose(in);
    
    /********************Profil.str schreiben**********************/
    
    itoa( counter,counter_char, 10 );
    str[0]='\0';
    strcat(str,daten_verzeichnis);
    strcat(str,counter_char);
    strcat(str,".str");
    out=fopen(str,"w");
    
    /**** Kopf schreiben****/
    
    fprintf(out,"CC Gewaesser\n");
    //	fprintf(out,"%s\n", gewaesser_name); //konnte nicht gedruckt werden
    //	fprintf(out,"%s\n",bufgewname);  17.9.96 geändert
    char bezeichnung[100];
    lese_projektbezeichnung(bezeichnung);
    fprintf(out,"%s\n",bezeichnung);
    fprintf(out,"Station 0 + ");
    
    double st_anfang;
    st_anfang=steuerdaten.anfang*1000;
    
    fprintf(out,"%.2lf ",st_anfang);
    fprintf(out,"bis 0 + ");
    
    double st_ende;
    st_ende=steuerdaten.ende*1000;
    
    fprintf(out,"%.2f ",st_ende);
    fprintf(out,"m\n");
    //fprintf(out,"%s\n", str_zustand);  17.9.96 geändert
    fprintf(out,"%s\n",steuerdaten.info);
    /*			fprintf(out,"% 4d% 4d% 4d% 4d% 4d% 4d\n",
    steuerdaten.ia,steuerdaten.nhyd,steuerdaten.ncar,
    steuerdaten.idat, steuerdaten.iauto,steuerdaten.nfrou);
    */
    /*			fprintf(out,"% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d\n",
    steuerdaten.ia,steuerdaten.nhyd,steuerdaten.ncar,
    steuerdaten.idat, steuerdaten.iauto,steuerdaten.nfrou,
    steuerdaten.ifp, steuerdaten.idr);
    */
    int intizmax=atoi(steuerdaten.izmax);
    double doublesm=atof(steuerdaten.sm);
    fprintf(out,"% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 4d% 8.3lf\n",
      steuerdaten.ia,steuerdaten.nhyd,steuerdaten.ncar,
      steuerdaten.idat, steuerdaten.iauto,steuerdaten.nfrou,
      steuerdaten.ifp, steuerdaten.idr, steuerdaten.ipunkt,
      intizmax,steuerdaten.nposey, steuerdaten.nbeta,
      steuerdaten.iform, steuerdaten.inn, doublesm);
    
    double a,b,c,d;
    a=atof(steuerdaten.epsh);
    b=atof(steuerdaten.epsv);
    c=atof(steuerdaten.rny);
    d=atof(steuerdaten.cwr);
    //			fprintf(out,"% 8.2lf% 8.2lf% 12.2lf% 10.2lf\n",
    //					  a,b,c,d);
    /**16.02.96: geändert**/
    //fprintf(out,"%lf %lf %lf %lf\n", a, b, c, d);//Dick 11.10.99
    fprintf(out,"%lf %lf %lf %lf %8.2lf %8.2lf %8.2lf %8.2lf %8.1lf\n",
      a, b, c, d,steuerdaten.dhwmax,steuerdaten.vfmax,steuerdaten.hzvmax,
      steuerdaten.faklhg,steuerdaten.ffmax);//Dick 11.10.99 //Grenzwerte
    /****oberen Block schreiben****/
    
    ende_compare=steuerdaten.ende;
    anfang_compare=steuerdaten.anfang;
    
    anzahl_slist = xvt_slist_count(datei_list_a);
    pabstand=abstand_anfang;
    IncProgress();
    if (bBreakBerechnung)
      break;
    for(zaehler=1;zaehler<=anzahl_slist;zaehler++)
    {
      char* strang_ptr=xvt_slist_get_elt(datei_list_a,zaehler-1,0L);
      strcpy(datei_anfang,strang_ptr);
      strang_ptr=xvt_slist_get_elt(datei_list_e,zaehler-1,0L);
      strcpy(datei_ende,strang_ptr);
      strang_ptr=xvt_slist_get_elt(stationen_list,zaehler-1,0L);
      strcpy(stationen_text,strang_ptr);
      
      datei_anfang[12]='\0';
      datei_ende[12]='\0';
      stationen_text[strlen(stationen_text)]='\0';
      
      /**** Ablußwert holen****/
      
      qwert_datei[0]='\0';
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,qwert_datei,50);
      strcat(qwert_datei,"\\");
      strcat(qwert_datei,batbuffer);
      qwert_datei[strlen(qwert_datei)-3]='\0';
      strcat(qwert_datei,"qwt");
      qwert=fopen(qwert_datei,"r");
      
      q_compare[0]='\0';
      q_return=1;
      ap_double=atof(stationen_text);
      ap=ap_double;
      
      while ((q_return!=0) && (!feof(qwert)))
      {
        fscanf(qwert,"%s",q_compare);
        q_return=strcmp(q_compare,steuerdaten.q);
        fgets(qsaetze,20,qwert);
        anzahl_qsatz=atoi(qsaetze);
      }
      
      j=1;
      char muell[20];
      
      if (anzahl_qsatz!=0)
      {
        fscanf(qwert,"%lf",&station_compare);
        fgets(abfluss,15,qwert);
        //neu 22.1.2002
        if(zaehler==1)
        {
          if(ap<ep)
          {
            while(station_compare<ap && j < (unsigned)anzahl_qsatz )
            {
              fscanf(qwert,"%lf",&station_compare);
              if(station_compare<=ap)
                fgets(abfluss,15,qwert);
              else
                fgets(muell,15,qwert);
              j++;
              
            }
          }
          if(ap>ep)
          {
            while(station_compare>ap && j < (unsigned)anzahl_qsatz )
            {
              fscanf(qwert,"%lf",&station_compare);
              if(station_compare>=ap)
                fgets(abfluss,15,qwert);
              else
                fgets(muell,15,qwert);
              j++;
              
            }
            
          }
        }
        //				j=1;
        //				char muell[20];
        //ende neu 22.1.2002
        
        while ((station_compare!=ap) && (j < (unsigned)anzahl_qsatz) )
        {
          fscanf(qwert,"%lf",&station_compare);
          if(station_compare==ap)
            fgets(abfluss,15,qwert);
          else
            fgets(muell,15,qwert); //Bley 8.11.2000 v. 10 auf 15 geändert
          j++;
        }
      }
      if ((station_compare!=ap) && (zaehler!=1))
      {
        abfluss[0]='\0';
      }
      else
      {
        for (j=0;j<=(strlen(abfluss)+1);j++)
        {
          if (abfluss[j]=='\n')
            abfluss[j]='\0';
        }
        abfluss[strlen(abfluss)-1]='\0'; //auf 2Stelle h.Komma kuerzen
      }
      fclose(qwert);
      /****/
      k=FALSE;
      k2=FALSE;
      if ((steuerdaten.kalmin[0]=='K') || (steuerdaten.nasabs[0]=='N'))
      {
        k=teste_k(nasim_anfang, ap_double);
        if(zaehler==anzahl_slist)
          k2=teste_k(nasim_anfang,steuerdaten.ende);
        if(k2==TRUE)
          k=FALSE;
      }

      abfluss_zahl=atof(abfluss);
      
      /*		fprintf(out,"         %s  %s  % 8.2lf% 8.2lf% 8.2lf",
      datei_anfang, datei_ende, pabstand->pabstand1,
      pabstand->pabstand2,pabstand->pabstand3);
      geändert 30.9.96
      */
      fprintf(out,"         %s  %s  % 11.2lf% 11.2lf% 11.2lf",
        datei_anfang, datei_ende, pabstand->pabstand1,
        pabstand->pabstand2,pabstand->pabstand3);
      
      if (abfluss_zahl>0)
        fprintf(out,"% 8.2lf", abfluss_zahl);
      else
        fprintf(out,"        ");
      if ((k==TRUE) ||(k2==TRUE))
      {
        if(k==TRUE)
          fprintf(out,"         K\n");
        if(k2==TRUE)
          fprintf(out,"        KK\n");
      }
      else
        fprintf(out,"          \n");
      pabstand=pabstand->next_abstand;
    } //for
    
    while(abstand_anfang)
    {
      pabstand=abstand_anfang;
      abstand_anfang=pabstand->next_abstand;
      if(pabstand!=NULL)
      {
        delete pabstand;
        pabstand=NULL;
      }
    }
    
    /**************Wert für Anfangswasserspiegel schaetzen*******/
    element=xvt_slist_get_first(datei_list_a);
    char* strang_ptr=xvt_slist_get(datei_list_a,element,0L);
    strcpy(datei_anfang,strang_ptr);
    strcpy(file_spec.name,datei_anfang);
    daten_verzeichnis[strlen(daten_verzeichnis)-1]='\0';
    xvt_fsys_convert_str_to_dir(daten_verzeichnis,&file_spec.dir);
    xvt_fsys_set_dir(&file_spec.dir);
    read_profildatei(pWPL, &STR_SPEC.dir, datei_anfang );
    list->GetMinMax(&Maxwert,1);
    strcat(daten_verzeichnis,"\\\0");
    steuerdaten.w_anfang=(float)Maxwert.maxY+2;
    list->DeleteList();
    
    
    /***************unteren Teil schreiben***********************/
    
    fprintf(out,"RANDBEDINGUNG\n");
    fprintf(out,"CC Text\n");
    
    /****EICHVO/EICHFL****/
    
    if (steuerdaten.eich[0]=='E')
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      //	for (i=0;i<strlen(gewaesser_name);i++)     //GEWAESSERNAME
      //		temp_zaehler[i+2]=gewaesser_name[i];
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)      // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.eich);i++)   // TYP
        temp_zaehler[i+25]=steuerdaten.eich[i];
      
      he=atof(steuerdaten.he);                   // HE-ENDW.
      wsp=atof(steuerdaten.hoehe);               // WSP-ANFW.
      
      fprintf(out,"%s% 8.2lf% 8.2lf                \n",
        temp_zaehler,wsp,he);
    } //eich
    
    /*****WSP******/
    if (steuerdaten.wasser[1]=='S' && steuerdaten.wehran[0]!='W' && steuerdaten.eich[0]!='E')//Dick 12.11.99
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      //				  for (i=0;i<strlen(gewaesser_name);i++)       //GEWAESSERNAME
      //						temp_zaehler[i+2]=gewaesser_name[i];
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)        // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.wasser);i++)   // TYP
        temp_zaehler[i+25]=steuerdaten.wasser[i];
      if(steuerdaten.schiess==1)
        temp_zaehler[i+25]='-';
      
      gcvt(st_anfang,10,anfang_chr);
      m=45;
      for (i=strlen(anfang_chr);i>0;i--)           // STATION
      {
        temp_zaehler[m]=anfang_chr[i-1];
        m--;
      }
      
      wsp=atof(steuerdaten.hoehe);                 // WSP
      
      for(i=0;i<=15;i++)                           // LEER
        leer[i]=' ';
      leer[16]='\0';
      
      fprintf(out,"%s% 8.2lf%s        \n",temp_zaehler,wsp,leer);
      
    } //WSP
    /*******Ende WSP*******/
    
    /****HGRENZ****/
    
    if (steuerdaten.wasser[1]=='G' && steuerdaten.wehran[0]!='W')//Dick 12.11.99)
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)       // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.wasser);i++)  // TYP
        temp_zaehler[i+25]=steuerdaten.wasser[i];
      if(steuerdaten.schiess==1)
        temp_zaehler[i+24]='-';
      
      gcvt(st_anfang,10,anfang_chr);
      m=45;
      for (i=strlen(anfang_chr);i>0;i--)          // STATION
      {
        temp_zaehler[m]=anfang_chr[i-1];
        m--;
      }
      
      for(i=0;i<=15;i++)                           // LEER
        leer[i]=' ';
      leer[16]='\0';
      
      fprintf(out,"%s% 8.2f%s        \n",temp_zaehler,steuerdaten.w_anfang,leer);
      
    } //HRENZ
    
    /****HNORM****/
    
    if (steuerdaten.wasser[1]=='N' && steuerdaten.wehran[0]!='W')//Dick 12.11.99)
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      //			for (i=0;i<strlen(gewaesser_name);i++)      // GEWAESSENAME
      //				temp_zaehler[i+2]=gewaesser_name[i];
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)       // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.wasser);i++)  // TYP
        temp_zaehler[i+25]=steuerdaten.wasser[i];
      if(steuerdaten.schiess==1)
        temp_zaehler[i+25]='-';
      
      gcvt(st_anfang,10,anfang_chr);
      m=45;
      for (i=strlen(anfang_chr);i>0;i--)          // STATION
      {
        temp_zaehler[m]=anfang_chr[i-1];
        m--;
      }
      temp_zaehler[51]='\0';
      sjo=atof(steuerdaten.gefaelle);              // GEFAELLE
      float dummy=0.0;
      fprintf(out,"%s% 8.2f% 8.2f% 8.5lf        \n",
        temp_zaehler,steuerdaten.w_anfang,dummy,sjo);
    } //HNORM
    
    
    /****LPLOT****/
    
    if ((steuerdaten.lplot[0]=='L') && (steuerdaten.qplot[0]!='Q'))
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      //		for (i=0;i<strlen(gewaesser_name);i++)      //GEWAESSERNAME
      //			temp_zaehler[i+2]=gewaesser_name[i];
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)       // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.lplot);i++)   // TYP
        temp_zaehler[i+25]=steuerdaten.lplot[i];
      
      for(i=0;i<=23;i++)                          // LEER
        leer[i]=' ';
      leer[24]='\0';
      
      fprintf(out,"%s%s        \n",temp_zaehler,leer);
      
    } //LPLOT
    
    
    /****QLPLOT****/
    
    if ((steuerdaten.lplot[0]=='L') && (steuerdaten.qplot[0]=='Q'))
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      //		for (i=0;i<strlen(gewaesser_name);i++)    //GEWAESSERNAME
      //			temp_zaehler[i+2]=gewaesser_name[i];
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)     // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      temp_zaehler[25]='Q';		               // TYP
      temp_zaehler[26]='L';
      temp_zaehler[27]='P';
      temp_zaehler[28]='L';
      temp_zaehler[29]='O';
      temp_zaehler[30]='T';
      
      for(i=0;i<=23;i++)                       // LEER
        leer[i]=' ';
      leer[24]='\0';
      
      fprintf(out,"%s%s        \n",temp_zaehler,leer);
    } //QLPLOT
    
    
    
    /****QPLOT****/
    
    if ((steuerdaten.qplot[0]=='Q') && (steuerdaten.lplot[0]!='L'))
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      //		for (i=0;i<strlen(gewaesser_name);i++)      //GEWAESSERNAME
      //			temp_zaehler[i+2]=gewaesser_name[i];
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)       // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.qplot);i++)   // TYP
        temp_zaehler[i+25]=steuerdaten.qplot[i];
      
      for(i=0;i<=23;i++)                          // LEER
        leer[i]=' ';
      leer[24]='\0';
      
      fprintf(out,"%s%s        \n",temp_zaehler,leer);
    } //QPLOT
    
    
    
    /****HGRALL****/
    
    st_anfang=steuerdaten.anfang*1000;
    
    if (steuerdaten.hgralle[0]=='H')
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      //		  for (i=0;i<strlen(gewaesser_name);i++)       //GEWAESSERNAME
      //				temp_zaehler[i+2]=gewaesser_name[i];
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)        // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.hgralle);i++)  // TYP
        temp_zaehler[i+25]=steuerdaten.hgralle[i];
      
      for(i=0;i<=15;i++)                           // LEERZEICHEN
        leer[i]=' ';
      leer[16]='\0';
      
      fprintf(out,"%s% 8.2f%s        \n",temp_zaehler,steuerdaten.w_anfang,leer);
    } //hgrall
    
    
    
    /****HNORMA****/
    
    if (steuerdaten.normalle[0]=='H')
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      //	  for (i=0;i<strlen(gewaesser_name);i++)        // GEWAESSERNAME
      //			temp_zaehler[i+2]=gewaesser_name[i];
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)         // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.normalle);i++)  // Typ
        temp_zaehler[i+25]=steuerdaten.normalle[i];
      
      for(i=0;i<=15;i++)                            // LEERZEICHEN
        leer[i]=' ';
      leer[16]='\0';
      
      fprintf(out,"%s% 8.2f%s        \n",temp_zaehler,steuerdaten.w_anfang,leer);
    } //hnorma
    
    /****WEHRAN****/
    
    if (steuerdaten.wehran[0]=='W')
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)       // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.wehran);i++)  // TYP
        temp_zaehler[i+25]=steuerdaten.wehran[i];
      
      gcvt(st_anfang,10,anfang_chr);
      m=45;
      for (i=strlen(anfang_chr);i>0;i--)          // STATION
      {
        temp_zaehler[m]=anfang_chr[i-1];
        m--;
      }
      
      for(i=0;i<=15;i++)                           // LEER
        leer[i]=' ';
      leer[16]='\0';
      
      fprintf(out,"%s% 8.2f%s\n",temp_zaehler,steuerdaten.w_anfang,leer);
      
    }
    
    
    
    if (steuerdaten.kalmin[0]=='K')
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      for (i=0;i<6;i++)  // TYP
        temp_zaehler[i+25]=steuerdaten.kalmin[i];
      for(i=0;i<=23;i++)                           // LEER
        leer[i]=' ';
      leer[24]='\0';
      
      fprintf(out,"%s%s\n",temp_zaehler,leer);
    }
    
    /**alles folgende ist noch eventuell zu aendern****/
    
    if (steuerdaten.nasabs[0]=='N')
    {
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      for (i=0;i<6;i++)  // TYP
        temp_zaehler[i+25]=steuerdaten.nasabs[i];
      for(i=0;i<=23;i++)                           // LEER
        leer[i]=' ';
      leer[24]='\0';
      
      fprintf(out,"%s%s\n",temp_zaehler,leer);
      
      
    }
    
    
    /****NASALL****/
    
    if (steuerdaten.nasall[0]=='N')
    {
      steuerdaten.nasall[0]='\0';
      strcat(steuerdaten.nasall,"NASALL");
      steuerdaten.nasall[strlen(steuerdaten.nasall)]='\0';
      
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      //	  for (i=0;i<strlen(gewaesser_name);i++)          //GEWAESSERNAME
      //			temp_zaehler[i+2]=gewaesser_name[i];
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)          // Q-EREIGNSI
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      for (i=0;i<strlen(steuerdaten.nasall);i++)  // TYP
        temp_zaehler[i+25]=steuerdaten.nasall[i];
      
      for(i=0;i<=23;i++)                             // LEER
        leer[i]=' ';
      leer[24]='\0';
      
      
      fprintf(out,"%s%s        \n",temp_zaehler,leer);
    } //NASALL
    
    /****WQBEZ****/
    
    if (steuerdaten.wqbez[0]=='W')
    {
      /*************/
      for (i=0; i<=50;i++)
        temp_zaehler[i]=' ';
      temp_zaehler[51]='\0';
      
      for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
        temp_zaehler[i+2]=bufgewname[i];
      
      for (i=0;i<strlen(steuerdaten.q);i++)       // Q-EREIGNIS
        temp_zaehler[i+14]=steuerdaten.q[i];
      
      steuerdaten.wqbez[0]='\0';
      strcat(steuerdaten.wqbez,"WQBEZ");
      for (i=0;i<strlen(steuerdaten.wqbez);i++)  // TYP
        temp_zaehler[i+25]=steuerdaten.wqbez[i];
      
      gcvt(st_anfang,10,anfang_chr);
      m=45;
      for (i=strlen(anfang_chr);i>0;i--)          // STATION
      {
        temp_zaehler[m]=anfang_chr[i-1];
        m--;
      }
      temp_zaehler[51]='\0';
      min=atof(steuerdaten.qmin);
      step=atof(steuerdaten.qstep);
      max=atof(steuerdaten.qmax);
      fprintf(out,"%s% 8.2lf% 8.2lf% 8.2lf\n",
        temp_zaehler,min,step,max);
      
      /*****WEITERE ZEILE BEI WQBEZ************/
      if (steuerdaten.wasser[1]=='S')
      {
        for (i=0; i<=50;i++)
          temp_zaehler[i]=' ';
        temp_zaehler[51]='\0';
        
        for (i=0;i<strlen(steuerdaten.wasser);i++)   // TYP
          temp_zaehler[i+25]=steuerdaten.wasser[i];
        if(steuerdaten.schiess==1)
          temp_zaehler[i+25]='-';
        
        for(i=0;i<=15;i++)
          leer[i]=' ';
        leer[16]='\0';
        
        wsp=atof(steuerdaten.hoehe);
        fprintf(out,"%s% 8.2lf%s        \n",temp_zaehler,wsp,leer);
      } //WSP
      
      /*************/
      if (steuerdaten.wasser[1]=='G')
      {
        for (i=0; i<=50;i++)
          temp_zaehler[i]=' ';
        temp_zaehler[51]='\0';
        for (i=0;i<strlen(steuerdaten.wasser);i++)  // TYP
          temp_zaehler[i+25]=steuerdaten.wasser[i];
        if(steuerdaten.schiess==1)
          temp_zaehler[i+24]='-';
        
        for(i=0;i<=15;i++)                           // LEER
          leer[i]=' ';
        leer[16]='\0';
        fprintf(out,"%s% 8.2f%s        \n",temp_zaehler,steuerdaten.w_anfang,leer);
      } //HRENZ
      
      if (steuerdaten.wasser[1]=='N')
      {
        for (i=0; i<=50;i++)
          temp_zaehler[i]=' ';
        temp_zaehler[51]='\0';
        
        for (i=0;i<strlen(steuerdaten.wasser);i++)  // TYP
          temp_zaehler[i+25]=steuerdaten.wasser[i];
        if(steuerdaten.schiess==1)
          temp_zaehler[i+25]='-';
        
        sjo=atof(steuerdaten.gefaelle);              // GEFAELLE
        float dummy=0.0;
        fprintf(out,"%s% 8.2f% 8.2f% 8.5lf        \n",
          temp_zaehler,steuerdaten.w_anfang,dummy,sjo);
      } //HNORM
      
      if (steuerdaten.wehran[0]=='W')
      {
        for (i=0; i<=50;i++)
          temp_zaehler[i]=' ';
        temp_zaehler[51]='\0';
        
        for (i=0;i<strlen(steuerdaten.wehran);i++)  // TYP
          temp_zaehler[i+25]=steuerdaten.wehran[i];
        
        for(i=0;i<=15;i++)                           // LEER
          leer[i]=' ';
        leer[16]='\0';
        
        fprintf(out,"%s% 8.2f%s\n",temp_zaehler,steuerdaten.w_anfang,leer);
        
      }
      
     } //WQBEZ
         
         
         
     /****WTAU****/
         
     if (steuerdaten.wtau[0]=='W')
     {
       for (i=0; i<=50;i++)
         temp_zaehler[i]=' ';
       temp_zaehler[51]='\0';
       
       //	  for (i=0;i<strlen(gewaesser_name);i++)       //GEWAESSERNAME
       //			temp_zaehler[i+2]=gewaesser_name[i];
       
       for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
         temp_zaehler[i+2]=bufgewname[i];
       
       for (i=0;i<strlen(steuerdaten.q);i++)        // Q-EREIGNIS
         temp_zaehler[i+14]=steuerdaten.q[i];
       
       for (i=0;i<strlen(steuerdaten.wtau);i++)     // TYP
         temp_zaehler[i+25]=steuerdaten.wtau[i];
       
       for(i=0;i<=23;i++)                           // LEERZ.
         leer[i]=' ';
       leer[24]='\0';
       
       fprintf(out,"%s%s        \n",temp_zaehler,leer);
     } //WTAU
     
     /*****neu 20.3***/
     if (steuerdaten.qwv[0]=='Q')
     {
       for (i=0; i<=50;i++)
         temp_zaehler[i]=' ';
       temp_zaehler[51]='\0';
       
       //	  for (i=0;i<strlen(gewaesser_name);i++)       //GEWAESSERNAME
       //			temp_zaehler[i+2]=gewaesser_name[i];
       
       for (i=0;i<strlen(bufgewname);i++)       //GEWAESSERNAME
         temp_zaehler[i+2]=bufgewname[i];
       
       for (i=0;i<strlen(steuerdaten.q);i++)        // Q-EREIGNIS
         temp_zaehler[i+14]=steuerdaten.q[i];
       
       for (i=0;i<strlen(steuerdaten.qwv);i++)     // TYP
         temp_zaehler[i+25]=steuerdaten.qwv[i];
       
       for(i=0;i<=23;i++)                           // LEERZ.
         leer[i]=' ';
       leer[24]='\0';
       
       fprintf(out,"%s%s        \n",temp_zaehler,leer);
     } //QWV
     
     /****************/
     
     
     destroy_tabelle();
     
     
     Delete_Zeile();
     fclose(out);
     
     if (strang_list1!=NULL)
     {
       xvt_slist_destroy(strang_list1);
       strang_list1=NULL;
     }
     if (strang_list2!=NULL)
     {
       xvt_slist_destroy(strang_list2);
       strang_list2=NULL;
     }
     if (strang_list3!=NULL)
     {
       xvt_slist_destroy(strang_list3);
       strang_list3=NULL;
     }
     if (datei_list_a!=NULL)
     {
       xvt_slist_destroy(datei_list_a);
       datei_list_a=NULL;
     }
     if (datei_list_e!=NULL)
     {
       xvt_slist_destroy(datei_list_e);
       datei_list_e=NULL;
     }
     if (stationen_list!=NULL)
     {
       xvt_slist_destroy(stationen_list);
       stationen_list=NULL;
     }
     /************************************************************************/
     //NEU Dick 24.11.99
     for(i=1;i<=6;i++)
     {
       if(i==1)
       {
         batbuffer[2]='w';      //tmp.wsp ins ausgabeverzeichnis
         batbuffer[3]='s';
       }
       if(i==2)
       {
         batbuffer[2]='e'; //27.9.96     //tmp.erg
         batbuffer[3]='r';  //27.9.96
       }
       if(i==3)
       {                                //tmp.bew
         batbuffer[2]='b'; //27.9.96
         batbuffer[3]='e';  //27.9.96
       }
       if(i==4)
       {                                //tmp.qpo
         batbuffer[2]='q'; //27.9.96
         batbuffer[3]='p';  //27.9.96
       }
       if(i==5)
       {                                 //tmp.lpo
         batbuffer[2]='l'; //27.9.96
         batbuffer[3]='p';  //27.9.96
         
       }
       if(i==6)
       {                                 //tmp.lpo
         batbuffer[2]='p'; //27.9.96
         batbuffer[3]='l';  //27.9.96
         
       }
       strcpy(filename1, daten_ausgabe);
       strcat(filename1, batbuffer);
       DeleteFile(filename1);
     }
     
     batbuffer[2]='0'; //27.9.96
     batbuffer[3]='0';  //27.9.96
     //ENDE NEU
     
     strcpy(str_help,daten_verzeichnis);
     strcat(str_help,batbuffer);
     str_help[strlen(str_help)-3]='\0';
     strcat(str_help,"psi");
     test_help=access(str_help,00);
     if(test_help==0)
       Schreibe_Verlust_Datei_Knauf(batbuffer,counter_char); //in verluste.cpp
     
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, counter_char);
     strcat(filename1, ".str");
     strcpy(filename2, daten_verzeichnis);
     strcat(filename2, "profil.str");
     if (!CopyFile(filename1, filename2, FALSE))
       break;
     
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, counter_char);
     strcat(filename1, ".psi");
     strcpy(filename2, daten_verzeichnis);
     strcat(filename2, "verluste.tmp");
     CopyFile(filename1, filename2, FALSE);
     
     DeleteFile(filename1);
     
     vzk_datei[0]='\0';
     xvt_fsys_convert_dir_to_str(&STR_SPEC.dir,vzk_datei,MAX_PATH);
     strcat(vzk_datei,"\\");
     strcat(vzk_datei,batbuffer);
     vzk_datei[strlen(vzk_datei)-3]='\0';
     strcat(vzk_datei,"vzk");
     
     
     strcpy( filename1, daten_verzeichnis );
     strcat(filename1, "tmp.wsp");
     DeleteFile( filename1 );
     
     strcpy(filename1, vzk_datei);
     strcpy(filename2, daten_verzeichnis);
     strcat(filename2, "profil.vzk");
     CopyFile(filename1, filename2, FALSE);
     
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, counter_char);
     strcat(filename1, ".str");
     DeleteFile(filename1);
     
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "profil.str");
     strcpy(filename2, start_dir);
     strcat(filename2, "profil.str");
     if (!CopyFile(filename1, filename2, FALSE))
       break;
     
     /*zum Testen für Knauf, Datei erhalten: 26.9.1996*/
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "profil.str");
     strcpy(filename2, daten_ausgabe);
     strcat(filename2, "profil$.str");
     CopyFile(filename1, filename2, FALSE);
     
     batbuffer[2]='w';
     batbuffer[3]='s';
     
     strcpy(filename1, start_dir);
     strcat(filename1, "b2lwa.exe");
     strcpy(filename2, batbuffer);
     
     ::GetStartupInfo(&sui);
     sui.lpReserved = NULL;
     sui.lpTitle = "B2LWA.EXE";
     sui.dwFlags |= STARTF_USESHOWWINDOW;
     sui.wShowWindow = SW_SHOWNORMAL;
     IncProgress();
     if (bBreakBerechnung)
       break;
     if (!::CreateProcess(filename1, filename2, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui, &pi))
       break;
     hProc_Wspr = pi.hProcess;
     i = 0;
     while (GetExitCodeProcess(pi.hProcess,&exit_ausw) && exit_ausw==STILL_ACTIVE)
     {
       if (!(i%1000))
         IncProgress();
       i++;
     }
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "fehler.b2l");
     strcpy(filename2, daten_ausgabe);
     strcat(filename2, "error2.log");
     if (counter==1)
     {
       DeleteFile(filename2);
       out= fopen(filename2,"w");
     }
     else
       out= fopen(filename2,"a+");
     if ((in = fopen(filename1, "r"))!=NULL)
     {
       fgets(error_line, MAX_PATH, in);		// ignore first line
       fgets(error_line, MAX_PATH, in);		// ignore second line
       while (!feof(in))
       {
         error_line[0] = '\0';
         fgets(error_line, MAX_PATH, in);
         if (error_line[0]!=0)
           fprintf(out, "%s\n", error_line);
       }
       fclose(in);
     }
     if ( out )
       fclose(out);
     
     if (exit_ausw!=0)
       break;
     exit_ausw = 1;
     hProc_Wspr = NULL;
     IncProgress();
     if (bBreakBerechnung)
       break;
     
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "tmp.wsp");
     strcpy(filename2, daten_verzeichnis);
     strcat(filename2, batbuffer);
     CopyFile(filename1, filename2, FALSE);
     
     batbuffer[2]='0';
     batbuffer[3]='0';
     
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "verluste.tmp");
     DeleteFile(filename1);
     
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "profil.vzk");
     DeleteFile(filename1);
     
     /* tmp.wsp nach Datei mit namen schreiben vor ber. 27.9.96 */
     batbuffer[2]='w'; //27.9.96
     batbuffer[3]='s'; //27.9.96
     
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "tmp.wsp");
     strcpy(filename2, daten_verzeichnis);
     strcat(filename2, batbuffer);
     CopyFile(filename1, filename2, FALSE);
     
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "wsp.ctr");
     ctr_file = fopen(filename1,"w");
     fprintf(ctr_file,"%s\n",start_dir);
     fprintf(ctr_file,"%s\n", batbuffer);  //statt darüberliegender Zeile
     batbuffer[2]='e';
     batbuffer[3]='r';
     fprintf(ctr_file,"%s\n",batbuffer);
     batbuffer[2]='b';
     batbuffer[3]='e';
     fprintf(ctr_file,"%s\n",batbuffer);
     batbuffer[2]='q';
     batbuffer[3]='p';
     fprintf(ctr_file,"%s\n",batbuffer);
     batbuffer[2]='l';
     batbuffer[3]='p';
     fprintf(ctr_file,"%s\n",batbuffer);
     fprintf(ctr_file,"0\n",batbuffer);
     fclose(ctr_file);
     
     strcpy(filename1, "Wasserspiegelberechnung läuft...");
     SetProgressText(filename1);
     strcpy(filename1, start_dir);
     strcat(filename1, "wspr.exe");
     ::GetStartupInfo(&sui);
     sui.lpReserved = NULL;
     sui.lpTitle = "WSPR.EXE";
     sui.dwFlags |= STARTF_USESHOWWINDOW;
     sui.wShowWindow = SW_SHOWNORMAL;
     IncProgress();
     if (bBreakBerechnung)
       break;
     if (!::CreateProcess(filename1, NULL, NULL, NULL, TRUE, NORMAL_PRIORITY_CLASS, NULL, NULL, &sui, &pi))
       break;
     hProc_Wspr = pi.hProcess;
     i = 0;
     while (GetExitCodeProcess(pi.hProcess,&exit_ausw) && exit_ausw==STILL_ACTIVE)
     {
       if (!(i%1000))
         IncProgress();
       i++;
     }
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "wspr.eer");
     strcpy(filename2, daten_ausgabe);
     strcat(filename2, "error2.log");
     if ((in = fopen(filename1, "r"))!=NULL)
     {
       out= fopen(filename2,"a+");
       fgets(error_line, MAX_PATH, in);		// ignore first line
       fgets(error_line, MAX_PATH, in);		// ignore second line
       while (!feof(in))
       {
         error_line[0] = '\0';
         fgets(error_line, MAX_PATH, in);
         if (error_line[0]!=0)
           fprintf(out, "%s\n", error_line);
       }
       fclose(out);
       fclose(in);
     }
     IncProgress();
     if (bBreakBerechnung)
       break;
     hProc_Wspr = NULL;
     
     if (exit_ausw!=0)
       break;
     
     for(i=1;i<=5;i++)
     {
       if(i==1)
       {
         batbuffer[2]='w';      //tmp.wsp ins ausgabeverzeichnis
         batbuffer[3]='s';
       }
       if(i==2)
       {
         batbuffer[2]='e'; //27.9.96     //tmp.erg
         batbuffer[3]='r';  //27.9.96
       }
       if(i==3)
       {                                //tmp.bew
         batbuffer[2]='b'; //27.9.96
         batbuffer[3]='e';  //27.9.96
       }
       if(i==4)
       {                                //tmp.qpo
         batbuffer[2]='q'; //27.9.96
         batbuffer[3]='p';  //27.9.96
       }
       if(i==5)
       {                                 //tmp.lpo
         batbuffer[2]='l'; //27.9.96
         batbuffer[3]='p';  //27.9.96
         
       }
       strcpy(filename1, daten_verzeichnis);
       strcat(filename1, batbuffer);
       strcpy(filename2, daten_ausgabe);
       strcat(filename2, batbuffer);
       CopyFile(filename1, filename2, FALSE);
       DeleteFile(filename1);
     }
     
     batbuffer[2]='0'; //27.9.96
     batbuffer[3]='0';  //27.9.96
     
     //TAPE18.N85 UMBENENNEN
     batbuffer[2]='N'; //27.9.96
     batbuffer[3]='5';  //27.9.96
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "tape18.n85");
     strcpy(filename2, daten_ausgabe);
     strcat(filename2, batbuffer);
     CopyFile(filename1, filename2, FALSE);
     DeleteFile(filename1);
     
     //TAPE18.N86 UMBENENNEN
     batbuffer[2]='N'; //27.9.96
     batbuffer[3]='6';  //27.9.96
     strcpy(filename1, daten_verzeichnis);
     strcat(filename1, "tape18.n86");
     strcpy(filename2, daten_ausgabe);
     strcat(filename2, batbuffer);
     CopyFile(filename1, filename2, FALSE);
     DeleteFile(filename1);
     
     counter++;
  } //FOR BATCHLIST
  
  /************ALLGEMEINES LOESCHEN***************************/
  DeleteFileFromDir( daten_verzeichnis, "tape18.n85" );
  DeleteFileFromDir( daten_verzeichnis, "kopf.txt" );
  DeleteFileFromDir( daten_verzeichnis, "profil.str" );
  DeleteFileFromDir( daten_verzeichnis, "wsp.ctr" );
  DeleteFileFromDir( daten_verzeichnis, "qvou**.tmp" );
  DeleteFileFromDir( daten_verzeichnis, "profil.vzk" );
  DeleteFileFromDir( daten_verzeichnis, "verluste.tmp" );
  DeleteFileFromDir( daten_verzeichnis, "f77l3.eer" );
  DeleteFileFromDir( daten_verzeichnis, "lf90.eer" );
  
  strcpy(filename1, daten_verzeichnis);
  strcat(filename1, "profil.log");
  strcpy(filename2, daten_ausgabe);
  strcat(filename2, "profil.log");
  CopyFile(filename1, filename2, FALSE);
  hProc_dag_bat = pi.hProcess;
  
  timer_zaehler2=0;
  timermain2=FALSE;
  timer_main2=xvt_timer_create(main_win,1000); //in wspwin.cpp
  EndProgressDlg();
} //Funktion

void Init_Steuerdaten_LWA(st_daten *daten)
{
  daten->ia=0;
  daten->info[0]='\0';
  daten->anfang=0.0;
  daten->ende=0.0;
  daten->nhyd=1;
  daten->wehran[0]='\0';
  daten->schiess=0;
  daten->wasser[0]='\0';
  daten->wasser[6]='\0';
  daten->hoehe[0]='\0';
  daten->gefaelle[0]='\0';
  daten->eich[0]='\0';
  daten->he[0]='\0';
  daten->hgralle[0]='\0';
  daten->normalle[0]='\0';
  daten->ia=0;
  daten->kalmin[0]='\0';
  daten->nasabs[0]='\0';
  daten->nasall[0]='\0';
  daten->qwv[0]='\0';
  daten->wtau[0]='\0';
  daten->qplot[0]='\0';
  daten->lplot[0]='\0';
  daten->qmin[0]='\0';
  daten->qstep[0]='\0';
  daten->qmax[0]='\0';
  daten->ifp=0;
  daten->idr=0;
  strcpy(daten->lplot,"LPLOT");
  strcpy(daten->qplot,"QPLOT");
  
  
  strcpy(daten->izmax,"67");
  daten->ipunkt=0;
  daten->sm[0]='\0';
  daten->nposey=0;
  daten->nbeta=0;
  daten->iform=0;
  daten->inn=0;
  daten->ncar=0;
  daten->iauto=0;
  
  daten->idat=0;
  daten->nfrou=0;
  
  strcpy(daten->epsh,"0.005");
  strcpy(daten->epsv,"0.01");
  strcpy(daten->rny,"1.31");
  strcpy(daten->cwr,"1.5");
  daten->hmo=0;
  daten->idr=0;
  daten->wsf_q=0;
  daten->wsf_l=0;
  daten->kalinin=0;
  daten->u_grenze=0;//Dick 7.9.99
  daten->dhwmax=2.0;//Dick 28.09.99
  daten->vfmax=8.0;//Dick 28.09.99
  daten->hzvmax=1.0;//Dick 28.09.99
  daten->faklhg=5.0;//Dick 28.09.99
  daten->ffmax=5000.0;//Dick 28.09.99
  
}

int copy_fileb(char alte_datei[100], char neue_datei[100])

{
  FILE *alt;
  FILE *neuer_file;
  int zurueck=-1;
  BOOLEAN oeffnen=TRUE;
  char zeile[300];
  int i;

  
  if( alte_datei == NULL || alte_datei[0] == 0 || ( alt = fopen( alte_datei,"r" ) ) == NULL )
  {
    //xvt_dm_post_note("Datei %s kann nicht geöffnet werden",alte_datei);
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
    xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
    xvt_dm_post_note("%s%s%s",buf,alte_datei,buf2);
    oeffnen=FALSE;
  }
  if((neuer_file=fopen(neue_datei,"w"))==NULL)
  {
    //xvt_dm_post_note("Datei %s kann nicht geöffnet werden",neue_datei);
    char buf[200],buf2[200];//Dick 26.11.99
    xvt_res_get_str(STR_DATEI,buf,sizeof(buf));
    xvt_res_get_str(STR_CANOTOPEN,buf2,sizeof(buf2));
    xvt_dm_post_note("%s%s%s",buf,neue_datei,buf2);
    oeffnen=FALSE;
  }
  
  for(i=0;i<=299;i++)
    zeile[i]=' ';
  
  if(oeffnen==TRUE)
  {
    fgets(zeile,290,alt);
    while(!feof(alt))
    {  fprintf(neuer_file, "%s", zeile);
    fgets(zeile,290,alt);
    }
  }
  fclose(alt);
  fclose(neuer_file);
  return(zurueck);
  
}//Funktion

/**********************************************************************/
void slashn_entfernenb(char *string)
{
  int i=0;
  
  for (i=0;i<=(INT)strlen(string);i++)
  {
    if(string[i]=='\n')
      string[i]='\0';
  }
}
