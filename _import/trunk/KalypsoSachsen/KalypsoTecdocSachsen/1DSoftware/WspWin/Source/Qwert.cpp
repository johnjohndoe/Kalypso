#include <windows.h>
#include "xvt.h"
#include "wspwin.h"
#include "resource.h"

#include "global_types.h"

#include "global_vars.h"

#include "list.h"
#include "readprof.h"

#include "util.h"
#include "strang.h"

#include "global.h"
#include "read.h"
#include <math.h>

#include "util2.h"
#include "bce_allg.h"

#include "qlist.h"


extern TABELLE *nasim_anfang, *nasim_ptr, *nasim_ende;
extern int anzahl_elem;
extern SLIST list_anfang, list_ende;

QWertDatei  *ptr_wfixwert,
*ptr_wfixwert_anfang,
*ptr_wfixwert_ende  ;
extern BOOLEAN exist_wsf_wert;
extern BOOLEAN qwert_fehlt;//Dick 10.02.99
//
extern BOOLEAN teilgebiete; //extern //Dick 28.10.99

QSatz       *ptr_qsatz,
*ptr_t,
*ptr_h ;

void Delete_Qwert_Datei(void)
{
  while (ptr_qwert_anfang)
  {
    ptr_qwert = ptr_qwert_anfang;
    ptr_qwert_ende=ptr_qwert_anfang;
    ptr_qwert_anfang =ptr_qwert_anfang->next;
    ptr_h = ptr_qwert->datensatz;
    ptr_t = ptr_qwert->datensatz;
    while(ptr_h)
    {
      ptr_qsatz = ptr_h;
      ptr_t=ptr_h;
      ptr_h=ptr_h->next_ds;
      if (ptr_qsatz!=NULL)
      {
        delete ptr_qsatz;
        ptr_qsatz=NULL;
        
      }
    }
    
    if (ptr_qwert!=NULL)
    {
      delete ptr_qwert;
      ptr_qwert=NULL;
    }
  }
}; // Delete_Qwert_Datei


/***************************************************/
void MakeNewNode(void)
{
  ptr_qwert = new QWertDatei;
  ptr_qwert->datensatz = new QSatz;
  ptr_qsatz = ptr_qwert ->datensatz;
  ptr_qsatz->next_ds = NULL;
  ptr_h = ptr_t = ptr_qsatz;
  
  ptr_qwert->info[0] ='\0';
  ptr_qsatz->x=BCE_NAN;
  ptr_qsatz->y=BCE_NAN;
  ptr_qsatz->z=BCE_NAN;//Dick 2.02.99
  ptr_qsatz->ds_nr=1;
  ptr_qsatz->optimiert=FALSE;//Dick 20.07.99
  
  if (!ptr_qwert_anfang)
    ptr_qwert_anfang = ptr_qwert;
  else
    ptr_qwert_ende->next = ptr_qwert;
  
  ptr_qwert_ende = ptr_qwert;
  ptr_qwert_ende ->next = NULL;
}
/***************************************************/
int TesteEndeQwert(void)
{
  BOOLEAN ende = FALSE;
  QWertDatei *help;
  int anzahl=1;
  
  ptr_qwert = ptr_qwert_anfang;
  while (!ende)
  {
    if(ptr_qwert->next!=NULL)
    {
      if ((strlen(ptr_qwert->info) >1) &&(strlen(ptr_qwert->next->info) >1))
      {
        if (ptr_qwert !=NULL)
        {
          ptr_qwert = ptr_qwert->next;
          anzahl++;
        }
      }
      else
      {
        help = ptr_qwert->next;
        delete help->datensatz;
        delete help;
        ptr_qwert->next = NULL;
        ende = TRUE;
      }
    }
    else
      ende=TRUE;
  }
  return anzahl;
}; // TesteEndeQwert

/***************************************************/
void MakeNewQSatz(int anzahl)
{
  for (int i=2;i<=anzahl;i++)
  {
    ptr_qsatz = new QSatz;
    ptr_qsatz->x=BCE_NAN;
    ptr_qsatz->y=BCE_NAN;
    ptr_qsatz->z=BCE_NAN;//Dick 2.02.99
    ptr_qsatz->ds_nr=i;
    ptr_qsatz->optimiert=FALSE;//Dick 20.07.99
    
    ptr_t->next_ds = ptr_qsatz;
    ptr_t = ptr_qsatz;
    ptr_t->next_ds = NULL;
  }
}
/***************************************************/
void ReadWerte(int n1)         // n1 = Anzahl in QWERT.DAT
{
  char chr,
    *temp;// temp[100];
  int i=0;
  double x,y;
  int change_strang_war_da=FALSE;
  temp = new char[100];
  
  ptr_qsatz=ptr_qwert->datensatz;
  //change_strang=FALSE;
  
  
  for (int k=1;k <= n1;k++)
	 {
    //if (!change_strang)
    //{
    i=0;
    chr ='$';
    while(chr != ' ')      //  X - einlesen
		  {
      chr = fgetc(qwert_datei);
      temp[i] = chr;
      i++;
		  }
    temp[i]='\0';
    x=atof (temp);
    
    i=0;
    while (chr == ' ')
      chr=fgetc(qwert_datei);
    while( (chr != '\n')&&(chr !=' ') )    // Y -einlesen
		  {
      temp[i] = chr;
      i++;
      chr=fgetc(qwert_datei);
		  }
    //Dick 29.07.99
    if(chr == ' ')
      while (chr != '\n')
        chr=fgetc(qwert_datei);
      //
      temp[i]='\0';
      y = atof (temp);
      //  ok=fscanf(qwert_datei,"%lf",&z);
      if ((x == ptr_qsatz->x) &&(ptr_qsatz->y==BCE_NAN))
      {
        ptr_qsatz->y = y;
      }
      else
      {
        while ((ptr_qsatz->x != x ) &&( ptr_qsatz->next_ds !=NULL))
          ptr_qsatz = ptr_qsatz->next_ds;
        if (x == ptr_qsatz->x)
        {
          if(ptr_qsatz->y==BCE_NAN || ptr_qsatz->optimiert==TRUE)
            ptr_qsatz->y = y;
        }
        if (x!=ptr_qsatz->x)
        {
          change_strang=TRUE;
          change_strang_war_da=TRUE;
        }
      } //else
      ptr_qsatz = ptr_qwert->datensatz;
      //NEU Dick 20.07.99 beim geänderten Strang Abfluss auf nexte setzen
      if(change_strang)
      {
        while ((ptr_qsatz->x <= x ) &&( ptr_qsatz->next_ds !=NULL))
          ptr_qsatz = ptr_qsatz->next_ds;
        if(ptr_qsatz->y==BCE_NAN || ptr_qsatz->optimiert==TRUE)
        {
          ptr_qsatz->y = y;
          ptr_qsatz->optimiert=TRUE;
        }
        change_strang=FALSE;
        ptr_qsatz = ptr_qwert->datensatz;
      }
      //
      //} // if change_strang=FALSE
  }  // ende for...
  change_strang=change_strang_war_da;
  delete temp;
}
/***************************************************/
void ReadWerteWSF(int n1)         // n1 = Anzahl in .WSF
{
  char chr,
    *temp;// temp[100];
  int i=0;
  double x,y;
  
  temp = new char[100];
  
  ptr_qsatz=ptr_qwert->datensatz;
  //change_strang=FALSE;
  
  
  for (int k=1;k <= n1;k++)
	 {
    //	  if (!change_strang)
    //	  {
    i=0;
    chr ='$';
    while(chr != ' ')      //  X - einlesen
		  {
      chr = fgetc(wsfix_datei);
      temp[i] = chr;
      i++;
		  }
    temp[i]='\0';
    x=atof (temp);
    
    i=0;
    while (chr == ' ')
      chr=fgetc(wsfix_datei);
    while( (chr != '\n')&&(chr !=' ') )    // Y -einlesen
		  {
      temp[i] = chr;
      i++;
      chr=fgetc(wsfix_datei);
		  }
    temp[i]='\0';
    y = atof (temp);
    
    if ((x == ptr_qsatz->x) &&(ptr_qsatz->z==BCE_NAN))
    {
      ptr_qsatz->z = y;
    }
    else
    {
      while ((ptr_qsatz->x != x ) &&( ptr_qsatz->next_ds !=NULL))
        ptr_qsatz = ptr_qsatz->next_ds;
      if (x == ptr_qsatz->x)
      {
        if(ptr_qsatz->z==BCE_NAN)
          ptr_qsatz->z = y;
      }
      /*if (x!=ptr_qsatz->x)//Dick 20.07.99 für WSF irrelevant
      {
      change_strang=TRUE;
    }*/
    } //else
    ptr_qsatz = ptr_qwert->datensatz;
    //} // if change_strang=FALSE
  }  // ende for...
  delete temp;
}
/***************************************************
*       lese  QWERT.DAT                            *
*                                                  *
***************************************************/
void lese_qwert_datei(void)
{
  int i,ende,len,anzahl=0;
  char *file_str,
    *temp,
    *str,*str2,
    *hilfe,
    chr;
  SLIST_ELT e;
  
  file_str = new char[80];
  temp     = new char[100];
  str      = new char[100];
  str2      = new char[100];
  int end=0,name_exist=-1,anzahl_f=0;
  char str_f[100],file_str_f[80];
  // öffne Liste der qwert-Dateien
  
  scr.datensatz=1;//Dick 4.06.99 sonst rutscht es weiter
  
  str[0]='\0';
  STR_SPEC.name[12]='\0'; //sicherheitshalber wg. ehemals Fehler
  strcpy(str,STR_SPEC.name);
  len=strlen(str);
  str[len-3]='\0';
  len=0;
  
  if(!teilgebiete)
  {
    strcpy(str_f,str);
    strcat(str_f,"wsf");
    strcpy(wsfix_spec.name,str_f);
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str_f, 50);
    xvt_fsys_convert_str_to_dir(str_f,&wsfix_spec.dir);
    str_f[0]='\0';
    xvt_fsys_convert_dir_to_str(&wsfix_spec.dir,file_str_f,79);
    strcat(file_str_f,"\\");
    strcat(file_str_f,wsfix_spec.name);
    if ((wsfix_datei =fopen(file_str_f,"r+"))==NULL)
      exist_wsf_wert=FALSE;
    else     
      exist_wsf_wert=TRUE;
  }
  if(!teilgebiete)
    strcat(str,"qwt");
  else
    strcat(str,"tgb");
  strcpy(qwert_spec.name, str);
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 50);
  xvt_fsys_convert_str_to_dir(str,&qwert_spec.dir);
  str[0]='\0';
  
  xvt_fsys_convert_dir_to_str(&qwert_spec.dir,file_str,79);
  strcat(file_str,"\\");
  strcat(file_str,qwert_spec.name);
  if ((qwert_datei =fopen(file_str,"r+"))==NULL)
  {
    //xvt_dm_post_error("Es existiert keine Abflußdatei !");
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_KEINE_QWERTDATEI,buf,sizeof(buf));
    xvt_dm_post_error("%s",buf);
    
    if(wsfix_datei!=NULL) //Dick 4.02.99
      fclose(wsfix_datei);
    qwert_fehlt=TRUE;//Dick 10.02.99
  }
  else
  {
    if(teilgebiete)//Dick 28.10.99
    {
      //Anzahl ermitteln
      fgets(str2,90,qwert_datei);
      if(!feof(qwert_datei))
        anzahl=0;
      while(!feof(qwert_datei))
      {
        fgets(str2,90,qwert_datei);
        if(!feof(qwert_datei))
          anzahl++;
      }
      rewind(qwert_datei);
      fgets(str2,90,qwert_datei);//Dick 7.02.00 in erste Zeile steht Anzahl->überlesen
    }
    
    MakeNewNode();
    if(!teilgebiete)  //bei qwertdatei Ereignis und Anzahl einlesen	 //Dick 28.10.99
      chr = fgetc(qwert_datei);
    
    change_strang=FALSE;
    while ((!feof(qwert_datei)))//Dick 20.07.99 auskommentiert// && (!change_strang))
    {
      if(!teilgebiete)//Dick 28.10.99
      {
        ende = feof(qwert_datei);
        i=0;
        while (chr == ' ')
          chr=fgetc(qwert_datei);
        if (chr == '\n')
          chr=fgetc(qwert_datei);
        while ((chr != ' ')&&(!feof(qwert_datei)) )  // Info einlesen
        {
          str[i]=chr;
          chr = fgetc(qwert_datei);
          i++;
        }
        str[i]='\0';
        strcpy(ptr_qwert->info ,str);
        
        if(exist_wsf_wert) //Dick 2.02.99
        {
          end=fscanf(wsfix_datei,"%s",str);
          if(end!=0&&end!=EOF)
          {
            name_exist=(strcmp(str,ptr_qwert->info)==0)?1:0;
          }
        }
        i=0;
        while ((chr !='\n')&&(!feof(qwert_datei))  ) // Anzahl
        {
          temp[i]=chr;
          chr = fgetc(qwert_datei);
          i++;
        }
        temp[i]='\0';
        anzahl = atoi(temp);
      }//ende 
      
      ptr_qwert->anzahl = anzahl;
      if(!teilgebiete)//Dick 28.10.99
        if(exist_wsf_wert) //Dick 2.02.99
          if(end!=0&&end!=EOF)
          {
            end=fscanf(wsfix_datei,"%d",&anzahl_f);
            if(end==0||end==EOF||name_exist==0)
            {
              anzahl_f=0;
              exist_wsf_wert=FALSE;
            }
          }
          
          read_anfang_ende();
          int count1=xvt_slist_count(list_anfang);
          int count2=xvt_slist_count(list_ende);
          if(count1>0 && count2>0)
          {
            
            for (e=xvt_slist_get_first(list_ende);e!=NULL;e=xvt_slist_get_next(list_ende,e))
              hilfe=xvt_slist_get(list_ende,e,0l);
            xvt_slist_add_at_elt(list_anfang,NULL,hilfe,0);
            anzahl_elem=xvt_slist_count(list_anfang);
            zeige_slist(list_anfang);
            if (!feof(qwert_datei))
            {
              MakeNewQSatz(anzahl_elem);
              LeseSlistInQwert(list_anfang,scr.datensatz);
              if (anzahl>anzahl_elem)//Dick 20.07.99 		 
                change_strang=TRUE;		  		 
              ReadWerte(anzahl);
              if(!teilgebiete)//Dick 28.10.99		  
                if (exist_wsf_wert&&end!=0&&end!=EOF)//Dick 20.07.99
                {
                  ReadWerteWSF(anzahl_f);
                }
                chr = fgetc(qwert_datei);
                ende = feof(qwert_datei);
                MakeNewNode();
                scr.datensatz++;
            }
          } //if counts >0
          else
            change_strang=TRUE;
    } //while !feof qwert_datei
    fclose(qwert_datei);
    if(!teilgebiete)//Dick 28.10.99		  
      if(wsfix_datei!=NULL)
        fclose(wsfix_datei);
  }
  
  delete[] file_str;
  delete[] temp;
  delete[] str;
  delete[] str2;
}; // lese_qwert_datei

/***************************************************/
void lese_ereignisse(void)
{
  char *help;
  help =new char[21];//Dick 2.06.99 11->21
  int i=0;
  if (abflussereignisse !=NULL)
	 {
    xvt_slist_destroy(abflussereignisse);
    abflussereignisse =NULL ;
	 }
  if ((abflussereignisse=xvt_slist_create())==0)
  {
    //xvt_dm_post_error("fehler");
    char buf[200];//Dick 26.11.99
    xvt_res_get_str(STR_ERROR,buf,sizeof(buf));
    xvt_dm_post_error("%s",buf);
  }
  else
  {
    ptr_qwert =ptr_qwert_anfang;
    
    while (ptr_qwert != NULL)
    {
      strcpy (help,ptr_qwert->info);
      xvt_slist_add_at_elt(abflussereignisse,NULL,help,i);
      i++;
      ptr_qwert = ptr_qwert->next;
    }
  }
  zeige_slist(abflussereignisse);
  delete[] help;
}; // lese_ereignisse
/***************************************************************************/
void LeseSlistInQwert(SLIST list, int nr)
{
  //char temp2[15];
  SLIST_ELT e;
  char *temp;
  int ds=1;
  char *temp2;
  
  temp2= new char[15];
  ptr_qwert=ptr_qwert_anfang;
  while((ds<nr) && (ptr_qwert->next!=NULL))
  {
    ptr_qwert=ptr_qwert->next;
    ds++;
  }
  
  ptr_qsatz=ptr_qwert->datensatz;
  
  for(e = xvt_slist_get_first(list);e!=NULL;
  e = xvt_slist_get_next(list,e))
  {
    temp=xvt_slist_get(list,e,0L);
    strcpy(temp2,temp);
    ptr_qsatz->x=atof(temp2);
    if(ptr_qsatz!=NULL)
      ptr_qsatz=ptr_qsatz->next_ds;
  }
  delete[] temp2;
}
/**************************************************************/
void win122_get_scroll_daten(Scroller *pscr)
{
  int i;
  
  for (i=0;i<=14;i++)
  {
    pscr->x[i] = BCE_NAN;
    pscr->y[i] = BCE_NAN;
    pscr->z[i] = BCE_NAN;      // Z-Array initialisieren
  }
  ptr_qwert=ptr_qwert_anfang;
  
  i=1;
  while((i<pscr->datensatz) && (ptr_qwert->next!=NULL))
  {
    ptr_qwert=ptr_qwert->next;
    i++;
  }
  
  ptr_qsatz=ptr_qwert->datensatz;
  
  while((ptr_qsatz->ds_nr<pscr->scrollpos) &&( ptr_qsatz->next_ds!=NULL))
  {
    ptr_qsatz=ptr_qsatz->next_ds;
  }
  
  for (i=0;i<5;i++)
  {
    if (ptr_qsatz!=NULL)
    {
      pscr->x[i] = ptr_qsatz->x;
      pscr->y[i] = ptr_qsatz->y;
      pscr->z[i] = ptr_qsatz->z;//Dick 2.02.99
      ptr_qsatz=ptr_qsatz->next_ds;
    }
  }
  
  //pscr->z[0] = ptr_qwert->hoehe;//Dick 2.02.99 weg
  
}
/****************************************************************/

void display_win122_edit(WINDOW *win,Scroller *pscr)
{
	 //char str[15];
	 char *str;
   int i;
   
   str =new char[15];
   for(i=0;i<5;i++)
   {
     if (pscr->x[i] == BCE_NAN)
       str[0]='\0';
     else                             // horizontal
       // gcvt(pscr->x[i],10,str);
       sprintf(str,"%.4lf",pscr->x[i]);
     xvt_vobj_set_title(win[i],str);
     
     if (pscr->y[i] == BCE_NAN)
       str[0]='\0';
     else                             // horizontal
       // gcvt(pscr->y[i],10,str);
       sprintf(str,"%.4lf",pscr->y[i]);	  
     xvt_vobj_set_title(win[i+5],str);
     //Neu Dick 2.02.99
     if (pscr->z[i] == BCE_NAN)
       str[0]='\0';
     else                             // WSF		
       sprintf(str,"%.4lf",pscr->z[i]);	  
     xvt_vobj_set_title(win[i+11],str);
     //Ende 
   }
   
   delete[] str;
}
/***************************************************************/
void win122_save_scroll_daten(Scroller *pscr,char str[11],int nr,int mode)
{
  double zahl;
  int i,fehler;
  
  fehler = is_zahl(str);
  if (fehler == -1)
    zahl = BCE_NAN;
  else
    zahl=atof(str);
  ptr_qwert=ptr_qwert_anfang;
  
  i=1;
  while((i<pscr->datensatz) && (ptr_qwert->next!=NULL))
  {
    ptr_qwert=ptr_qwert->next;
    i++;
  }
  ptr_qsatz = ptr_qwert->datensatz;
  
  while((ptr_qsatz->ds_nr < pscr->scrollpos+nr) &&( ptr_qsatz->next_ds!=NULL))
  {
    ptr_qsatz=ptr_qsatz->next_ds;
  }
  if(mode==0)
    ptr_qsatz->y = zahl;
  else
    if(mode==1)
      ptr_qsatz->z = zahl;
}
/***************************************************************/
void SaveWerte(int n1)         // n1 = Anzahl in QWERT.DAT
{
  char chr,
    *temp;//temp[100];
  int i=0;
  double x,y;
  
  temp = new char[100];
  
  ptr_qsatz=ptr_qwert->datensatz;
  
  
  for (int k=1;k <= n1;k++)
	 {
    i=0;
    chr ='$';
    while(chr != ' ')      //  X - einlesen
		  {
      chr = fgetc(qwert_datei);
      temp[i] = chr;
      i++;
		  }
    temp[i]='\0';
    x=atof (temp);
    
    i=0;
    while (chr == ' ')
      chr=fgetc(qwert_datei);
    while( (chr != '\n')&&(chr !=' ') )    // Y -einlesen
		  {
      temp[i] = chr;
      i++;
      chr=fgetc(qwert_datei);
		  }
    temp[i]='\0';
    y = atof (temp);
    
    if (x == ptr_qsatz->x)
    {
      ptr_qsatz->y = y;
    }
    else
    {
      while ((ptr_qsatz->x != x ) &&( ptr_qsatz !=NULL))
        ptr_qsatz = ptr_qsatz->next_ds;
      if (x == ptr_qsatz->x)
      {
        ptr_qsatz->y = y;
      }
      else 
      {
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_ERROR_INQWERTDATEI,buf,sizeof(buf));
        xvt_dm_post_note("%s",buf);
        //xvt_dm_post_note (" Fehler in qwert-Datei !");
      }
    }
    ptr_qsatz = ptr_qwert->datensatz;
    
    if (chr != '\n')
      fscanf(qwert_datei,"%10lf", &ptr_qwert->hoehe);
    
	 }  // ende for...
  delete[] temp;
}

/***************************************************
*       schreibe -  QWERT.DAT                          *
*                                                  *
***************************************************/
int schreibe_qwert_datei(void)
{
  int anzahl=0, len;
  
  int anzahl_f=0;
  int anzahl_temp=0,anzahl_f_temp=0;//Dick 11.02.99
  char *file_str,*str;// char file_str[80],str[110];
  
  file_str = new char[80];
  str      = new char[110];
  char file_str_f[80],str_f[110];
  // öffne Liste der qwert-Dateien
  
  str[0]='\0';
  STR_SPEC.name[12]='\0'; //sicherheitshalber wg. ehemals Fehler
  strcpy(str,STR_SPEC.name);
  len=strlen(str);
  str[len-3]='\0';
  len=0;
  
  if(!teilgebiete)//Dick 28.10.99
  {
    //Neu Dick 2.02.99
    strcpy(str_f,str);
    strcat(str_f,"wsf");
    strcpy(wsfix_spec.name,str_f);
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str_f, 50);
    xvt_fsys_convert_str_to_dir(str_f,&wsfix_spec.dir);
    str_f[0]='\0';
    xvt_fsys_convert_dir_to_str(&wsfix_spec.dir,file_str_f,79);
    strcat(file_str_f,"\\");
    strcat(file_str_f,wsfix_spec.name);
    
    //ende neu
  }
  
  if(!teilgebiete)//Dick 28.10.99
    strcat(str,"qwt");
  else
    strcat(str,"tgb");
  strcpy(qwert_spec.name, str);
  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 50);
  xvt_fsys_convert_str_to_dir(str,&qwert_spec.dir);
  str[0]='\0';
  
  xvt_fsys_convert_dir_to_str(&qwert_spec.dir,file_str,79);
  strcat(file_str,"\\");
  strcat(file_str,qwert_spec.name);
  
  anzahl=0;
  ptr_qwert = ptr_qwert_anfang;
  
  while (ptr_qwert != NULL)
		{
    ptr_qsatz = ptr_qwert->datensatz;
    
    while (ptr_qsatz != NULL)
		  {
      if (ptr_qsatz->y != BCE_NAN)
        anzahl++;
      if(!teilgebiete)//Dick 28.10.99      
        if (ptr_qsatz->z != BCE_NAN)//Dick 2.02.99
          anzahl_f++;
        
        ptr_qsatz = ptr_qsatz->next_ds;
		  }
    ptr_qwert = ptr_qwert->next;
		}
  
  if (anzahl==0)
  {
    int testback=access(file_str,00);
    if (testback==0)
      remove(file_str);
  }
  
  if(!teilgebiete)//Dick 28.10.99      
    if (anzahl_f==0 || anzahl==0)
    {
      int testback=access(file_str_f,00);
      if (testback==0)
        remove(file_str_f);
    }
    
    if(!teilgebiete)//Dick 28.10.99
      if (anzahl!=0)  //anzahl_f!=0 &&
      {
        
        if ((wsfix_datei =fopen(file_str_f,"w"))==NULL)
        {
          //xvt_dm_post_error("Kann  Datei:\n %s \nnicht anlegen !",file_str_f);
          char buf[200],buf2[200];//Dick 26.11.99
          xvt_res_get_str(STR_CANNOT_CREATE1,buf,sizeof(buf));
          xvt_res_get_str(STR_CANNOT_CREATE2,buf2,sizeof(buf2));
          xvt_dm_post_error("%s%s%s",buf,file_str_f,buf2);
          // weitere Fehlerbehandlung....
          
          return -1;
        }
      }
      
      if (anzahl!=0)
      {
        
        if ((qwert_datei =fopen(file_str,"w"))==NULL)
        {
          //xvt_dm_post_error("Kann  Datei:\n %s \nnicht anlegen !",file_str);
          char buf[200],buf2[200];//Dick 26.11.99
          xvt_res_get_str(STR_CANNOT_CREATE1,buf,sizeof(buf));
          xvt_res_get_str(STR_CANNOT_CREATE2,buf2,sizeof(buf2));
          xvt_dm_post_error("%s%s%s",buf,file_str,buf2);
          // weitere Fehlerbehandlung....
          delete[] str;
          delete[] file_str;
          return -1;
        }
        else
        {
          ptr_qwert = ptr_qwert_anfang;
          
          while (ptr_qwert != NULL)
          {
            anzahl_temp=0;
            anzahl_f_temp=0;//Dick 11.02.99
            ptr_qsatz = ptr_qwert->datensatz;
            
            
            while (ptr_qsatz != NULL)
            {
              if (ptr_qsatz->y != BCE_NAN)
                anzahl_temp++;
              if (ptr_qsatz->z != BCE_NAN)//Dick 11.02.99
                anzahl_f_temp++;
              ptr_qsatz = ptr_qsatz->next_ds;
            }
            if(anzahl_temp!=0)
            {
              fprintf(qwert_datei,"%s ",ptr_qwert->info);
              fprintf(qwert_datei,"%i\n",anzahl_temp);
              if(!teilgebiete)//Dick 28.10.99
              {
                fprintf(wsfix_datei,"%s ",ptr_qwert->info);//Dick 11.02.99
                fprintf(wsfix_datei,"%i\n",anzahl_f_temp); //Dick 11.02.99
              }
            }
            
            ptr_qsatz=ptr_qwert->datensatz;
            while (ptr_qsatz != NULL)
            {
              if (ptr_qsatz->y != BCE_NAN)
              {
                fprintf(qwert_datei,"%.4lf ",ptr_qsatz->x);
                if(!teilgebiete)//Dick 28.10.99
                  fprintf(qwert_datei,"%.4f\n",ptr_qsatz->y);
                else
				{
					// cast value to int, if not, printf will write '0'
					int yInt = int( ptr_qsatz->y );
                  fprintf(qwert_datei,"%i\n",yInt);
				}
              }
              if(!teilgebiete)//Dick 28.10.99
                if (ptr_qsatz->z != BCE_NAN) //Dick 11.02.99
                {
                  fprintf(wsfix_datei,"%.4lf ",ptr_qsatz->x);
                  fprintf(wsfix_datei,"%.4f\n",ptr_qsatz->z);
                }
                ptr_qsatz = ptr_qsatz->next_ds;
            }
            ptr_qwert = ptr_qwert->next;
          } // while end
        }  //else end
        
        fclose(qwert_datei);
        if(!teilgebiete)//Dick 28.10.99
          fclose(wsfix_datei);
        
      } // anzahl!=0
 
 delete[] str;
 delete[] file_str;
 return 0;
}

/************************************************************
*   Dialog206                                               *
*                                                           *
*  struct TABELLE                                           *
*	  {                                                      *
*     int index                                            *
*		double anfang,ende;                                   *
*		TABELLE *next;                                        *
*	  }                                                      *
*                                                           *
*************************************************************/
extern WINDOW dlg206_edit_nasim[6],
dlg206_edit_ber[6],
dlg206_edit_kal[6];


TABELLE *MakeNewTabelle(int anzahl)
{
  TABELLE *tab_ptr, *ptr_ende, *ptr_anfang=NULL;
  
  for (int i=1;i<=anzahl;i++)
  {
    tab_ptr = new 	TABELLE;
    tab_ptr->next = NULL;
    tab_ptr->index = i;
    tab_ptr->anfang = BCE_NAN;
    tab_ptr->ende   = BCE_NAN;
    
    if (!ptr_anfang)
      ptr_anfang = tab_ptr;
    else
      ptr_ende->next = tab_ptr;
    ptr_ende = tab_ptr;
    ptr_ende->next = NULL;
    
  }
  return ptr_anfang;
}
/***************************************************/
void dlg206_get_daten(WINDOW *win,TABELLE *ptr_anfang,int position)
{
  //char temp[15];
  char *temp;
  TABELLE *ptr;
  
  temp =new char[15];
  
  ptr = ptr_anfang;
  for (int i=1;i<position;i++)
    ptr = ptr->next;
  
  for (i=0;i<=2;i++)
  {
    if (ptr !=NULL)
    {
      // Anfangsprofil
      if (ptr->anfang == BCE_NAN)
        temp[0]='\0';
      else
        //		gcvt(ptr->anfang,10,temp);
        sprintf(temp,"%lf",ptr->anfang);
      xvt_vobj_set_title(win[i+3],temp);
      // Endprofil
      if (ptr->ende == BCE_NAN)
        temp[0]='\0';
      else
        //		gcvt(ptr->ende,10,temp);
        sprintf(temp,"%lf",ptr->ende);
		    xvt_vobj_set_title(win[i],temp);
        ptr = ptr->next;
    }
  }
  delete[] temp;
}
/***************************************************/
void dlg206_save_daten(WINDOW *win,TABELLE *ptr_anfang,int position)
{
  //char temp[15];
  TABELLE *ptr;
  char *temp;
  
  temp=new char[15];
  
  ptr = ptr_anfang;
  for (int i=1;i<position;i++)
    ptr = ptr->next;
  
  for (i=0;i<=2;i++)
  {
    if (ptr !=NULL)
    {
      // Anfangsprofil
      xvt_vobj_get_title(win[i+3],temp,10);
      if (strlen(temp)==0)
        ptr->anfang=BCE_NAN;
      else
        ptr->anfang=atof(temp);
      // Endprofil
      xvt_vobj_get_title(win[i],temp,10);
      if (strlen(temp)==0)
        ptr->ende=BCE_NAN;
      else
        ptr->ende=atof(temp);
      ptr = ptr->next;
    }
  }
  delete[] temp;
}
/*************************************************************************/
void destroy_tabelle(void)
{
  while (nasim_anfang)
  {
    nasim_ptr = nasim_anfang;
    nasim_ende=nasim_anfang;
    nasim_anfang = nasim_anfang->next;
    
    if (nasim_ptr!=NULL)
    {
      delete nasim_ptr;
      nasim_ptr=NULL;
    }
  }
  
}



/************************************************************************/
int write_tabelle(FILE *out,TABELLE *ptr_anfang,char *titel)
{
  TABELLE *ptr;
  //char temp[15];
  int zaehler=0;
  char *temp;
  
  temp=new char[15];
  ptr = ptr_anfang;
  
  while (ptr != NULL)
  {
    if ((ptr->anfang != BCE_NAN)&&(ptr->ende != BCE_NAN))
      zaehler++;
    ptr = ptr->next;
  }
  fprintf(out,"%s\t",titel);
  fprintf(out,"%i\n",zaehler);
  
  ptr = ptr_anfang;
  while (ptr != NULL)
  {
    if ((ptr->anfang != BCE_NAN)&&(ptr->ende != BCE_NAN))
    {
    /*gcvt(ptr->anfang,10,temp);
    fprintf(out,"%s\t",temp);
      */
      fprintf(out,"%lf\t",ptr->anfang);
      
      /*gcvt(ptr->ende,10,temp);
      fprintf(out,"%s\n",temp);*/
      fprintf(out,"%lf\n",ptr->ende);
    }
    ptr = ptr->next;
  }
  delete[] temp;
  return zaehler;
}
/************************************************************************/
int read_tabelle(FILE *in, TABELLE *ptr_anfang)
{
  TABELLE *ptr;
  char titel[15];
  int zaehler=0;
  
  ptr =ptr_anfang;
  fscanf(in,"%s",&titel);
  fscanf(in,"%d\n",&zaehler);
  
  for (int i=1;i<=zaehler;i++)
  {
    if(ptr != NULL)
    {
      fscanf(in,"%lf",&ptr->anfang);
      fscanf(in,"%lf\n",&ptr->ende);
    }
    if (i<zaehler)
      ptr=ptr->next;
  }
  
  return zaehler;
}
/************************************************************************/

BOOLEAN teste_k(TABELLE *ptr_anfang, double station)
{
	BOOLEAN k_return=FALSE;
	TABELLE *ptr;
	
	ptr=ptr_anfang;
	
	// 30.3.99 nasim_ende ermitteln
	while (ptr->next!=NULL && ptr->next->anfang!=BCE_NAN)
	{
		ptr=ptr->next;
	}
	nasim_ende=ptr;
	
	ptr=ptr_anfang;
	//ende 30.3.99 nasim_ende ermitteln
	while (ptr!= NULL)
	{
		if ((ptr->anfang != BCE_NAN)&&(ptr->ende != BCE_NAN))
		{
			if(ptr==nasim_ende)
			{
				if (( fabs( ptr->anfang - station ) < 0.0001) || ( fabs( ptr->ende - station ) < 0.0001 ))	
					k_return=TRUE;
			}
			else
			{
				if ( fabs( ptr->anfang- station ) < 0.0001 )
					k_return=TRUE;
            }
		}
		ptr=ptr->next;
	}
	return k_return;
}

/************************************************************/
int teste_anzahl_hq(void)
{
  //DIE FUNKTION TESTET WIEVIELE ABFLUSSEREIGNISSE IN DER QWERT-DATEI
  //NOTIERT SIND - AUFRUF AUS FKT:SCHREIBE-BAT IN SCHREIBE.cpp
  //GRUND: BCEWSP-FORTRAN BRAUCHT BEI WASSERSPIEGELLAGENBER. NUR INDEX WENN
  //MEHR ALS EIN EREIGNIS
  
  lese_qwert_datei();
  TesteEndeQwert();
  lese_ereignisse();
  
  int abflussanzahl=xvt_slist_count(abflussereignisse);
  Delete_Qwert_Datei();
  if(abflussereignisse!=NULL)
	 {
    xvt_slist_destroy(abflussereignisse);
    abflussereignisse=NULL;
	 }
  return abflussanzahl;
  
}

/********************************************************************/


