#include <windows.h>
#include "xvt.h"
#include "wspwin.h"

#include "global_types.h"

#include "global.h"

extern SLIST neue_str_list;

#pragma optimize ( "", off )
/***********************************************************************/
void Schreibe_Verlust_Datei_Knauf( char* buffer, char* counter )
{
		//char str[100];
		char *str;
    
    FILE *in;
    FILE *out;
    char muell[6][15];//Dick 17.06.99
    char swert[6][15];//Dick 17.06.99
    double stationswert;
    double dwert[5];//Dick 17.06.99
    
    str=new char[200];//Dick 17.06.99
    xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 80);
    strcat(str,"\\");
    strcat(str,buffer);
    int len=strlen(str);
    str[len-3]='\0';
    strcat(str,"psi");
    
    if((in=fopen(str,"r+"))==NULL)
    {
      
    }
    else
    {
      str[0]='\0';
      xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 80);
      strcat(str,"\\");
      strcat(str,counter);
      strcat(str,".psi");
      
      out=fopen(str,"w+");
      
      str[0]='\0';
      fgets(str,115,in);//Dick 17.06.99
      while(!feof(in))
      {
        for(int i=0;i<=4;i++)//Dick 17.06.99
        {
          dwert[i]=0;
          swert[i][0]='\0';
        }
        
        sscanf(str,"%s %s %s %s %s %s %s %s %s %s %s %s",
          muell[0],swert[0],muell[1],swert[1],muell[2],swert[2],
          muell[3],swert[3],muell[4],swert[4],muell[5],swert[5]);//Dick 17.06.99
        
        /***als string eingelsen, weil double nicht klappt bei unbekannt***/
        stationswert=atof(swert[0]);
        for(i=0;i<=4;i++)//Dick 17.06.99
        {
          if(swert[i+1][0]!='\0')
            dwert[i]=atof(swert[i+1]);
        }
        dwert[0]=dwert[0]+dwert[1]+dwert[2]+dwert[3]+dwert[4];//Dick 17.06.99
        fprintf(out,"%10.4f%10.3f\n",stationswert,dwert[0]);
        fgets(str,115,in);//Dick 17.06.99
      }
      fclose(in);
      fclose(out);
    }
    delete[]str;
}
#pragma optimize ( "", off )
/************************************************************/
void Schreibe_Verlust_Datei_BCE(void)
{
  SLIST_ELT ee;
  int zaehler;
  //char str[100];
  char *str;
  char zahl1[12],
				zahl2[12],
        zaehler_char[4],
        help[25],
        namechar[15];
  FILE *in, *out;
  char *nameptr;
  
  
  str=new char[100];
  zaehler=1;
  
  for(ee=xvt_slist_get_first(neue_str_list);
  ee!=NULL;ee=xvt_slist_get_next(neue_str_list,ee))
  {
		  xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 80);
      strcat(str,"\\");
      itoa(zaehler,zaehler_char,10);
      strcat(str,"temp");
      strcat(str,zaehler_char);
      strcat(str,".psi");
      
      int testint=access(str,00);
      if(testint==0)
      {
        in=fopen(str,"r+");
        nameptr=xvt_slist_get(neue_str_list,ee,0L);
        strcpy(namechar,nameptr);
        str[0]='\0';
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 80);
        strcat(str,"\\");
        strcat(str,namechar);
        str[strlen(str)-3]='\0';
        strcat(str,"psi");
        if(!feof(in))
          fgets(help,25,in);
        if(!feof(in))
        {
          out=fopen(str,"w+");
          
          while(!feof(in))
          {
            zahl1[0]='\0';
            zahl2[0]='\0';
            sscanf(help,"%s %s",zahl1,zahl2);
            if((zahl1[0]!='\0') && (zahl2[0]!='\0'))
              fprintf(out,"STATION %s EINLAUF %s\n",zahl1,zahl2);
            if(!feof(in))
              fgets(help,25,in);
          }
          fclose(out);
        }
        fclose(in);
        
        xvt_fsys_convert_dir_to_str(&STR_SPEC.dir, str, 80);
        strcat(str,"\\temp");
        itoa(zaehler,zaehler_char,10);
        strcat(str,zaehler_char);
        strcat(str,".psi");
        
        remove(str);
      }
      zaehler++;
  }
  delete[]str;
}
