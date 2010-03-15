#include <windows.h>
#include "xvt.h"

#include "wspwin.h"		// GHJ
#include "resource.h"

#include "global_types.h"

#include "list.h"

#include "global.h"
#include "bce_allg.h"

#include "plot.h"

extern SLIST header_profil;


int get_line_12(LINE12 *l12)
{
  char *line_12,*f1;
  int i,tz,p=0,line=0;

  memset(l12->anlage,0,15);
  l12->stationsbeschr	=0;
  l12->antragsteller 	=0;
  l12->querfelder    	=0;
  l12->schriftfeldzeilen[0]=0;
  l12->schriftfeldzeilen[1]=0;
  l12->schriftfeldzeilen[2]=0;
  l12->textzeilen[0]=0;
  l12->textzeilen[1]=0;
  l12->textzeilen[2]=0;

  if (header_profil !=NULL)
	  line_12=xvt_slist_get_elt(header_profil,11,0);  //Zeile 12 Profildatei

  if (strlen(line_12)>25)
	{
	 strncpy(l12->anlage,line_12,12);
	 l12->anlage[12]='\0';
	 l12->stationsbeschr				=line_12[13]-48;
	 l12->antragsteller 				=line_12[15]-48;
	 l12->querfelder    				=line_12[17]-48;

	 l12->schriftfeldzeilen[0]    =line_12[19]-48;
	 l12->schriftfeldzeilen[1]    =line_12[22]-48;
	 l12->schriftfeldzeilen[2]    =line_12[25]-48;

	 l12->textzeilen[0]    =line_12[20]-48;
	 l12->textzeilen[1]    =line_12[23]-48;
	 l12->textzeilen[2]    =line_12[26]-48;

	 for (i=0;i<3;i++)
		 if (l12->textzeilen[i]>2)
			{
			 l12->textzeilen[i]=2;
			 return 0;
			}
	 if (strlen(line_12)>28)
		{
		 f1 = &line_12[28];    //an dieser Stelle kann evt. Text folgen
		 tz=l12->textzeilen[0]+l12->textzeilen[1]+l12->textzeilen[2];

		 for (i=1;i<=tz;i++)
		  if (*f1 !='\0')
			 {
			  while ((f1[0] !=' ')&&(f1[0] !='\0'))
				 {
				  l12->text[line][p]=f1[0];
				  p++;
				  f1++;
				 }
			  l12->text[line][p]='\0';
			  f1++;
			  line++;
			  p=0;
			 }
		}
	}
 return 1;
}
/*********************************************************************/
int save_line_12(LINE12 *l12)
{
 char temp[100],*p;
 for (int i=0;i<100;i++)
	temp[i]=' ';          //init
 if (header_profil !=NULL)
  {
	strncpy(temp,l12->anlage,strlen(l12->anlage));
	temp[13] = l12->stationsbeschr+48;  //int-Wert+ascii(48)
	temp[15] = l12->antragsteller+48;
	temp[17] = l12->querfelder+48;

	temp[19] = l12->schriftfeldzeilen[0]+48;
	temp[22] = l12->schriftfeldzeilen[1]+48;
	temp[25] = l12->schriftfeldzeilen[2]+48;

	temp[20] = l12->textzeilen[0]+48;
	temp[23] = l12->textzeilen[1]+48;
	temp[26] = l12->textzeilen[2]+48;

	p=&temp[28];

	for (int j=0;j<3;j++)
	 for (i=0;i<l12->textzeilen[j];i++)
	  {
		 if ((l12->textzeilen[j]>0)&&(strlen(l12->text[j*2+i])>0) )
			{
			 strncpy(p,l12->text[j*2+i],strlen(l12->text[j*2+i]));
			 p=p + strlen(l12->text[j*2+i]) +1;
			}
	  }
	p[0]='\0';
	xvt_slist_change_str(header_profil,temp,11); //keine xvt-Funktion -->util.cpp
  }
 return 1;
}
/*********************************************************************/
void set_options(WINDOW *lb,WINDOW *edit_pl100, LINE12 *l12)
{
 /*  Listbuttons in Dialog plot100.cpp setzen  */
  switch (l12->querfelder)
	  {
				 case 0:
					  xvt_vobj_set_enabled(lb[3],FALSE);
					  xvt_vobj_set_enabled(lb[4],FALSE);
					  xvt_vobj_set_enabled(lb[5],FALSE);
					  xvt_vobj_set_enabled(lb[6],FALSE);
					  xvt_vobj_set_enabled(lb[7],FALSE);
					  xvt_vobj_set_enabled(lb[8],FALSE);

					  xvt_vobj_set_enabled(edit_pl100[1],FALSE); //Textzeilen
					  xvt_vobj_set_enabled(edit_pl100[2],FALSE);
					  xvt_vobj_set_enabled(edit_pl100[3],FALSE);
					  xvt_vobj_set_enabled(edit_pl100[4],FALSE);
					  xvt_vobj_set_enabled(edit_pl100[5],FALSE);
					  xvt_vobj_set_enabled(edit_pl100[6],FALSE);
					break;
				 case 1:
					  xvt_vobj_set_enabled(lb[3],TRUE);
					  xvt_vobj_set_enabled(lb[4],FALSE);
					  xvt_vobj_set_enabled(lb[5],FALSE);
					  xvt_vobj_set_enabled(lb[6],TRUE);
					  xvt_vobj_set_enabled(lb[7],FALSE);
					  xvt_vobj_set_enabled(lb[8],FALSE);

					  xvt_vobj_set_enabled(edit_pl100[1],TRUE); //Textzeilen
					  xvt_vobj_set_enabled(edit_pl100[2],TRUE);
					  xvt_vobj_set_enabled(edit_pl100[3],FALSE);
					  xvt_vobj_set_enabled(edit_pl100[4],FALSE);
					  xvt_vobj_set_enabled(edit_pl100[5],FALSE);
					  xvt_vobj_set_enabled(edit_pl100[6],FALSE);
					break;
				 case 2:
					  xvt_vobj_set_enabled(lb[3],TRUE);
					  xvt_vobj_set_enabled(lb[4],TRUE);
					  xvt_vobj_set_enabled(lb[5],FALSE);
					  xvt_vobj_set_enabled(lb[6],TRUE);
					  xvt_vobj_set_enabled(lb[7],TRUE);
					  xvt_vobj_set_enabled(lb[8],FALSE);

					  xvt_vobj_set_enabled(edit_pl100[1],TRUE); //Textzeilen
					  xvt_vobj_set_enabled(edit_pl100[2],TRUE);
					  xvt_vobj_set_enabled(edit_pl100[3],TRUE);
					  xvt_vobj_set_enabled(edit_pl100[4],TRUE);
					  xvt_vobj_set_enabled(edit_pl100[5],FALSE);
					  xvt_vobj_set_enabled(edit_pl100[6],FALSE);
					break;
				 case 3:
					  xvt_vobj_set_enabled(lb[3],TRUE);
					  xvt_vobj_set_enabled(lb[4],TRUE);
					  xvt_vobj_set_enabled(lb[5],TRUE);
					  xvt_vobj_set_enabled(lb[6],TRUE);
					  xvt_vobj_set_enabled(lb[7],TRUE);
					  xvt_vobj_set_enabled(lb[8],TRUE);

					  xvt_vobj_set_enabled(edit_pl100[1],TRUE); //Textzeilen
					  xvt_vobj_set_enabled(edit_pl100[2],TRUE);
					  xvt_vobj_set_enabled(edit_pl100[3],TRUE);
					  xvt_vobj_set_enabled(edit_pl100[4],TRUE);
					  xvt_vobj_set_enabled(edit_pl100[5],TRUE);
					  xvt_vobj_set_enabled(edit_pl100[6],TRUE);
					break;
		 default:break;
	  }
	 if (l12->querfelder>0)
	  switch (l12->textzeilen[0])
	  {
				case 0:
					xvt_vobj_set_enabled(edit_pl100[1],FALSE);
					xvt_vobj_set_enabled(edit_pl100[2],FALSE);
				  break;
				case 1:
					xvt_vobj_set_enabled(edit_pl100[1],TRUE);
					xvt_vobj_set_enabled(edit_pl100[2],FALSE);
				  break;
				case 2:
					xvt_vobj_set_enabled(edit_pl100[1],TRUE);
					xvt_vobj_set_enabled(edit_pl100[2],TRUE);
				  break;
		 default:break;
	  };
	if (l12->querfelder>1)
	  switch (l12->textzeilen[1])
	  {
				case 0:
					xvt_vobj_set_enabled(edit_pl100[3],FALSE);
					xvt_vobj_set_enabled(edit_pl100[4],FALSE);
				  break;
				case 1:
					xvt_vobj_set_enabled(edit_pl100[3],TRUE);
					xvt_vobj_set_enabled(edit_pl100[4],FALSE);
				  break;
				case 2:
					xvt_vobj_set_enabled(edit_pl100[3],TRUE);
					xvt_vobj_set_enabled(edit_pl100[4],TRUE);
				  break;
		 default:break;
	  };
	if (l12->querfelder>2)
	 switch (l12->textzeilen[2])
	  {
				case 0:
					xvt_vobj_set_enabled(edit_pl100[5],FALSE);
					xvt_vobj_set_enabled(edit_pl100[6],FALSE);
				  break;
				case 1:
					xvt_vobj_set_enabled(edit_pl100[5],TRUE);
					xvt_vobj_set_enabled(edit_pl100[6],FALSE);
				  break;
				case 2:
					xvt_vobj_set_enabled(edit_pl100[5],TRUE);
					xvt_vobj_set_enabled(edit_pl100[6],TRUE);
				  break;
		 default:break;
	  };
}
/******************************************************************************/
int get_line_15(LINE15 *l15,SLIST header)
{
  char *line_15,*p1;
  int pos;

  if (header_profil !=NULL)
	  line_15=xvt_slist_get_elt(header,14,0);  //Zeile 15 der Profildatei

  memset(l15->dist_schriftfeld,0,15);
  memset(l15->paperlength,0,15);
  memset(l15->paperhigh,0,15);
  memset(l15->bezugshoehe_NN,0,15);
  memset(l15->x_mass,0,15);
  memset(l15->y_mass,0,15);
  l15->u_schriftfeld='\0';
  l15->u_werte='\0';
  l15->u_bzghoehe='\0';
  memset(l15->schriftfeldzeilen,0,25);

  if (strlen(line_15)>10)
	{
	 p1 = &line_15[0];
	 while (p1[0]!='\0')
	  {
		while ((p1[0]==' ')&&(p1[0]!='\0'))
		  p1++;
		pos=0;
		while ((p1[0]!=' ')&&(pos <15)&&(p1[0]!='\0'))
		  {
			l15->dist_schriftfeld[pos]=p1[0];
			p1++;
			pos++;
		  }
		while ((p1[0]==' ')&&(p1[0]!='\0'))
		  p1++;
		pos=0;
		while ((p1[0]!=' ')&&(pos <15)&&(p1[0]!='\0'))
		  {
			l15->paperlength[pos]=p1[0];
			p1++;
			pos++;
		  }
		while ((p1[0]==' ')&&(p1[0]!='\0'))
		  p1++;
		pos=0;
		while ((p1[0]!=' ')&&(pos <15)&&(p1[0]!='\0'))
		  {
			l15->bezugshoehe_NN[pos]=p1[0];
			p1++;
			pos++;
		  }
		while ((p1[0]==' ')&&(p1[0]!='\0'))
		  p1++;
		pos=0;
		while ((p1[0]!=' ')&&(pos <15)&&(p1[0]!='\0'))
		  {
			l15->x_mass[pos]=p1[0];
			p1++;
			pos++;
		  }
		if (pos<15)
		 l15->x_mass[pos]='\0';

		while ((p1[0]==' ')&&(p1[0]!='\0'))
		  p1++;
		pos=0;
		while ((p1[0]!=' ')&&(pos <15)&&(p1[0]!='\0'))
		  {
			l15->y_mass[pos]=p1[0];
			p1++;
			pos++;
		  }
		while ((p1[0]==' ')&&(p1[0]!='\0'))
		  p1++;
		pos=0;
		while ((p1[0]!=' ')&&(pos <15)&&(p1[0]!='\0'))
		  {
			l15->paperhigh[pos]=p1[0];
			p1++;
			pos++;
		  }
		while ((p1[0]==' ')&&(p1[0]!='\0'))
		  p1++;
		pos=0;
		while ((p1[0]!=' ')&&(pos <2)&&(p1[0]!='\0'))
		  {
			l15->u_schriftfeld=p1[0];
			p1++;
			pos++;
		  }
		while ((p1[0]==' ')&&(p1[0]!='\0'))
		  p1++;
		pos=0;
		while ((p1[0]!=' ')&&(pos <2)&&(p1[0]!='\0'))
		  {
			l15->u_werte=p1[0];
			p1++;
			pos++;
		  }
		while ((p1[0]==' ')&&(p1[0]!='\0'))
		  p1++;
		pos=0;
		while ((p1[0]!=' ')&&(pos <2)&&(p1[0]!='\0'))
		  {
			l15->u_bzghoehe=p1[0];
			p1++;
			pos++;
		  }
		if (l15->u_schriftfeld-48 > 0)  //betrachte Schriftfeldzeilen zur Unterteilung
		  {
			if (l15->u_schriftfeld-48 > 5)
				 l15->u_schriftfeld=5;     // es können max. 5 Schriftfeldzeilen bearbeitet werden !!
			for (int i=0;i<l15->u_schriftfeld-48;i++)
				{
				 while ((p1[0]==' ')&&(p1[0]!='\0'))
				 p1++;
				 pos=0;
				 while ((p1[0]!=' ')&&(pos <2)&&(p1[0]!='\0'))
					 {
					  l15->schriftfeldzeilen[i]=p1[0];
					  p1++;
					  pos++;
					 }
				}

		  }
		else p1[0]='\0';
	  } //-while
	}
 return 1;
}

/******************************************************************************/
int save_line_15(LINE15 *l15)
{
 int ret;
 char temp[100],*p;
 for (int i=0;i<100;i++)
	 temp[i]=' ';

 ret = is_zahl(&l15->dist_schriftfeld[0]);
 if (ret > 0 )
	strcpy(temp,l15->dist_schriftfeld);
 else
	strcpy(temp,"0.00");
 strcat(temp,"  ");

 ret = is_zahl(&l15->paperlength[0]);
 if (ret > 0 )
	strcat(temp,l15->paperlength);
 else
	strcat(temp,"0.00");
 strcat(temp,"  ");

 ret = is_zahl(&l15->bezugshoehe_NN[0]);
 if (ret > 0 )
	strcat(temp,l15->bezugshoehe_NN);
 else
	strcat(temp,"0.00");
 strcat(temp,"  ");

 ret = is_zahl(&l15->x_mass[0]);
 if (ret > 0 )
	strcat(temp,l15->x_mass);
 else
	strcat(temp,"0.00");
 strcat(temp,"  ");

 ret = is_zahl(&l15->y_mass[0]);
 if (ret > 0 )
	strcat(temp,l15->y_mass);
 else
	strcat(temp,"0.00");
 strcat(temp,"  ");

 ret = is_zahl(&l15->paperhigh[0]);
 if (ret > 0 )
	strcat(temp,l15->paperhigh);
 else
	strcat(temp,"0.00");
 strcat(temp,"  ");

 p=&temp[strlen(temp)];
 p[0]=' ';
 p++;
 p[0]=l15->u_schriftfeld;
 p=p+2;
 p[0]=l15->u_werte;
 p=p+2;
 p[0]=l15->u_bzghoehe;

 p=p+2;
 p[0]=l15->schriftfeldzeilen[0];
 p=p+2;
 p[0]=l15->schriftfeldzeilen[1];
 p=p+2;
 p[0]=l15->schriftfeldzeilen[2];
 p=p+2;
 p[0]=l15->schriftfeldzeilen[3];
 p=p+2;
 p[0]=l15->schriftfeldzeilen[4];
 p++;
 p[0]='\0';
 xvt_slist_change_str(header_profil,temp,14); //keine xvt-Funktion -->util.cpp
 return 1;
}
/******************************************************************************/
void mark_line(WINDOW win, int position)
{
 RCT rct;
 CBRUSH brush;
 short middle;

/***************** GHJ ****************/
 switch (position)
  {
	case 0: middle = (int)(res_factor*421);
	 break;
	case 1: middle = (int)(res_factor*16);
	 break;
	case 2: middle = (int)(res_factor*45);
	 break;
	case 3: middle = (int)(res_factor*76);
	 break;
	case 4: middle = (int)(res_factor*104);
	 break;
	case 5: middle = (int)(res_factor*133);
	 break;
	case 6: middle = (int)(res_factor*161);
	 break;
	case 7: middle = (int)(res_factor*192);
	 break;
	case 8: middle = (int)(res_factor*221);
	 break;
	case 9: middle = (int)(res_factor*250);
	 break;
	case 10: middle = (int)(res_factor*279);
	 break;
	case 11: middle = (int)(res_factor*307);
	 break;
	case 12: middle = (int)(res_factor*337);
	 break;
	case 13: middle = (int)(res_factor*366);
	 break;
	case 14: middle = (int)(res_factor*395);
	 break;
	default:
	 break;
  };

 brush.pat =   PAT_CROSS;
 brush.color = COLOR_YELLOW;
 xvt_dwin_set_cbrush (win,&brush);
 xvt_dwin_set_std_cpen(win,TL_PEN_BLACK);
 xvt_dwin_set_draw_mode(win,M_XOR);
 xvt_rect_set(&rct,(int)(res_factor*10),middle,(int)(res_factor*112),middle+(int)(res_factor*25));//Dick 10.03.99 118 -> 112
 xvt_dwin_draw_rect(win,&rct);
 /**************************/
}
/**************************************************************************/
int read_plotter_cfg(PLOTTER *plotter_def)
{
 FILE *plotter_file;
 char tmp[200];

// xvt_fsys_convert_dir_to_str(start_dir,tmp,140);
 strcpy(tmp,start_dir);
 strcat(tmp, "\\PRODXF\\PLOTTER.CFG");

 if ((plotter_file = fopen(tmp,"r"))==NULL)
	{
	 //xvt_dm_post_error("Plotterkonfigurationsdatei\n%s\nist nicht lesbar !",tmp);
     char buf[200],buf2[200];
     xvt_res_get_str(STR_PLOT_NOTE_1,buf,sizeof(buf));
     xvt_res_get_str(STR_PLOT_NOTE_2 ,buf2,sizeof(buf2));
     xvt_dm_post_error("%s\n%s\n%s",buf,tmp,buf2);
	 return 0;
	}
 else
	{
	 for (int position=0;position<32;position++)
	  if (!feof(plotter_file))
		{
		 fscanf(plotter_file,"%s\n",plotter_def[position].layer);
		 fscanf(plotter_file,"%i\n",&plotter_def[position].color);
		 fscanf(plotter_file,"%i\n",&plotter_def[position].line);
		}

	 fclose (plotter_file);
	 return 1;
	}
}
/**************************************************************************/
int save_plotter_cfg(PLOTTER *plotter_def)
{
 FILE *plotter_file;
 char tmp[200];

// xvt_fsys_convert_dir_to_str(&PROJ_SPEC.dir,tmp,140);
// strcat(tmp, "\plotter.cfg");
 strcpy(tmp,start_dir);
 strcat(tmp, "\\PRODXF\\PLOTTER.CFG");

 if ((plotter_file = fopen(tmp,"w"))==NULL)
	{
	 //xvt_dm_post_error("Plotterkonfigurationsdatei\n%s\nkann nicht gesichert werden !",tmp);
     char buf[200],buf2[200];
     xvt_res_get_str(STR_PLOT_NOTE_1,buf,sizeof(buf));
     xvt_res_get_str(STR_PLOT_NOTE_3 ,buf2,sizeof(buf2));
     xvt_dm_post_error("%s\n%s\n%s",buf,tmp,buf2);
	 return 0;
	}
 else
	{
	 for (int position=0;position<32;position++)
		{
		 fprintf(plotter_file,"%s\n",plotter_def[position].layer);
		 fprintf(plotter_file,"%d\n",plotter_def[position].color);
		 fprintf(plotter_file,"%d\n",plotter_def[position].line);
		}

	 fclose (plotter_file);
	 return 1;
	}

}
/**************************************************************************/

