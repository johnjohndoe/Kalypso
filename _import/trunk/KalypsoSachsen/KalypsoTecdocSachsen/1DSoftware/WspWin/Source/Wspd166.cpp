/*  Handler for dialog 166 : DLG_CHANGE_STATION ("Stationen wählen ")*/

#include <windows.h>
#include "xvt.h"

#include "wspwin.h"
#include "resource.h"
#include "wsphilfe.h"

#include "typen.h"
#include "l_typen.h"

#include "global_types.h"
#include "global_vars.h"
#include "..\..\wspdlg\include\export.h"

#include "list.h"
#include "readprof.h"
#include "dis_prof.h"

/*
#define DLG_RES_ID DLG_CHANGE_STATION
#define DLG_FLAGS 0x0L
#define DLG_CLASS ""
#define DLG_MODE WD_MODELESS
*/

/***********  Prototypen: ****************/
void ActionDlg166(EVENT *xdEvent,int pos);
int Suche_index(void);
void GetStrangName(int index, char* wahlname);
int GetSelectedName(char *wahlname);

/***********  Prototypen: ****************/


WINDOW dlg_166, listbutton_station;
static int pos166;
char station166[20];


extern WSP_PROFIL_LISTE* pWPL;
extern WINDOW win116_typ;
extern MinMax pmm;
extern WINDOW Edit_Win116[15];
extern int draw_liste[50];
extern WINDOW scroll_116;//Neu 2.07.98
extern ZOOM zoom_info;
extern BOOL GaussProfilMitRuecksprung;
extern BOOLEAN checkbox, checkbox_alle; //Dick 16.12.98 aus W116
extern TXEDIT tx_comment116;
extern BOOLEAN berechnen, editieren;
extern draw_typ_liste_allg[200];
extern SLIST ber_var_text;


extern XVT_HELP_INFO hi;

/*************   GHJ   *************/
static WNDPROC defWndProc;
LRESULT CALLBACK Dlg166WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  switch (uMsg)
  {
  case WM_HELP:
    {
      LPHELPINFO lphi = (LPHELPINFO)lParam;
      if (hi!=NULL_HELP_INFO)
      {
        xvt_help_display_topic(hi, HID_KAPITEL_6_3);
      }
    }
    break;
    
  default:
    break;
  }
  return CallWindowProc(defWndProc, hwnd, uMsg, wParam, lParam);
}
/**********************************/


/*  Handler for dialog DLG_CHANGE_STATION (Dlg166)*/
long XVT_CALLCONV1 DLG_CHANGE_STATION_eh XVT_CALLCONV2 (WINDOW xdWindow, EVENT *xdEvent)
{
  short xdControlId = xdEvent->v.ctl.id;
  
  switch (xdEvent->type) {
  case E_CREATE:
    {
      /*************   GHJ   *************/
      if (WIN_116!=NULL_WIN)
        SetParent((HWND)xvt_vobj_get_attr(xdWindow, ATTR_NATIVE_WINDOW), (HWND)xvt_vobj_get_attr(WIN_116, ATTR_NATIVE_WINDOW));
      defWndProc = (WNDPROC)GetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC);
      SetWindowLong((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), GWL_WNDPROC, (LONG)&Dlg166WindowProc);
      ChangeFontAndSize((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW));  // GHJ
      RECT rect;
      POINT pt1, pt2;
      if (WIN_116!=NULL_WIN)
      {
        ::GetWindowRect(::GetDlgItem((HWND)xvt_vobj_get_attr(WIN_116,ATTR_NATIVE_WINDOW), WIN_GRAPH_116_CHECKBOX_34), &rect);
        pt2.x = rect.left;
        pt2.y = rect.bottom;
        ::ScreenToClient((HWND)xvt_vobj_get_attr(WIN_116,ATTR_NATIVE_WINDOW), &pt2);
        pt2.x -= 10;
        ::GetWindowRect((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), &rect);
        pt1.x = pt2.x - (rect.right-rect.left);
        pt1.y = pt2.y - (rect.bottom-rect.top);
        ::MoveWindow((HWND)xvt_vobj_get_attr(xdWindow,ATTR_NATIVE_WINDOW), pt1.x, pt1.y, pt2.x-pt1.x, pt2.y-pt1.y, TRUE);
      }
      xvt_vobj_set_visible(xdWindow, TRUE);
      /***********************************/
      
      dlg_166 = xdWindow;
      listbutton_station = xvt_win_get_ctl(xdWindow,DLG_CHANGE_STATION_COMBO);
      
      char stat[100];
      double stationX;
      STRANG *local_strang = strang_anfang;
      STRANG *pre_strang = NULL;
      if(strang_anfang!=NULL)
        stationX=strang_anfang->anfang;
      else
        stationX=BCE_NAN;
      while(stationX!=BCE_NAN)
      {
        stationX=local_strang->anfang;
        if(local_strang->anfang != BCE_NAN )
        {
          if(pre_strang==NULL)
          {
            sprintf(&stat[0],"%.4lf",local_strang->anfang);
            xvt_list_add(listbutton_station,-1,(char*)stat);
          }
          else 
            if(strcmp(local_strang->name_anfang,pre_strang->name_ende)!=0)
            {
              sprintf(&stat[0],"%.4lf",local_strang->anfang);
              xvt_list_add(listbutton_station,-1,(char*)stat);
            }
        }
        stationX=local_strang->ende;
        if(local_strang->ende != BCE_NAN)//Dick 30.07.99
        {
          sprintf(&stat[0],"%.4lf",local_strang->ende);
          xvt_list_add(listbutton_station,-1,(char*)stat);
        }
        pre_strang=local_strang;
        local_strang = local_strang->next;
      }
      
      pos166=Suche_index(); //file_spec.name als Kriterium
      if(pos166==-1)
        pos166=dlg136_select; //nur sicherheitshalber
      xvt_list_set_sel(listbutton_station,pos166,TRUE);
      
      if (hi!=NULL_HELP_INFO)
        xvt_help_set_win_assoc(hi, xdWindow, HID_KAPITEL_4_4_3, 0L);
    }
    break;
  case E_DESTROY:
    {
      dlg_166 = NULL_WIN;
    }
    break;
  case E_CLOSE:
    {
      xvt_vobj_destroy(xdWindow);
    }
    break;
  case E_CONTROL:
    {
      
      switch(xdControlId) {
      case DLG_CHANGE_STATION_LOW: /* "vorhergehende Station" */
        {
          if (zoom_info.level>0)
          {
            xdEvent->type =E_USER;
            xdEvent->v.user.id=EVENT_ZOOM_NORMAL;
            xvt_win_dispatch_event(WIN_117,xdEvent);
            xdEvent->v.user.id=E_USER_ZOOM_NORMAL;
            xvt_win_dispatch_event(WIN_116,xdEvent);
          }
          if (pos166 >0)
          {
            pos166--;
            xvt_list_set_sel(listbutton_station,pos166,TRUE);
            ActionDlg166(xdEvent,pos166);
            xvt_scr_set_focus_vobj(Edit_Win116[0]);//2.07.98den Focus auf dem SCROLLBAR setzen damit der Grafik sich aktualiesiert
            xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu Dick 7.07.98
          }
          else MessageBeep(-1);
        }
        break;
      case DLG_CHANGE_STATION_HIGH: /* "nächste Station" */
        {
          if (zoom_info.level>0)
          {
            xdEvent->type =E_USER;
            xdEvent->v.user.id=EVENT_ZOOM_NORMAL;
            xvt_win_dispatch_event(WIN_117,xdEvent);
            xdEvent->v.user.id=E_USER_ZOOM_NORMAL;
            xvt_win_dispatch_event(WIN_116,xdEvent);
          }
          
          if (pos166 < xvt_list_count_all(lwin)-1-(xvt_list_count_all(lwin)-xvt_list_count_all(listbutton_station)))//Dick 30.07.99
          {
            pos166++;
            xvt_list_set_sel(listbutton_station,pos166,TRUE);
            ActionDlg166(xdEvent,pos166);
            xvt_scr_set_focus_vobj(Edit_Win116[0]);//2.07.98den Focus auf dem SCROLLBAR setzen damit der Grafik sich aktualiesiert
            xvt_sbar_set_range(scroll_116, HVSCROLL, 1, ds_info[1]-3);//Neu Dick 7.07.98
          }
          else 
            MessageBeep(-1);
          
        }
        break;
      case DLG_CHANGE_STATION_COMBO:
        {
          int xpos;
          
          if (zoom_info.level>0)
          {
            xdEvent->type =E_USER;
            xdEvent->v.user.id=EVENT_ZOOM_NORMAL;
            xvt_win_dispatch_event(WIN_117,xdEvent);
            xdEvent->v.user.id=E_USER_ZOOM_NORMAL;
            xvt_win_dispatch_event(WIN_116,xdEvent);
          }
          xpos = xvt_list_get_sel_index(listbutton_station);
          if (xpos !=pos166)
          {
            pos166 = xpos;
            ActionDlg166(xdEvent,pos166);
          }
        }
        break;
      }
    }
  case E_USER:
    {
      switch (xdEvent->v.user.id) {
      case -1:
      default:
        break;
      }
    }
    break;
  default:
    break;
  }
  return 0L;
}

/*********************************************************************************/
/*********************************************************************************/

// !!! ---> siehe auch File: aufnehm.cpp:  Fkt:void selektion(void)  !!!
/*********************************************************************************/
/*********************************************************************************/
/*********************************************************************************/
void change_selektion_dlg166(char *prstr)
{
  if (strlen(prstr)>0)
  {
    for (int i=0;i<12;i++)
      file_spec.name[i] = prstr[i+44];
    file_spec.name[12]='\0';
    
    for (i=12;i<19;i++)
      station166[i-12] = prstr[i];
    station166[7]='\0';
    
    /******ZUSTAND****/
    int j=0;
    for (i=0;i<11;i++)
    {
      if(prstr[i+33]!=' ')
      {
        zustand[j] = prstr[i+33];
        j++;
      }
    }
    zustand[j]='\0';
    /********VZK********/
    j=0;
    for (i=0;i<4;i++)
    {
      if(prstr[i+29]!=' ')
      {
        vzk[j]=prstr[i+29];
        j++;
      }
    }
    vzk[j]='\0';
    /**********PK**************/
    j=0;
    for (i=0;i<10;i++)
    {
      if(prstr[19+i]!=' ')
      {
        pk[j]=prstr[19+i];
        j++;
      }
    }
    pk[j]='\0';
  }
}
/*********************************************************************************/
void ActionDlg166(EVENT *xdEvent,int pos)
{
  char *str;
  //Dick 11.02.99 weg double s;
  int alttyp,typ_gefunden=-1,i;
  int typ_alt[TYPE_SIZE];
  int ds_info0_alt,draw_liste_alt[50];
  int element_nr_alt=0,element_nr_neu=0;
  
  
  str = new char[111];
  xdEvent->type = E_USER;
  xdEvent->v.user.id=E_USER_AUTOSAVE;
  xvt_win_dispatch_event(WIN_116,xdEvent);
  
  //scr.datensatz = 1;//Dick 21.08.98 weg 
  scr.scrollpos = 1;
  
  for (i=0;i<=49;i++) //Dick 16.12.98
    draw_liste_alt[i]=draw_liste[i];
  
  draw_liste[0]=1;
  //draw_liste[1]=1;//Dick 21.08.98 weg 
  for (i=2;i<=49;i++)  draw_liste[i] =0;
  
  for(i=0;i<TYPE_SIZE;i++)//Dick 16.12.98
    typ_alt[i]=typ[i];
  ds_info0_alt=ds_info[0];
  //
  win116_loesche_edit_felder();   //->dis_prof.h
  xvt_vobj_set_title(win116_typ,"\0");
  
  char wahlname[15];
  GetStrangName(pos,wahlname);
  dlg136_select=GetSelectedName(wahlname);
  
  if (dlg136_select >=0)
  {
    if (xvt_list_get_elt(lwin,dlg136_select,str,110))
    {
      change_selektion_dlg166(str);   //file.spec name, vzk pk,zustand holen
      alttyp=typ[scr.datensatz]; //Dick 21.08.98
      for(i=1;i<=scr.datensatz;i++)
      {
        if(typ[i]==typ[scr.datensatz])
          element_nr_alt++;
      }
      if( read_profildatei( pWPL, &STR_SPEC.dir, file_spec.name ) ) //Dick 25.08.99 damit nicht absturzt
      {
        xdEvent->type=E_CONTROL;
        if(pos>0)
          xdEvent->v.ctl.id=DLG_CHANGE_STATION_LOW;
        else
          xdEvent->v.ctl.id=DLG_CHANGE_STATION_HIGH;
        xvt_win_dispatch_event(dlg_166,xdEvent);
      }
      //Neu Dick 21.08.98
      for(i=1;i<=ds_info[0];i++)
      {
        if(alttyp==typ[i]&& element_nr_neu <element_nr_alt)
        {
          typ_gefunden=i;
          element_nr_neu++;
        }
      }
      if(typ_gefunden<0)
      {
        scr.datensatz = 1;
        draw_liste[1]=1;
      }
      else
      {
        scr.datensatz = typ_gefunden;
        draw_liste[1]=typ[typ_gefunden];
      }
      //Ende Neu                    
      //Neu Dick 16.12.98
      bool geaendert=FALSE;
      if(checkbox || checkbox_alle)
        for(i=2;i<=ds_info[0];i++)
        {
          geaendert=FALSE;
          for(int j=2;j<=ds_info0_alt;j++)
          {
            if(typ[i]==typ_alt[j] && /*typ[i]!=alttyp &&*/ draw_liste_alt[j]==1)
            {
              draw_liste[i]=1;
              if(!geaendert)
              {
                draw_liste_alt[j]=0;
                geaendert=TRUE;
              }
            }
          }
          for(int k=1;k<=draw_typ_liste_allg[0];k++)
          {
            if(typ[i]==draw_typ_liste_allg[k])
            {
              if(typ[i]==STATION || typ[i]==STATION_FIX ||
                typ[i]==SOHLHOEHE_2 || typ[i]==WASSERSPIEGEL_2 ||
                typ[i]==BOESCHUNG_LINKS_2 || typ[i]==BOESCHUNG_RECHTS_2)
              {
                if(ber_var_text!=NULL)
                {
                  char var_text[MAX_PATH];
                  char *line;
                  SLIST_ELT e;
                  if(list->ExtractInfo2String(i,var_text))
                  {
                    bool gefunden=FALSE;
                    for (e=xvt_slist_get_first(ber_var_text);e!=NULL;e=xvt_slist_get_next(ber_var_text,e))
                    {
                      line = xvt_slist_get(ber_var_text,e,0L);
                      if(xvt_str_match(var_text,line,TRUE))
                        gefunden=TRUE;
                    }
                    if(gefunden)
                      draw_liste[i]=1;
                    else
                      draw_liste[i]=0;
                  }
                  else
                    draw_liste[i]=0;
                }
                else
                  draw_liste[i]=0;                             
              }
              else
                draw_liste[i]=1;                       
            }
          }
        }
        if(checkbox_alle)
          for (i=2;i<=49;i++)  draw_liste[i] =1;
          //Ende Neu
          list->GetMinMax(&pmm,scr.datensatz);
          list->GetScrollDaten(&scr);
          display_prof116(&Edit_Win116[0]);
          xvt_dwin_invalidate_rect(WIN_117,0);
          
          // ****  LISTBUTTON   *************
          xvt_list_clear(pWPL->window);
          list->GetDateninfo(pWPL->window);     //in:title_list
          
          xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_6),netz_dat[0]);
          test_line9(str);                              //Station
          xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_7),str);
          xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_8),vzk);
          xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_9),pk);
          xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_41), zustand);    //Zustand
          
          if (! list->GetSonderProfilTyp(&str[0]))  // Profiltyp
            strcpy(str,"offenes Profil");
          GaussProfilMitRuecksprung=FALSE;
          xvt_vobj_set_title(xvt_win_get_ctl(WIN_116,WIN_GRAPH_116_EDIT_39),str);
   }
  }
  if (!((berechnen)&& (editieren))) //d.h. Längsschnitt
  {
    SLIST_ELT e_c;
    char *line;
    if (slist_comment == NULL)
      slist_comment = xvt_slist_create();
    xvt_tx_clear(tx_comment116);
    for (e_c=xvt_slist_get_first(slist_comment);e_c!=NULL;e_c=xvt_slist_get_next(slist_comment,e_c))
    {
      line = xvt_slist_get(slist_comment,e_c,0L);
      
      if (!xvt_tx_add_par(tx_comment116, USHRT_MAX,line ))
      {
        char buf[200];//Dick 26.11.99
        xvt_res_get_str(STR_ERROR_TXTEDIT_OUT,buf,sizeof(buf));
        xvt_dm_post_error("%s",buf);
        //xvt_dm_post_error("Fehler bei TXEDIT -Ausgabe");
      }
    }
  }
  xdEvent->type = E_CONTROL;
  xdEvent->v.ctl.id=WIN_GRAPH_116_LISTBUTTON_33;
  xvt_win_dispatch_event(WIN_116,xdEvent);
  delete[] str;
}
/*********************************************************************************/
int Suche_index(void)
{
  STRANG *local_strang = strang_anfang;
  int teste=-1,j=0;
  char letzte_name[15];
  strcpy(letzte_name,local_strang->name_anfang);
  for (int i=0;i<anzahl_profil_dat_entries;i++)  
  {
    if (local_strang !=NULL)
    {
      if(xvt_str_compare_ignoring_case(local_strang->name_anfang,letzte_name)!=0)
        j++;
      teste=xvt_str_compare_ignoring_case(local_strang->name_anfang,file_spec.name);
      if(teste==0)
        return j;
      strcpy(letzte_name,local_strang->name_anfang);
      if(xvt_str_compare_ignoring_case(local_strang->name_ende,letzte_name)!=0)
        j++;
      teste=xvt_str_compare_ignoring_case(local_strang->name_ende,file_spec.name);
      if(teste==0)
        return j;
      strcpy(letzte_name,local_strang->name_ende);
      
      
      local_strang = local_strang->next;
    }
  }

  return -1;
}

/*********************************************************************************/
void GetStrangName(int index, char* wahlname)
{
  STRANG *local_strang = strang_anfang;
  double stationX;     
  STRANG *pre_strang = NULL;
  int i=0;
  if(strang_anfang!=NULL)
    stationX=strang_anfang->anfang;
  else
    stationX=BCE_NAN;
  while(stationX!=BCE_NAN)
  {
    stationX=local_strang->anfang;
    
    if(local_strang->anfang != BCE_NAN )
    {
      if(pre_strang==NULL)
      {
        if(i==index)
        {
          strcpy(wahlname,local_strang->name_anfang);
          break;
        }
        
        i++;
      }
      else 
        if(strcmp(local_strang->name_anfang,pre_strang->name_ende)!=0)
        {
          if(i==index)
          {
            strcpy(wahlname,local_strang->name_anfang);
            break;
          }
          
          i++;
        }
    }
    stationX=local_strang->ende;
    if(i==index)
    {
      strcpy(wahlname,local_strang->name_ende);
      break;
    }
    if(local_strang->ende != BCE_NAN)//Dick 30.07.99
    {
      i++;
    }
    pre_strang=local_strang;
    local_strang = local_strang->next;
  }
}
/*********************************************************************************/
int GetSelectedName(char *wahlname)
{/*aus lwin (Dialog136) den Index zu station zurückgeben*/
  char *str,*tmp;
  //Dick 11.02.99  weg double ds;
  
  str = new char[111];
  tmp=new char[10];
  char testname[15];
  int teste=-1;
  
  int anzahl=xvt_list_count_all(lwin);
  
  for (int i=0;i<anzahl_profil_dat_entries;i++)
  {
    if (i>=anzahl)
      xvt_dm_post_note("Error GetSelectedStation");
    
    if(xvt_list_get_elt(lwin,i,str,110))
    {
      for (int j=0;j<12;j++)
        testname[j] =str[j+44];
      testname[12]='\0';
      int teste=xvt_str_compare_ignoring_case(testname,wahlname);
      if(teste==0)
      {
        delete[] str;
        delete[] tmp;
        return i;
      }
    }
  }
  delete[] str;
  delete[] tmp;
  return -1;
}
