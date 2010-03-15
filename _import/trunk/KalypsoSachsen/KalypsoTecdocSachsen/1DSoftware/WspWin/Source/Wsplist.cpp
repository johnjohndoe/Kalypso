//** wsplist.cpp **//

#include <windows.h>
#include "xvt.h"

#include "global_types.h"
#include "slist.h"

#include "global_vars.h"

#include "list.h"
#include "wsplist.h"

/***************************************************/
int SaveGlobalData(PROFILDATA *data)
// GlobaleDaten -> data
{
  SLIST_ELT e;
  char *temp;
  
  if ( data !=NULL)
  {
    if (xvt_slist_is_valid(header_profil))
    {
      if (data->slist_header!=NULL)
      {
        DeleteWspSList(data->slist_header);
        data->slist_header=NULL;
      }
      for (e=xvt_slist_get_first(header_profil);e!=NULL;
					 e=xvt_slist_get_next(header_profil,e) )
           {
             temp = xvt_slist_get(header_profil,e,0L);
             data->slist_header=AppendStringToWspSList(data->slist_header,temp);
           }
    }
    else
      xvt_dm_post_note("Fehler in SListen-Verwaltung #1");
    
    if (slist_comment !=NULL)
    {
      if (data->slist_comment!=NULL)
      {
        DeleteWspSList(data->slist_comment);
        data->slist_comment=NULL;
      }
      for (e=xvt_slist_get_first(slist_comment);e!=NULL;
      e=xvt_slist_get_next(slist_comment,e) )
      {
        temp = xvt_slist_get(slist_comment,e,0L);
        AppendStringToWspSList(data->slist_comment,temp);
      }
    }
    
    temp = new char[255];
    
    for (int i=0;i<TYPE_SIZE;i++)//Dick 22.06.99
      data->ds_info[i]=ds_info[i];
    for (i=0;i<TYPE_SIZE;i++)//Dick 22.06.99
      data->typ[i]=typ[i];
    data->anzahl_ds    =anzahl_ds;
    data->exist_plot   =exist_plot;
    xvt_fsys_convert_dir_to_str(&file_spec.dir,temp,99);
    xvt_fsys_convert_str_to_dir(temp,&data->file.dir);
    strcpy(data->file.name,file_spec.name);
    delete[] temp;
    
    return 1;
  }
  else return 0;
}
/***************************************************/
int GetGlobalData(PROFILDATA *data)
// Data -> GlobaleDaten
{
  char *temp;
  struct _WSP_SLIST *tmp_slist;
  
  if ( data !=NULL)
  {
    temp = new char[255];
    
    if (xvt_slist_is_valid(header_profil))  //alte Daten löschen
      xvt_slist_destroy(header_profil);
    header_profil=xvt_slist_create();
    
    if ((data->slist_header !=NULL)&&(header_profil !=NULL))
    {
      tmp_slist = data->slist_header;
      while (tmp_slist !=NULL)
      {
        if(tmp_slist->string!=NULL)
          strcpy(temp,tmp_slist->string);
        xvt_slist_add_at_elt(header_profil,NULL,temp,0L);
        tmp_slist = tmp_slist->next;
      }
    }
    if(data->slist_comment !=NULL)
    {
      if (xvt_slist_is_valid(slist_comment))  //alte Daten löschen
        xvt_slist_destroy(slist_comment);
      slist_comment=xvt_slist_create();
    }
    
    if ((data->slist_comment !=NULL)&&(slist_comment !=NULL))
    {
      tmp_slist = data->slist_comment;
      while (tmp_slist !=NULL)
      {
        if(tmp_slist->string!=NULL)
          strcpy(temp,tmp_slist->string);
        xvt_slist_add_at_elt(slist_comment,NULL,temp,0L);
        tmp_slist = tmp_slist->next;
      }
    }
    
    for (int i=0;i<TYPE_SIZE;i++)//Dick 22.06.99
      ds_info[i]=data->ds_info[i];
    for (i=0;i<TYPE_SIZE;i++)//Dick 8.12.98
      typ[i]=data->typ[i];
    anzahl_ds=data->anzahl_ds;
    exist_plot=data->exist_plot   ;
    xvt_fsys_convert_dir_to_str(&data->file.dir,temp,99);
    xvt_fsys_convert_str_to_dir(temp,&file_spec.dir);
    delete[] temp;
    strcpy(file_spec.name,data->file.name);
    return 1;
  }
  else return 0;
}

/*********************************************/
WSP_PROFIL_LISTE* Init_Profil_Liste(WSP_PROFIL_LISTE *wpl)
{
/*neues Element vom Typ WSP_PROFIL_LISTE an wpl anhängen und
Subelemente alloziieren
  */
  WSP_PROFIL_LISTE *new_wpl,*tmp_wpl;
  
  new_wpl = new WSP_PROFIL_LISTE;
  if( new_wpl )
  {
    new_wpl->data   = new PROFILDATA;
    new_wpl->PList  = new List();
    new_wpl->window = NULL;
    new_wpl->PListNext=NULL;
    
    Init_PData(new_wpl->data);
  }
  
  if( new_wpl && new_wpl->PList )
  {
    if( !wpl )
    {
      new_wpl->index = 0;
      wpl = new_wpl;
      wpl->PListNext = NULL;
    }
    else
    {
      tmp_wpl = wpl;
      while(tmp_wpl->PListNext )
        tmp_wpl = tmp_wpl->PListNext;
      tmp_wpl->PListNext = new_wpl;
      tmp_wpl->PListNext->index = tmp_wpl->index + 1;
    }
    
    return wpl;
  }
  else 
    return wpl;
}
/***************************************************/
int Init_PData(PROFILDATA *pd)
{
  if( pd )
  {
    pd->slist_header = NULL;
    pd->slist_comment = NULL;
    
    pd->ds_info = new int[TYPE_SIZE];
    pd->typ = new int[TYPE_SIZE];
    pd->anzahl_ds = 0;
    pd->exist_plot = FALSE;
    pd->exist_gel2 = FALSE;
    pd->exist_comment = FALSE;
    return 1;
  }
  else 
    return 0;
}
/***************************************************/
void Delete_Profil_Liste( WSP_PROFIL_LISTE *wpl )
{
  WSP_PROFIL_LISTE* new_wpl = NULL;
  
  if ( wpl && wpl->PListNext )
    new_wpl=wpl->PListNext;
  while( wpl )
  {
    if( wpl->data )
    {
      Delete_PData( wpl->data );
      wpl->data = NULL;
    }
    if(wpl->PList )
      wpl->PList = NULL;

    wpl = NULL;
    if (new_wpl)
    {
      wpl= new_wpl;
      if(new_wpl->PListNext )
        new_wpl=wpl->PListNext;
      else 
        new_wpl = NULL;
    }
  }
  wpl = NULL;
}
/***************************************************/
int Delete_PData( PROFILDATA* pd )
{
  if( pd )
  {
    if( pd->slist_header )
    {
      DeleteWspSList( pd->slist_header );
      pd->slist_header = NULL;
    }
    if(pd->slist_comment )
    {
      DeleteWspSList( pd->slist_comment );
      pd->slist_comment = NULL;
    }
    
    if( pd->ds_info )
      delete[] pd->ds_info;
    if( pd->typ )
      delete[] pd->typ;
    if( pd )
      delete pd;
    pd = NULL;
    return 1;
  }
  else 
    return 0;
}
/***************************************************/
