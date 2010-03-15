#include "windows.h"
#include "xvt.h"

#include "global_types.h"
#include "slist.h"

/***************************************************/
WSP_SLIST* AppendStringToWspSList(WSP_SLIST *list,char *string)
 {
  char *ptr=NULL;
  WSP_SLIST *tmp,*ptmp;

  ptmp=list;

  ptr = strrchr(string,'\n'); // Zeilentrenner herausfiltern
  if ((ptr!=NULL)&&(ptr[0]=='\n')) ptr[0] ='\0';

  {
  if (list!=NULL)
	{
	 tmp = new WSP_SLIST;
	 while (list->next)
		list = list->next;
	 if (tmp)
		{
		 list->next= tmp;
		 tmp->next=NULL;
		 tmp->string = new char[strlen(string)+1];
		 strcpy(tmp->string,string);
		 return ptmp;
		}
	 else return NULL;
	}
  else
	{
	 list = new WSP_SLIST;
	 list->string = new char[strlen(string)+1];
	 strcpy(list->string,string);
	 list->next=NULL;
	 return list;
	}
  }
  return list;
 }


int DeleteWspSList(WSP_SLIST*list)
 {
  WSP_SLIST *tmp;

  while (list)
	{
	 tmp = list;
	 if (tmp->string!=NULL)
	  {
		delete tmp->string;
		tmp->string=NULL;
	  }
		list=list->next;
    	delete tmp;
        tmp=NULL;
  }

  return 0;
 }

/***************************************************/
int GetStringFromWspSList(WSP_SLIST *liste,int i,char *string)
{
 /* aus einer WspSlist das i-te Element zurückliefern
	 (das erste Element hat i=0)
 */
 WSP_SLIST *tmp;
 int zaehler=0;

 tmp = liste;
 if (tmp !=NULL)
  {
	while ((tmp!=NULL) && (zaehler < i))
	{
	 tmp = tmp->next;
	 zaehler++;
	}
	if ((tmp !=NULL)&&(tmp->string!=NULL))
	  strcpy(string,tmp->string);
	else string[0]='\0';
	return 1;
  }
 else return 0;
}
/***************************************************/
int ChangeStringInWspSList(WSP_SLIST *liste,char *str,int line)
{
 /* in einer WspSlist das i-te Element(line) gegen char *str tauschen
	 (das erste Element hat i=0)
	 das alte Element wird gelöscht
 */
 WSP_SLIST *tmp1,*tmp2;
 int zaehler=0;

 tmp1 = liste;
 if (tmp1 !=NULL)
  {
	if (line ==0)  //erstes Element
	 {
	  delete tmp1->string;
	  tmp1->string = new char[strlen(str)+1];
	  strcpy(tmp1->string,str);
	  return 1;
	 }

	while ((tmp1!=NULL) && (zaehler < line-1))
	{
	 tmp1 = tmp1->next;
	 zaehler++;
	}
	if ((tmp1 !=NULL)&&(str !=NULL))
	 {
	  if (tmp1->next !=NULL)
		  tmp2 = tmp1->next->next;
	  else tmp2 = NULL;
	  delete tmp1->next;
	  tmp1->next = new WSP_SLIST;
	  tmp1->next->string = new char[strlen(str)+1];
	  strcpy(tmp1->next->string,str);
	  tmp1->next->next=tmp2;
	  return 1;
	 }
  }
 return 0;
}
/***************************************************/
int WspSListCountElem(WSP_SLIST *liste)
{
 int counter=0;
 while(liste)
  {
	counter++;
	liste=liste->next;
  }
 return counter;
}
/***************************************************/
WSP_SLIST* WspSlistInsertSList(WSP_SLIST *liste,WSP_SLIST *insert_liste)
/* 'insert_liste'  als erste Elemente in 'liste' einhängen*/
{
 WSP_SLIST *tmp1,*tmp2,*tmp3;

 tmp1 = liste;
 tmp2 = insert_liste;
 tmp3 = NULL;

 if (insert_liste)
 {
	while (tmp2 !=NULL)    // neue Liste:tmp3 anlegen
	  {
		tmp3 = AppendStringToWspSList(tmp3,tmp2->string);
		tmp2 = tmp2->next;
	  }
	if (tmp3 !=NULL)
	  {
		tmp2 = tmp3;
		while (tmp2->next !=NULL)
			tmp2 = tmp2->next;

		//Liste tmp3 vor 'liste' einhängen
		if ((liste!=NULL)&&(tmp3!=NULL))
		 {
		  liste = tmp3;
		  tmp2->next = tmp1;
		 }
	  }
 }

 return liste;
}


/******************************************************************************/
void GetDateString(char *str_datum)
{
  char *tmp;
  tmp = new char[100];
  
  SYSTEMTIME systime;
  
  GetSystemTime(&systime);
  itoa( (char)systime.wDay, tmp, 10 );
  strcpy( str_datum, tmp );
  strcat( str_datum, "." );
  
  itoa( (char)systime.wMonth, tmp, 10 );
  strcat( str_datum, tmp );
  strcat( str_datum, "." );
  
  itoa( (int) systime.wYear, tmp, 10 );
  strcat( str_datum, tmp );
  
  delete[] tmp;
}
/******************************************************************************/

