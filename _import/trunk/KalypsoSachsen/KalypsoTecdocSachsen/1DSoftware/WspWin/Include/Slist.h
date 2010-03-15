#ifndef _SLIST_H
#define _SLIST_H


void GetDateString(char *); // in laengs.dll/Jabron1.cpp und wspwin.exe/printer.cpp und volume1.cpp

int ChangeStringInWspSList(WSP_SLIST *,char *,int); // nur für laengs.dll
int        GetStringFromWspSList(WSP_SLIST *,int,char*); // nur laengs.dll und wspwin(list.cpp
WSP_SLIST* AppendStringToWspSList(WSP_SLIST*,char *); // überall
int        DeleteWspSList(WSP_SLIST*); // überall
int        WspSListCountElem(WSP_SLIST *liste); // nur laengs.dll und wspwin/list
WSP_SLIST* WspSlistInsertSList(WSP_SLIST *liste,WSP_SLIST *insert_liste); // nur laengs.dll/jabron1

#endif // _SLIST_H_
