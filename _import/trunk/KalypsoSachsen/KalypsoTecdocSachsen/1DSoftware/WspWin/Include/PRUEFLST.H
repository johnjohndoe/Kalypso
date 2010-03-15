/*            PRUEFLST.H                     18.07.97       */

#ifndef _PRUEFLST_H_INCLUDED_
#define _PRUEFLST_H_INCLUDED_

#define MAX_SEL_LIST_LENGTH 90 // wspd168

int WritePrueflistenDatei(char *BerVar,BOOL bew); // wspm001
int WriteDefPrueflistenDatei(void); // wspm001
int ReadDefPrueflistenDatei(int *def_list); // wspd168
int GetPrListSelectedItems(WINDOW win,long *sel); // wspd168

#endif _PRUEFLST_H_INCLUDED_