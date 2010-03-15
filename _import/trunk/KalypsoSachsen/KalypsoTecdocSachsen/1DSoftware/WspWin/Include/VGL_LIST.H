/*            VGL_LIST.H                     18.07.97       */

#ifndef _VGL_LIST_H_INCLUDED_
#define _VGL_LIST_H_INCLUDED_

#define SPACE 32

int WriteVglListenDatei(char *BerVar1,char *BerVar2,char *BerVar3); // wspm001
int WriteVglListHeader(FILE *f,char *BerVar1,char *BerVar2,char *BerVar3); // hier
int GetVglListSelectedItems(WINDOW win,long *sel); // wspd169
int WriteVglListItems(FILE *f,long *sel); // hier


#endif _VGL_LIST_H_INCLUDED_