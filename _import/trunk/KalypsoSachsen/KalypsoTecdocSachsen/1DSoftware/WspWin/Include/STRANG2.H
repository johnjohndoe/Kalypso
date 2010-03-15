#ifndef _STRANG2_H_
#define _STRANG2_H_

struct STRANG;

void strang_in_slist(void); // wspd211
void verzweigtes_profil_sortieren( int autosort, int vorwaerts); // wspd136, aufnehm, w120
int CompareKeys( double station1, int vzk1, int pk1, double station2, int vzk2, int pk2, 
                 BOOL bRichtung = TRUE ); // readprof

#endif // _STRANG2_H_