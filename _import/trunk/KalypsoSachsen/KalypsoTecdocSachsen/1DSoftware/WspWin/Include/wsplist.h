#ifndef _WSPLIST_H_
#define _WSPLIST_H_

int SaveGlobalData(PROFILDATA*); // flaeche, laengs2, volume1, wspm001, profverl
int GetGlobalData(PROFILDATA*); // dito
WSP_PROFIL_LISTE* Init_Profil_Liste(WSP_PROFIL_LISTE *); // flaeche, laengs2, volum1, wsplp2qp, wspwin, laengs.dll
int Init_PData(PROFILDATA*); // hier
void Delete_Profil_Liste(WSP_PROFIL_LISTE*); // flaeche ,laengs2, volume1, wsplp2qp, wspwin, laengs.dll
int Delete_PData(PROFILDATA*); // wsplist1



#endif // _WSPLIST_H_