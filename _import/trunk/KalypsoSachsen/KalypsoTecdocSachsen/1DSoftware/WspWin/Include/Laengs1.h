#ifndef _LAENGS2_H_
#define _LAENGS2_H_


int ConvertLaengsprofilLwaToBce(char*,char*,WSP_PROFIL_LISTE *pWPL,char*,char*,char*,int); // wspd203.cpp, wspwin.cpp
int Ersetze_Station_mit_Abstand_dll(WSP_PROFIL_LISTE *pWPL,DIRECTORY *dirp,char* filename, STRANG *pstrang, int strang_entries); // in wspd203.cpp
int ConvertLaengsprofilBceToBce(char* file_bce,WSP_PROFIL_LISTE *pWPL,char* file_str,char* ber_var,char* abfluss_var,int enable_wsf); // in wspwin.cpp


#endif // _LAENGS2_H_