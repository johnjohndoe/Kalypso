/*
Jabron Header  WIN95 Version
27.09.96 Andresen
*/
#ifndef _JABRON_H
#define _JABRON_H

/*****************************************************
Prototypen:Class Jabron

  Datei JABRON.CPP
  
*****************************************************/
class Jabron : public List  // nur in Jabron1.cpp benutzt
{
public:
  Jabron();
  ~Jabron(void);
  
  UINT JabWriteJab( FILE* out, const UINT nr, LPCSTR name, PROFILDATA* profilData, double station, double gefaelle, bool bInterpolBruecke );
  
  int WriteJabXKoord(char *string,int ds); // jabron1.cpp
  int WriteJabYKoord(char *string,int ds); // jabron1.cpp
  int WriteTrennflaechen(char *string,int ds,int typ); // jabron1.cpp
  int WriteDurchstBer(char *string,int ds,int typ); // jabron1.cpp
  int WriteJabKS(char *string,int ds,int erst=0); // // jabron1.cpp
  int WriteJabKSG(char *string,int ds); // jabron1.cpp
  int WriteJabKSL(char *string,int ds); // jabron1.cpp
  int WriteJabKSR(char *string,int ds); // // jabron1.cpp
  int WriteJabKSLU(char *string,int ds,int l_sohle); // jabron1.cpp
  int WriteJabKSRU(char *string,int ds,int r_sohle); // jabron1.cpp
  int WriteJabKS_Sohle(char *string,int ds,int l_sohle,int r_sohle); // jabron1.cpp
  
  int DoGeschlProfil(int maxDatensatz); // jabron1.cpp
  
  int BuildDsInfo(WSP_PROFIL_LISTE *wpl); // jabron1.cpp
  
private:
  void JabWriteGerinne( FILE* out, UINT nr, LPCSTR prof_name, PROFILDATA* profData, double station, double gefaelle, double deltaY, LPCSTR commentStr = NULL ); // jabron1.cpp
  void JabWriteGeschlossen( FILE* out, UINT nr, LPCSTR prof_name, PROFILDATA* profData, double station, double gefaelle, double laenge );
  void JabWriteKreis( FILE* out, UINT nr, LPCSTR prof_name, PROFILDATA* profData, double station, double laenge, const bool bKS );
  
  void JabWriteXY( FILE* out, Koord* profil, double deltaY );
  void JabWriteKS( FILE* out, Koord* profil, const int count );
  void JabWriteTrennfl(FILE *out,Profildatei *ptr_profil,int ds_nr);
  void JabWriteDurchstBereiche( FILE *out, Koord* gelKoord, Koord* durchstKoord );
  void JabWriteBordvoll( FILE* out, Koord* gelKoord, Koord* bordKoord );
  void JabWriteBewuchs( FILE* out, int* ds_info );
  void JabWriteComment( FILE* out, PROFILDATA* profilData, LPCSTR commentStr = NULL );
};


#endif // _JABRON_H