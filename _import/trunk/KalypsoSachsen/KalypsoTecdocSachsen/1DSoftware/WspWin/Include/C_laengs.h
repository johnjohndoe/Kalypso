/**********************************************************************

	 MODUL:  Konvertieren von LWA-LängsProfil --> BCE LängsProfil

	 CLASS   LaengsProfil
				11.09.96 Andresen


**********************************************************************/

#ifndef _C_LAENGS_H_
#define _C_LAENGS_H_

#include "list.h"

class LaengsProfil : public List // wird nur in laengs1.cpp benutzt
{
 protected:
	WSP_SLIST *slist_bauwerk;

 public:
  LaengsProfil();
  ~LaengsProfil(void);
  void WriteLPTypDaten(int nummer,int typ,char* info="\0"); // laengs1.cpp
  int  CopyKoordToList(int typ,int nummer,double x,double y); // laengs1.cpp
  int  BuildDsInfo(WSP_PROFIL_LISTE *wpl); //  laengs1.cpp
  void SetBceLPTypDaten(int typ,char* info); // laengs1.cpp
  void SetPtrEnde(Profildatei *anfang); // laengs1.cpp
};


#endif // _C_LAENGS_H_