// MapStateProfiles.h: Schnittstelle für die Klasse CStateProfilesMap.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPSTATEPROFILES_H__E10560D0_D360_11D9_9697_000C29C56F8A__INCLUDED_)
#define AFX_MAPSTATEPROFILES_H__E10560D0_D360_11D9_9697_000C29C56F8A__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "commonMfc/include/itemChooser.h"
#include "profilAuswahl.h"

class State;
class CrossSection;


class CStateProfilesMap  
{
private:
	  typedef std::map<State*, std::vector<CrossSection*>*> LoadedProfilesMap;
	  LoadedProfilesMap m_map;

public:
	CStateProfilesMap( ProfilInfoArray& profilInfos );
	virtual ~CStateProfilesMap();
	State* ChooseStrang( CWnd* wnd = NULL );

private:
	void Add( State* state, CrossSection* cs );
};

#endif // !defined(AFX_MAPSTATEPROFILES_H__E10560D0_D360_11D9_9697_000C29C56F8A__INCLUDED_)
