// MapStateProfiles.cpp: Implementierung der Klasse CStateProfilesMap.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "MapStateProfiles.h"
#include "wspprj/include/state.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CStateProfilesMap::CStateProfilesMap( ProfilInfoArray& profilInfos )
{
	for( int i = 0; i < profilInfos.GetSize(); i++ )
	{
		ProfilInfo* info = profilInfos[i];
		if( info->bInMap )
		{
			State* state = info->state;
			CrossSection* cs = info->cs;

			Add( state, cs );
		}
	}
}

CStateProfilesMap::~CStateProfilesMap()
{
	for( LoadedProfilesMap::iterator mapIt = m_map.begin(); mapIt != m_map.end(); mapIt++ )
		delete mapIt->second;
}

/* private: */
void CStateProfilesMap::Add( State* state, CrossSection* cs )
{
	std::vector<CrossSection*>* list;
	LoadedProfilesMap::iterator mapIt = m_map.find( state );
	if( mapIt != m_map.end() )
		list = mapIt->second;
	else
	{
		list = new std::vector<CrossSection*>;
		m_map.insert( LoadedProfilesMap::value_type( state, list ) );
	}
	
	list->push_back( cs );
}

State* CStateProfilesMap::ChooseStrang( CWnd* wnd /* = NULL */ )
{
	CItemChooser<State*>::TypeMap stateMap;
	for( LoadedProfilesMap::iterator mapIt = m_map.begin(); mapIt != m_map.end(); mapIt++ )
	{
		State* state = mapIt->first;
		stateMap.SetAt( state->GetName(), state );
	}

	CItemChooser<State*> chooser( stateMap, "Profilabstände ermitteln", "Welcher Zustand?", wnd );
	if( chooser.DoModal() != IDOK )
		return 0;

	return chooser.GetSelectedItem();
}
