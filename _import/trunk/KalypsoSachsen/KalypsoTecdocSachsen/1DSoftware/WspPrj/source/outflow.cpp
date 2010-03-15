#include "stdafx.h"

#include "3dcoord.h"
#include "state.h"

#include "outflow.h"

  ////////////////////////////
  //  Klasse  OutFlow
  ///////////////////////////

/* The Default Constructor */
OutFlow::OutFlow(Project* pProject, State* pState)
{
	m_pProject = pProject;
	m_pState = pState;
	m_nC3DCoordIndex = 0;
}

OutFlow::~OutFlow()
{
	int i;
	C3DCoord *crd;

	for (i=0; i<m_C3DCoords.GetSize(); i++)
	{
		crd = m_C3DCoords.GetAt(i);
		delete crd;
	}
	m_C3DCoords.RemoveAll();
}

OutFlow* OutFlow::Clone( Project* pProject, State* pState )
// erzeugt eine copy von sich selbst
// Rückgabewert.
//          Zeiger auf das neu erzeugte Element
{
  OutFlow* of = new OutFlow( pProject, pState );

  of->SetName( m_name );
  for ( int i = 0; i < m_C3DCoords.GetSize(); i++ )
    of->AddCoord( new C3DCoord( *m_C3DCoords[i] ) );

  return of;
}; // Clone

void OutFlow::SetName(CString& name)
{
	m_name = name;
	if (m_pState!=NULL)
		m_pState->SetOutFlowModified();
}

CString OutFlow::GetName()
{
	return m_name;
}

void OutFlow::AddCoord(C3DCoord* crd)
{
	m_C3DCoords.SetAtGrow(m_C3DCoords.GetSize(), crd);
	if (m_pState!=NULL)
		m_pState->SetOutFlowModified();
}

void OutFlow::RemoveCoord(C3DCoord* crd)
{
	C3DCoord *match;
	int i;

	for (i=0; i<m_C3DCoords.GetSize(); i++)
	{
		match = m_C3DCoords.GetAt(i);
		if (match==crd)
		{
			delete crd;
			m_C3DCoords.RemoveAt(i);
			break;
		}
	}
	if (m_pState!=NULL)
		m_pState->SetOutFlowModified();
}

void OutFlow::InsertCoordAt(int n, C3DCoord* crd)
{
	if (n>=0 && n<m_C3DCoords.GetSize())
	{
		m_C3DCoords.InsertAt(n, crd);
	}
	if (m_pState!=NULL)
		m_pState->SetModified();
}

C3DCoord* OutFlow::GetCoordAt(int n)
{
	if (n>=0 && n<m_C3DCoords.GetSize())
		return m_C3DCoords.GetAt(n);
	return NULL;
}

C3DCoord* OutFlow::GetFirstCoord()
{
	m_nC3DCoordIndex = 0;
	return GetNextCoord();
}

C3DCoord* OutFlow::GetNextCoord()
{
	if (m_nC3DCoordIndex<0 || m_nC3DCoordIndex>=m_C3DCoords.GetSize())
		return NULL;
	else
		return m_C3DCoords.GetAt(m_nC3DCoordIndex++);
}

C3DCoord* OutFlow::GetLastCoord()
{
	m_nC3DCoordIndex = m_C3DCoords.GetSize()-1;
	return GetPrevCoord();
}

C3DCoord* OutFlow::GetPrevCoord()
{
	if (m_nC3DCoordIndex<0 || m_nC3DCoordIndex>=m_C3DCoords.GetSize())
		return NULL;
	else
		return m_C3DCoords.GetAt(m_nC3DCoordIndex--);
}

int OutFlow::GetNumCoords()
{
	return m_C3DCoords.GetSize();
}

BOOL OutFlow::WSPFIsDefined()
{
	int i;

	for (i=0; i<m_C3DCoords.GetSize(); i++)
	{
		C3DCoord *pCrd;

    pCrd = m_C3DCoords[i];
    if( pCrd->dz != std::numeric_limits<double>::infinity() )
      return TRUE;
	}
	return FALSE;
}

