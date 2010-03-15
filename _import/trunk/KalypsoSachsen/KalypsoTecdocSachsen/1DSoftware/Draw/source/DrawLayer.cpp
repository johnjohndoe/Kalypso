// DrawLayer.cpp: Implementierung der Klasse CDrawLayer.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "DrawLayer.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

/*!
* MFC-Serialisierung des CDrawLayer
* @see IMPLEMENT_SERIAL
*/
void CDrawLayer::Serialize( CArchive& ar )
{
  // ruft die Mutterklasse NICHT auf: die Zuordnung der Objekte erfolgt durch die Serialisierung von CDrawDoc

	if( ar.IsStoring() )
	{
    ar << int( 0 ); // Versionsnummer dieses Serialisierungsschemas

    ar << m_name;
    ar << m_flags;
	}
	else
	{
    int nVersion;
    ar >> nVersion;

    switch( nVersion )
    {
    case 0:
      ar >> m_name;
      ar >> m_flags;
      break;
      
    default:
      AfxThrowArchiveException( CArchiveException::badSchema );
      break;
    }
  }
}