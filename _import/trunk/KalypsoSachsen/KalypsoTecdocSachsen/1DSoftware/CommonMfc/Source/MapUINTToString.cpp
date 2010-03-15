// MapUINTToString.cpp: Implementierung der Klasse CMapUINTToString.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "MapUINTToString.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif


//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

/////////////////
// Operationen //
/////////////////

void CMapUIntToString::Add( const CMapUIntToString& otherMap )
// fügt alle Schlüssel/Werte paare einer bestehenden Map
// zu dieser hinzu
{
  POSITION pos = otherMap.GetStartPosition();
  while( pos != NULL )
  {
    UINT key;
    CString value;
    otherMap.GetNextAssoc( pos, key, value );
    SetAt( key, value );
  } // while pos
} // Add
