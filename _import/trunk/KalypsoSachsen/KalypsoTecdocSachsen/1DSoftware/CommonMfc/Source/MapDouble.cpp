// MapDouble.cpp: Implementierung der Klasse CMapDouble.
//
//////////////////////////////////////////////////////////////////////

#include "stdAfx.h"

#include "MapDouble.h"

void CMapDouble::SetMins( const CMapDouble& other )
// fügt alle Werte einer anderen Map zu dieser hinzu. 
// Ist der Schlüssel bereits vorhanden, wird das Minimum der beiden Werte angenommen
// Parameter:
//        const CMapDouble& other: die Schlüssel/Werte dieser Map werden hinzugefügt
{
  // die hinzuzufügenden Werte durchlaufen
  POSITION pos = other.GetStartPosition();
  while( pos != NULL )
  {
    double key, newValue;
    other.GetNextAssoc( pos, key, newValue );
    
    // schauen, ob dieser Schlüssen schon existiert,
    // falls ja, den Wert mit dem alten Minimieren
    double oldValue;
    if( Lookup( key, oldValue ) )
      newValue = min( newValue, oldValue );

    // jetzt den Wert tatsächlich hinzufügen
    SetAt( key, newValue );
  } // while minPos
} // AdaptMin

void CMapDouble::SetMaxs( const CMapDouble& other )
// fügt alle Werte einer anderen Map zu dieser hinzu. 
// Ist der Schlüssel bereits vorhanden, wird das Maxmimum der beiden Werte angenommen
// Parameter:
//        const CMapDouble& other: die Schlüssel/Werte dieser Map werden hinzugefügt
{
  // die hinzuzufügenden Werte durchlaufen
  POSITION pos = other.GetStartPosition();
  while( pos != NULL )
  {
    double key, newValue;
    other.GetNextAssoc( pos, key, newValue );
    
    // schauen, ob dieser Schlüssen schon existiert,
    // falls ja, den Wert mit dem alten Minimieren
    double oldValue;
    if( Lookup( key, oldValue ) )
      newValue = max( newValue, oldValue );

    // jetzt den Wert tatsächlich hinzufügen
    SetAt( key, newValue );
  } // while minPos
} // AdaptMax

void CMapDouble::AddMap( const CMapDouble& other )
// fügt alle Werte einer anderen Mapzu dieser hinzu
// Parameter:
//        const CMapDouble& other: die Schlüssel/Werte dieser Map werden hinzugefügt
{
  // die hinzuzufügenden Werte durchlaufen
  POSITION pos = other.GetStartPosition();
  while( pos != NULL )
  {
    double key, newValue;
    other.GetNextAssoc( pos, key, newValue );
    
    // jetzt den Wert hinzufügen
    SetAt( key, newValue );
  } // while minPos
} // AddMap


#ifdef _DEBUG

void CMapDouble::AssertValid()
{
  CMap<double, double, double, double>::AssertValid();
} // AssertValid

void CMapDouble::Dump( CDumpContext& dc ) const
{
  dc << "A CMapDouble with " << GetCount() << " Elements: \n";
  POSITION pos = GetStartPosition();
  while( pos != NULL )
  {
    double key, value;
    GetNextAssoc( pos, key, value );
    dc << key << " -> " << value << "\n";
  } // while minPos
} // Dump

#endif
