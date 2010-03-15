// MapDouble.cpp: Implementierung der Klasse CMapDouble.
//
//////////////////////////////////////////////////////////////////////

#include "stdAfx.h"

#include "MapDouble.h"

void CMapDouble::SetMins( const CMapDouble& other )
// f�gt alle Werte einer anderen Map zu dieser hinzu. 
// Ist der Schl�ssel bereits vorhanden, wird das Minimum der beiden Werte angenommen
// Parameter:
//        const CMapDouble& other: die Schl�ssel/Werte dieser Map werden hinzugef�gt
{
  // die hinzuzuf�genden Werte durchlaufen
  POSITION pos = other.GetStartPosition();
  while( pos != NULL )
  {
    double key, newValue;
    other.GetNextAssoc( pos, key, newValue );
    
    // schauen, ob dieser Schl�ssen schon existiert,
    // falls ja, den Wert mit dem alten Minimieren
    double oldValue;
    if( Lookup( key, oldValue ) )
      newValue = min( newValue, oldValue );

    // jetzt den Wert tats�chlich hinzuf�gen
    SetAt( key, newValue );
  } // while minPos
} // AdaptMin

void CMapDouble::SetMaxs( const CMapDouble& other )
// f�gt alle Werte einer anderen Map zu dieser hinzu. 
// Ist der Schl�ssel bereits vorhanden, wird das Maxmimum der beiden Werte angenommen
// Parameter:
//        const CMapDouble& other: die Schl�ssel/Werte dieser Map werden hinzugef�gt
{
  // die hinzuzuf�genden Werte durchlaufen
  POSITION pos = other.GetStartPosition();
  while( pos != NULL )
  {
    double key, newValue;
    other.GetNextAssoc( pos, key, newValue );
    
    // schauen, ob dieser Schl�ssen schon existiert,
    // falls ja, den Wert mit dem alten Minimieren
    double oldValue;
    if( Lookup( key, oldValue ) )
      newValue = max( newValue, oldValue );

    // jetzt den Wert tats�chlich hinzuf�gen
    SetAt( key, newValue );
  } // while minPos
} // AdaptMax

void CMapDouble::AddMap( const CMapDouble& other )
// f�gt alle Werte einer anderen Mapzu dieser hinzu
// Parameter:
//        const CMapDouble& other: die Schl�ssel/Werte dieser Map werden hinzugef�gt
{
  // die hinzuzuf�genden Werte durchlaufen
  POSITION pos = other.GetStartPosition();
  while( pos != NULL )
  {
    double key, newValue;
    other.GetNextAssoc( pos, key, newValue );
    
    // jetzt den Wert hinzuf�gen
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
