// MapObject.cpp: Implementierung der Klasse CMapObject.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "commonMfc/include/variant_helper.h"
#include "mapHelper.h"

#include "MapObject.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CMapObject::CMapObject( CMoFields& fields )
// liest alle Felder von Fields aus und fügt sie als
// festgelegte Daten m_field hinzu
{
  if( LPDISPATCH(fields) )
  {
    FOR_EACH_IN( CMoField, field, fields )
      if( LPDISPATCH(field) )
      {
        CString name = field.GetName();
        CComVariant var = CComVariant( field.GetValue() );
        SetAt( name, var );
      }
      END_FOR;
  }
}

CComVariant CMapObject::GetAt( const CString& name )
{
  CComVariant value;
  Lookup( name, value );
  
  return value;
} // GetAt

void CMapObject::AddAttributes( const CMapObject& other, bool bOverwrite )
// Fügt die Attribute eines anderen Objektes zu denen dieses Objekt zu.
// Parameter:
//      const CMapObject& other: das andere Objekt
//      bool bOverwrite: falls true, werden vorhandene, gleichlautende überschrieben, 
//                        bei false nicht
{
  POSITION pos = other.GetStartPosition();
  CComVariant value;
  CComVariant dummyVal;
  CString name;
  while( pos )
  {
    other.GetNextAssoc( pos, name, value );
    if( bOverwrite || !Lookup( name, dummyVal ) )
      SetAt( name, value );
  }; // while pos
}; // AddAttributes

void CMapObject::SortByProfilID( const CMapObjectArray& in, CMapObjectArrays& out )
// erstellt ein Array von Arrays von MapObjects
// Der Index des Arrays entspricht der ProfilID aller MapObjects in diesem Array
// Parameter:
//        const CMapObjectArray& in: ein Array von MapObjects, die ein Feld namens "ProfilID" haben sollten
//        CMapObjectArrays& out: dieses Array wird erst geleert und dann mit 
//                          Arrays von CMapObjects gefüllt. Alle Objekte innerhalb eines
//                          Arrays haben die gleiche ProfilID, die ID ihres Arrays
{
  for( int i = 0; i < in.GetSize(); i++ )
  {
    const int profilID = COleVariantEx( in[i]->GetAt( MO2_FIELD_PROFILID ) );
    if( profilID + 1 > out.GetSize() )
      out.SetSize( profilID + 1 );
    out[profilID].Add( in[i] );
  }; // for i
}; // SortByProfilID

void CMapObject::AggregateArrays( const CMapObjectArrays& in, CMapObjectArray& out )
// erstellt aus einem CMapObjectsArrays ein CMapObjectsArray indem es alles
// Objekte aus den einzelnen Arrays in ein grossen zusammenwirft
// Parameter:
//      const CMapObjectArrays& in: diese Arrays werden ausgelesen
//      const CMapObjectArray& out: hier hinein werden alle Objekte abgelegt
{
  for( int i = 0; i < in.GetSize(); i++ )
  {
    for( int j = 0; j < in[i].GetSize(); j++ )
      out.Add( in[i][j] );
  } // for i
} // AggregateArrays

CMapObjectArray::CMapObjectArray( bool bDestroy /* = false */ ) : CTypedPtrArray<CPtrArray, CMapObject*>()
{
  this->bDestroy = bDestroy;
};

CMapObjectArray::CMapObjectArray( CMapObjectArray& other )
{
  *this = other;
}

CMapObjectArray::~CMapObjectArray()
{
  if( bDestroy )
  {
    for( int i = 0; i < GetSize(); i++ )
    {
      CMapObject* ob = GetAt( i );
      if( ob != NULL )
        delete ob;
    } // for i
  }
}

void CMapObjectArray::operator=( CMapObjectArray& other )
{
  // kopiert die Referenzen
  bDestroy = false; // dieses ist eine Kopie, also darf auf keinen Fall gelöscht werden
  for( int i = 0; i < other.GetSize(); i++ )
    Add( other[i] );
} // operator=
