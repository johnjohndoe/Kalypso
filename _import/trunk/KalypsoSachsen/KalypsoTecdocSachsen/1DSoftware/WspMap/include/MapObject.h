// MapObject.h: Schnittstelle für die Klasse CMapObject.
//
// Die Klasse CMapObject ist eine Wrapper Klasse für 
// Objekte eines MapObject Layers
// Objekte dieser klasse repäsentieren ein zu eins
// Objekte im Layer. Initialisiert werden sie mit einem gültigen
// CMoFields Objekt welches ausgelesen wird
//
// Desweiteren gibts ein paar nützliche statische Funktionen zum 
// umsortieren von CMapObjectsArrays
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPOBJECT_H__F9816653_25E4_11D6_B2A4_00104BB3E525__INCLUDED_)
#define AFX_MAPOBJECT_H__F9816653_25E4_11D6_B2A4_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CMoFields;
class CMapObject;
class CMapObjectArray;


typedef CArray<CMapObjectArray, CMapObjectArray&> CMapObjectArrays;

class CMapObject : public CMap<CString, LPCTSTR, CComVariant, CComVariant&>
{
public:
  CMapObject() : CMap<CString, LPCTSTR, CComVariant, CComVariant&>() {};
  CMapObject( const CMapObject& other ); // CopyConstruktor
  CMapObject( CMoFields& fields );
  
  CComVariant GetAt( const CString& name );
  void AddAttributes( const CMapObject& other, bool bOverwrite );

public:
  static void SortByProfilID( const CMapObjectArray& in, CMapObjectArrays& out );
  static void AggregateArrays( const CMapObjectArrays& in, CMapObjectArray& out );
};


class CMapObjectArray : public CTypedPtrArray<CPtrArray, CMapObject*>
// Array von CMapObject-Pointern
// diese werden nur dann vernichtet, wenn es explizit gewünscht ist
{
public:
  CMapObjectArray( bool bDestroy = false ); 
  CMapObjectArray( CMapObjectArray& other ); // Copyconstruktor 
  ~CMapObjectArray();

  void operator=( CMapObjectArray& other );

private:
  bool bDestroy; // ob die Referenzierte CMapObjects zerstört werden sollen oder nicht
};

class CMapObjectIndex 
{
private:
	typedef std::map<CComVariant, CMapObject*> Index;
	Index m_index;

public:
	CMapObjectIndex( const CString& field, const CMapObjectArray& objects )
	{
		for( int i = 0; i < objects.GetSize(); i++ )
		{
			CComVariant value = objects.GetAt( i )->GetAt( field );
			m_index.insert( Index::value_type( value, objects[i] ) );
		}
	}

public:
	CMapObject* GetAt( const CComVariant& key )
	{
		Index::iterator it = m_index.find( key );
		if( it == m_index.end() )
			return 0;

		return it->second;
	}
};

#endif // !defined(AFX_MAPOBJECT_H__F9816653_25E4_11D6_B2A4_00104BB3E525__INCLUDED_)
