/*! Time-stamp: <@(#)assocNN.h   09.03.03 - 13:53:32   Belger>
 *********************************************************************
 *  @file   : assocNN.h
 *
 *  Author  : Belger                              Date: 09.03.03
 *
 *  Purpose : Declaration of template AssocNN
 *
 *********************************************************************
 */

#if !defined(AFX_DRAWOBJLAYERASSOC_H__7D2245E3_507F_11D7_B39A_00104BB3E525__INCLUDED_)
#define AFX_DRAWOBJLAYERASSOC_H__7D2245E3_507F_11D7_B39A_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <list>
#include <map>
#include <algorithm>

namespace BCE
{
/*!
 * @class AssocNN
 * 
 * Vorlagenklasse für die Moddelierung von bidirektionalen N-N Beziehungen
 * 
 * Diese Vorlagenklasse erhält zwei Vorlagenargumente: S und T.
 * Die Klasser erlaubt zwei Objekte vom Typ S und T in Relation zu stellen oder nicht.
 * Der Benutzer kann dann abfragen, welceh Elemente T zu S in Relation stehen und andersrum.
*/
template<typename First, typename Second>
class AssocNN
{
  //
  // Typdeklarationen
  //
public:
  typedef std::list<First> FirstContainer;
  typedef FirstContainer::iterator FirstIterator;

  typedef std::list<Second> SecondContainer;
  typedef SecondContainer::iterator SecondIterator;

private:
  typedef std::map<First, SecondContainer*> FirstAssocMap;
  typedef FirstAssocMap::iterator FirstAssocIterator;

  typedef std::map<Second, FirstContainer*> SecondAssocMap;
  typedef SecondAssocMap::iterator SecondAssocIterator;

  //
  // Konstruktion / Destruktion
  //
public:

  /*!
   * Destruktor.
   *
   * Zerstört alle erzeugten Objektlisten
   */
  ~AssocNN()
  {
    for( FirstAssocIterator fstIter = m_firstToSecondAssoc.begin(); fstIter != m_firstToSecondAssoc.end(); fstIter++ )
      delete fstIter->second;

    for( SecondAssocIterator sndIter = m_secondToFirstAssoc.begin(); sndIter != m_secondToFirstAssoc.end(); sndIter++ )
      delete sndIter->second;
  };

public:
  void AddRelation( First first, Second second )
  {
    // die zugeordneten Second's
    SecondContainer* sndCont = _GetAssocToFirst( first );

    // wenn das Element noch nicht vorhanden ist, jetzt einfügen
    SecondIterator sndIt = std::find( sndCont->begin(), sndCont->end(), second );
    if( sndIt == sndCont->end() )
      sndCont->push_back( second );

    // jetzt andersrum

    // die zugeordneten Fst's
    FirstContainer* fstCont = _GetAssocToSecond( second );

    // wenn das Element noch nicht vorhanden ist, jetzt einfügen
    FirstIterator fstIt = std::find( fstCont->begin(), fstCont->end(), first );
    if( fstIt == fstCont->end() )
      fstCont->push_back( first );
  };

  void RemoveRelation( const First first, const Second second )
  {
    // die zugeordneten Second's
    SecondContainer* sndCont = _GetAssocToFirst( first );
    sndCont->remove( second ); // Element aus Liste nehmen, wenn vorhanden

    // jetzt andersrum

    // die zugeordneten Fst's
    FirstContainer* fstCont = _GetAssocToSecond( second );
    fstCont->remove( first ); // Element aus Liste nehmen, wenn vorhanden
  };

  void RemoveFirst( const First& first );
  void RemoveSecond( const Second& second );

  const SecondContainer* GetAssocToFirst( First first )
  {
    return _GetAssocToFirst( first );
  };

  const FirstContainer* GetAssocToSecond( Second second )
  {
    return _GetAssocToSecond( second );
  };

private:

  /*!
   * Gibt die Liste der zu einem First-Element assoziierten Elemente zurück
   * Es wird in jedem Fall eine Liste zurückgegeben, notfalls hier auch eine erzeugt
   *
   * @param first : zu diesem element assozierte Elemente werden gesucht
   *
   * @return SecondContainer*  : die assoziierten Elemente
   */
  SecondContainer* _GetAssocToFirst( First first )
  {
    FirstAssocIterator fstFind = m_firstToSecondAssoc.find( first );

    SecondContainer* sndCont = 0;
    if( fstFind == m_firstToSecondAssoc.end() )
    {
      // neuen Eintrag erzeugen
      sndCont = new SecondContainer();
      m_firstToSecondAssoc[first] = sndCont;
    }
    else
      sndCont = fstFind->second;

    return sndCont;
  };

  /*!
   * Gibt die Liste der zu einem Second-Element assoziierten Elemente zurück
   * Es wird in jedem Fall eine Liste zurückgegeben, notfalls hier auch eine erzeugt
   *
   * @param second : zu diesem Element assozierte Elemente werden gesucht
   *
   * @return FirstContainer*  : die assoziierten Elemente
   */
  FirstContainer* _GetAssocToSecond( Second second )
  {
    SecondAssocIterator sndFind = m_secondToFirstAssoc.find( second );

    FirstContainer* fstCont = 0;
    if( sndFind == m_secondToFirstAssoc.end() )
    {
      // neuen Eintrag erzeugen
      fstCont = new FirstContainer();
      m_secondToFirstAssoc[second] = fstCont;
    }
    else
      fstCont = sndFind->second;

    return fstCont;
  };

private:
  FirstAssocMap m_firstToSecondAssoc;
  SecondAssocMap m_secondToFirstAssoc;
}; // class AssocNN

class RelationNotFoundException{};
} // namespace BCE

#endif // !defined(AFX_DRAWOBJLAYERASSOC_H__7D2245E3_507F_11D7_B39A_00104BB3E525__INCLUDED_)



/*
void CDrawObjLayerAssoc::RemoveObject( const long objID )
{
  CDrawLayerNameSet* pLayers = GetDrawLayers( objID );
  POSITION pos = pLayers->GetHeadPosition();
  while( pos )
  {
    CString name = pLayers->GetNext( pos );
    RemoveRelation( objID, name );
  }
};

void CDrawObjLayerAssoc::RemoveLayer( const CString& name )
{
  CDrawObjIDSet* pObjs = GetDrawObjects( name );
  POSITION pos = pObjs->GetHeadPosition();
  while( pos )
  {
    long id = pObjs->GetNext( pos );
    RemoveRelation( id, name );
  }
};

CDrawObjLayerAssoc::CDrawLayerNameSet* CDrawObjLayerAssoc::GetDrawLayers( const long objID )
{
  CDrawLayerNameSet* pLayers = NULL;
  m_drawObj2LayerMap.Lookup( objID, pLayers );
  return pLayers;
};

CDrawObjLayerAssoc::CDrawObjIDSet* CDrawObjLayerAssoc::GetDrawObjects( const CString& name )
{
  CDrawObjIDSet* pObjs = NULL;
  m_drawLayer2ObjMap.Lookup( name, pObjs );
  return pObjs;
};


void CDrawObjLayerAssoc::Serialize( CArchive& ar )
{
  // ich vertraue nicht der MFC-Collection Serialisierung, also alles selbst machen
  CObject::Serialize( ar );

	if( ar.IsStoring() )
	{
    ar << int( 0 ); // Versionsummer dieses Serialisierungsschemas

    // Layer->Obj
    ar << m_drawLayer2ObjMap.GetCount();
    POSITION layerPos = m_drawLayer2ObjMap.GetStartPosition();
    while( layerPos )
    {
      CDrawObjIDSet* pObjSet;
      CString layerName;

      m_drawLayer2ObjMap.GetNextAssoc( layerPos, layerName, pObjSet );

      ar << layerName;
      ar << pObjSet->GetCount();
      POSITION setPos = pObjSet->GetHeadPosition();
      while( setPos )
      {
        long id = pObjSet->GetNext( setPos );
        ar << id;
      }
    }

    // Layer->Obj
    ar << m_drawObj2LayerMap.GetCount();
    POSITION objPos = m_drawObj2LayerMap.GetStartPosition();
    while( objPos )
    {
      CDrawLayerNameSet* pLayerSet;
      long id;

      m_drawObj2LayerMap.GetNextAssoc( objPos, id, pLayerSet );

      ar << id;
      ar << pLayerSet->GetCount();
      POSITION setPos = pLayerSet->GetHeadPosition();
      while( setPos )
      {
        CString layerName = pLayerSet->GetNext( setPos );
        ar << layerName;
      }
    }
	}
	else
	{
    int nVersion;
    ar >> nVersion;

    DeleteContents();

    switch( nVersion )
    {
    case 0:
      {
        // Layer->Obj
        int layerCount = 0;
        ar >> layerCount;
        for( int layerN = 0; layerN < layerCount; layerN++ )
        {
          CString layerName;
          ar >> layerName;

          CDrawObjIDSet* pObjSet = new CDrawObjIDSet();
          m_drawLayer2ObjMap[layerName] = pObjSet;
          
          int objCount = 0;
          ar >> objCount;
          for( int objN = 0; objN < objCount; objN++ )
          {
            long id;
            ar >> id;
            pObjSet->AddTail( id );
          }
        } // for layerN


        // Obj->Layer
        int objCount = 0;
        ar >> objCount;
        for( int objN = 0; objN < objCount; objN++ )
        {
          long objID;
          ar >> objID;

          CDrawLayerNameSet* pLayerSet = new CDrawLayerNameSet();
          m_drawObj2LayerMap[objID] = pLayerSet;
          
          int layerCount = 0;
          ar >> layerCount;
          for( int layerN = 0; layerN < layerCount; layerN++ )
          {
            CString layerName;
            ar >> layerName;
            pLayerSet->AddTail( layerName );
          }
        } // for objN
      } // case 0
      break;
      
    default:
      AfxThrowArchiveException( CArchiveException::badSchema );
      break;
    }
  }
};
*/