/*! Time-stamp: <@(#)DrawLayer.h   09.03.03 - 12:44:44   Belger>
 *********************************************************************
 *  @file   : DrawLayer.h
 *
 *  Author  : Belger                              Date: 09.03.03
 *
 *  Purpose : Declaration of class CDrawLayer
 *
 *********************************************************************
 */

#if !defined(AFX_DRAWLAYER_H__45ED1D14_4FA9_11D7_B399_00104BB3E525__INCLUDED_)
#define AFX_DRAWLAYER_H__45ED1D14_4FA9_11D7_B399_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "drawObj.h"

 /*!
 * @class CDrawLayer
 *
 * Diese Klasse stellt einen Layer im CAD System dar, besser noch: eine Gruppe von Zeichenobjekten
 *
 * @usage
 * 
 * Jeder Layer hat einen Namen und ein paar Flags, die die Art des Layers beschreiben
 * Die (n-n) Zuordnung zwischen Layern und Zeichenobjekten (CDrawObj) findet in einer eigenen
 * Klasse statt (CDrawObjLayerAssoc).
 * 
*/
class CDrawLayer : public CDrawObjList
{
  //
  // Members
  //
private:
  /// Der Name des Layers, dient als ID
  CString m_name;

  /// die Flags dieses Layers
  unsigned long m_flags;

  //
  // Typdeklarationen
  //
public:
  /// Aufzählung der möglichen Flags
  enum flags
  {
    /// ist sichtbar, d.h .wird beim Zeichnen berücksichtigt
    visible_flag = 0x2L
  };

  //
  // Konstruktion / Destruktion
  //
public:
  /// leerer Konstruktor, wird nur fürs serialisieren benötigt
  CDrawLayer() : m_flags( 0 ) {};

  /*!
  * Standardkonstruktor
  *
  * @param name Der Name des Layers, sollte eindeutig sein
  */
  CDrawLayer( const CString& name ) : m_name( name ), m_flags( 0 ) {};

  /*!
  * Der Copy-Konsturktor
  */
  CDrawLayer( const CDrawLayer& other ) : m_name( other.m_name ), m_flags( other.m_flags ) {};

  //
  // Accessors
  //
public:
  /// Gibt den Namen des Layers zurück
  const CString& GetName() const { return m_name; };

  /*!
  * Setzt die Flags, bereits gesetzte Flags bleiben erhalten
  *
  * @param flags Eine &-Kombination der zu setztenden Flags
  */
  void SetFlags( const long flags ) { m_flags |= flags; };

  /*!
  * Löscht Flags
  *
  * @param Eine &-Kombination der zu löschenden Flags, alle anderen bleibe nerhalten
  */
  void UnsetFlags( const long flags ) { m_flags &= ~flags; };

 /*! Gibt zurück, ob dieser Layer sichtbar ist, falls ja, wird er von CDrawDoc
  * gezeichnet
  * @return true, wenn der Layer sichtbar ist
  */
  bool IsVisible() const { return ( m_flags & visible_flag ) != 0L; };

  //
  // Serialisierung
  //
public:
  /// Serialisierung eines Layers in ein CArchiv
  void Serialize( CArchive& ar );

  //
  // Operatoren
  //
private:
  /// Zuweisungsoperator, soll nie benutzt werden, deswegen privat
  CDrawLayer& operator=( const CDrawLayer& other ) {};

};

/// hilfsdeklaration für Mengen von Layer'n
typedef CArray<CDrawLayer*, CDrawLayer*> CDrawLayerArray;

#endif // !defined(AFX_DRAWLAYER_H__45ED1D14_4FA9_11D7_B399_00104BB3E525__INCLUDED_)
