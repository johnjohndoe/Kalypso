/*! Time-stamp: <@(#)MarkMapObject.h   22.10.02 - 15:36:59   Belger>
 *********************************************************************
 *  @class   : CMarkMapObject
 *
 *  Author  : Belger                              Date: 22.10.02
 *
 *  Purpose : Markiert ein Objekt in der Karte auf eine bestimmte Art und
 *            Weise. Nach löschen der Klasse wird das objekt automatisch
 *            wieder de-markiert.
 *
 *********************************************************************
 */

#if !defined(AFX_MARKMAPOBJECT_H__B2A41D83_E5B3_11D6_B341_00104BB3E525__INCLUDED_)
#define AFX_MARKMAPOBJECT_H__B2A41D83_E5B3_11D6_B341_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CMoMap;
class CMapLayer;
class CMoGeoEvent;

class CMarkMapObject  
{
public:
  CMarkMapObject::CMarkMapObject( LPDISPATCH dispObject, CMapLayer* mapLayer, CMoRectangle& extent, CMoTrackingLayer trackLayer, const int symbolID );
	virtual ~CMarkMapObject();

private:
  CMoGeoEvent* m_pMark; // die Markierung
  CMoTrackingLayer m_trackLayer;
};

#endif // !defined(AFX_MARKMAPOBJECT_H__B2A41D83_E5B3_11D6_B341_00104BB3E525__INCLUDED_)
