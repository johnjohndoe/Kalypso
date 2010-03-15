/*! Time-stamp: <@(#)profil.h   27.08.02 - 14:24:59   Belger>
 *********************************************************************
 *  @file   : profil.h
 *
 *  Project : WSPWIN
 *
 *  Package : WSPWIN Plotter
 *
 *  Company : BCE
 *
 *  Author  : Belger                              Date: 27.08.02
 *
 *  Purpose : Declaration of class 
 *
 *********************************************************************
 */

#if !defined(AFX_PROFIL_H__217FB233_7D0B_11D6_B2E7_00104BB3E525__INCLUDED_)
#define AFX_PROFIL_H__217FB233_7D0B_11D6_B2E7_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "..\..\commonMfc\include\rect.h"

#include "drawobj.h"
#include "plotdoc.h"
#include "plotArranger.h"

class CProfil : public CDrawObjList
{
  ////////////////////////////////
  // Typdefinitionen            //
  ////////////////////////////////
public:
	enum Format { minStations, stationsToGround, maxStations };

  ////////////////
  // Konstanten //
  ////////////////
public:
  static UINT LCOMMENT_DIST;

  ////////////////////////////////
  // Konstruktion / Destruktion //
  ////////////////////////////////
public:
  CProfil();


  /////////////////
  // Operationen //
  /////////////////
public:
  void AddObject( CDrawObj* pObj );
  BOOL RemoveObject( CDrawObj* pObj );

  CDoubleIRect CalcProfilSizes( CView* pView );

  ///////////////
  // Attribute //
  ///////////////

public:
  Format GetFormat() const { return m_nProfilFormat; };
  void SetFormat( const Format f ) { m_nProfilFormat = f; };

  double GetFrom() const { return m_dFrom; };
  void SetFrom( const double dFrom ) { m_dFrom = dFrom; };

  double GetTo() const { return m_dTo; };
  void SetTo( const double dTo ) { m_dTo = dTo; };
  
  double GetHeight() const { return m_dHeight; };
  void SetHeight( const double dHeight ) { m_dHeight = dHeight; };

  double GetMaxHeight() const { return m_dMaxHeight; };
  void SetMaxHeight( const double dMaxHeight ) { m_dMaxHeight = dMaxHeight; };

  double GetLowPoint() const { return m_dLowPoint; };
  void SetLowPoint( const double dLowPoint ) { m_dLowPoint = dLowPoint; };

  CDoubleIRect GetTotalRect() { return m_rectTotal; };
  void SetTotalRect( const CDoubleIRect& rect ) { m_rectTotal = rect; };

  CDoubleIRect GetVisibleRect() const { return m_rectVisible; };
  void SetVisibleRect( const CDoubleIRect& rect ) { m_rectVisible = rect; };

  CPlotArranger::Align GetAlign() const { return m_align; };
  void SetAlign( const CPlotArranger::Align algn ) { m_align = algn; };

  BOOL GetAutoAnfang() const { return m_bAutoAnfang; };
  void SetAutoAnfang( const BOOL bNew ) { m_bAutoAnfang = bNew; };

  BOOL GetAutoEnde() const { return m_bAutoEnde; };
  void SetAutoEnde( const BOOL bNew ) { m_bAutoEnde = bNew; };

  BOOL GetAutoHeight() const { return m_bAutoHeight; };
  void SetAutoHeight( const BOOL bNew ) { m_bAutoHeight = bNew; };

  CSize GetRealSize( double xScale, double yScale, CView* pView );

  void SetRangeDbType( const int dbType ) { m_rangeDbType = dbType; }
  int GetRangeDbType() const { return m_rangeDbType; }

  void GetDbRange( int dbType, double& from, double& to ) const;

  void GetDataBlocks( CArray<int, int>& dbArray ) const;

private:
  Format m_nProfilFormat;
  CPlotArranger::Align m_align;

  double m_dFrom, m_dTo, m_dHeight; // rechte, linke und untere Begrenzung des Profils
  int m_rangeDbType; // nur den durch diesen Datenblock gegebene Bereich wird gezeigt
  double m_dMaxHeight;	// maximum for Bezugshoehe

  double m_dLowPoint;			// x-coord of lowest point in Gelaendehoehe

  BOOL m_bAutoAnfang, m_bAutoEnde, m_bAutoHeight; // ob linke, rechte und untere Begrenzung automatisch gewählt werden sollen

  CDoubleIRect m_rectTotal;   // die Gesamtgrösse des Profils
  CDoubleIRect m_rectVisible; // die sichtbare Grösse des Profils
};


#endif // !defined(AFX_PROFIL_H__217FB233_7D0B_11D6_B2E7_00104BB3E525__INCLUDED_)
