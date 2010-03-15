/*! Time-stamp: <@(#)table.h   27.08.02 - 11:51:52   Belger>
 *********************************************************************
 *  @file   : table.h
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

#if !defined(AFX_TABLE_H__97111D35_7943_11D6_B2E4_00104BB3E525__INCLUDED_)
#define AFX_TABLE_H__97111D35_7943_11D6_B2E4_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CPlotterView;
class CPlotterDoc;

#include "drawobj.h"

/*!
CLASS
    CTable 

    Diese Klasse hält alle Draw-Objekte, welche zur Tabelle gehören ( nur die
    Tabelle, nicht die Legende )

USAGE
*/
class CTable : public CDrawObjListArray  
{
  ////////////////
  // Konstanten //
  ////////////////
public:
  enum TableFormat { allLines, onlyLinesWithText, allValues };
  static UINT LeftOffset;

  ////////////////////////////////
  // Konstruktion / Destruktion //
  ////////////////////////////////
public:
  CTable();

  /////////////////
  // Operationen //
  /////////////////
public:
  CIntIRect Update( CPlotterDoc* pDoc, CView* pView, const double dFrom, const double dTo, const CDoubleIRect& rectProfile, const UINT profilWidth, const CIntPoint& basePoint,  const int nXValueFormat, const int nYValueFormat, const CUIntArray& tableHeights );
  CIntIRect UpdateRowSize( int index,  const double dFrom, const double dTo, CPlotterDoc* pDoc, CView* pView, const int nXValueFormat, const int nYValueFormat );

    
  void GetDataBlocks( CMap<int, int, int, int>& dbMap ) const;
  void GetDbRange( int dbType, double* from, double* to ) const;
  int GetDbIndex( int dbType ) const;

  ///////////////
  // Attribute //
  ///////////////
public:
  TableFormat GetTableFormat() const { return m_nTableFormat; };
  void SetTableFormat( TableFormat tF ) { m_nTableFormat = tF; };

protected:
  int m_leftoffset, m_rightoffset;

  TableFormat m_nTableFormat;

private:
  void SetOffset( CMap<double, double, int, int>*, CMap<double, double, int, int>*, const double, const int );
};

#endif // !defined(AFX_TABLE_H__97111D35_7943_11D6_B2E4_00104BB3E525__INCLUDED_)
