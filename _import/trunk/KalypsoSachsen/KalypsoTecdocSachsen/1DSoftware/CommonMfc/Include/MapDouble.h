// MapDouble.h: Schnittstelle für die Klasse CMapDouble.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPDOUBLE_H__CA5CC1C3_9304_11D6_B2F9_00104BB3E525__INCLUDED_)
#define AFX_MAPDOUBLE_H__CA5CC1C3_9304_11D6_B2F9_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// Die fehlende Double-Map in MFC
class CMapDouble : public CMap<double, double, double, double>  
{
  /////////////////
  // Operationen //
  /////////////////
public:
  void SetMins( const CMapDouble& other );
  void SetMaxs( const CMapDouble& other );
  void AddMap( const CMapDouble& other );

#ifdef _DEBUG
  void AssertValid();
  void Dump( CDumpContext& dc ) const;
#endif
};

#endif // !defined(AFX_MAPDOUBLE_H__CA5CC1C3_9304_11D6_B2F9_00104BB3E525__INCLUDED_)
