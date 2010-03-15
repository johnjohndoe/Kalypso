// ContextHelp.h: Schnittstelle für die Klasse CContextHelp.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_CONTEXTHELP_H__5E39BAC0_AB7E_11D5_BE84_00104BB3E525__INCLUDED_)
#define AFX_CONTEXTHELP_H__5E39BAC0_AB7E_11D5_BE84_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class IDDEWnd;
#include "MapUIntToString.h"

///////////////////////////////////////////////////////////////////////////////
// Eine Klasse zur Implementation von ContextHilfe mittels PDF - Dateien     //
// Die Klasse muss nach der Erzeugung noch mittels Init initialisiert werden //
///////////////////////////////////////////////////////////////////////////////

class CContextHelp  
{
  // Konstruktion / Destruktion
public:
  CContextHelp();
  void DeleteContents();
  BOOL Init( IDDEWnd* pDDEWnd, const CString& helpFile, const CMapUIntToString& idMap );

  // Operationen
public:
  void ShowHelp( DWORD dwData, UINT nCmd );

  // Attribute
protected:
  IDDEWnd* m_ddeWnd;
  CString m_helpFile;
  CMapUIntToString m_idMap;
}; // class CContextHelp

#endif // !defined(AFX_CONTEXTHELP_H__5E39BAC0_AB7E_11D5_BE84_00104BB3E525__INCLUDED_)
