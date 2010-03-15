// MapUINTToString.h: Schnittstelle für die Klasse CMapUINTToString.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPUINTTOSTRING_H__586A12E5_940F_11D6_B2FA_00104BB3E525__INCLUDED_)
#define AFX_MAPUINTTOSTRING_H__586A12E5_940F_11D6_B2FA_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CMapUIntToString : public CMap<UINT, UINT, CString, LPCTSTR>
{
  /////////////////
  // Operationen //
  /////////////////
public:
  void Add( const CMapUIntToString& otherMap );
};

#endif // !defined(AFX_MAPUINTTOSTRING_H__586A12E5_940F_11D6_B2FA_00104BB3E525__INCLUDED_)
