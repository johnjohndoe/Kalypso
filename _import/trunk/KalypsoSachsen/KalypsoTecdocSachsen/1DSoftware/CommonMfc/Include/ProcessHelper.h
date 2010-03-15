// ProcessHelper.h: Schnittstelle für die Klasse CProcessHelper.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PROCESSHELPER_H__586A12E3_940F_11D6_B2FA_00104BB3E525__INCLUDED_)
#define AFX_PROCESSHELPER_H__586A12E3_940F_11D6_B2FA_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

////////////////////////////////////////////////////////
// Eine Hilfsklasse zum Umgang mit externen Prozessen //
////////////////////////////////////////////////////////
class CProcessHelper  
{
  /////////////////
  // Operationen //
  /////////////////
public:
  static void StartExternProcess( const CString& appName );
};

#endif // !defined(AFX_PROCESSHELPER_H__586A12E3_940F_11D6_B2FA_00104BB3E525__INCLUDED_)
