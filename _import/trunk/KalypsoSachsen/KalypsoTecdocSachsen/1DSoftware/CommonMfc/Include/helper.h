// Helper.h: Schnittstelle für die Klasse CHelper.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_HELPER_H__1261EF93_673F_11D6_B2D6_00104BB3E525__INCLUDED_)
#define AFX_HELPER_H__1261EF93_673F_11D6_B2D6_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// Hilfsklasse, welche nur statische Member enthält
class CHelper  
{
public:
  static BOOL DeleteFileToDustbin( const CString& filePath );
  static void AppendMenu( HINSTANCE hInstance, HMENU hMainMenu, const int menuID );
};

// Kleine Hilfsklass zum Debuggen
class CVoidStorer
{
  // Statics //
private:
  CVoidStorer() {};

  // Non-Statics //
public:
  static void* GetData() { return m_data; };
  static void SetData( void* data ) { m_data = data; };

private:
  static void* m_data;
};


#endif // !defined(AFX_HELPER_H__1261EF93_673F_11D6_B2D6_00104BB3E525__INCLUDED_)
