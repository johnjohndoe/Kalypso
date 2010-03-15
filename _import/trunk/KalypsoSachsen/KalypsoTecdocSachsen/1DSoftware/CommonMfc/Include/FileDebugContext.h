// FileDebugContext.h: Schnittstelle für die Klasse CFileDebugContext.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_FILEDEBUGCONTEXT_H__5A7B1D53_7791_11D6_B2E2_00104BB3E525__INCLUDED_)
#define AFX_FILEDEBUGCONTEXT_H__5A7B1D53_7791_11D6_B2E2_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


// Die Klasse stellt verschiedene Dateibasierte Debugkontexte zur Verfügung,
// welche sich anhand eines Namens auswählen lassen
// Mal wieder eine abart des Singleton-Patterns
class CFileDebugContext : public CDumpContext  
{
  // die innere Klasse, welche die Daten hält
  static class CHelpMap : public CTypedPtrMap<CMapStringToPtr, CString, CFileDebugContext*>
  {
  public:
    ~CHelpMap()
    {
      POSITION pos = GetStartPosition();
      while( pos != NULL )
      {
        CString name;
        CFileDebugContext* pContext;
        GetNextAssoc( pos, name, pContext );
        delete pContext;
      } // while pos
    }; // Destruktor
  }; // inner Class CHelpMap

  // Konstruktion
public:
  ~CFileDebugContext();

private:
	CFileDebugContext( CFile* pFile );
  
  // Members
  CFile* m_pFile;

  // Statisches
public:
  static CFileDebugContext* CreateInstance( const CString& name, const CString& fileName );
  static CFileDebugContext* GetInstance( const CString& name );
  static BOOL DestroyInstance( const CString& name );

private:
  static CHelpMap theContexts;

};


#endif // !defined(AFX_FILEDEBUGCONTEXT_H__5A7B1D53_7791_11D6_B2E2_00104BB3E525__INCLUDED_)
